;;; agent-shell-claude-status.el --- Claude Code session status in the header -*- lexical-binding: t; -*-

;;; Commentary:

;; Fork-only (demoj1).  claude-agent-acp does not map the CLI's
;; `post_turn_summary', `task_summary', `permission_check_status' and
;; `bridge_state' frames to ACP and logs them as "Unexpected case" on stderr
;; instead (agentclientprotocol/claude-agent-acp#1532).  We ask for them raw
;; via `_meta.claudeCode.emitRawSDKMessages', show the turn summary under its
;; response and the rest in the header, and
;; drop the stderr noise from Notices.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(declare-function agent-shell--update-header-and-mode-line "agent-shell")
(declare-function agent-shell-prompt-ranges "agent-shell")
(declare-function shell-maker-busy "shell-maker")
(declare-function agent-shell-reload "agent-shell")
(declare-function agent-shell--active-requests-p "agent-shell")
(declare-function agent-shell--state "agent-shell")

(defgroup agent-shell-claude-status nil
  "Claude Code session status in agent-shell's header."
  :group 'agent-shell)

(defcustom agent-shell-claude-status-max-width 60
  "Maximum width of each status text in the header."
  :type 'integer
  :group 'agent-shell-claude-status)

(defface agent-shell-claude-status-face '((t :inherit shadow))
  "Face of the task and completed status."
  :group 'agent-shell-claude-status)

(defface agent-shell-claude-status-attention-face '((t :inherit warning))
  "Face of a status that needs the user."
  :group 'agent-shell-claude-status)

(defface agent-shell-claude-status-failed-face '((t :inherit error))
  "Face of a failed turn."
  :group 'agent-shell-claude-status)

(defcustom agent-shell-claude-status-rate-limit-threshold 75
  "Show the rate limit in the header from this percent of utilization."
  :type 'integer
  :group 'agent-shell-claude-status)

(defface agent-shell-claude-status-suggestion-face
  '((t :inherit (completion-preview shadow)))
  "Face of the predicted next prompt."
  :group 'agent-shell-claude-status)

(defconst agent-shell-claude-status--raw-filters
  (append (mapcar (lambda (subtype) `((type . "system") (subtype . ,subtype)))
                  '("post_turn_summary" "task_summary" "permission_check_status"
                    "bridge_state" "session_state_changed"))
          '(((type . "rate_limit_event"))
            ((type . "prompt_suggestion"))))
  "SDK messages requested raw from the adapter.")

;; The adapter writes all its logs to stderr (stdout is the ACP channel), and
;; agent-shell shows any stderr as Notices.
(defcustom agent-shell-claude-status-ignored-stderr-regexps
  (list (concat "\\`[ \t]*Unexpected case: {\"type\":\"system\",\"subtype\":\""
                (regexp-opt '("post_turn_summary" "task_summary"
                              "permission_check_status" "bridge_state"))
                "\"")
        "\\`[ \t]*\\[idle-compaction\\] ")
  "Adapter stderr lines not shown in Notices."
  :type '(repeat regexp)
  :group 'agent-shell-claude-status)

(defvar-local agent-shell-claude-status--task nil
  "Current `task_summary' text.")

(defvar-local agent-shell-claude-status--checking nil
  "Tool use ids whose permission check is in progress.")

(defvar-local agent-shell-claude-status--bridge nil
  "Last `bridge_state' message of Remote Control.")

(defvar agent-shell-claude-status--rate-limit nil
  "Last `rate_limit_info'; global, the limit is the account's.")

(defvar-local agent-shell-claude-status--suggestion nil
  "Predicted next prompt, dropped once the user types or submits.")

(defvar-local agent-shell-claude-status--suggestion-overlay nil)

(defvar-local agent-shell-claude-status--last-summary nil
  "Last `post_turn_summary' message and its arrival time, as (TIME . MESSAGE).")

(defun agent-shell-claude-status-session-meta ()
  "Return the `claudeCode' session meta entry asking for the raw frames."
  (cons 'emitRawSDKMessages (vconcat agent-shell-claude-status--raw-filters)))

(defun agent-shell-claude-status--truncate (text)
  "TEXT on one line, cut to `agent-shell-claude-status-max-width'."
  (truncate-string-to-width (replace-regexp-in-string "[\n\t ]+" " " (string-trim text))
                            agent-shell-claude-status-max-width nil nil "…"))

(defun agent-shell-claude-status--segment (icon text face)
  "Status segment: ICON and TEXT in FACE, full TEXT in the tooltip."
  (propertize (concat icon " " (agent-shell-claude-status--truncate text))
              'face face 'help-echo text))

(defun agent-shell-claude-status-header ()
  "Status text for the current shell's header, or nil when there is none."
  (let* ((bridge agent-shell-claude-status--bridge)
         (segments
          (delq
           nil
           (list
            (when bridge
              (let ((state (map-elt bridge 'state))
                    (detail (map-elt bridge 'detail)))
                (propertize
                 (pcase state
                   ("connected" "📡")
                   ("failed" "📡✗")
                   (_ "📡…"))
                 'face (if (equal state "failed")
                           'agent-shell-claude-status-failed-face
                         'agent-shell-claude-status-face)
                 'help-echo (concat "Remote Control: " state
                                    (when (stringp detail) (concat " — " detail))))))
            (when agent-shell-claude-status--checking
              (agent-shell-claude-status--segment "🔍" "проверка разрешения…"
                                                  'agent-shell-claude-status-attention-face))
            (when agent-shell-claude-status--task
              (agent-shell-claude-status--segment "⚙" agent-shell-claude-status--task
                                                  'agent-shell-claude-status-face))
            (agent-shell-claude-status--rate-limit-segment)))))
    (when segments
      (string-join segments " "))))

(defun agent-shell-claude-status--rate-limit-segment ()
  "Header segment for the account's rate limit, nil while it is far."
  (when-let* ((info agent-shell-claude-status--rate-limit))
    (let* ((status (map-elt info 'status))
           (utilization (map-elt info 'utilization))
           (percent (and (numberp utilization) (round (* 100 utilization))))
           (resets (map-elt info 'resetsAt))
           (resets (and (numberp resets) (if (> resets 1e11) (/ resets 1000) resets)))
           (reset-text
            (when resets
              (format-time-string
               (if (equal (format-time-string "%F" resets) (format-time-string "%F"))
                   "%H:%M" "%d.%m %H:%M")
               resets)))
           (rejected (equal status "rejected")))
      (when (or rejected
                (equal status "allowed_warning")
                (and percent (>= percent agent-shell-claude-status-rate-limit-threshold)))
        (propertize
         (concat (if rejected "⛔ лимит" (concat "⚠ " (if percent (format "%d%%" percent) "лимит")))
                 (when reset-text (concat " · сброс " reset-text)))
         'face (if rejected 'agent-shell-claude-status-failed-face
                 'agent-shell-claude-status-attention-face)
         'help-echo (format "Rate limit: %s, %s" status
                            (or (map-elt info 'rateLimitType) "?")))))))

(defun agent-shell-claude-status--update-all-headers ()
  "Redraw the header of every shell, for account-wide state."
  (dolist (buffer (buffer-list))
    (with-current-buffer buffer
      (when (derived-mode-p 'agent-shell-mode)
        (agent-shell--update-header-and-mode-line)))))

(defun agent-shell-claude-status--summary-text (message)
  "Lines for `post_turn_summary' MESSAGE under the response it summarizes."
  (let* ((category (map-elt message 'status_category))
         (detail (map-elt message 'status_detail))
         (action (map-elt message 'needs_action))
         (face (pcase category
                 ("completed" 'agent-shell-claude-status-face)
                 ("failed" 'agent-shell-claude-status-failed-face)
                 (_ 'agent-shell-claude-status-attention-face)))
         (icon (pcase category ("completed" "✓") ("failed" "✗") (_ "⏸"))))
    (concat "\n\n"
            (propertize (concat "  ↳ " icon " " (string-trim (or detail category "")))
                        'face face)
            (when (and (stringp action) (not (string-blank-p action))
                       (not (equal action detail)))
              (propertize (concat "\n    ждёт: " (string-trim action))
                          'face 'agent-shell-claude-status-attention-face)))))

(defun agent-shell-claude-status--store-summary (message)
  "Keep `post_turn_summary' MESSAGE on the marker of the last finished response.
The response of the run before the last one: the last run is the live
prompt when idle, or the running prompt when busy."
  (when-let* ((ranges (agent-shell-prompt-ranges))
              (range (car (last ranges 2)))
              ((cdr ranges))
              (marker (map-elt range :input-end))
              ((< marker (point-max))))
    (with-silent-modifications
      (let ((inhibit-read-only t))
        (put-text-property marker (1+ marker) 'agent-shell-claude-status-summary message)))))

(defun agent-shell-claude-status--render-summaries ()
  "Draw each stored turn summary after the end of its response text.
Redrawn after every relabel: the summary can arrive before the response
text below a collapsed block is rendered."
  (remove-overlays (point-min) (point-max) 'agent-shell-claude-status-summary t)
  (let ((ranges (agent-shell-prompt-ranges)))
    (while (cdr ranges)
      (when-let* ((marker (map-elt (car ranges) :input-end))
                  ((< marker (point-max)))
                  (message (get-text-property marker 'agent-shell-claude-status-summary))
                  (end (save-excursion
                         (goto-char (map-elt (cadr ranges) :prompt-start))
                         (skip-chars-backward " \t\n")
                         (point)))
                  ((> end marker)))
        (let ((ov (make-overlay (1- end) end nil t nil)))
          (overlay-put ov 'agent-shell-claude-status-summary t)
          (overlay-put ov 'after-string (agent-shell-claude-status--summary-text message))))
      (setq ranges (cdr ranges)))))

(defun agent-shell-claude-status--input-start ()
  "Start of the prompt input, or nil without a process."
  (when-let* ((process (get-buffer-process (current-buffer))))
    (marker-position (process-mark process))))

(defun agent-shell-claude-status--suggestion-visible-p ()
  "Non-nil when the predicted prompt is shown at point."
  (and (overlayp agent-shell-claude-status--suggestion-overlay)
       (overlay-buffer agent-shell-claude-status--suggestion-overlay)
       (eq (point) (overlay-start agent-shell-claude-status--suggestion-overlay))))

(defun agent-shell-claude-status--hide-suggestion ()
  (when (overlayp agent-shell-claude-status--suggestion-overlay)
    (delete-overlay agent-shell-claude-status--suggestion-overlay))
  (setq agent-shell-claude-status--suggestion-overlay nil))

(defun agent-shell-claude-status--refresh-suggestion ()
  "Show the predicted prompt in an empty idle input, drop it once input starts."
  (agent-shell-claude-status--hide-suggestion)
  (when-let* ((suggestion agent-shell-claude-status--suggestion)
              (start (agent-shell-claude-status--input-start)))
    (cond
     ((< start (point-max))
      (setq agent-shell-claude-status--suggestion nil))
     ((not (shell-maker-busy))
      (let ((ov (make-overlay start start nil t nil)))
        (overlay-put ov 'after-string
                     (concat (propertize (substring suggestion 0 1)
                                         'face 'agent-shell-claude-status-suggestion-face
                                         'cursor t)
                             (propertize (substring suggestion 1)
                                         'face 'agent-shell-claude-status-suggestion-face)))
        (setq agent-shell-claude-status--suggestion-overlay ov))))))

(defun agent-shell-claude-status-accept-suggestion ()
  "Insert the predicted next prompt into the input."
  (interactive)
  (when-let* ((suggestion agent-shell-claude-status--suggestion))
    (setq agent-shell-claude-status--suggestion nil)
    (agent-shell-claude-status--hide-suggestion)
    (goto-char (point-max))
    (insert suggestion)))

(defvar agent-shell-claude-status-suggestion-mode-map
  (let ((map (make-sparse-keymap))
        (binding `(menu-item "" agent-shell-claude-status-accept-suggestion
                             :filter ,(lambda (command)
                                        (when (agent-shell-claude-status--suggestion-visible-p)
                                          command)))))
    (define-key map (kbd "TAB") binding)
    (define-key map (kbd "<tab>") binding)
    map)
  "Tab accepts the predicted prompt while it is shown, else keeps its binding.")

(define-minor-mode agent-shell-claude-status-suggestion-mode
  "Show Claude Code's predicted next prompt in an empty input; Tab accepts it."
  :lighter nil
  (dolist (hook '(post-command-hook agent-shell-chat-after-relabel-hook))
    (if agent-shell-claude-status-suggestion-mode
        (add-hook hook #'agent-shell-claude-status--refresh-suggestion nil t)
      (remove-hook hook #'agent-shell-claude-status--refresh-suggestion t)))
  (unless agent-shell-claude-status-suggestion-mode
    (agent-shell-claude-status--hide-suggestion)))

(defun agent-shell-claude-status--apply (message)
  "Update the current buffer's status from raw SDK MESSAGE."
  (pcase (map-elt message 'type)
    ("rate_limit_event"
     (setq agent-shell-claude-status--rate-limit (map-elt message 'rate_limit_info))
     (agent-shell-claude-status--update-all-headers))
    ("prompt_suggestion"
     (let ((suggestion (map-elt message 'suggestion)))
       ;; Busy here means the next prompt was sent before the prediction arrived.
       (when (and (stringp suggestion) (not (string-blank-p suggestion))
                  (not (shell-maker-busy)))
         (setq agent-shell-claude-status--suggestion (string-trim suggestion))
         (agent-shell-claude-status-suggestion-mode 1)
         (agent-shell-claude-status--refresh-suggestion))))
    ("system"
     (agent-shell-claude-status--apply-system message))))

(defun agent-shell-claude-status--apply-system (message)
  "Update the current buffer's status from raw system MESSAGE."
  (pcase (map-elt message 'subtype)
    ("post_turn_summary"
     (setq agent-shell-claude-status--last-summary (cons (current-time) message))
     (agent-shell-claude-status--store-summary message)
     (add-hook 'agent-shell-chat-after-relabel-hook
               #'agent-shell-claude-status--render-summaries nil t)
     (agent-shell-claude-status--render-summaries))
    ("task_summary"
     (let ((detail (map-elt message 'detail)))
       (setq agent-shell-claude-status--task
             (and (stringp detail) (not (string-empty-p detail)) detail))))
    ("permission_check_status"
     (let ((id (map-elt message 'tool_use_id)))
       (setq agent-shell-claude-status--checking
             (if (equal (map-elt message 'status) "checking")
                 (cl-adjoin id agent-shell-claude-status--checking :test #'equal)
               (remove id agent-shell-claude-status--checking)))))
    ("bridge_state"
     (setq agent-shell-claude-status--bridge message))
    ("session_state_changed"
     (pcase (map-elt message 'state)
       ("idle" (setq agent-shell-claude-status--checking nil))
       ("running" (setq agent-shell-claude-status--suggestion nil)
        (agent-shell-claude-status--hide-suggestion)))))
  (agent-shell--update-header-and-mode-line))

(defun agent-shell-claude-status-handle-notification (event)
  "Handle `_claude/sdkMessage' from EVENT's `:acp-notification'.
Returns non-nil when handled, so agent-shell skips its default handling."
  (let ((notification (map-elt event :acp-notification)))
    (when (equal (map-elt notification 'method) "_claude/sdkMessage")
      (when-let* ((buffer (map-elt (map-elt event :state) :buffer))
                  ((buffer-live-p buffer)))
        (with-current-buffer buffer
          (agent-shell-claude-status--apply
           (map-nested-elt notification '(params message)))))
      t)))

(defun agent-shell-claude-status-filter-error (acp-error)
  "Drop stderr lines matching `agent-shell-claude-status-ignored-stderr-regexps'.
Returns ACP-ERROR, a copy without those lines, or nil if nothing is left."
  (let ((message (map-elt acp-error 'message)))
    (if (not (stringp message))
        acp-error
      (let* ((lines (split-string message "\n"))
             (kept (seq-remove (lambda (line)
                                 (seq-some (lambda (re) (string-match-p re line))
                                           agent-shell-claude-status-ignored-stderr-regexps))
                               lines)))
        (cond ((= (length kept) (length lines)) acp-error)
              ((string-blank-p (string-join kept "\n")) nil)
              (t (cons (cons 'message (string-join kept "\n"))
                       (map-delete (copy-alist acp-error) 'message))))))))

(defun agent-shell-claude-status--ago (time)
  "Short age of TIME: \"5 мин\", \"2 ч\"."
  (let ((secs (floor (float-time (time-subtract (current-time) time)))))
    (cond ((< secs 60) "сейчас")
          ((< secs 3600) (format "%d мин" (/ secs 60)))
          ((< secs 86400) (format "%d ч" (/ secs 3600)))
          (t (format "%d дн" (/ secs 86400))))))

(defun agent-shell-claude-status-buffer-text ()
  "One line about what the current shell is doing or waiting for."
  (cond
   ((shell-maker-busy)
    (propertize (concat "⚙ " (or agent-shell-claude-status--task "работает"))
                'face 'agent-shell-claude-status-face))
   (agent-shell-claude-status--checking
    (propertize "🔍 проверка разрешения…" 'face 'agent-shell-claude-status-attention-face))
   (agent-shell-claude-status--last-summary
    (let* ((message (cdr agent-shell-claude-status--last-summary))
           (category (map-elt message 'status_category))
           (action (map-elt message 'needs_action))
           (text (if (and (stringp action) (not (string-blank-p action)))
                     action
                   (or (map-elt message 'status_detail) category ""))))
      (concat
       (propertize (concat (pcase category ("completed" "✓") ("failed" "✗") (_ "⏸"))
                           " " (replace-regexp-in-string "[\n\t ]+" " " (string-trim text)))
                   'face (pcase category
                           ("completed" 'agent-shell-claude-status-face)
                           ("failed" 'agent-shell-claude-status-failed-face)
                           (_ 'agent-shell-claude-status-attention-face)))
       (propertize (concat " · " (agent-shell-claude-status--ago
                                  (car agent-shell-claude-status--last-summary)))
                   'face 'shadow))))
   (t "")))

(eval-when-compile (require 'ibuf-macs))
(declare-function ibuffer-make-column-filename-and-process "ibuffer")

(with-eval-after-load 'ibuffer
  (define-ibuffer-column agent-status
    (:name "Status / Filename")
    (if (derived-mode-p 'agent-shell-mode)
        (agent-shell-claude-status-buffer-text)
      (ibuffer-make-column-filename-and-process buffer mark))))

;;;###autoload
(defun agent-shell-reload-idle ()
  "Reload every idle agent shell with its session ID; busy ones are skipped.
Picks up new session meta, e.g. after updating agent-shell."
  (interactive)
  (let (reloaded skipped)
    (save-selected-window
      ;; `agent-shell-restart' displays a shell that is not on screen.
      (cl-letf (((symbol-function 'agent-shell--display-buffer) #'ignore))
        (dolist (buffer (buffer-list))
          (when (and (buffer-live-p buffer)
                     (with-current-buffer buffer (derived-mode-p 'agent-shell-mode)))
            (let ((name (buffer-name buffer)))
              (if (with-current-buffer buffer
                    (agent-shell--active-requests-p (agent-shell--state)))
                  (push name skipped)
                (condition-case err
                    (with-current-buffer buffer
                      (agent-shell-reload)
                      (push name reloaded))
                  (error (push (format "%s (%s)" name (error-message-string err))
                               skipped)))))))))
    (message "Перезапущено: %d%s" (length reloaded)
             (if skipped (format "; пропущены: %s" (string-join skipped ", ")) ""))))

(provide 'agent-shell-claude-status)

;;; agent-shell-claude-status.el ends here
