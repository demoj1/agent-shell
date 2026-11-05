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

(defconst agent-shell-claude-status--subtypes
  '("post_turn_summary" "task_summary" "permission_check_status" "bridge_state"
    "session_state_changed")
  "System subtypes requested raw from the adapter.")

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

(defun agent-shell-claude-status-session-meta ()
  "Return the `claudeCode' session meta entry asking for the raw frames."
  (cons 'emitRawSDKMessages
        (vconcat (mapcar (lambda (subtype) `((type . "system") (subtype . ,subtype)))
                         agent-shell-claude-status--subtypes))))

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
                                                  'agent-shell-claude-status-face))))))
    (when segments
      (string-join segments " "))))

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

(defun agent-shell-claude-status--apply (message)
  "Update the current buffer's status from raw SDK MESSAGE."
  (pcase (map-elt message 'subtype)
    ("post_turn_summary"
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
     (when (equal (map-elt message 'state) "idle")
       (setq agent-shell-claude-status--checking nil))))
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

(provide 'agent-shell-claude-status)

;;; agent-shell-claude-status.el ends here
