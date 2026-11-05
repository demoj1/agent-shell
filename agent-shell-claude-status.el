;;; agent-shell-claude-status.el --- Claude Code session status in the mode line -*- lexical-binding: t; -*-

;;; Commentary:

;; Fork-only (demoj1).  claude-agent-acp does not map the CLI's
;; `post_turn_summary', `task_summary', `permission_check_status' and
;; `bridge_state' frames to ACP and logs them as "Unexpected case" on stderr
;; instead (agentclientprotocol/claude-agent-acp#1532).  We ask for them raw
;; via `_meta.claudeCode.emitRawSDKMessages', show them in the mode line and
;; drop the stderr noise from Notices.

;;; Code:

(require 'cl-lib)
(require 'map)
(require 'seq)
(require 'subr-x)

(defgroup agent-shell-claude-status nil
  "Claude Code session status in agent-shell's mode line."
  :group 'agent-shell)

(defcustom agent-shell-claude-status-max-width 60
  "Maximum width of the status text in the mode line."
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

(defvar-local agent-shell-claude-status--turn nil
  "Last `post_turn_summary' message, nil while a turn runs.")

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
  "Mode line segment: ICON and TEXT in FACE, full TEXT in the tooltip."
  (concat " " (propertize (concat icon " " (agent-shell-claude-status--truncate text))
                          'face face 'help-echo text)))

(defun agent-shell-claude-status--mode-line ()
  "Mode line text for the current shell buffer."
  (let ((turn agent-shell-claude-status--turn)
        (bridge agent-shell-claude-status--bridge))
    (concat
     (when bridge
       (let ((state (map-elt bridge 'state))
             (detail (map-elt bridge 'detail)))
         (concat " " (propertize
                      (pcase state
                        ("connected" "📡")
                        ("failed" "📡✗")
                        (_ "📡…"))
                      'face (if (equal state "failed")
                                'agent-shell-claude-status-failed-face
                              'agent-shell-claude-status-face)
                      'help-echo (concat "Remote Control: " state
                                         (when (stringp detail) (concat " — " detail)))))))
     (when agent-shell-claude-status--checking
       (agent-shell-claude-status--segment "🔍" "проверка разрешения…"
                                           'agent-shell-claude-status-attention-face))
     (when (and agent-shell-claude-status--task (not turn))
       (agent-shell-claude-status--segment "⚙" agent-shell-claude-status--task
                                           'agent-shell-claude-status-face))
     (when turn
       (let ((category (map-elt turn 'status_category))
             (detail (or (map-elt turn 'status_detail) ""))
             (action (map-elt turn 'needs_action)))
         (pcase category
           ("completed" (agent-shell-claude-status--segment
                         "✓" detail 'agent-shell-claude-status-face))
           ("failed" (agent-shell-claude-status--segment
                      "✗" detail 'agent-shell-claude-status-failed-face))
           (_ (agent-shell-claude-status--segment
               "⏸" (if (and (stringp action) (not (string-empty-p action))) action detail)
               'agent-shell-claude-status-attention-face))))))))

(defconst agent-shell-claude-status--mode-line-entry
  '(:eval (agent-shell-claude-status--mode-line)))

(defun agent-shell-claude-status--apply (message)
  "Update the current buffer's status from raw SDK MESSAGE."
  (pcase (map-elt message 'subtype)
    ("post_turn_summary"
     (setq agent-shell-claude-status--turn message))
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
       ("running" (setq agent-shell-claude-status--turn nil))
       ("idle" (setq agent-shell-claude-status--checking nil)))))
  (unless (member agent-shell-claude-status--mode-line-entry mode-line-misc-info)
    (setq-local mode-line-misc-info
                (cons agent-shell-claude-status--mode-line-entry mode-line-misc-info)))
  (force-mode-line-update))

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
