;;; agent-shell-easysession.el --- Lazy session restoration -*- lexical-binding: t; -*-

;;; Commentary:
;; Enable `agent-shell-easysession-mode' before loading an EasySession session.

;;; Code:

(require 'agent-shell)
(require 'easysession)

(defvar-local agent-shell-easysession--data nil
  "Saved agent and session identifier for a pending shell.")

(defvar agent-shell-easysession-placeholder-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "RET") #'agent-shell-easysession-resume)
    map)
  "Keymap for pending shells.")

(define-derived-mode agent-shell-easysession-placeholder-mode special-mode
  "Agent Resume"
  "Wait until RET is pressed to resume an agent session.")

(defun agent-shell-easysession--save ()
  "Return session data, for example ((session-id . \"abc\") (agent . claude-code))."
  (let ((data (if (eq major-mode 'agent-shell-easysession-placeholder-mode)
                  agent-shell-easysession--data
                `((session-id . ,(map-nested-elt agent-shell--state '(:session :id)))
                  (agent . ,(map-nested-elt agent-shell--state '(:agent-config :identifier)))))))
    (when (and (stringp (map-elt data 'session-id))
               (not (string-empty-p (map-elt data 'session-id))))
      data)))

(defun agent-shell-easysession--config (data)
  "Resolve DATA's agent identifier against the current configurations."
  (when (map-elt data 'agent)
    (seq-find (lambda (config)
                (equal (map-elt config :identifier) (map-elt data 'agent)))
              (agent-shell--resolved-agent-configs))))

(defun agent-shell-easysession--validate (item)
  "Return non-nil if saved ITEM has a session and an available agent."
  (let ((data (map-elt item 'data)))
    (and (stringp (map-elt data 'session-id))
         (not (string-empty-p (map-elt data 'session-id)))
         (agent-shell-easysession--config data))))

(defun agent-shell-easysession--restore (item)
  "Create a pending shell from saved ITEM without starting an agent."
  (let ((directory default-directory)
        (buffer (get-buffer-create (map-elt item 'buffer-name))))
    (with-current-buffer buffer
      (agent-shell-easysession-placeholder-mode)
      (setq-local default-directory directory)
      (setq-local agent-shell-easysession--data (map-elt item 'data))
      (let ((inhibit-read-only t))
        (insert (format "⏸ %s · %s · сессия %s — RET для resume\n"
                        (map-elt agent-shell-easysession--data 'agent)
                        directory
                        (truncate-string-to-width
                         (map-elt agent-shell-easysession--data 'session-id) 8))))
      (set-buffer-modified-p nil))
    buffer))

;;;###autoload
(defun agent-shell-unload ()
  "Stop the current agent and keep a placeholder for resuming with RET.

Works from shell and viewport buffers.  Refuse busy shells and sessions
without a saved identifier.  Preserve the name, directory and session ID;
resume remains explicit after saving and restoring with EasySession."
  (declare (modes agent-shell-mode
                  agent-shell-viewport-view-mode
                  agent-shell-viewport-edit-mode))
  (interactive)
  (let* ((shell (or (agent-shell--current-shell)
                    (user-error "Not in a shell or viewport buffer")))
         (viewport (agent-shell-viewport--buffer :shell-buffer shell :existing-only t))
         (windows (append (get-buffer-window-list shell nil t)
                          (when viewport (get-buffer-window-list viewport nil t))))
         (name (buffer-name shell))
         (directory (buffer-local-value 'default-directory shell))
         (cleanup (buffer-local-value 'agent-shell--pending-directory-cleanup shell))
         (data (with-current-buffer shell (agent-shell-easysession--save))))
    (when (agent-shell--active-requests-p (buffer-local-value 'agent-shell--state shell))
      (user-error "Agent is busy; wait for it to finish before unloading"))
    (unless (and data (agent-shell-easysession--config data))
      (user-error "Session has no resumable identifier or available agent"))
    (when cleanup
      (setq data (append data `((pending-directory-cleanup . ,cleanup)))))
    ;; Unloading must not delete the directory needed by the resumed session.
    (with-current-buffer shell
      (setq-local agent-shell--pending-directory-cleanup nil))
    (unwind-protect
        (unless (kill-buffer shell)
          (user-error "Cancelled"))
      (when (buffer-live-p shell)
        (with-current-buffer shell
          (setq-local agent-shell--pending-directory-cleanup cleanup))))
    (let* ((default-directory directory)
           (pending (agent-shell-easysession--restore
                     `((buffer-name . ,name) (data . ,data)))))
      (dolist (window (seq-filter #'window-live-p windows))
        (set-window-buffer window pending))
      pending)))

(defun agent-shell-easysession-resume ()
  "Replace the current placeholder with its resumed agent shell."
  (interactive)
  (unless (eq major-mode 'agent-shell-easysession-placeholder-mode)
    (user-error "Not in a pending agent shell"))
  (let* ((data agent-shell-easysession--data)
         (config (or (agent-shell-easysession--config data)
                     (user-error "Agent is unavailable: %s" (map-elt data 'agent))))
         (name (buffer-name))
         (directory default-directory)
         (windows (get-buffer-window-list (current-buffer) nil t)))
    (unless (kill-buffer (current-buffer))
      (user-error "Cancelled"))
    (let* ((default-directory directory)
           (buffer
            (condition-case err
                (agent-shell--start :config config
                                    :session-id (map-elt data 'session-id)
                                    :new-session t :no-focus t :cwd directory)
              (error
               ;; Keep failed sessions available for an explicit retry.
               (let ((pending (agent-shell-easysession--restore
                               `((buffer-name . ,name) (data . ,data)))))
                 (dolist (window (seq-filter #'window-live-p windows))
                   (set-window-buffer window pending)))
               (signal (car err) (cdr err))))))
      (shell-maker-set-buffer-name buffer name)
      (with-current-buffer buffer
        (setq-local agent-shell--pending-directory-cleanup
                    (map-elt data 'pending-directory-cleanup)))
      (dolist (window (seq-filter #'window-live-p windows))
        (set-window-buffer window buffer))
      buffer)))

;;;###autoload
(define-minor-mode agent-shell-easysession-mode
  "Persist agent sessions and resume them when RET is pressed."
  :global t
  :group 'agent-shell
  (dolist (mode '(agent-shell-mode agent-shell-easysession-placeholder-mode))
    (if agent-shell-easysession-mode
        (easysession-add-managed-major-mode
         mode :save #'agent-shell-easysession--save
         :restore #'agent-shell-easysession--restore
         :validate #'agent-shell-easysession--validate)
      (easysession-remove-managed-major-mode mode))))

(provide 'agent-shell-easysession)
;;; agent-shell-easysession.el ends here
