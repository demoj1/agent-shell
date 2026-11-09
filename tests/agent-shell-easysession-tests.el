;;; agent-shell-easysession-tests.el --- Session persistence tests -*- lexical-binding: t; -*-

;;; Commentary:
;; Exercise persistence without starting external agents.

;;; Code:

(require 'ert)
(require 'cl-lib)
(require 'agent-shell-easysession)

(ert-deftest agent-shell-easysession-save-test ()
  (with-temp-buffer
    (setq-local agent-shell--state
                '((:session . ((:id . "session-123")))
                  (:agent-config . ((:identifier . claude-code)))))
    (should (equal (agent-shell-easysession--save)
                   '((session-id . "session-123") (agent . claude-code))))
    (dolist (id '(nil ""))
      (setq-local agent-shell--state
                  `((:session . ((:id . ,id)))
                    (:agent-config . ((:identifier . claude-code)))))
      (should-not (agent-shell-easysession--save)))))

(ert-deftest agent-shell-easysession-validate-test ()
  (let ((agent-shell-agent-configs '(((:identifier . claude-code)))))
    (should (agent-shell-easysession--validate
             '((data . ((session-id . "123") (agent . claude-code))))))
    (dolist (data '(((session-id . "") (agent . claude-code))
                    ((agent . claude-code))
                    ((session-id . "123") (agent . missing))
                    ((session-id . "123")) nil))
      (should-not (agent-shell-easysession--validate `((data . ,data)))))))

(ert-deftest agent-shell-easysession-placeholder-roundtrip-test ()
  (let ((easysession--managed-major-modes nil)
        (agent-shell-easysession-mode nil)
        (agent-shell-agent-configs '(((:identifier . claude-code))))
        (default-directory temporary-file-directory)
        (item '((buffer-name . " *pending-agent-test*")
                (major-mode . agent-shell-mode)
                (data . ((session-id . "session-123") (agent . claude-code))))))
    (unwind-protect
        (progn
          (agent-shell-easysession-mode 1)
          (easysession--handler-load-managed-major-modes
           `(("managed-major-modes" . (,item))))
          (with-current-buffer " *pending-agent-test*"
            (should (eq major-mode 'agent-shell-easysession-placeholder-mode))
            (should (equal default-directory temporary-file-directory))
            (should (equal agent-shell-easysession--data (map-elt item 'data)))
            (should (string-match-p "сессия session-" (buffer-string)))
            (should (eq (key-binding (kbd "RET")) #'agent-shell-easysession-resume))
            (let ((saved (easysession--get-managed-major-mode-buffer-info
                          (current-buffer))))
              (should (agent-shell-easysession--validate saved))
              (should (equal (map-elt saved 'data) (map-elt item 'data)))))
          (should-not (assq 'agent-shell-viewport-view-mode easysession--managed-major-modes))
          (agent-shell-easysession-mode -1)
          (should-not easysession--managed-major-modes))
      (when (get-buffer " *pending-agent-test*")
        (kill-buffer " *pending-agent-test*")))))

(ert-deftest agent-shell-easysession-resume-windows-test ()
  (let* ((default-directory temporary-file-directory)
         (agent-shell-agent-configs '(((:identifier . claude-code) (:name . "Fresh"))))
         (item '((buffer-name . " *resume-agent-test*")
                 (data . ((session-id . "session-123") (agent . claude-code)))))
         (pending (agent-shell-easysession--restore item))
         (calls 0))
    (unwind-protect
        (save-window-excursion
          (delete-other-windows)
          (set-window-buffer (selected-window) pending)
          (set-window-buffer (split-window-right) pending)
          (let ((windows (get-buffer-window-list pending)))
            (cl-letf (((symbol-function 'agent-shell--start)
                       (lambda (&rest args)
                         (setq calls (1+ calls))
                         (should-not (buffer-live-p pending))
                         (should (equal (plist-get args :config)
                                        (car agent-shell-agent-configs)))
                         (should (equal (plist-get args :session-id) "session-123"))
                         (should (equal (plist-get args :cwd) temporary-file-directory))
                         (should (equal default-directory temporary-file-directory))
                         (should (plist-get args :new-session))
                         (should (plist-get args :no-focus))
                         (get-buffer-create " *resumed-agent-test*"))))
              (agent-shell-easysession--window-change (car windows))
              (should (= calls 1))
              (dolist (window windows)
                (should (equal (buffer-name (window-buffer window))
                               " *resume-agent-test*")))
              (with-current-buffer " *resume-agent-test*"
                (should (equal shell-maker--buffer-name-override " *resume-agent-test*"))
                (should-not (memq #'agent-shell-easysession--window-change
                                  window-buffer-change-functions)))
              (agent-shell-easysession--window-change (car windows))
              (should (= calls 1)))))
      (dolist (name '(" *resume-agent-test*" " *resumed-agent-test*"))
        (when (get-buffer name) (kill-buffer name))))))

(ert-deftest agent-shell-easysession-resume-error-test ()
  (let ((agent-shell-agent-configs '(((:identifier . claude-code))))
        (item '((buffer-name . " *failed-agent-test*")
                (data . ((session-id . "123") (agent . claude-code))))))
    (unwind-protect
        (with-current-buffer (agent-shell-easysession--restore item)
          (cl-letf (((symbol-function 'agent-shell--start)
                     (lambda (&rest _) (error "Unavailable executable"))))
            (should-error (agent-shell-easysession-resume)))
          (with-current-buffer " *failed-agent-test*"
            (should (equal agent-shell-easysession--data (map-elt item 'data)))
            (should-not (memq #'agent-shell-easysession--window-change
                              window-buffer-change-functions))))
      (when (get-buffer " *failed-agent-test*") (kill-buffer " *failed-agent-test*")))))

(provide 'agent-shell-easysession-tests)
;;; agent-shell-easysession-tests.el ends here
