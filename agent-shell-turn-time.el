;;; agent-shell-turn-time.el --- Turn time and duration on chat labels -*- lexical-binding: t; -*-

;;; Commentary:

;; Fork-only (demoj1): "Me  HH:MM · N мин назад" and "Claude  HH:MM · затрачено N"
;; on `agent-shell-chat-mode' labels.  agent-shell keeps no per-turn time, so
;; each anchor (prompt run start / response marker) is stamped once with a text
;; property and appended to its label overlay after every relabel.

;;; Code:

(require 'map)
(require 'agent-shell-chat-mode)

(declare-function agent-shell-prompt-ranges "agent-shell")

(defgroup agent-shell-turn-time nil
  "Turn time and duration on agent-shell chat labels."
  :group 'agent-shell)

(defcustom agent-shell-turn-time-refresh-secs 15
  "Seconds between relative time refreshes."
  :type 'integer
  :group 'agent-shell-turn-time)

(defface agent-shell-turn-time-face '((t :inherit shadow :height 0.85))
  "Face of turn time on chat labels."
  :group 'agent-shell-turn-time)

(defvar agent-shell-turn-time--timer nil)

(defvar-local agent-shell-turn-time--initialized nil
  "Non-nil once this buffer's earlier history was baselined as unknown.")

(defun agent-shell-turn-time--relfmt (time)
  "Format TIME as \"HH:MM · N назад\", just \"HH:MM\" for the first 10 seconds."
  (let* ((abs (format-time-string "%H:%M" time))
         (secs (max 0 (floor (float-time (time-subtract (current-time) time))))))
    (cond ((< secs 10) abs)
          ((< secs 60)    (concat abs (format " · %d сек назад" secs)))
          ((< secs 3600)  (concat abs (format " · %d мин назад" (/ secs 60))))
          ((< secs 86400) (concat abs (format " · %d ч назад" (/ secs 3600))))
          (t              (concat abs (format " · %d дн назад" (/ secs 86400)))))))

(defun agent-shell-turn-time--plural (n one few many)
  "Russian plural form for N: ONE, FEW or MANY."
  (let ((n10 (% n 10)) (n100 (% n 100)))
    (cond ((and (= n10 1) (/= n100 11)) one)
          ((and (>= n10 2) (<= n10 4) (or (< n100 10) (>= n100 20))) few)
          (t many))))

(defun agent-shell-turn-time--duration (secs)
  "Format SECS as \"1 минута 30 секунд\" or \"2 часа 5 минут\"."
  (let ((s (max 0 (round secs)))
        (plural #'agent-shell-turn-time--plural))
    (cond
     ((< s 60) (format "%d %s" s (funcall plural s "секунда" "секунды" "секунд")))
     ((< s 3600)
      (let ((m (/ s 60)) (r (% s 60)))
        (concat (format "%d %s" m (funcall plural m "минута" "минуты" "минут"))
                (when (> r 0)
                  (format " %d %s" r (funcall plural r "секунда" "секунды" "секунд"))))))
     (t
      (let ((h (/ s 3600)) (m (/ (% s 3600) 60)))
        (concat (format "%d %s" h (funcall plural h "час" "часа" "часов"))
                (when (> m 0)
                  (format " %d %s" m (funcall plural m "минута" "минуты" "минут")))))))))

(defun agent-shell-turn-time--prompt-anchors ()
  "Starts of submitted prompt runs, without the live prompt."
  (let ((ranges (agent-shell-prompt-ranges)) res)
    (while ranges
      (let ((range (car ranges)))
        (when (or (cdr ranges)
                  (get-text-property (map-elt range :input-end)
                                     'shell-maker--marker))
          (push (map-elt range :prompt-start) res)))
      (setq ranges (cdr ranges)))
    (nreverse res)))

(defun agent-shell-turn-time--response-anchors ()
  "Starts of the response markers."
  (save-excursion
    (goto-char (point-min))
    (let (res (pos (point-min)))
      (while (setq pos (text-property-any pos (point-max) 'shell-maker--marker t))
        (push pos res)
        (setq pos (or (next-single-property-change pos 'shell-maker--marker
                                                   nil (point-max))
                      (point-max))))
      (nreverse res))))

(defun agent-shell-turn-time--put (pos prop value)
  "Put PROP VALUE on the char at POS without touching undo or modified state."
  (with-silent-modifications
    (let ((inhibit-read-only t))
      (put-text-property pos (min (point-max) (1+ pos)) prop value))))

(defun agent-shell-turn-time--stamp ()
  "Stamp submit time on new prompts; responses are stamped when they finish."
  (let ((now (current-time)))
    (dolist (pos (agent-shell-turn-time--prompt-anchors))
      (unless (get-text-property pos 'agent-shell-turn-time)
        (agent-shell-turn-time--put pos 'agent-shell-turn-time now)))))

(defun agent-shell-turn-time--baseline ()
  "Mark earlier history as `unknown' so it does not read as \"just now\"."
  (dolist (pos (append (agent-shell-turn-time--prompt-anchors)
                       (agent-shell-turn-time--response-anchors)))
    (unless (get-text-property pos 'agent-shell-turn-time)
      (agent-shell-turn-time--put pos 'agent-shell-turn-time 'unknown))))

(defun agent-shell-turn-time--strip (s)
  "Return S without the chars of our suffix."
  (let (acc (i 0) (n (length s)))
    (while (< i n)
      (unless (get-text-property i 'agent-shell-turn-time s)
        (push (substring s i (1+ i)) acc))
      (setq i (1+ i)))
    (apply #'concat (nreverse acc))))

(defun agent-shell-turn-time--decorate (ov anchor property)
  "Append ANCHOR's time and duration to PROPERTY of label overlay OV."
  (let ((t0 (get-text-property anchor 'agent-shell-turn-time))
        (dur (get-text-property anchor 'agent-shell-turn-time-duration)))
    (when (consp t0)
      (let* ((clean (agent-shell-turn-time--strip (overlay-get ov property)))
             ;; A label can start with blank lines; append to the name's row.
             (name-start (string-match "[^ \t\n]" clean))
             (nl (and name-start
                      ;; Right-aligned label tails (the running task) stay after the time.
                      (or (text-property-any name-start (length clean)
                                             'agent-shell-label-right t clean)
                          (string-match "\n" clean name-start))))
             (suffix (propertize
                      (concat " " (agent-shell-turn-time--relfmt t0)
                              (when (numberp dur)
                                (concat " · затрачено "
                                        (agent-shell-turn-time--duration dur))))
                      'face 'agent-shell-turn-time-face 'agent-shell-turn-time t)))
        (overlay-put ov property
                     (if nl (concat (substring clean 0 nl) suffix (substring clean nl))
                       (concat clean suffix)))))))

(defun agent-shell-turn-time--label-anchor (ov ranges)
  "Find the prompt or response marker that label OV belongs to among RANGES."
  (let ((tag (overlay-get ov 'agent-shell-chat--tag))
        (start (overlay-start ov)) (end (overlay-end ov)) anchor)
    (while (and ranges (not anchor))
      (let* ((range (car ranges))
             (prompt (map-elt range :prompt-start))
             (prompt-end (map-elt range :prompt-end))
             (marker (map-elt range :input-end))
             (next (or (map-elt (cadr ranges) :prompt-start) (point-max)))
             (response (get-text-property marker 'shell-maker--marker)))
        (setq anchor
              (if (eq tag 'agent)
                  (when (and response (<= prompt-end start) (< start next)) marker)
                (when (and (or (cdr ranges) response)
                           (>= end prompt) (< start prompt-end))
                  prompt))))
      (setq ranges (cdr ranges)))
    anchor))

(defun agent-shell-turn-time--render ()
  "Draw turn time on every label of the current buffer."
  (let ((ranges (agent-shell-prompt-ranges)))
    (dolist (ov (overlays-in (point-min) (point-max)))
      (let ((tag (overlay-get ov 'agent-shell-chat--tag)))
        (when (memq tag '(me me-label agent))
          (when-let* ((anchor (agent-shell-turn-time--label-anchor ov ranges)))
            (dolist (property (if (eq tag 'me) '(before-string)
                                '(display before-string)))
              (let ((value (overlay-get ov property)))
                (when (and (stringp value)
                           (string-match-p "[^ \t\n]"
                                           (agent-shell-turn-time--strip value)))
                  (agent-shell-turn-time--decorate ov anchor property))))))))))

(defun agent-shell-turn-time--after-relabel ()
  "Stamp new prompts and redraw times after chat mode relabels."
  (with-demoted-errors "agent-shell-turn-time: %S"
    (when agent-shell-chat--labeled
      (unless agent-shell-turn-time--initialized
        (agent-shell-turn-time--baseline)
        (setq-local agent-shell-turn-time--initialized t))
      (agent-shell-turn-time--stamp)
      (agent-shell-turn-time--render))))

(defun agent-shell-turn-time--on-finish (&rest _)
  "Stamp the finished response with its time and submit-to-finish duration.
Only an unstamped marker: `clear' and reprints run this hook too and must
not restamp an old turn."
  (with-demoted-errors "agent-shell-turn-time finish: %S"
    (when (bound-and-true-p agent-shell-chat--labeled)
      (let ((m (car (last (agent-shell-turn-time--response-anchors))))
            (p (car (last (agent-shell-turn-time--prompt-anchors)))))
        (when (and m (null (get-text-property m 'agent-shell-turn-time)))
          (let ((now (current-time))
                (t0 (and p (get-text-property p 'agent-shell-turn-time))))
            (agent-shell-turn-time--put m 'agent-shell-turn-time now)
            (when (consp t0)
              (agent-shell-turn-time--put m 'agent-shell-turn-time-duration
                                          (float-time (time-subtract now t0)))))
          (agent-shell-turn-time--render))))))

(defun agent-shell-turn-time--tick ()
  "Refresh relative times in visible labeled buffers."
  (with-demoted-errors "agent-shell-turn-time: %S"
    (dolist (buf (buffer-list))
      (when (and (buffer-local-value 'agent-shell-chat--labeled buf)
                 (get-buffer-window-list buf nil t))
        (with-current-buffer buf (agent-shell-turn-time--render))))))

(defun agent-shell-turn-time--clear (buf)
  "Remove turn time suffixes and stamps from BUF."
  (with-current-buffer buf
    (dolist (ov (overlays-in (point-min) (point-max)))
      (when (memq (overlay-get ov 'agent-shell-chat--tag) '(me me-label agent))
        (dolist (property '(display before-string))
          (when (stringp (overlay-get ov property))
            (overlay-put ov property
                         (agent-shell-turn-time--strip (overlay-get ov property)))))))
    (with-silent-modifications
      (let ((inhibit-read-only t))
        (remove-text-properties (point-min) (point-max)
                                '(agent-shell-turn-time nil
                                  agent-shell-turn-time-duration nil))))
    (kill-local-variable 'agent-shell-turn-time--initialized)))

;;;###autoload
(define-minor-mode agent-shell-turn-time-mode
  "Show turn time and response duration on agent-shell chat labels."
  :global t
  :group 'agent-shell-turn-time
  (if agent-shell-turn-time-mode
      (progn
        (add-hook 'agent-shell-chat-after-relabel-hook
                  #'agent-shell-turn-time--after-relabel)
        (add-hook 'shell-maker-finish-output-hook
                  #'agent-shell-turn-time--on-finish)
        (unless agent-shell-turn-time--timer
          (setq agent-shell-turn-time--timer
                (run-with-timer agent-shell-turn-time-refresh-secs
                                agent-shell-turn-time-refresh-secs
                                #'agent-shell-turn-time--tick)))
        (dolist (buf (buffer-list))
          (when (buffer-local-value 'agent-shell-chat--labeled buf)
            (with-current-buffer buf (agent-shell-turn-time--after-relabel)))))
    (remove-hook 'agent-shell-chat-after-relabel-hook
                 #'agent-shell-turn-time--after-relabel)
    (remove-hook 'shell-maker-finish-output-hook
                 #'agent-shell-turn-time--on-finish)
    (when agent-shell-turn-time--timer
      (cancel-timer agent-shell-turn-time--timer)
      (setq agent-shell-turn-time--timer nil))
    (dolist (buf (buffer-list))
      (when (buffer-local-value 'agent-shell-chat--labeled buf)
        (agent-shell-turn-time--clear buf)))))

(provide 'agent-shell-turn-time)

;;; agent-shell-turn-time.el ends here
