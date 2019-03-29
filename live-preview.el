;;; live-preview.el --- Live preview by any shell command while editing -*- lexical-binding: t -*-
;;
;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-live-preview
;; Version: 0.1.0
;; Package-Requires: ((emacs "24.4") (cl-lib "0.5"))
;; Keywords: languages util
;;
;; This file is not part of GNU Emacs.
;;
;;; Commentary:
;;
;; Renders a live preview of whatever you are editing in a side
;; window.
;;
;; You can give any shell command or Emacs Lisp function to render the
;; preview. The preview is rendered whenever you are idle for a few
;; seconds. Different buffers can have different preview commands.
;; There is only one global preview buffer; whenever you go idle in a
;; buffer that has a preview command, the preview buffer is updated
;; with a preview of that buffer.
;;
;;; Code:

(defvar live-preview-command-history
  nil "List of old live preview commands.")

(defvar-local live-preview-command
  nil
  "Shell command or Emacs Lisp function to render the live preview.

When live-preview-mode is on, this command is run whenever you
are idle for a few seconds to update the preview.

The value can be:

* nil or a blank string -- No live preview is rendered.

* a string -- A shell command line to render the preview from
  standard input to standard output.

* a function -- Called with one argument, the source buffer. When
  called, the current buffer is the empty preview buffer. The
  function shall insert a preview of the source buffer there. The
  function is free to also make other change to the preview
  buffer, such as changing to a suitable major mode.")

(defun live-preview-show ()
  "Update the live preview immediately."
  (let ((src-buf (current-buffer))
        (pre-buf (get-buffer-create "*live-preview*"))
        (command live-preview-command))
    (unless (eq src-buf pre-buf)
      (with-current-buffer pre-buf
        (when (get-buffer-process (current-buffer))
          (interrupt-process)
          (while (process-live-p (get-buffer-process (current-buffer)))
            (sleep-for 0.1)))
        (unless (or (null command)
                    (and (stringp command) (string-blank-p command)))
          (cond ((stringp command)
                 (start-process-shell-command
                  "live-preview" (current-buffer) command)
                 (let ((all-output ""))
                   (set-process-filter
                    (get-buffer-process (current-buffer))
                    (lambda (process new-output)
                      (if (< (length all-output) (* 100 1024))
                          (setq all-output (concat all-output new-output))
                        (interrupt-process process))))
                   (set-process-sentinel
                    (get-buffer-process (current-buffer))
                    (lambda (process state)
                      (when (equal "finished\n" state)
                        (with-current-buffer pre-buf
                          (widen)
                          (let ((old-point (point)))
                            (erase-buffer)
                            (insert all-output)
                            (goto-char (goto-char (min old-point (point-max))))
                            (set-marker (process-mark process) (point))
                            (set-mark (point))))))))
                 (let ((input (with-current-buffer src-buf
                                (save-excursion
                                  (save-restriction
                                    (widen)
                                    (buffer-string))))))
                   (process-send-string nil input)
                   (process-send-eof)))
                ((functionp command)
                 (save-excursion
                   (erase-buffer)
                   (funcall command src-buf)))
                (t
                 (insert "live-preview-command is not a string or function")))
          (save-selected-window
            (display-buffer pre-buf)))))))

(defun live-preview (command)
  "Turn live preview on or off for this buffer and set the preview COMMAND."
  (interactive
   (list (read-from-minibuffer
          "Preview command in this buffer: "
          live-preview-command nil nil
          'live-preview-command-history)))
  (setq command (unless (and (stringp command) (string-blank-p command))
                  command))
  (setq-local live-preview-command command)
  (cond (command
         (unless live-preview-mode
           (live-preview-mode))
         (message "Live preview on in this buffer"))
        (t
         (message "Live preview off in this buffer")))
  command)

;;;###autoload
(define-minor-mode live-preview-mode
  "Toggle automatic live preview in a side window.

When this minor mode (Live) is enabled, a live preview of your
source document is shown in a side window and updated whenever
you are idle for a few seconds.

Though this minor mode is enabled globally, only buffers that
have a `live-preview-command' cause a preview to be rendered."
  :lighter " Live"
  :global t
  (cancel-function-timers #'live-preview-show)
  (when live-preview-mode
    (run-with-idle-timer 2 t #'live-preview-show)))

(provide 'live-preview)

;;; live-preview.el ends here
