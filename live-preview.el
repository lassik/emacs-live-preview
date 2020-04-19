;;; live-preview.el --- Live preview by any shell command while editing -*- lexical-binding: t -*-
;;
;; Copyright 2019 Lassi Kortela
;; SPDX-License-Identifier: ISC
;; Author: Lassi Kortela <lassi@lassi.io>
;; URL: https://github.com/lassik/emacs-live-preview
;; Version: 0.1.1
;; Package-Requires: ((emacs "24.4"))
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
;; This is useful for previewing e.g. manual pages or other
;; documentation while writing them. Instead of a preview, could also
;; run a validator or crunch some statistics.
;;
;; Maybe in the future: graphics support (e.g. render HTML or TeX as
;; an image and show it in Emacs).
;;
;;; Code:

(eval-when-compile (require 'subr-x))  ; For string-blank-p.

(defconst live-preview--buffer-name
  "*live-preview*"
  "Name of the special buffer in which live previews are rendered.")

(defvar live-preview-command-history
  nil
  "List of old live preview commands.")

(defvar live-preview-idle-seconds
  2
  "How many seconds you have to be idle before live preview is updated.")

(defvar live-preview-max-buffer-size
  (* 100 1024)
  "Truncate preview if it is longer than this many characters.

This is meant to guard against rogue preview commands that
generate much more output than was intended.")

(defvar-local live-preview-command
  nil
  "Shell command or Emacs Lisp function to render the live preview.

This is the preview command for the current buffer. When
live-preview-mode is on, this command is run whenever you are
idle for a few seconds to update the preview.

The value can be:

* nil or a blank string -- No live preview is rendered.

* a string -- A shell command line to render the preview from
  standard input to standard output. Trivial examples are \"cat\"
  and \"tr a z\". Note that commands like \"make\" do not work
  because they do not expect to read data from standard input. An
  example of a complex command is rendering a Unix manual page
  written in the AsciiDoc markup language: \"asciidoctor -b
  manpage -o - - | nroff -man | col -bx\".

* a function -- Called with one argument, the source buffer. When
  called, the current buffer is the empty preview buffer. The
  function shall insert a preview of the source buffer there. The
  function is free to also make other change to the preview
  buffer, such as changing to a suitable major mode.")

(defun live-preview--stop ()
  "Stop any running live preview rendering process."
  (with-current-buffer (get-buffer-create live-preview--buffer-name)
    (when (get-buffer-process (current-buffer))
      (interrupt-process)
      (while (process-live-p (get-buffer-process (current-buffer)))
        (sleep-for 0.1)))))

(defun live-preview--from-scratch (thunk)
  "Internal helper function to render live preview using THUNK."
  (with-current-buffer (get-buffer live-preview--buffer-name)
    (with-selected-window (or (get-buffer-window (current-buffer) t)
                              (selected-window))
      (widen)
      (let ((old-line (line-number-at-pos (point)))
            (old-column (current-column))
            (old-start (window-start)))
        (erase-buffer)
        (funcall thunk)
        (widen)
        (let ((num-lines (count-lines (point-min) (point-max))))
          (goto-char (point-min))
          (forward-line (max 0 (1- (min old-line num-lines)))))
        (let ((line-length (- (point-at-eol) (point-at-bol))))
          (forward-char (min old-column line-length)))
        (set-mark (point))
        (set-window-start nil old-start)))))

(defun live-preview--show-shell (src-buf command)
  "Internal helper function to render live preview via shell command.

SRC-BUF is the user's source buffer that should be previewed.
COMMAND is the shell command as a string."
  (start-process-shell-command
   "live-preview" (current-buffer) command)
  (let ((all-output ""))
    (set-process-filter
     (get-buffer-process (current-buffer))
     (lambda (process new-output)
       (if (< (length all-output) live-preview-max-buffer-size)
           (setq all-output (concat all-output new-output))
         (interrupt-process process))))
    (set-process-sentinel
     (get-buffer-process (current-buffer))
     (lambda (process state)
       (when (equal "finished\n" state)
         (live-preview--from-scratch
          (lambda ()
            (insert all-output)
            (set-marker (process-mark process) (point)))))))
    (let ((input (with-current-buffer src-buf
                   (save-excursion
                     (save-restriction
                       (widen)
                       (buffer-string))))))
      (process-send-string nil input)
      (process-send-eof))))

(defun live-preview--show-function (src-buf userfun)
  "Internal helper function to render live preview via Lisp function.

SRC-BUF is the user's source buffer that should be previewed.
USERFUN is the Emacs Lisp function that renders the preview."
  (live-preview--from-scratch (lambda () (funcall userfun src-buf))))

(defun live-preview-show ()
  "Update the live preview immediately.

You don't normally need to call this function yourself. This is
called by a timer whenever you have been idle for a few seconds."
  (live-preview--stop)
  (let ((src-buf (current-buffer))
        (pre-buf (get-buffer-create live-preview--buffer-name))
        (command live-preview-command))
    (unless (or (eq src-buf pre-buf)
                (null command)
                (and (stringp command) (string-blank-p command)))
      (with-current-buffer pre-buf
        (cond ((stringp command)
               (live-preview--show-shell src-buf command))
              ((functionp command)
               (live-preview--show-function src-buf command))
              (t
               (insert "live-preview-command is not a string or function")))
        (save-selected-window
          (display-buffer pre-buf))))))

;;;###autoload
(define-minor-mode live-preview-mode
  "Toggle automatic live preview in a side window.

When this minor mode (Live) is enabled, a live preview of your
source document is shown in a side window and updated whenever
you are idle for a few seconds.

Use the `live-preview' command (M-x live-preview) in any buffer
to set the preview command for that buffer, or to turn the
preview off for that buffer.

Though this minor mode is enabled globally, only buffers that
have a `live-preview-command' cause a preview to be rendered."
  :lighter " Live"
  :global t
  (cancel-function-timers #'live-preview-show)
  (when live-preview-mode
    (run-with-idle-timer live-preview-idle-seconds t #'live-preview-show)))

;;;###autoload
(defun live-preview (command)
  "Turn live preview on or off for this buffer and set the preview command.

If COMMAND is blank, the preview is turned off. Else it can be a
string (shell command) or an Emacs Lisp function. Please see the
documentation for the `live-preview-command' variable for
details.

If you call this command interactively (i.e. \\<global-map>\\[live-preview])
it will ask you to type a shell command in the minibuffer (you
can leave it blank to turn off the preview for the current
buffer). If you call this function from Lisp (e.g. a hook in your
`user-init-file'), you can also set a Lisp function."
  (interactive
   (list (read-from-minibuffer
          "Preview command in this buffer: "
          (and (stringp live-preview-command) live-preview-command)
          nil nil 'live-preview-command-history)))
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

(provide 'live-preview)

;;; live-preview.el ends here
