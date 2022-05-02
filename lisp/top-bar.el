;;; top-bar.el --- frame-local top-bar -*- lexical-binding: t; -*-

;; Author: Mingkai Dong <mk@dong.mk>
;; Keywords: frame top-bar
;; Maintainer: mk@dong.mk

;;; Commentary:

;; Provides `top-bar-mode' to control display of the top bar.

;;; Code:

(eval-when-compile
  (require 'cl-lib)
  (require 'seq))


(defgroup top-bar nil
  "Frame-local tabs."
  :group 'convenience
  :version "27.1")

(defgroup top-bar-faces '((top-bar custom-face)) ; top-bar is defined in faces.el
  "Faces used in the tab bar."
  :group 'top-bar
  :group 'faces
  :version "27.1")


(defun top-bar--top-bar-lines-for-frame (frame)
  "Determine and return the value of `top-bar-lines' for FRAME.
Return 0 if `top-bar-mode' is not enabled.  Otherwise return
either 1 or 0 depending on the value of the customizable variable
`top-bar-show', which see."
  (if top-bar-mode 1 0))

;;; Core function
(defun top-bar--update-top-bar-lines (&optional frames)
  "Update the `top-bar-lines' frame parameter in FRAMES.
If the optional parameter FRAMES is omitted, update only
the currently selected frame.  If it is t, update all frames
as well as the default for new frames.  Otherwise FRAMES should be
a list of frames to update."
  (let ((frame-lst (cond ((null frames)
                          (list (selected-frame)))
                         ((eq frames t)
                          (frame-list))
                         (t frames))))
    ;; Loop over all frames and update `top-bar-lines'
    (dolist (frame frame-lst)
      (unless (frame-parameter frame 'top-bar-lines-keep-state)
        (set-frame-parameter frame 'top-bar-lines
                             (top-bar--top-bar-lines-for-frame frame)))))
  ;; Update `default-frame-alist'
  (when (eq frames t)
    (setq default-frame-alist
          (cons (cons 'top-bar-lines (if top-bar-mode 1 0))
                (assq-delete-all 'top-bar-lines default-frame-alist)))))

(define-minor-mode top-bar-mode
  "Toggle the top bar in all graphical frames (Top Bar mode)."
  :global t
  ;; It's defined in C/cus-start, this stops the d-m-m macro defining it again.
  :variable top-bar-mode

  (top-bar-format-set "    HILO")
  ;; Recalculate `top-bar-lines' for all frames
  (top-bar--update-top-bar-lines t)
  )


(defun toggle-top-bar-mode-from-frame (&optional arg)
  "Toggle tab bar on or off, based on the status of the current frame.
Used in the Show/Hide menu, to have the toggle reflect the current frame.
See `top-bar-mode' for more information."
  (interactive (list (or current-prefix-arg 'toggle)))
  (if (eq arg 'toggle)
      (top-bar-mode (if (> (frame-parameter nil 'top-bar-lines) 0) 0 1))
    (top-bar-mode arg)))

(defun toggle-frame-top-bar (&optional frame)
  "Toggle top bar of the selected frame.
When calling from Lisp, use the optional argument FRAME to toggle
the top bar on that frame.
This is useful if you want to enable the top bar individually
on each new frame when the global `top-bar-mode' is disabled,
or if you want to disable the top bar individually on each
new frame when the global `top-bar-mode' is enabled, by using

  (add-hook \\='after-make-frame-functions #\\='toggle-frame-top-bar)"
  (interactive)
  (set-frame-parameter frame 'top-bar-lines
                       (if (> (frame-parameter frame 'top-bar-lines) 0) 0 1)))

(defvar top-bar-map
  (let ((map (make-sparse-keymap)))
    (define-key map [t] 'ignore)
    map)
  "Keymap for the commands used on the top bar.")

(global-set-key [top-bar]
                `(menu-item ,(purecopy "top bar") ignore
                            :filter top-bar-make-keymap))

(defun top-bar-make-keymap (&optional _ignore)
  "Generate an actual keymap from `tab-bar-map'.
Its main job is to show tabs in the tab bar
and to bind mouse events to the commands."
  tab-bar-map)

(defun top-bar-format-set (format-string &optional frame)
  "Set top bar FORMAT-STRING on the FRAME."
  (set-frame-parameter frame 'top-bar-format (propertize format-string
                                                         'keymap top-bar-map))
  (set-frame-parameter frame 'top-bar-lines 1))



(provide 'top-bar)

;;; top-bar.el ends here
