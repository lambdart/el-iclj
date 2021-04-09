;;; iclj-overlay.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj
;; Version: 0.0.1 Alpha
;; Keywords:
;;
;; This file is NOT part of GNU Emacs.
;;
;;; MIT License
;;
;; Copyright (c) 2020 lambdart
;;
;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be included in
;; all copies or substantial portions of the Software.
;;
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.
;;
;;; Commentary:
;;
;;; Code:

(require 'iclj-comint)

(defvar-local iclj-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the process output text.")

(defvar-local iclj-overlay-enabled-flag nil
  "Non-nil means the overlay is available in the current buffer.")

(defun iclj-overlay-handler (buffer)
  "Handle the overlay OUTPUT display in the current BUFFER."
  ;; ensure the redirect buffer exists
  (let ((redirect-buffer (iclj-comint-redirect-buffer))
        (last-line "nil"))
    (unless (not (buffer-live-p redirect-buffer))
      (save-excursion
        ;; get the last line
        (with-current-buffer redirect-buffer
          ;; goto the final of the buffer and move one line up
          (goto-char (point-max))
          (forward-line -1)
          ;; verify the need of move one line up
          (and (looking-at-p "nil") (forward-line -1))
          ;; extract the last line
          (let ((beg (point)) end)
            (end-of-line)
            (setq end (point))
            (setq last-line (buffer-substring-no-properties beg end))))))
    ;; show overlay with the last-line text in the 'from-buffer'
    (with-current-buffer buffer
      ;; update last line
      (setq last-line (concat " => " last-line))
      ;; move overlay to the point
      (move-overlay iclj-overlay (point) (point) (current-buffer))
      ;; the current C cursor code doesn't know to use the overlay's
      ;; marker's stickiness to figure out whether to place the cursor
      ;; before or after the string, so let's spoon-feed it the pos.
      (put-text-property 0 1 'cursor t last-line)
      ;; show the overlay text
      (overlay-put iclj-overlay 'after-string last-line))))

(defun iclj-overlay-delete ()
  "Remove `iclj-overlay' display (if any) prior to new user input."
  (delete-overlay iclj-overlay))

(provide 'iclj-overlay)

;;; iclj-overlay.el ends here
