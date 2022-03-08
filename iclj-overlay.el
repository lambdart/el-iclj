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

(require 'iclj-util)

(defgroup iclj-overlay nil
  "Iclj overlay features."
  :prefix "iclj-overlay-"
  :group 'iclj-overlay)

(defvar-local iclj-overlay-enabled nil
  "Indicates with overlay is already present in the current buffer.")

(defvar-local iclj-overlay (make-overlay (point-min)
                                         (point-min)
                                         nil
                                         t
                                         t)
  "Overlay used to display the process output text.")

(defun iclj-overlay-display (buffer text)
  "Display overlay with TEXT in the BUFFER."
  (when (and iclj-overlay-enabled (buffer-live-p buffer))
    (with-current-buffer buffer
      ;; move overlay to the right point
      (move-overlay iclj-overlay (point) (point) (current-buffer))
      ;; the current C cursor code doesn't know to use the overlay's
      ;; marker's stickiness to figure out whether to place the cursor
      ;; before or after the string, so let's spoon-feed it the pos.
      (put-text-property 0 1 'cursor t text)
      ;; put overlay after-string (text) property
      (overlay-put iclj-overlay 'after-string text))))

(defun iclj-overlay-delete ()
  "Remove `iclj-overlay' display (if any) prior to new user input."
  (delete-overlay iclj-overlay))

(defun iclj-overlay-enable ()
  "Enable overlay."
  (interactive)
  (setq iclj-overlay-enabled
        (prog1 t
          (add-hook 'pre-command-hook #'iclj-overlay-delete nil t))))

(defun iclj-overlay-disable ()
  "Disable overlay."
  (interactive)
  (setq iclj-overlay-enabled
        (prog1 nil
          (remove-hook 'pre-command-hook #'iclj-overlay-delete t))))

(provide 'iclj-overlay)

;;; iclj-overlay.el ends here
