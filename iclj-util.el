;;; iclj-util.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-util.el
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

(defun iclj-util-bounds-of-thing-at-point ()
  "Return expression bounds at point."
  (let ((bounds (if (use-region-p)
                    (cons (region-beginning) (region-end))
                  (or (bounds-of-thing-at-point 'symbol)
                      (bounds-of-thing-at-point 'word)
                      (cons (point)
                            (point))))))
    bounds))

(defun iclj-util-valid-buffer-last-line (buffer)
  "Return a 'valid' last line from BUFFER."
  (with-current-buffer buffer
    (save-excursion
      (widen)
      ;; go to the end of the buffer
      (goto-char (point-max))
      ;; always backward one line
      (forward-line -1)
      ;; while not a 'valid' line keep goin' backwards
      (while (and (> (point) (point-min))
                  (looking-at-p "nil"))
        (forward-line -1))
      ;; return the string that represents the last 'valid' line
      (buffer-substring-no-properties (point)
                                      (progn
                                        (end-of-line) (point))))))

(defun iclj-util-last-line (buffer)
  "Return the BUFFER last non-nil line."
  (let ((last-line "nil"))
    (if (not (buffer-live-p buffer)) last-line
      (iclj-util-valid-buffer-last-line buffer))))

(provide 'iclj-util)

;;; iclj-util.el ends here
