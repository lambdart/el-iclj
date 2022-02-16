;;; iclj-eval.el --- summary -*- lexical-binding: t -*-
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
(require 'iclj-overlay)

(defgroup iclj-eval nil
  "Iclj overlay features."
  :prefix "iclj-eval-"
  :group 'iclj-eval)

(defcustom iclj-eval-display-overlay-flag t
  "Non-nil means display eval output overlay."
  :group 'iclj-overlay
  :type 'boolean)

;;;###autoload
(defun iclj-eval-handler (output-buffer source-buffer)
  "Get last line from OUTPUT-BUFFER and display it in the SOURCE-BUFFER.
Overlay is the front end of choice."
  (when iclj-eval-display-overlay-flag
    (unless iclj-overlay-enabled
      (funcall 'iclj-overlay-enable))
    (let ((last-line (iclj-util-last-line output-buffer
                                          iclj-util-eoc
                                          "nil")))
      ;; display overlay with last-line in current buffer
      (iclj-overlay-display source-buffer
                            (concat " => " last-line)))))

(provide 'iclj-eval)

;;; iclj-eval.el ends here
