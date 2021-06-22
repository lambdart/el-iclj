;;; iclj-mode.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-mode.el
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

(require 'iclj-op)
(require 'iclj-eldoc)
(require 'iclj-eval)
(require 'iclj-comint)
(require 'iclj-overlay)
(require 'iclj-apropos)

(defvar iclj-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "<tab>")   #'iclj-op-completion-at-point)
    (define-key map (kbd "C-M-x")   #'iclj-op-eval-defn) ; Gnu convention
    (define-key map (kbd "C-c C-e") #'iclj-op-eval-last-sexp)
    (define-key map (kbd "C-x C-e") #'iclj-op-eval-last-sexp)  ; Gnu convention
    (define-key map (kbd "C-c C-r") #'iclj-op-eval-region)
    (define-key map (kbd "C-c C-c") #'iclj-op-eval-buffer)
    (define-key map (kbd "C-c C-d") #'iclj-op-doc)
    (define-key map (kbd "C-c C-f") #'iclj-op-find-doc)
    (define-key map (kbd "C-c C-a") #'iclj-op-apropos)
    (define-key map (kbd "C-c C-l") #'iclj-op-load-file)
    (define-key map (kbd "C-c C-b") #'iclj-op-load-buffer-file-name)
    (define-key map (kbd "C-c C-q") #'iclj-comint-quit)
    map)
  "Clojure REPL commands (or operations) keymap.")

(defun iclj-define-menu ()
  "Define the minor mode menu."
  (easy-menu-define iclj-mode-menu iclj-mode-map
    "Iclj Minor Mode Menu"
    '("ICLJ"
      ["Eval region" iclj-op-eval-region t]
      ["Eval buffer" iclj-op-eval-buffer t]
      ["Eval function" iclj-op-eval-defn t]
      ["Eval last sexp" iclj-op-eval-last-sexp t]
      "--"
      ["Load file" iclj-op-load-file t]
      ["Set NS" iclj-op-set-ns t]
      ["NS vars" iclj-op-ns-vars t]
      "--"
      ["Doc" iclj-op-doc t]
      ["Apropos" iclj-op-apropos t]
      ["Find-Doc" iclj-op-find-doc t]
      "--"
      ["Quit REPL" iclj-comint-quit])))

(defvar iclj-mode nil)

;;;###autoload
(defun iclj-mode-state ()
  "Show \\{iclj-mode} state, i.e: on or off."
  (interactive)
  (message "iclj-mode %s" (if iclj-mode "on" "off")))

(defun iclj-mode-setup (functions)
  "Call setup FUNCTIONS list (enable or disable functions)."
  (mapc (lambda (f) (funcall f)) functions))

;;;###autoload
(define-minor-mode iclj-mode
  "Minor mode for interacting with the Clojure REPL.

If called interactively, toggle \\[iclj-mode].  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode. Enable the mode if ARG is nil,
omitted, or is a positive number. Disable the mode if
ARG is a negative number.

The mode's hook is called both when the mode is enabled and when
it is disabled.

The following commands are available:

\\{iclj-mode-map}"

  :lighter ""
  :keymap iclj-mode-map
  (cond
   (iclj-mode
    ;; enable
    (iclj-mode-setup '(iclj-define-menu
                       iclj-eldoc-enable
                       iclj-overlay-enable)))
   (t
    ;; disable
    (iclj-mode-setup '(iclj-eldoc-disable
                       iclj-overlay-disable)))))

;; init iclj-mode after clojure-mode
;; (add-hook 'clojure-mode-hook 'iclj-mode)

(provide 'iclj-mode)

;;; iclj-mode.el ends here
