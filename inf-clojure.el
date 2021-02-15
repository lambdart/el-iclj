;;; inf-clojure.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/inf-clojure.el
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

(require 'comint)
(require 'clojure-mode)

(defgroup inf-clojure nil
  "Clojure comint/inferior functionalities."
  :prefix "inf-clojure-"
  :group 'clojure)

(defcustom inf-clojure-prompt-read-only t
  "If non-nil, the comint buffer will be read-only."
  :group 'inf-clojure
  :type 'boolean)

(defcustom inf-clojure-pop-to-buffer-flag nil
  "Non-nil means pop to buffer in the same window."
  :group 'inf-clojure
  :type 'boolean)

(defcustom inf-clojure-buffer-name "clojure"
  "Inferior buffer default name."
  :group 'inf-clojure
  :type 'string)

(defcustom inf-clojure-debug-buffer-name "clojure-debug"
  "Inferior debug buffer name."
  :group 'inf-clojure
  :type 'string)

(defcustom inf-clojure-prompt-regexp "^[^>\n]*>+ *"
  "Regexp to recognize prompt."
  :group 'inf-clojure
  :type 'regexp)

(defcustom inf-clojure-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "Clojure comint input filter regex."
  :group 'inf-clojure
  :type 'regexp)

(defcustom inf-clojure-proc-timeout 2
  "The `accept-process-output' timeout in seconds."
  :group 'inf-clojure
  :type 'integer)

(defcustom inf-clojure-program (executable-find "clojure")
  "Clojure executable full path program."
  :group 'info-clojure
  :type 'string)
;; :set  `(lambda (symbol value)
;;          (set symbol (executable-find value))))

(defcustom inf-clojure-args '()
  "Command-line arguments to pass to `inf-clojure-program'."
  :group 'info-clojure
  :type 'list)

(defcustom inf-clojure-start-file nil
  "Inferior Clojure start file."
  :group 'info-clojure
  :type 'string)

(defcustom inf-clojure-source-modes '(clojure-mode)
  "Used to determine if a buffer contains Clojure sourc code.
If it's loaded into a buffer that is in one of these major modes, it's
considered a Clojure source file by `inf-clojure-load-file'."
  :group 'inf-clojure
  :type '(repeat function))

(defvar inf-clojure-version "0.01 Alpha"
  "Current version string.")

(defvar inf-clojure-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the process output text.")

(defvar inf-clojure-proc-buffer nil
  "The `inf-clojure' process buffer.")

(defvar inf-clojure-debug-buffer ""
  "The `inf-clojure' debug buffer.")

(defvar inf-clojure-proc-output-list '()
  "Process output string list.")

(defvar inf-clojure-last-text-output ""
  "Process last  string text output.")

(defvar inf-clojure-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.")

(defun inf-clojure-proc ()
  "Return comint buffer current process."
  (and inf-clojure-proc-buffer
       (get-buffer-process inf-clojure-proc-buffer)))

(defun inf-clojure-proc-parse-output ()
  "Parse process output list to string."
  (let ((text (mapconcat (lambda (str) str)
                         (reverse inf-clojure-proc-output-list) "")))
    ;; cache the filtered last text output
    (setq inf-clojure-last-text-output
          (dolist (regexp `(,inf-clojure-filter-regexp
                            ,comint-prompt-regexp
                            "[\r\n]$")
                          text)
            (setq text (replace-regexp-in-string regexp "" text))))))

(defun inf-clojure-display-output ()
  "Parse and display the comint output."
  (let ((text (inf-clojure-proc-parse-output)))
    (message " %s" text)))

(defun inf-clojure-proc-wait (proc timeout)
  "Wait for the PROC finishes or leave reaches the TIMEOUT (in seconds)."
  (let ((string (car inf-clojure-proc-output-list)))
    ;; wait loop
    (while (not (string-match-p comint-prompt-regexp string))
      ;; accept more output
      (accept-process-output proc timeout)
      ;; wait a little bit
      (sleep-for nil 100))
    ;; finally display the text output
    (inf-clojure-display-output)))

(defun inf-clojure-comint-preoutput-filter (string)
  "Return the output STRING."
  (let ((string (if (stringp string) string "")))
    ;; save the output
    (push string inf-clojure-proc-output-list)
    ;; return string to the comint buffer (implicit)
    string))

(defun inf-clojure-comint-send (send-func &optional timeout &rest args)
  "Send ARGS (string or region) using the chosen SEND-FUNC.
Possible values of SEND-FUNC are: `comint-send-string' or `comint-send-region'.
TIMEOUT, the `accept-process-output' timeout."
  (let ((proc (inf-clojure-proc))
        (timeout (or timeout inf-clojure-proc-timeout))
        (comint-preoutput-filter-functions '(inf-clojure-comint-preoutput-filter)))
    (when (process-live-p proc)
      ;; last comint output list should always start empty
      (setq inf-clojure-proc-output-list '()
            inf-clojure-last-text-output "")
      ;; send string (or region) to the process
      (apply 'funcall send-func proc args)
      ;; always send a new line
      (comint-send-string proc "\n")
      ;; accept more output
      (accept-process-output proc timeout)
      ;; wait for the process finishes
      (inf-clojure-proc-wait proc timeout))))

(defun inf-clojure-comint-send-string (string &optional timeout)
  "Send STRING to the current inferior process.
TIMEOUT, the `accept-process-output' timeout."
  ;; send string
  (inf-clojure-comint-send #'comint-send-string timeout string))

(defun inf-clojure-comint-send-region (start end &optional timeout)
  "Send region delimited bu START/END to the inferior process.
TIMEOUT, the `accept-process-output' timeout."
  (inf-clojure-comint-send #'comint-send-region timeout start end))

(defun inf-clojure-comint-input-sender (_ string)
  "Comint input sender STRING function."
  (inf-clojure-comint-send-string string))

(defun inf-clojure-comint-input-filter (string)
  "Don't save anything on the STRING matching `inf-clojure-filter-regexp'."
  (not (string-match-p inf-clojure-filter-regexp string)))

(defun inf-clojure-comint-get-old-input ()
  "Snarf the sexp ending at point."
  (save-excursion
    (let ((end (point)))
      (backward-sexp)
      (buffer-substring (point) end))))

(defun inf-clojure-comint-setup ()
  "Helper function to setup `comint-mode' related variables."
  (setq comint-process-echoes nil
        comint-input-ignoredups nil
        comint-use-prompt-regexp t))

(defun inf-clojure-comint-quit ()
  "Quit Clojure comint, i.e, quit subjob and kill the buffer."
  (interactive)
  ;; quit the subjob, if necessary
  (and inf-clojure-proc-buffer
       (with-current-buffer inf-clojure-proc-buffer
         (comint-quit-subjob)))
  ;; kill the buffer
  (kill-buffer inf-clojure-proc-buffer))

;;;###autoload
(defun inf-clojure-comint-run ()
  "Run an inferior instance of some Clojure REPL inside Emacs."
  (interactive)
  (let ((buffer (apply 'make-comint
                       inf-clojure-buffer-name
                       inf-clojure-program
                       inf-clojure-start-file
                       inf-clojure-args)))
    (unless buffer
      (error "[inf-clojure]: make-comint fails"))
    ;; check comint process
    (comint-check-proc buffer)
    ;; save current process buffer
    (setq inf-clojure-proc-buffer buffer)
    ;; start info-clojure-mode (comint related) and maybe jump to the buffer
    (with-current-buffer buffer (inf-clojure-mode))
    ;; display buffer
    (display-buffer buffer 'display-buffer-pop-up-window)))

(defun inf-clojure-display-version ()
  "Echo the current `inf-clojure' version."
  (interactive)
  (message "inf-clojure (version %s)" inf-clojure-version))

(defun inf-clojure-display-overlay ()
  "Display `inf-clojure-overlay' with the last process output."
  (let ((text (concat " => " inf-clojure-last-text-output)))
    (move-overlay inf-clojure-overlay (point) (point) (current-buffer))
    ;; The current C cursor code doesn't know to use the overlay's
    ;; marker's stickiness to figure out whether to place the cursor
    ;; before or after the string, so let's spoon-feed it the pos.
    (put-text-property 0 1 'cursor t text)
    (overlay-put inf-clojure-overlay 'before-string text)))

(defun inf-clojure-delete-overlay ()
  "Remove `info-clojure-overlay' display (if any) prior to new user input."
  (delete-overlay inf-clojure-overlay))

(defun inf-clojure-echo-last-output ()
  "Echo the last process output."
  (interactive)
  (message " %s" inf-clojure-last-text-output))

(defun inf-clojure-send-definition ()
  "Send definition to the inferior Clojure process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (inf-clojure-comint-send-region (point) end))))

(defun inf-clojure-eval-expression (string)
  "Eval STRING sexp in the current inferior Clojure process."
  (interactive (list (read-string "Eval: ")))
  ;; eval string s-expression
  (inf-clojure-comint-send-string string))

(defun inf-clojure-eval-last-sexp ()
  "Send the previous sexp to the inferior Clojure process."
  (interactive)
  (inf-clojure-comint-send-region
   (save-excursion (backward-sexp) (point)) (point)))

(defun inf-clojure-eval-buffer ()
  "Send the current buffer to the inferior Clojure process."
  (interactive)
  (save-excursion (widen)
                  (let ((case-fold-search t))
                    (inf-clojure-comint-send-region (point-min)
                                                    (point-max)))))

(defun inf-clojure-eval-region (start end)
  "Send the current region delimited by START/END to the inferior Clojure process."
  (interactive "r")
  (inf-clojure-comint-send-region start end))

(defun inf-clojure-load-file (file-name)
  "Load a Clojure file defined by its FILE-NAME into the inferior process."
  (interactive (comint-get-source "Clojure file: " inf-clojure-prev-l/c-dir/file
                                  inf-clojure-source-modes t))
  ;; if the file is loaded into a buffer, and the buffer is modified, the user
  ;; is queried to see if he wants to save the buffer before proceeding with
  ;; the load or compile
  (comint-check-source file-name)
  ;; cache previous directory/filename
  (setq inf-clojure-prev-l/c-dir/file
        (cons (file-name-directory file-name)
              (file-name-nondirectory file-name)))
  ;; load file
  (inf-clojure-comint-send-string
   (format "(clojure.core/load-file \"%s\")" file-name)))

(defun inf-clojure-syntax-table ()
  "Clojure-mode syntax table copy."
  (and (require 'clojure-mode nil t)
       (copy-syntax-table clojure-mode-syntax-table)))

(defvar inf-clojure-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-l") #'inf-clojure-load-file)
    (define-key map (kbd "C-c C-q") #'inf-clojure-comint-quit)
    (easy-menu-define inf-clojure-mode-menu map
      "Inferior Clojure REPL Menu"
      '("Inf-Clojure REPL"
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        "--"
        ["Load file" inf-clojure-load-file t]
        "--"
        ["Quit" inf-clojure-comint-quit]
        "--"
        ["Version" inf-clojure-display-version]))
    map))

(defvar inf-clojure-minor-mode-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x")   #'inf-clojure-send-definition) ; Gnu convention
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp)  ; Gnu convention
    (define-key map (kbd "C-c C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'inf-clojure-send-definition) ; SLIME/CIDER style
    (define-key map (kbd "C-c C-b") #'inf-clojure-eval-buffer)
    (define-key map (kbd "C-c C-r") #'inf-clojure-eval-region)
    (define-key map (kbd "C-c C-l") #'inf-clojure-load-file)
    (define-key map (kbd "C-c C-q") #'inf-clojure-comint-quit)
    (easy-menu-define inf-clojure-minor-mode-menu map
      "Inferior Clojure Minor Mode Menu"
      '("Inf-Clojure"
        ["Eval region" inf-clojure-eval-region t]
        ["Eval buffer" inf-clojure-eval-buffer t]
        ["Eval defn" inf-clojure-send-definition t]
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        "--"
        ["Load file..." inf-clojure-load-file t]
        "--"
        ["Quit REPL" inf-clojure-comint-quit]))
    map))

;;;###autoload
(define-minor-mode inf-clojure-minor-mode
  "Minor mode for interacting with the inferior Clojure process buffer.

The following commands are available:

\\{inf-clojure-minor-mode-map}"
  :lighter ""
  :keymap inf-clojure-minor-mode-map
  (setq-local comint-input-sender 'inf-clojure--send-string))

(define-derived-mode inf-clojure-mode comint-mode "Inf-Clojure"
  "Major mode for `inf-clojure' comint buffer.

\\<inf-clojure-mode-map>"

  :group 'inf-clojure
  :syntax-table (inf-clojure-syntax-table)
  ;; set comint variables
  ;;
  (setq comint-prompt-regexp inf-clojure-prompt-regexp
        comint-prompt-read-only inf-clojure-prompt-read-only
        comint-input-sender (function inf-clojure-comint-input-sender)
        comint-input-filter (function inf-clojure-comint-input-filter)
        comint-get-old-input (function inf-clojure-comint-get-old-input))
  ;; setup clojure mode
  (when (require 'clojure nil t)
    (clojure-mode-variables)
    (clojure-font-lock-setup)
    (set (make-local-variable 'font-lock-defaults) '(clojure-font-lock-keywords t)))
  ;; set local paragraph variables
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) inf-clojure-prompt-regexp))

;;;###autoload
(add-hook 'inf-clojure-mode-hook 'inf-clojure-comint-setup)

(provide 'inf-clojure)

;;; inf-clojure.el ends here
