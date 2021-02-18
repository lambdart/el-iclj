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

(defvar inf-clojure-debug-buffer-name "*CLOJURE-DEBUG*"
  "Debug (output) buffer name.")

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

(defvar inf-clojure-ops-alist
  `((load-file . "(clojure.core/load-file \"%s\")")
    (doc       . "(clojure.repl/doc %s)")
    (find-doc  . "(clojure.repl/find-doc \"%s\")")
    (source    . "(clojure.repl/source %s)")
    (apropos   . "(doseq [var (sort (clojure.repl/apropos \"%s\"))] (println (str var)))")
    (ns-vars   . "(clojure.repl/dir %s)")
    (set-ns    . "(clojure.core/in-ns '%s)"))
  "Operation associative list: (OP-KEY . OP-FMT).")

(defvar inf-clojure-version "0.01 Alpha"
  "Current version string.")

(defvar inf-clojure-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the process output text.")

(defvar inf-clojure-proc-buffer nil
  "Comint process buffer.")

(defvar inf-clojure-debug-buffer nil
  "Debug/Output buffer.")

(defvar inf-clojure-proc-output-list '()
  "Process output string list.")

(defvar inf-clojure-last-output-text ""
  "Process (cache) last output text.")

(defvar inf-clojure-last-output-line ""
  "Process (cache) last output line.")

(defvar inf-clojure-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.")

(defun inf-clojure-proc ()
  "Return comint buffer current process."
  (and inf-clojure-proc-buffer
       (get-buffer-process inf-clojure-proc-buffer)))

(defun inf-clojure-proc-sentinel (process event)
  "Sentinel function to handle (PROCESS EVENT) relation."
  (princ (format "Process: %s had the event '%s'" process event)))

(defun inf-clojure-proc-cache-output ()
  "Parse and cache the process output."
  (let ((text (mapconcat (lambda (str) str)
                         (reverse inf-clojure-proc-output-list) "")))
    ;; cache the filtered last text output
    (setq inf-clojure-last-output-text
          (dolist (regexp `(,inf-clojure-filter-regexp
                            ,comint-prompt-regexp
                            "[\r\n]$")
                          text)
            (setq text (replace-regexp-in-string regexp "" text))))
    ;; cache the last output line
    (setq inf-clojure-last-output-line
          (car (last (split-string inf-clojure-last-output-text "\n"))))))

(defun inf-clojure-proc-logs-output ()
  "Logs cached output (debug buffer)."
  ;; insert text in the debug buffer
  (with-current-buffer inf-clojure-debug-buffer
    (let ((buffer-read-only nil))
      (erase-buffer)
      (insert inf-clojure-last-output-text)))
  ;; echo last line
  (message "=> %s" inf-clojure-last-output-line))

(defun inf-clojure-proc-wait (proc timeout)
  "Wait for the PROC output, leave if reaches the TIMEOUT."
  (let ((string (car inf-clojure-proc-output-list)))
    ;; wait loop
    (while (and (stringp string)
                (not (string-match-p comint-prompt-regexp string)))
      ;; accept more output
      (accept-process-output proc timeout)
      ;; wait a little bit
      (sleep-for nil 100))
    ;; parse text output
    (inf-clojure-proc-cache-output)
    ;; finally display the text output
    (inf-clojure-proc-logs-output)))

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
            inf-clojure-last-output-text "")
      ;; send string (or region) to the process
      (apply 'funcall send-func proc args)
      ;; always send a new line
      (comint-send-string proc "\n")
      ;; accept process output
      (accept-process-output proc timeout)
      ;; wait for the process finishes
      (inf-clojure-proc-wait proc timeout))))

(defun inf-clojure-comint-send-string (string &optional op-key timeout)
  "Send STRING to the current inferior process.
Format the string selecting the right format using the OP-KEY.
TIMEOUT, the `accept-process-output' timeout."
  (let ((string (if (not op-key) string
                  (format (cdr (assoc op-key inf-clojure-ops-alist)) string))))
    ;; send string
    (inf-clojure-comint-send #'comint-send-string timeout string)))

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
  (setq comint-process-echoes t
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

(defun inf-clojure-comint--setup-debug-buffer ()
  "Setup the debug/output buffer."
  ;; kill debug buffer (if necessary)
  (kill-buffer inf-clojure-debug-buffer)
  ;; create and save the new buffer
  (setq inf-clojure-debug-buffer
        (get-buffer-create inf-clojure-debug-buffer-name))
  ;; change the major mode
  (with-current-buffer inf-clojure-debug-buffer
    ;; set read only
    (setq-local buffer-read-only t)
    ;; set clojure major mode if available
    (and (require 'clojure-mode nil t)
         (fboundp 'clojure-mode)
         (clojure-mode))))

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
    ;; set process sentinel
    (set-process-sentinel (get-buffer-process buffer) 'inf-clojure-proc-sentinel)
    ;; save current process buffer
    (setq inf-clojure-proc-buffer buffer)
    ;; start info-clojure-mode (comint related) and maybe jump to the buffer
    (with-current-buffer buffer (inf-clojure-mode))
    ;; setup debug buffer (if necessary)
    (inf-clojure-comint--setup-debug-buffer)
    ;; display buffer
    (display-buffer buffer 'display-buffer-pop-up-window)))

(defun inf-clojure-display-version ()
  "Echo the current `inf-clojure' version."
  (interactive)
  (message "inf-clojure (version %s)" inf-clojure-version))

(defun inf-clojure-display-overlay (line)
  "Display `inf-clojure-overlay' with the last LINE output."
  (move-overlay inf-clojure-overlay (point) (point) (current-buffer))
  ;; The current C cursor code doesn't know to use the overlay's
  ;; marker's stickiness to figure out whether to place the cursor
  ;; before or after the string, so let's spoon-feed it the pos.
  (put-text-property 0 1 'cursor t line)
  (overlay-put inf-clojure-overlay 'after-string line))

(defun inf-clojure-delete-overlay ()
  "Remove `info-clojure-overlay' display (if any) prior to new user input."
  (delete-overlay inf-clojure-overlay))

(defun inf-clojure-echo-last-output ()
  "Echo the last process output."
  (interactive)
  (message " %s" inf-clojure-last-output-text))

(defun inf-clojure-eval-defn ()
  "Send 'defn' to the inferior Clojure process."
  (interactive)
  (save-excursion
    (end-of-defun)
    (let ((end (point)))
      (beginning-of-defun)
      (inf-clojure-comint-send-region (point) end))))

(defun inf-clojure-read-thing (&optional prompt thing)
  "Return `thing-at-point' (string) or read it.
If PROMPT is non-nil use it as the read prompt.
If THING  is non-nil use it as the `thing-at-point' parameter,
default: 'symbol."
  (let* ((str (thing-at-point (or thing 'symbol) t))
         (fmt (if (not str) "%s: " "%s [%s]: "))
         (prompt (format fmt (or prompt "Str: ") str)))
    ;; return the read list string
    (list (read-string prompt nil nil str))))

(defun inf-clojure-eval-expression (string)
  "Eval STRING sexp in the current inferior Clojure process."
  (interactive (inf-clojure-read-thing "Eval" 'sexp))
  ;; eval string s-expression
  (inf-clojure-comint-send-string string))

(defun inf-clojure-eval-last-sexp ()
  "Send the previous sexp to the inferior Clojure process."
  (interactive)
  ;; send region of the last expression
  (inf-clojure-comint-send-region
   (save-excursion (backward-sexp) (point)) (point))
  ;; display the output in the overlay
  (inf-clojure-display-overlay
   (concat " => " inf-clojure-last-output-line)))

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
  (interactive (comint-get-source "Clojure file: "
                                  inf-clojure-prev-l/c-dir/file
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
  (inf-clojure-comint-send-string file-name 'load-file))

(defun inf-clojure-doc (name)
  "Invoke (doc NAME) operation."
  ;; map string function parameter
  (interactive (inf-clojure-read-thing "Doc"))
  ;; send doc operation
  (inf-clojure-comint-send-string name 'doc))

(defun inf-clojure-find-doc (str-or-regex)
  "Invoke (find-doc STR-OR-REGEX) operation."
  ;; map string function parameter
  (interactive (inf-clojure-read-thing "Find-doc"))
  ;; send doc operation
  (inf-clojure-comint-send-string str-or-regex 'find-doc))

(defun inf-clojure-source (symbol)
  "Invoke Clojure (source SYMBOL) operation."
  ;; map string function parameter
  (interactive (inf-clojure-read-thing "Source for"))
  ;; send source operation
  (inf-clojure-comint-send-string symbol 'source))

(defun inf-clojure-apropos (str-or-regex)
  "Invoke Clojure (apropos STR-OR-REGEX) operation."
  ;; map string function parameter
  (interactive (inf-clojure-read-thing "Search for"))
  ;; send apropos operation
  (inf-clojure-comint-send-string str-or-regex 'apropos))

(defun inf-clojure-ns-vars (nsname)
  "Invoke Clojure (dir NSNAME) operation."
  ;; map string function parameter
  (interactive (inf-clojure-read-thing "Namespace"))
  ;; send apropos operation
  (inf-clojure-comint-send-string nsname 'ns-vars))

(defun inf-clojure-set-ns (name)
  "Invoke Clojure (in-ns NAME) operation."
  ;; map string function parameter
  (interactive (inf-clojure-read-thing "Name"))
  ;; send apropos operation
  (inf-clojure-comint-send-string name 'set-ns))

(defun inf-clojure-syntax-table ()
  "Clojure-mode syntax table copy."
  (and (require 'clojure-mode nil t)
       (boundp 'clojure-mode-syntax-table)
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
    (define-key map (kbd "C-M-x")   #'inf-clojure-eval-defn) ; Gnu convention
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp)  ; Gnu convention
    (define-key map (kbd "C-c C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'inf-clojure-eval-defn) ; SLIME/CIDER style
    (define-key map (kbd "C-c C-b") #'inf-clojure-eval-buffer)
    (define-key map (kbd "C-c C-r") #'inf-clojure-eval-region)
    (define-key map (kbd "C-c C-l") #'inf-clojure-load-file)
    (define-key map (kbd "C-c C-q") #'inf-clojure-comint-quit)
    (easy-menu-define inf-clojure-minor-mode-menu map
      "Inferior Clojure Minor Mode Menu"
      '("Inf-Clojure"
        ["Eval region" inf-clojure-eval-region t]
        ["Eval buffer" inf-clojure-eval-buffer t]
        ["Eval function" inf-clojure-eval-defn t]
        ["Eval last sexp" inf-clojure-eval-last-sexp t]
        "--"
        ["Load file..." inf-clojure-load-file t]
        "--"
        ["Quit REPL" inf-clojure-comint-quit]))
    map))

;;;###autoload
(define-minor-mode inf-clojure-minor-mode
  "Minor mode for interacting with the inferior Clojure (comint) process buffer.

If called interactively, toggle ‘Inf-Clojure minor mode’.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode’s hook is called both when the mode is enabled and when
it is disabled.

The following commands are available:

\\{inf-clojure-minor-mode-map}"

  :lighter ""
  :keymap inf-clojure-minor-mode-map
  (cond
   (inf-clojure-minor-mode
    ;; set local input sender
    (setq-local comint-input-sender #'inf-clojure-comint-input-sender)
    ;; add delete overlay hook
    (add-hook 'pre-command-hook #'inf-clojure-delete-overlay))
   (t
    ;; ensure overlay was deleted
    (inf-clojure-delete-overlay)
    ;; remove delete overlay hook
    (remove-hook 'pre-command-hook #'inf-clojure-delete-overlay))))

(define-derived-mode inf-clojure-mode comint-mode "Inf-Clojure"
  "Major mode for `inf-clojure' comint buffer.

Runs a Clojure interpreter with the help of comint-mode,
use the buffer abstraction as the main I/O bridge between
Emacs and the subprocess.

You can send text to the inferior Clojure process from other buffers or the `minibuffer'
directly.

    `inf-clojure-eval-defn'  sends function definition
    `inf-clojure-eval-region' sends the current region
    `inf-clojure-eval-buffer' sends the current buffer

The following commands are available:

\\{inf-clojure-minor-mode-map}"

  :group 'inf-clojure
  :syntax-table (inf-clojure-syntax-table)
  ;; set comint variables
  (setq comint-prompt-regexp inf-clojure-prompt-regexp
        comint-prompt-read-only inf-clojure-prompt-read-only
        ;; functions
        comint-input-sender  #'inf-clojure-comint-input-sender
        comint-input-filter  #'inf-clojure-comint-input-filter
        comint-get-old-input #'inf-clojure-comint-get-old-input)
  ;; set clojure mode variable
  (when (require 'clojure-mode nil t)
    (set (make-local-variable 'font-lock-defaults) '(clojure-font-lock-keywords t)))
  ;; set local paragraph variables
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) inf-clojure-prompt-regexp))

;;;###autoload
(add-hook 'inf-clojure-mode-hook 'inf-clojure-comint-setup)

(provide 'inf-clojure)

;;; inf-clojure.el ends here
