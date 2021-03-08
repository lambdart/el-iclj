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

(defcustom inf-clojure-comint-prompt-read-only t
  "If non-nil, the comint buffer will be read-only."
  :group 'inf-clojure
  :type 'boolean)

(defcustom inf-clojure-pop-to-buffer-flag nil
  "Non-nil means pop to buffer in the same window."
  :group 'inf-clojure
  :type 'boolean)

(defcustom inf-clojure-display-comint-buffer-flag t
  "Non-nil means display comint-buffer after its creation."
  :group 'inf-clojure
  :type 'boolean)

(defcustom inf-clojure-buffer-name "clojure"
  "Inferior buffer default name."
  :group 'inf-clojure
  :type 'string)

(defcustom inf-clojure-comint-prompt-regexp "^\\( *#_\\|[^=> \n]+\\)=> *"
  "Regexp to recognize prompt."
  :group 'inf-clojure
  :type 'regexp)

(defcustom inf-clojure-filter-regexp "\\`\\s *\\(:\\(\\w\\|\\s_\\)\\)?\\s *\\'"
  "Clojure comint input filter regex."
  :group 'inf-clojure
  :type 'regexp)

(defcustom inf-clojure-program (executable-find "clojure")
  "Clojure executable full path program."
  :group 'info-clojure
  :type 'string
  :set  `(lambda (symbol value)
           (set symbol (executable-find value))))

(defcustom inf-clojure-debug-buffer-name "clojure-debug"
  "Inferior debug buffer name."
  :group 'inf-clojure
  :type 'string)

(defcustom inf-clojure-program-args '()
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

(defcustom inf-clojure-comint-output-timeout 2
  "Comint accept output timeout in seconds."
  :group 'inf-clojure
  :type 'integer)

(defvar inf-clojure-ops-alist
  `((load-file . "(clojure.core/load-file \"%s\")")
    (doc       . "(clojure.repl/doc %s)")
    (find-doc  . "(clojure.repl/find-doc \"%s\")")
    (source    . "(clojure.repl/source %s)")
    (apropos   . "(doseq [var (sort (clojure.repl/apropos \"%s\"))] (println (str var)))")
    (ns-vars   . "(clojure.repl/dir %s)")
    (set-ns    . "(clojure.core/in-ns '%s)"))
  "Operation associative list: (OP-KEY . OP-FMT).")

(defvar inf-clojure-version "0.02 Alpha"
  "Current version string.")

(defvar inf-clojure-overlay (make-overlay (point-min) (point-min) nil t t)
  "Overlay used to display the process output text.")

(defvar inf-clojure-proc-buffer nil
  "Comint process buffer.")

(defvar inf-clojure-debug-buffer nil
  "Debug/Output buffer.")

(defvar inf-clojure-comint-output-cache '()
  "Process output string list.")

(defvar inf-clojure-last-output-text ""
  "Process (cache) last output text.")

(defvar inf-clojure-last-output-line ""
  "Process (cache) last output line.")

(defvar inf-clojure-prev-l/c-dir/file nil
  "Caches the last (directory . file) pair.")

(defvar inf-clojure-comint-filter-in-progress nil
  "Indicates if the comint filter function is still in progress.")

(defun inf-clojure-display-version ()
  "Echo the current `inf-clojure' version."
  (interactive)
  (message "[INF-CLOJURE] (version %s)" inf-clojure-version))

(defun inf-clojure-display-overlay (text)
  "Display TEXT using the `inf-clojure-overlay' at point."
  (move-overlay inf-clojure-overlay (point) (point) (current-buffer))
  ;; The current C cursor code doesn't know to use the overlay's
  ;; marker's stickiness to figure out whether to place the cursor
  ;; before or after the string, so let's spoon-feed it the pos.
  (put-text-property 0 1 'cursor t text)
  (overlay-put inf-clojure-overlay 'after-string text))

(defun inf-clojure-delete-overlay ()
  "Hide `info-clojure-overlay' display."
  (delete-overlay inf-clojure-overlay))

(defun inf-clojure-get-proc ()
  "Return current inf-clojure- process."
  (get-buffer-process (if (buffer-live-p inf-clojure-proc-buffer)
                          inf-clojure-proc-buffer
                        (inf-clojure-comint-run))))

(defun inf-clojure-proc-sentinel (process event)
  "Sentinel function to handle (PROCESS EVENT) relation."
  (princ (format "Process: %s had the event '%s'" process event)))

(defun inf-clojure-filter-output-string (string)
  "Parse the process output STRING."
  (dolist (regexp `(,inf-clojure-comint-prompt-regexp) string)
    (setq string (replace-regexp-in-string regexp "" string))))

(defun inf-clojure-display-output ()
  "Display last output from the csi interpreter.

This function display (or insert the text) in diverse locations
with is controlled by the following custom flags:

`inf-clojure-display-overlay-flag',
`inf-clojure-insert-in-message-flag',
`inf-clojure-insert-in-debug-flag'

See its documentations to understand the be behavior
that will be imposed if they are true."

  ;; parse text output
  (let ((text (mapconcat
               (lambda (str)
                 (inf-clojure-filter-output-string str))
               (reverse inf-clojure-comint-output-cache) "")))
    ;; display the output in the overlay
    (inf-clojure-display-overlay (concat " => " text))
    ;; echo the output line
    (message " => %s" text)
    ;; return nil (convention)
    nil))

(defun inf-clojure-comint-in-progress-timeout ()
  "Timeout, set the control variable to nil."
  (setq inf-clojure-comint-filter-in-progress nil))

(defun inf-clojure-comint-preoutput-filter (string)
  "Return the output STRING."
  (let ((string (if (stringp string) string "")))
    ;; save the output
    (push string inf-clojure-comint-output-cache)
    ;; verify filter in progress control variable
    (when (string-match-p inf-clojure-comint-prompt-regexp string)
      (setq inf-clojure-comint-filter-in-progress nil))
    ;; return string to the comint buffer (implicit)
    string))

(defun inf-clojure-comint-send (send-func &rest args)
  "Send ARGS (region/string) using the chosen SEND-FUNC.
SEND-FUNC possible values: `comint-send-string', `comint-send-region'."
  (let ((proc (inf-clojure-get-proc)))
    ;; check if the process is alive
    (if (not (process-live-p proc))
        (message "[INF-CLOJURE]: Error, process not found")
      ;; comint output cache should always start empty
      (setq inf-clojure-comint-output-cache '()
            inf-clojure-comint-filter-in-progress nil)
      ;; send string (or region) to the process
      (apply 'funcall send-func proc args)
      ;; always send a new line
      (comint-send-string proc "\n")
      ;; run with timer
      (run-with-timer inf-clojure-comint-output-timeout
                      nil
                      'inf-clojure-comint-in-progress-timeout)
      ;; wait for the final prompt
      (while inf-clojure-comint-filter-in-progress
        (sleep-for 0 100)))))

;;;###autoload
(defun inf-clojure-comint-run ()
  "Run an inferior instance of some Clojure REPL inside Emacs."
  (interactive)
  (let ((buffer (apply 'make-comint
                       inf-clojure-buffer-name
                       inf-clojure-program
                       inf-clojure-start-file
                       inf-clojure-program-args)))
    (unless buffer
      (error "[INF-CLOJURE]: Error, wasn't possible to start the process"))
    ;; check comint process
    (comint-check-proc buffer)
    ;; set process sentinel
    (set-process-sentinel (get-buffer-process buffer) 'inf-clojure-proc-sentinel)
    ;; start inferior clojure comint mode
    (with-current-buffer buffer (inf-clojure-comint-mode))
    ;; display buffer if the custom flag is true
    (when inf-clojure-display-comint-buffer-flag
      (display-buffer buffer 'display-buffer-pop-up-window))
    ;; cache the process buffer
    (setq inf-clojure-proc-buffer buffer)))

(defun inf-clojure-comint-send-string (string &optional op-key)
  "Send STRING to the current inferior process.
Format the string selecting the right format using the OP-KEY.
TIMEOUT, the `accept-process-output' timeout."
  (let ((string (if (not op-key) string
                  (format (cdr (assoc op-key inf-clojure-ops-alist)) string))))
    ;; send string
    (inf-clojure-comint-send #'comint-send-string string)))

(defun inf-clojure-comint-send-region (start end)
  "Send region delimited bu START/END to the inferior process."
  (inf-clojure-comint-send #'comint-send-region start end))

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
  (inf-clojure-display-output))

(defun inf-clojure-eval-buffer ()
  "Send the current buffer to the inferior Clojure process."
  (interactive)
  (save-excursion
    (widen)
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
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-M-x")   #'inf-clojure-eval-defn) ; Gnu convention
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp) ; Gnu convention
    (define-key map (kbd "C-c C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-c") #'inf-clojure-eval-defn) ; SLIME/CIDER style
    (define-key map (kbd "C-c C-b") #'inf-clojure-eval-buffer)
    (define-key map (kbd "C-c C-r") #'inf-clojure-eval-region)
    (define-key map (kbd "C-c C-l") #'inf-clojure-load-file)
    (define-key map (kbd "C-c C-q") #'inf-clojure-comint-quit)
    map)
  "Inferior Clojure Minor Mode Map.")

(defun inf-clojure-define-menu ()
  "Define `inf-clojure' minor mode menu."
  (easy-menu-define inf-clojure-mode-menu inf-clojure-mode-map
    "Inferior Clojure Minor Mode Menu"
    '("Inf-Clojure"
      ["Eval region" inf-clojure-eval-region t]
      ["Eval buffer" inf-clojure-eval-buffer t]
      ["Eval function" inf-clojure-eval-defn t]
      ["Eval last sexp" inf-clojure-eval-last-sexp t]
      "--"
      ["Load file..." inf-clojure-load-file t]
      "--"
      ["Quit REPL" inf-clojure-comint-quit])))

(defvar inf-clojure-comint-mode-map
  (let ((map (copy-keymap comint-mode-map)))
    (define-key map (kbd "C-x C-e") #'inf-clojure-eval-last-sexp)
    (define-key map (kbd "C-c C-l") #'inf-clojure-load-file)
    (define-key map (kbd "C-c C-q") #'inf-clojure-comint-quit)
    map)
  "Extended Comint Mode Map.")

;;;###autoload
(define-minor-mode inf-clojure-mode
  "Minor mode for interacting with the Clojure REPL.

If called interactively, toggle `inf-clojure-mode'.  If the
prefix argument is positive, enable the mode, and if it is zero
or negative, disable the mode.

If called from Lisp, toggle the mode if ARG is ‘toggle’.  Enable
the mode if ARG is nil, omitted, or is a positive number.
Disable the mode if ARG is a negative number.

The mode’s hook is called both when the mode is enabled and when
it is disabled.

The following commands are available:

\\{inf-clojure-mode-map}"

  :lighter ""
  :keymap inf-clojure-mode-map
  (cond
   (inf-clojure-mode
    ;; define inf-clojure menu
    (inf-clojure-define-menu)
    ;; add comint preoutput filter function
    (add-hook 'comint-preoutput-filter-functions
              'inf-clojure-comint-preoutput-filter nil t)
    ;; add delete overlay hook
    (add-hook 'pre-command-hook #'inf-clojure-delete-overlay nil t))
   (t
    ;; ensure overlay was deleted
    (inf-clojure-delete-overlay)
    ;; remove preoutput filter function
    (remove-hook 'comint-preoutput-filter-functions
                 'inf-clojure-comint-preoutput-filter t)
    ;; remove delete overlay hook
    (remove-hook 'pre-command-hook #'inf-clojure-delete-overlay t))))

(define-derived-mode inf-clojure-comint-mode comint-mode "CLOJURE-REPL"
  "Major mode for the CLOJURE-REPL comint buffer.

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
  (setq comint-prompt-regexp inf-clojure-comint-prompt-regexp
        comint-prompt-read-only inf-clojure-comint-prompt-read-only
        comint-input-sender  (function inf-clojure-comint-input-sender)
        comint-input-filter  (function inf-clojure-comint-input-filter)
        comint-get-old-input (function inf-clojure-comint-get-old-input))
  ;; set clojure mode variable
  (when (require 'clojure-mode nil t)
    (set (make-local-variable 'font-lock-defaults)
         '(clojure-font-lock-keywords t)))
  ;; set local paragraph variables
  (set (make-local-variable 'paragraph-separate) "\\'")
  (set (make-local-variable 'paragraph-start) inf-clojure-comint-prompt-regexp))

;;;###autoload
(add-hook 'inf-clojure-comint-mode-hook 'inf-clojure-comint-setup)

(provide 'inf-clojure)

;;; inf-clojure.el ends here
