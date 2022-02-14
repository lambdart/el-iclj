;;; iclj-tq.el --- summary -*- lexical-binding: t -*-
;;
;; Author: lambdart <lambdart@protonmail.com>
;; Maintainer: lambdart
;; Homepage: https://github.com/lambdart/iclj-tq.el
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

(defvar iclj-tq-stream-name
  " *iclj-repl-server-stream*"
  "Default stream name.")

(defun iclj-tq-queue (tq)
  "Fetch TQ queue.
Looks like (queue proc . eoc)."
  (car tq))

(defun iclj-tq-proc (tq)
  "Fetch TQ process."
  (cadr tq))

(defun iclj-tq-proc-eoc (tq)
  "Fetch TQ process end of command indicator."
  (caddr tq))

(defun iclj-tq-prompt-regexp (tq)
  "Fetch TQ prompt regexp."
  (cadddr tq))

(defun iclj-tq-queue-head-input (tq)
  "Fetch TQ question.
The structure of `queue' is as follows:
\((input regexp closure . (handler . buffer))).
Input: string to send to the process."
  (caar (iclj-tq-queue tq)))

(defun iclj-tq-queue-head-waitp (tq)
  "Fetch TQ wait response predicate."
  (cadar (iclj-tq-queue tq)))

(defun iclj-tq-queue-head-handler (tq)
  "Fetch TQ handler.
Handler: function to call upon receiving a complete response from the process."
  (caddar (iclj-tq-queue tq)))

(defun iclj-tq-queue-head-orig-buffer (tq)
  "Fetch TQ origin buffer."
  (car (cdddar (iclj-tq-queue tq))))

(defun iclj-tq-queue-head-temp-buffer (tq)
  "Fetch TQ temporary buffer.
Buffer: process output buffer."
  (cdr (cdddar (iclj-tq-queue tq))))

(defun iclj-tq-queue-empty-p (tq)
  "Return non-nil if queue (TQ) is empty."
  (not (iclj-tq-queue tq)))

(defun iclj-tq-filter-chunk (tq string)
  "Return output STRING after TQ prompt-regexp removal."
  (let ((prompt-regexp (iclj-tq-prompt-regexp tq)))
    (format "%s"
            (if (and string (stringp string))
                ;; remove prompt
                (if prompt-regexp
                    (replace-regexp-in-string prompt-regexp "" string)
                  string
                  "")))))

(defun iclj-tq-call-handler (tq)
  "Call TQ function handler."
  (let ((func (iclj-tq-queue-head-handler tq)))
    ;; call function handler
    (funcall func
             (iclj-tq-queue-head-temp-buffer tq)
             (iclj-tq-queue-head-orig-buffer tq))))

(defvar iclj-tq-proc-eoc-found nil)

(defun iclj-tq-proc-filter (tq string)
  "Cache TQ output STRING."
  ;; analyse if output has the end of request indicator (prompt regexp)
  (let ((chunk (iclj-tq-filter-chunk tq string)))
    ;; skip if chunk is empty
    (unless (string-empty-p chunk)
      (let ((buffer (iclj-tq-queue-head-temp-buffer tq))
            (inhibit-read-only t))
        (when (buffer-live-p buffer)
          (with-current-buffer buffer
            (save-excursion
              ;; go to point max before insertion
              (goto-char (point-max))
              ;; insert the output chunk the temporary output buffer
              (insert chunk)))))
      ;; logs if EOC was found (debugging)
      (when (string-match-p (iclj-tq-proc-eoc tq) chunk)
        ;; change state of control variable
        (setq iclj-tq-proc-eoc-found t)
        ;; log the message
        (iclj-util-log "process: end of command!")
        ;; call the handler if it's waiting
        (when (iclj-tq-queue-head-waitp tq)
          (iclj-tq-call-handler tq))
        ;; remove queue head (operation)
        (iclj-tq-queue-pop tq)))))

;;;###autoload
(defun iclj-tq--make (process process-eoc &optional prompt-regexp)
  "Make and return a transaction queue that communicates with PROCESS.
PROCESS should be a sub process capable of sending and receiving
streams of bytes.  It may be a local process, or it may be connected
to a tcp server on another machine.

PROCESS-EOC is used to indicate the end of command.
PROMPT-REGEXP the prompt regex, is used to clean the response buffer's content."

  (let ((tq `(nil ,process ,process-eoc ,prompt-regexp)))
    (set-process-filter process
                        (lambda (_p s)
                          (iclj-tq-proc-filter tq s)))
    tq))

(defun iclj-tq-queue-add (tq
                          input
                          waitp
                          handler
                          orig-buffer
                          temp-buffer)
  "Add queue element: (INPUT WAIT-RESP-P HANDLER ORIG-BUFFER TEMP-BUFFER)
to the TQ head."
  (setcar tq
          (nconc (iclj-tq-queue tq)
                 `((,input
                    ,waitp
                    ,handler
                    ,orig-buffer .
                    ,temp-buffer))))
  t)

(defun iclj-tq-proc-send-input (tq)
  "Send TQ input using the correct process-send function."
  (let ((process (iclj-tq-proc tq))
        (input (iclj-tq-queue-head-input tq))
        (waitp (iclj-tq-queue-head-waitp tq)))
    ;; verify if process is alive
    (if (not (process-live-p process))
        (progn
          ;; delete process structure
          (delete-process process)
          ;; display log message
          (iclj-util-log "error: connection isn't live" t))
      ;; reset end of command state variable
      (setq iclj-tq-proc-eoc-found nil)
      ;; send region or string to the process
      (process-send-string process (concat input "\n"))
      ;; when waitp is non-nil do not wait for the response,
      ;; call the handler ASAP
      (if (not waitp)
          (iclj-tq-call-handler tq)
        ;; wait for the end of command indicator
        ;; TODO: add timeout here
        (while (and (null iclj-tq-proc-eoc-found)
                    (accept-process-output process 1 0 t))
          (sleep-for 0.01))))))

(defun iclj-tq-queue-head-kill-temp-buffer (tq)
  "Kill temporary output buffer if TQ queue head waitp is non-nil."
  (when (iclj-tq-queue-head-waitp tq)
    (kill-buffer (iclj-tq-queue-head-temp-buffer tq))))

(defun iclj-tq-queue-pop (tq)
  "Pop TQ queue element."
  (iclj-tq-queue-head-kill-temp-buffer tq)
  ;; update TQ car
  (setcar tq (cdr (car tq)))
  ;; send next queue operation (if any)
  (or (iclj-tq-queue-empty-p tq)
      (iclj-tq-proc-send-input tq)))

(defun iclj-tq-enqueue (tq
                        input
                        waitp
                        handler
                        orig-buffer
                        &optional delay-question)
  "Add a transaction to transaction queue TQ.
This sends the INPUT string to the process that TQ communicates with.

If WAITP is non-nil we call the HANDLER function ASAP, otherwise we
wait for the response end of command indicator.

The HANDLER is called passing two arguments: the response and
the ORIG-BUFFER.

If DELAY-QUESTION is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes."
  (let ((sendp (or (not delay-question)
                   (not (iclj-tq-queue tq)))))
    ;; add queue to the transmission queue
    (iclj-tq-queue-add tq
                       input
                       waitp
                       handler
                       orig-buffer
                       (iclj-util-get-buffer-create
                        (generate-new-buffer-name "*iclj-proc-output*")))
    ;; send queue to process
    (when sendp
      (iclj-tq-proc-send-input tq))))

(defun iclj-tq-proc-delete (tq)
  "Shut down transaction queue TQ terminating the process.
This function always returns nil."
  (progn (delete-process (iclj-tq-proc tq)) nil))

(defun iclj-tq-open (host port process-eoc &optional prompt-regexp)
  "Open transaction queue using the HOST/PORT to create the process.

PROCESS-EOC, the process end of command indicator (arbitrary).

When non-nil PROMPT-REGEXP will be used to filter the output chunk,
removing it from the response.

This function returns an instance of transmission queue, that as to be cached
by the caller."
  (let* ((process (open-network-stream iclj-tq-stream-name
                                       nil
                                       host
                                       port
                                       ;; :end-of-command iclj-tq-eoc
                                       :return-list nil
                                       :type 'network
                                       :nogreeting t
                                       :nowait nil))
         (tq (iclj-tq--make process
                            process-eoc
                            prompt-regexp)))
    ;; set our process filter
    (set-process-filter process
                        `(lambda (p s) (iclj-tq-proc-filter ',tq s)))
    ;; return transaction queue instance
    tq))

(provide 'iclj-tq)

;;; iclj-tq.el ends here
