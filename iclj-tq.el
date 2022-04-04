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

;; (require 'gv)
(require 'iclj-util)

(defvar iclj-tq-proc-eoc-found nil
  "End of command status indicator.")

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
  (cadr (cdddar (iclj-tq-queue tq))))

(defun iclj-tq-queue-head-wait-handler (tq)
  "Fetch TQ temporary buffer.
Buffer: process output buffer."
  (cdr (cdr (cdddar (iclj-tq-queue tq)))))

(defun iclj-tq-queue-empty-p (tq)
  "Return non-nil if queue (TQ) is empty."
  (not (iclj-tq-queue tq)))

(defmacro iclj-tq-with-live-process (tq &rest body)
  "Evaluate BODY forms if the TQ process is alive."
  (declare (indent 1)
           (debug t))
  `(when (iclj-tq-proc-live-p ,tq)
     ,@body))

(defun iclj-tq-filter-chunk (tq output)
  "Return OUTPUT string after TQ prompt-regexp cleanup."
  (replace-regexp-in-string (iclj-tq-prompt-regexp tq) "" output))

(defun iclj-tq-call-handler (tq)
  "Call TQ function handler."
  (progn
    (funcall (iclj-tq-queue-head-handler tq)
             (iclj-tq-queue-head-temp-buffer tq)
             (iclj-tq-queue-head-orig-buffer tq))
    ;; handler ends indicator
    (setcdr (cdr (cdddar (iclj-tq-queue tq))) nil)))

(defun iclj-tq-proc-filter (tq string)
  "Cache TQ output STRING."
  (mapc (lambda (fn)
          (and fn (funcall fn tq)))
        (if (setq iclj-tq-proc-eoc-found
                  (let ((temp (iclj-tq-filter-chunk tq
                                                    (if (and string
                                                             (stringp string))
                                                        string
                                                      ""))))
                    ;; insert chunk in buffer
                    (iclj-util-insert-chunk
                     (iclj-tq-queue-head-temp-buffer tq) temp)
                    ;; verifies if end of command was found
                    (iclj-util-with-log "process: end of command!" nil
                      (string-match-p (iclj-tq-proc-eoc tq) temp))))
            `((lambda (tq)
                (and (iclj-tq-queue-head-waitp tq)
                     (iclj-tq-call-handler tq)))
              iclj-tq-queue-pop)
          '())))

(defun iclj-tq-queue-add (tq
                          input
                          waitp
                          handler
                          orig-buffer
                          temp-buffer)
  "Add queue element: (INPUT WAIT-RESP-P HANDLER ORIG-BUFFER TEMP-BUFFER)
to the TQ head."
  (prog1 t
    (setcar tq (nconc (iclj-tq-queue tq)
                      `((,input
                         ,waitp
                         ,handler
                         ,orig-buffer
                         ,temp-buffer .
                         t))))))

(defun iclj-tq-wait--proc-loop (tq)
  "Wait TQ process output loop."
  ;; TODO: add timeout here (maybe local-quit)
  (let ((proc (iclj-tq-proc tq)))
    (and (process-live-p proc)
         (while (and (null iclj-tq-proc-eoc-found)
                     (accept-process-output proc 1 0 t))
           (sleep-for 0.01)))))

(defmacro iclj-tq-wait-proc-output (tq &rest body)
  "Evaluate BODY forms and force waiting for TQ process output confirmation."
  (declare (indent 1)
           (debug t))
  `(progn
     ,@body
     (iclj-tq-wait--proc-loop ,tq)))

(defmacro iclj-tq-eval-after-handler (tq func &rest body)
  "Evaluate BODY forms after TQ handler that will be called by FUNC ends."
  (declare (indent 2)
           (debug t))
  `(when (iclj-tq-proc-live-p ,tq)
     (mapc  #'funcall
            '(,func
              (lambda ()
                ;; TODO: add timeout here
                (while (eq (iclj-tq-queue-head-wait-handler ,tq) t)
                  (sleep-for 0.01)))))
     ,@body))

(defun iclj-tq--proc-send-input (proc string)
  "Send STRING to PROC stream."
  (if (not (process-live-p proc))
      (prog1 nil (iclj-tq-proc-delete proc))
    (prog1 t
      ;; reset eoc indicator variable
      (setq iclj-tq-proc-eoc-found nil)
      ;; send string to process stream
      (process-send-string proc (concat string "\n")))))

(defun iclj-tq-proc-send-input (tq)
  "Send TQ input using the correct process-send function."
  (and (iclj-tq--proc-send-input
        (iclj-tq-proc tq)
        (iclj-tq-queue-head-input tq))
       ;; call the handler function ASAP if wait is non-nil
       (not (iclj-tq-queue-head-waitp tq))
       (iclj-tq-call-handler tq)))

(defun iclj-tq-queue-head-kill-temp-buffer (tq)
  "Kill temporary output buffer if TQ queue head waitp is non-nil."
  (when (iclj-tq-queue-head-waitp tq)
    (kill-buffer (iclj-tq-queue-head-temp-buffer tq))))

(defun iclj-tq-queue-head-clean-temp-buffer (tq)
  "Delete end of command indicator from TQ temporary buffer."
  (apply 'iclj-util-delete-regexp
         `(,(or (iclj-tq-queue-head-temp-buffer tq) nil)
           ,(iclj-tq-proc-eoc tq))))

(defun iclj-tq-queue-pop (tq)
  "Pop TQ queue element."
  (mapc (lambda (fn) (funcall fn tq))
        `(iclj-tq-queue-head-kill-temp-buffer
          iclj-tq-queue-head-clean-temp-buffer
          (lambda (tq) (setcar tq (cdr (car tq))))
          (lambda (tq) (or (iclj-tq-queue-empty-p tq)
                           (iclj-tq-proc-send-input tq))))))

(defun iclj-tq-enqueue (tq
                        input
                        waitp
                        handler
                        orig-buffer
                        &optional
                        delay)
  "Add a transaction to transaction queue TQ.
This sends the INPUT string to the process that TQ communicates with.

If WAITP is non-nil we call the HANDLER function ASAP, otherwise we
wait for the response end of command indicator.

The HANDLER is called passing two arguments: the response and
the ORIG-BUFFER.

If DELAY is non-nil, delay sending this question until
the process has finished replying to any previous questions.
This produces more reliable results with some processes."
  ;; add queue to the transmission queue
  (iclj-tq-queue-add tq
                     input
                     waitp
                     handler
                     orig-buffer
                     (iclj-util-get-buffer-create
                      (generate-new-buffer-name "*iclj-proc-output*")))
  ;; send queue to process
  (and (or (not delay)
           (not (iclj-tq-queue tq))))
  (iclj-tq-proc-send-input tq))

(defun iclj-tq-proc-live-p (tq)
  "Return non-nil if process in the TQ is alive."
  (process-live-p (iclj-tq-proc tq)))

(defun iclj-tq-proc-delete (tq)
  "Shut down transaction queue TQ terminating the process.
This function always returns nil."
  (iclj-util-with-log "process deleted" t
    (delete-process (iclj-tq-proc tq))))

(defun iclj-tq-make (process process-eoc &optional prompt-regexp)
  "Set PROCESS filter and return the transaction queue.

PROCESS should be a sub process capable of sending and receiving
streams of bytes. It may be a local process, or it may be connected
to a tcp server on another machine.

PROCESS-EOC is used to indicate the end of command.

PROMPT-REGEXP the prompt regex, is used to clean the response buffer's content."

  (let ((tq `(nil ,process ,process-eoc ,prompt-regexp)))
    (set-process-filter process
                        (lambda (_p s)
                          (iclj-tq-proc-filter tq s)))
    tq))

(provide 'iclj-tq)

;;; iclj-tq.el ends here
