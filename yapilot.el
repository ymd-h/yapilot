;;; yapilot.el --- Yet Another (Co)Pilot -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hiroyuki Yamada

;; Author: Hiroyuki Yamada
;; Created: 2024-05-04
;; Package-Requires: (llm)


;; This file is not part of GNU Emacs.


;;; Commentary:
;; This package provide copilot-like functions based on LLM.

;;; Code:
(require 'llm)


(defgroup yapilot nil
  "Yet Another (Co)Pilot."
  :group 'external)

(defvar yapilot-llm-provider nil
  "LLM provider for yapilot.")

(defun yapilot--make-llm-prompt (prompt)
  "Make LLM prompt from PROMPT"
  (if (fboundp 'llm-make-chat-prompt)
      (llm-make-chat-prompt prompt)  ; llm v1.4.0+
    (make-llm-chat-prompt
     :interactions
     (list (make-llm-chat-prompt-interaction :role 'user :content prompt)))))

(defcustom yapilot-response-buffer-name
  "*yapilot*" "Buffer name of LLM response."
  :type '(string)
  :group 'yapilot)

(defcustom yapilot-review-prompt
  "Review the following source code, and point out how to improve the code.

Code
----
"
  "Prompt Template for Code Review."
  :type '(string)
  :group 'yapilot)

(defun yapilot--validate ()
  "Validate provider."
  (if (null yapilot-llm-provider)
      (error "LLM provider is nil.  Please set one of the `llm' providers to `yapilot-llm-provider'")))

(defun yapilot--show-response (response)
  "Show LLM RESPONSE at new buffer."
  (save-excursion
    (let ((buffer (generate-new-buffer yapilot-response-buffer-name)))
      (with-current-buffer buffer
        (progn (insert response)
               (display-buffer buffer))))))

  (defun yapilot--show-response-streaming (buffer response)
    "Show LLM (partial) RESPONSE at specified BUFFER."
    (save-excursion
      (with-current-buffer buffer
        (erase-buffer)
        (insert response))))

  (defun yapilot--chat (prompt)
    "Chat with LLM using PROMPT."
    (yapilot--validate)
    (llm-chat yapilot-llm-provider (yapilot--make-llm-prompt prompt)))

  (defun yapilot--chat-async (prompt)
    "Chat with LLM asyncronically using PROMPT."
    (yapilot--validate)
    (llm-chat-async
     yapilot-llm-provider
     (yapilot--make-llm-prompt prompt) #'yapilot--show-response #'ignore))

  (defun yapilot--chat-streaming (prompt)
    "Chat with LLM streaming using PROMPT."
    (yapilot--validate)
    (let* ((buffer (generate-new-buffer yapilot-response-buffer-name))
           (callback #'(lambda (response)
                         (yapilot--show-response-streaming buffer response))))
      (display-buffer buffer)
      (llm-chat-streaming yapilot-llm-provider
                          (yapilot--make-llm-prompt prompt) callback callback #'ignore)))

  (defun yapilot--review (code)
    "Review CODE."
    (let ((prompt (concat yapilot-review-prompt code)))
      (yapilot--chat-streaming prompt)))

  (defun yapilot-review-region (start end)
    "Review code at selected region.
Argument START is region start.
Argument END is region end."
    (interactive "r")
    (if (use-region-p)
        (yapilot--review (buffer-substring start end))
      (message "Region is not set")))

  (defun yapilot-review-buffer ()
    "Review code at whole buffer."
    (interactive)
    (yapilot--review (buffer-string)))


  (provide 'yapilot)
;;; yapilot.el ends here
