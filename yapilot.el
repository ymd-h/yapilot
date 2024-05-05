;;; yapilot.el --- Yet Another (Co)Pilot -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hiroyuki Yamada

;; Author: Hiroyuki Yamada
;; Created: 2024-05-04
;; Package-Requires: (llm markdown-mode)


;; This file is not part of GNU Emacs.


;;; Commentary:
;; This package provide copilot-like functions based on LLM.

;;; Code:
(require 'llm)
(require 'markdown-mode)


(defgroup yapilot nil
  "Yet Another (Co)Pilot."
  :group 'external)

(defvar yapilot-llm-provider nil
  "LLM provider for yapilot.")

(defcustom yapilot-llm-system-prompt
  "You are skillful and professional programmer.
You will answer user's question with programming code.
The code is always concrete and understandable without omitting anything."
  "System Prompt to guide LLM"
  :type '(string)
  :group 'yapilot)

(defun yapilot--make-llm-prompt (prompt)
  "Make LLM prompt from PROMPT"
  (if (fboundp 'llm-make-chat-prompt)
      (llm-make-chat-prompt prompt :context yapilot-llm-system-prompt) ; llm v1.4.0+
    (make-llm-chat-prompt
     :context yapilot-llm-system-prompt
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

(defun yapilot--response-buffer ()
  "Create and display yapilot response buffer"
  (let ((buffer (generate-new-buffer yapilot-response-buffer-name)))
    (save-excursion
      (with-current-buffer buffer
        (display-buffer buffer)
        (markdown-mode)))
    buffer))

(defun yapilot--show-response (response)
  "Show LLM RESPONSE at new buffer."
  (save-excursion
    (let ((buffer (yapilot--response-buffer)))
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
    (let* ((buffer (yapilot--response-buffer))
           (callback #'(lambda (response)
                         (yapilot--show-response-streaming buffer response))))
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
