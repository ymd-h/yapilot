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

(defcustom yapilot-response-buffer-name
  "yapilot" "Buffer name of LLM response."
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
	(with-current-buffer
		(generate-new-buffer yapilot-response-buffer-name)
	  (insert response))))

(defun yapilot--show-response-streaming (buffer response)
  "Show LLM (partial) RESPONSE at specified BUFFER."
  (save-excursion
	(with-current-buffer buffer
	  (erase-buffer)
	  (insert response))))

(defun yapilot--chat (prompt)
  "Chat with LLM.
Argument PROMPT prompt."
  (yapilot--validate)
  (llm-chat yapilot-llm-provider (llm-make-chat-prompt prompt)))

(defun yapilot--chat-async (prompt)
  "Chat with LLM asyncronically.
Argument PROMPT prompt."
  (yapilot--validate)
  (llm-chat-async yapilot-llm-provider (llm-make-chat-prompt prompt) #'yapilot--show-response #'ignore))

(defun yapilot--chat-streaming (prompt)
  "Chat with LLM streaming.
Argument PROMPT prompt."
  (yapilot--validate)
  (let* ((buffer (generate-new-buffer yapilot-response-buffer-name))
		 (callback #'(lambda (response) (yapilot--show-response-streaming buffer response))))
	(llm-chat-streaming yapilot-llm-provider (llm-make-chat-prompt prompt) #'callback #'callback #'ignore)))

(defun yapilot--review (code)
  "Review CODE."
  (let ((prompt (concat yapilot-review-prompt code)))
	(yapilot--chat-async prompt)))

(defun yapilot-review-region (start end)
  "Review code at selected region.
Argument START region start.
Argument END region end."
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
