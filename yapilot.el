;;; yapilot.el --- Yet Another (Co)Pilot -*- lexical-binding: t; -*-

;; Copyright (C) 2024 Hiroyuki Yamada

;; Author: Hiroyuki Yamada
;; Created: 2024-05-04
;; Package-Requires: (llm markdown-mode)


;; This file is not part of GNU Emacs.


;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.


;;; Commentary:
;; This package provides copilot-like functionalities using LLM.
;; This package calls LLM through ahyatt/llm <https://github.com/ahyatt/llm>,
;; so that you need to set one of the llm providers to `yapilot-llm-provider'.


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
  "System Prompt to guide LLM."
  :type '(string)
  :group 'yapilot)

(defun yapilot--make-llm-prompt (prompt)
  "Make LLM prompt from PROMPT."
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
```
%s
```
"
  "Prompt Template for Code Review.

Source code will be inserted at %s by `format' function."
  :type '(string)
  :group 'yapilot)

(defcustom yapilot-generate-prompt
  "Generate %1$s code based on the following instruction.
You must generate only the source code.
The code must not enclosed by ```.

Instruction
-----------
%2$s
"
  "Prompt Template for Code Generation.

Programming Language name will be inserted at %1$s,
instruction at %2$s by `format' function."
  :type '(string)
  :group 'yapilot)

(defcustom yapilot-complete-prompt
  "Complete the following %1$s code.
You must generate only the source code.

Partial Code
------------
```
%2$s
```
"
  "Prompt Template for Code Completion.

Programming Language name will be inserted at %1$s,
partial code at %2s by `format' function"
  :type '(string)
  :group 'yapilot)

(defvar yapilot--code-regexp
  "```.*
\\(\\(.*\n\\)*.*\\)
```"
  "Regular Expression to match code block returned from LLM.")

(defun yapilot--validate ()
  "Validate provider."
  (if (null yapilot-llm-provider)
      (error "LLM provider is nil.  Please set one of the `llm' providers to `yapilot-llm-provider'")))

(defun yapilot--language ()
  "Determine programming language from major mode."
  (if (listp mode-name) (car mode-name) mode-name))

(defun yapilot--response-buffer ()
  "Create and display yapilot response buffer."
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
  (let ((prompt (format yapilot-review-prompt code)))
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

(defun yapilot--generate-code (prompt)
  "Generate code based on PROMPT."
  (let ((response (yapilot--chat prompt)))
    (if (string-match yapilot--code-regexp response)
        (match-string 1 response)
      response)))

(defun yapilot--generate (language instruction)
  "Generate LANGUAGE code based on INSTRUCTION."
  (yapilot--generate-code (format yapilot-generate-prompt language instruction)))

(defun yapilot-generate-region (start end)
  "Generate code based on selected region.
Argument START is region start.
Argument END is region end.

Unlike `yapilot-complete-region',
generated code is inserted just after selected instruction."
  (interactive "r")
  (if (use-region-p)
      (progn
        (goto-char end)
        (insert (yapilot--generate
                 (yapilot--language)
                 (buffer-substring start end))))
    (message "Region is not set")))

(defun yapilot--complete (language code)
  "Complete LANGUAGE CODE."
  (yapilot--generate-code (format yapilot-complete-prompt language code)))

(defun yapilot-complete-region (start end)
  "Complete code based on selected region.
Argument START is region start.
Argument END is region end.

Unlike `yapilot-generate-region',
selected region is replaced with generated code, too."
  (interactive "r")
  (if (use-region-p)
      (let ((code (yapilot--complete
                   (yapilot--language)
                   (buffer-substring start end))))
        (goto-char start)
        (delete-region start end)
        (insert code))
    (message "Region is not set")))

(provide 'yapilot)
;;; yapilot.el ends here
