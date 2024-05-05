(toggle-debug-on-error)

(add-to-list
 'load-path
 (file-truename
  (concat (file-name-directory (or buffer-file-name load-file-name)) "..")))

(use-package yapilot
  :init
  (require 'llm-ollama)
  :config
  (setq yapilot-llm-provider (make-llm-ollama :chat-model "phi3")))
