;;; my configuration for interacting with BQN in Emacs ;;;
;;; https://mlochbaum.github.io/BQN/index.html


(use-package bqn-mode
  :bind (("C-c C-c C-e" . bqn-comint-send-buffer)
         ("C-c C-x C-e" . bqn-comint-send-region))
  :hook (bqn-mode . bqn-comint-buffer)
  :config
  (setq bqn-interpreter "~/comp/lang/array-languages/bqn/CBQN/BQN")
  (setq bqn-comint-interpreter-path "~/comp/lang/array-languages/bqn/CBQN/BQN")
  (require 'bqn-mode)
  (require 'bqn-keymap-mode)
  (require 'bqn-glyph-mode))

; (use-package bqn-key-prefix :after bqn-comint)
