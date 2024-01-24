;;; my configuration for interacting with Clojure in Emacs ;;;

(use-package cider
  :ensure t
  :init (add-hook 'clojure-mode-hook 'cider-mode)
  :config (add-hook 'cider-mode-hook 'eldoc-mode))
  ;(add-hook 'cider-repl-mode-hook #'paredit-mode))

(eval-after-load 'clojure-mode
  '(progn
     (define-key clojure-mode-map (kbd "<f12>") 'cider-eval-sexp-at-point)
     ; maps below are actually for S-<f12> but come up as <f10> ???
     (evil-define-key 'normal clojure-mode-map (kbd "<f10>") 'cider-insert-defun-in-repl)
     (evil-define-key 'insert clojure-mode-map (kbd "<f10>") 'cider-insert-defun-in-repl)
     (evil-define-key 'visual clojure-mode-map (kbd "<f10>") 'cider-insert-region-in-repl)
     (define-key clojure-mode-map (kbd "ESC <f12>") 'cider-switch-to-repl-buffer)
     (define-key cider-repl-mode-map (kbd "ESC <f12>") 'cider-switch-to-last-clojure-buffer)
     (evil-define-key 'normal clojure-mode-map (kbd "SPC") 'scroll-up)
     (evil-define-key 'visual clojure-mode-map (kbd "SPC") 'scroll-up)
     (define-key cider-repl-mode-map (kbd "C-l") 'cider-repl-clear-buffer)
     (define-key cider-repl-mode-map (kbd "<up>") 'cider-repl-previous-input)
     (define-key cider-repl-mode-map (kbd "<down>") 'cider-repl-backward-input)
     (map! :leader "<f12>" #'cider-doc)))
