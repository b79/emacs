;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

;; Place your private configuration here! Remember, you do not need to run 'doom
;; sync' after modifying this file!

(add-load-path! "~/.config/doom/")

;(require 'evil-leader)

(require 'comint)

(require 'fennel-mode)

;;; load some configuration files ;;;
(load! "~/.config/doom/jfm_ellama.el")  ; LLM interface
(load! "~/.config/doom/jfm_clojure.el") ; Clojure lang
(load! "~/.config/doom/jfm_prolog.el")  ; Prolog lang
(load! "~/.config/doom/jfm_bqn.el")     ; BQN lang

;; Some functionality uses this to identify you, e.g. GPG configuration, email
;; clients, file templates and snippets. It is optional.
(setq user-full-name "John Magolske"
      user-mail-address "trimtab@b79.net")

;; Doom exposes five (optional) variables for controlling fonts in Doom:
;;
;; - `doom-font' -- the primary font to use
;; - `doom-variable-pitch-font' -- a non-monospace font (where applicable)
;; - `doom-big-font' -- used for `doom-big-font-mode'; use this for
;;   presentations or streaming.
;; - `doom-unicode-font' -- for unicode glyphs
;; - `doom-serif-font' -- for the `fixed-pitch-serif' face
;;
;; See 'C-h v doom-font' for documentation and more examples of what they
;; accept. For example:
;;
(setq doom-font (font-spec :family "Liberation Mono" :size 36 )
      doom-variable-pitch-font (font-spec :family "Liberation Mono" :size 36))
;;
;; If you or Emacs can't find your font, use 'M-x describe-font' to look them
;; up, `M-x eval-region' to execute elisp code, and 'M-x doom/reload-font' to
;; refresh your font settings. If Emacs still can't find your font, it likely
;; wasn't installed correctly. Font issues are rarely Doom issues!

;; There are two ways to load a theme. Both assume the theme is installed and
;; available. You can either set `doom-theme' or manually load a theme with the
;; `load-theme' function. This is the default:

;; soft-wrap long lines at word boundaries -- don't break words in two when wrapping lines
(global-visual-line-mode 1)

;; corresponds to Vim setting: whichwrap=b,s,h,l,<,>,[,],~
(setq evil-cross-lines 1)

(setq doom-theme 'doom-one-jfm)

;; set comments to be highlighted in more vivid colors
(setq doom-one-brighter-comments t)

; set mode-line to dark-grey so it stands out
(set-face-background 'mode-line "brightblack")

;; set some colors for Markdown
(custom-set-faces
 '(markdown-header-face ((t (:foreground "color-237"))))
 '(markdown-header-face-1 ((t (:foreground "brightwhite"))))
 '(markdown-header-face-2 ((t (:foreground "color-71"))))
 '(markdown-header-face-3 ((t (:foreground "color-179"))))
 '(markdown-code-face ((t (:background "color-234" :foreground "color-136" )))))

;; Tone down the highlighting from `pulse-momentary-highlight-region` -- see pulse.el
;; This came up when using bqn-mode.el
(custom-set-faces
 '(pulse-highlight-start-face ((t (:background "#3a3a3a")))))

;; This determines the style of line numbers in effect. If set to `nil', line
;; numbers are disabled. For relative line numbers, set this to `relative'.
(setq display-line-numbers-type nil)

;; If you use `org' and don't want your org files in the default location below,
;; change `org-directory'. It must be set before org loads!
(setq org-directory "~/org/")

;; setup the recent files list with recentf
(setq recentf-save-file (expand-file-name "recentf" user-emacs-directory)
      recentf-max-saved-items 1000
      recentf-auto-cleanup 'never)

(recentf-mode 1)

(defun add-buffer-file-to-recentf ()
  "add file in buffer to the recent list, cleanup the list (remove duplicates, excluded files, etc.)"
  (interactive)
  (when (buffer-file-name)
    (recentf-add-file (buffer-file-name))
    (recentf-save-list)) nil)

;;; Key mappings ;;;

;; evil-mode has <up> & <down> mapped to evil-previous-line & evil-next-line,
;; let's allow more fine-grained navigation across long wrapped lines:
(define-key evil-normal-state-map (kbd "<up>") 'previous-line)
(define-key evil-visual-state-map (kbd "<up>") 'previous-line)
(define-key evil-normal-state-map (kbd "<down>") 'next-line)
(define-key evil-visual-state-map (kbd "<down>") 'next-line)

;; make Space / BackSpace do page down / page up in normal & visual modes
(define-key evil-normal-state-map (kbd "SPC") 'scroll-up)
(define-key evil-visual-state-map (kbd "SPC") 'scroll-up)
(define-key evil-normal-state-map (kbd "<space>") 'scroll-up)
(define-key evil-visual-state-map (kbd "<space>") 'scroll-up)
(define-key evil-normal-state-map (kbd "DEL") 'scroll-down)
(define-key evil-visual-state-map (kbd "DEL") 'scroll-down)
(define-key evil-normal-state-map (kbd "<backspace>") 'scroll-down)
(define-key evil-visual-state-map (kbd "<backspace>") 'scroll-down)

;; Map M-o to make current window the only window
(define-key global-map (kbd "M-o") 'delete-other-windows)

;; Map F8 key to save buffer and add it to the recentf list 
(global-set-key (kbd "<f8>")
                (lambda () (interactive)
                  (add-buffer-file-to-recentf)
                  (save-buffer)))

;; Used "xcape -e 'Control_L=Shift_L|F10'" to have the left Ctrl key behave as Shift-F10
;; when tapped (see https://github.com/alols/xcape).
;; Now, have that key bring up a list of open & recently open buffers with fuzzy search:
; (define-key input-decode-map "\e[19;2~" [(s-f10)])    ; this worked with URxvt...
(define-key input-decode-map "\e[21;2~" [(s-f10)])      ; this is what s-F10 prints in XTerm
(after! ivy
  ;; when using ivy-switch-buffer include recentf-mode files & bookmarks
  (set-variable 'ivy-use-virtual-buffers t)
  (set-variable 'ivy-virtual-abbreviate 'full) ; use full file paths with recentf, etc.
  (evil-global-set-key 'normal (kbd "<s-f10>") 'ivy-switch-buffer)
  (evil-global-set-key 'insert (kbd "<s-f10>") 'ivy-switch-buffer)
  (global-set-key (kbd "<s-f10>") 'ivy-switch-buffer)
  (define-key ivy-minibuffer-map (kbd "<s-f10>")
    (lambda () (interactive)
      (keyboard-escape-quit)(kbd-quit-minibuff-update))))


;;; Leader Mappings ;;;

;; https://discourse.doomemacs.org/t/what-are-leader-and-localleader-keys/153

;; Change Leader key - Use `xcape -e 'Alt_L=Shift_L|F8'` to have Left Alt key behave
;; as Shift-F8, then set that to be the leader key (so tapping the Meta key is Leader)
; (define-key input-decode-map "\e[17;2~" [(s-f8)])     ; this worked with URxvt...
(define-key input-decode-map "\e[19;2~" [(s-f8)])       ; this is what s-F8 prints in XTerm
(setq doom-leader-key "<s-f8>"
      doom-localleader-key "<s-f8>"
      doom-leader-alt-key "<s-f8>")

;; Map leader-return key map to different functions depending on active mode
;; wanted to use localleader for this, but that doesn't work with minor modes,
;; so ended up writing this conditional statement:
(map! :leader "RET"
      (cmd!
       (call-interactively
        (cond ((derived-mode-p 'markdown-mode) #'ellama-return-key)
              ((derived-mode-p 'clojure-mode) #'cider-eval-sexp-at-point)
              ((derived-mode-p 'prolog-mode) #'prolog-consult-predicate)
              ((derived-mode-p 'bqn-mode) #'bqn-comint-eval-dwim)
              ((user-error ".   .  .  key not mapped  .  .   ."))))))

;(map! :leader "o" 'other-window)

;; <leader> f toggles writeroom mode
(map! :leader
      "o" 'other-window
      "m" #'hide-mode-line-mode
      "0" #'olivetti-mode
      "w" #'writeroom-mode
      "`" #'+workspace/other
      "<left>" #'+workspace/switch-left
      "<right>" #'+workspace/switch-right)

;; switch to previous buffer when tapping leader twice (no way to do `:localleader "<localleader>"` ??)
(map! :localleader "<s-f8>" #'mode-line-other-buffer)

;; Add a two-character left margin -- works in terminal Emacs
;; https://github.com/doomemacs/doomemacs/issues/567
;(setq-default left-margin-width 2 right-margin-width 2)

;(add-hook 'window-configuration-change-hook
;          (lambda ()
;            (set-window-buffer nil (current-buffer))))

(add-hook 'markdown-mode-hook
          (lambda ()
            (setq markdown-hide-urls 1)))

(require 'olivetti)
(add-hook 'olivetti-mode-hook
          (lambda ()
            (setq olivetti-body-width 86)))

(add-hook 'olivetti-mode-off-hook
          (lambda ()
            (set-window-margins (car (get-buffer-window-list (current-buffer) nil t)) 1 1)))

;(add-hook 'writeroom-mode-hook #'doom-modeline-mode) ; to keep modeline while in writeroom mode
(set-variable 'writeroom-width 41) ; set writeroom mode width
(global-set-key (kbd "<f7>") 'writeroom-mode)

;; flyspell
(require 'flyspell)

(defun toggle-flyspell ()
  (interactive)
  (when (not (boundp 'flyspell-status))
    (make-variable-buffer-local 'flyspell-status))
  (cond (flyspell-status
         (setq flyspell-status nil)
         (flyspell-mode-off))
        (t (setq flyspell-status (not flyspell-status))
           (flyspell-buffer)
           (flyspell-mode))))
(define-key global-map (kbd "<f5>") 'toggle-flyspell)
;(global-set-key (kbd "<f6>") 'ispell-buffer)

;; use flyspell-correct plugin to use ivy with flyspell
;(require 'flyspell-correct-ivy)
;(setq flyspell-correct-interface 'flyspell-correct-ivy)

(add-hook 'flyspell-mode-hook
          (lambda ()
           ; (define-key flyspell-mode-map (kbd "<f3>") 'flyspell-correct-word-generic)
           ; (define-key ivy-mode-map (kbd "<f4>") 'minibuffer-keyboard-quit)
           ;(define-key flyspell-mode-map (kbd "<f6>") 'flyspell-goto-next-error)
            (define-key flyspell-mode-map (kbd "<f4>") 'evil-prev-flyspell-error)
            (define-key flyspell-mode-map (kbd "<f6>") 'evil-next-flyspell-error)))

(set-face-background 'flyspell-incorrect "red")
(set-face-background 'flyspell-duplicate "red")
(set-face-foreground 'flyspell-incorrect "black")
(set-face-foreground 'flyspell-duplicate "black")



;;;;


;; Whenever you reconfigure a package, make sure to wrap your config in an
;; `after!' block, otherwise Doom's defaults may override your settings. E.g.
;;
;;   (after! PACKAGE
;;     (setq x y))
;;
;; The exceptions to this rule:
;;
;;   - Setting file/directory variables (like `org-directory')
;;   - Setting variables which explicitly tell you to set them before their
;;     package is loaded (see 'C-h v VARIABLE' to look up their documentation).
;;   - Setting doom variables (which start with 'doom-' or '+').
;;
;; Here are some additional functions/macros that will help you configure Doom.
;;
;; - `load!' for loading external *.el files relative to this one
;; - `use-package!' for configuring packages
;; - `after!' for running code after a package has loaded
;; - `add-load-path!' for adding directories to the `load-path', relative to
;;   this file. Emacs searches the `load-path' when you load packages with
;;   `require' or `use-package'.
;; - `map!' for binding new keys
;;
;; To get information about any of these functions/macros, move the cursor over
;; the highlighted symbol at press 'K' (non-evil users must press 'C-c c k').
;; This will open documentation for it, including demos of how they are used.
;; Alternatively, use `C-h o' to look up a symbol (functions, variables, faces,
;; etc).
;;
;; You can also try 'gd' (or 'C-c c d') to jump to their definition and see how
;; they are implemented.

;;;;;;;;;;
