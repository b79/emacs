;; Some configuration and settings for using GPTel.
;; GPTel can be found here: https://github.com/karthink/gptel

(setq-default
 gptel-model "nollama/una-cybertron-7b-v2" ; Pick your default model
 gptel-backend
 (gptel-make-ollama
  "Ollama"  ; Any name of your choosing
  :host "localhost:11434"  ; Where it's running
  :stream t  ; Stream responses
  :models '("openhermes2.5-mistral" "nollama/una-cybertron-7b-v2" "mistral-openorca" "dolphin2.2-mistral:latest" "samantha-mistral:latest" "deepseek-coder:6.7b" "medllama2:latest" "open-orca-platypus2:latest")))  ; Installed models

;; No header line -- use mode line for status info and echo area for messages.
(setq gptel-use-header-line nil)

(setq-default gptel-prompt-prefix-alist
  '((markdown-mode . "## ")
    (org-mode . "*** ")
    (text-mode . "## ")))

(defun move-to-next-md-heading ()
  "Move to next Markdown heading"
  (interactive)
  (markdown-outline-next)
  (evil-end-of-line-or-visual-line)
  (right-char))

;;; move cursor down to response area after submitting prompt
(add-hook 'gptel-post-response-hook #'move-to-next-md-heading)
;(add-hook 'gptel-post-response-hook 'gptel-end-of-response) ; this didn't seem to work, so kept to the above
;(add-hook 'gptel-post-stream-hook 'gptel-auto-scroll) ; doesn't seem to work...

;; make highlighting of answer "pulse" rather than leaving it highlighted
(add-hook 'gptel-mode-hook (lambda () (setq pulse-flag "true")))

;; change name in status bar to say *LLM* not *ChatGPT*
(setq gptel-default-session "*LLM*")

;; some functions to call in the menu below...
(defun ollama-usage () (interactive) (shell-command "ps -o %cpu,%mem,cmd $(pgrep -d, -x ollama)"))
(defun ollama-terminate () (interactive) (shell-command "sudo /bin/pkill ollama"))

(defhydra gptel-dialog (nil nil :foreign-keys nil :hint nil :exit t)
" Press key for gptel command:
------------------------------
l  switch to or start LLM session with NAME.
m  gptel menu - change parameters of prompt
a  abort active gptel process in current buffer
u  list CPU and RAM used by ollama processes
k  terminate ollama processes

q quit"
  ("q" nil)
  ("l" gptel)
  ("m" gptel-menu)
  ("a" gptel-abort)
  ("u" ollama-usage)
  ("k" ollama-terminate))

;; send visually selected text as a prompt, display response in echo area
;; (does this pull in full buffer as context for the prompt...?)
(defun gptel-send-selection-echo-area (beg end)
  (interactive "r")
  (let ((prompt (buffer-substring beg end)))
    (when (string= prompt "") (user-error "A prompt is required."))
    (gptel-request
     message
     :callback
     (lambda (response info)
       (if (not response)
           (message "gptel-quick failed with message: %s" (plist-get info :status))
         (with-help-window "*llm_out*"
           (let ((inhibit-read-only t))
             (pop-to-buffer "*llm_out*")
 (gptel "*LLM*" response t)
             (insert))))))))

;; Send visually selected test to *LLM* buffer where it may be edited before being sent as a prompt
(defun gpt-send-selection ()
  (interactive)
    (evil-yank (region-beginning) (region-end))
    (gptel "*LLM*" nil nil t)
    (switch-to-buffer "*LLM*")
    ;(evil-goto-line)
    ;(evil-end-of-line)
    (evil-paste-after t))

;; Key Mappings

;(define-key global-map (kbd "M-l") 'gptel-dialog/body)

;; (define-key evil-normal-state-map (kbd "M-l") 'gptel-dialog/body)
;; (define-key evil-normal-state-map (kbd "M-l") 'gptel-dialog/body)
;; (define-key evil-visual-state-map (kbd "M-l") 'gpt-send-selection)

                                        ;(map! :leader "l" #'gptel "switch to or start gptel LLM session")
;(map! :leader "RET" 'gptel-send)

;; (add-hook 'evil-markdown-mode-hook
;;           (lambda ()
;;             (evil-define-key 'normal evil-markdown-mode-map (kbd "M-l") 'gptel-dialog/body)
;;             (evil-define-key 'insert evil-markdown-mode-map (kbd "M-l") 'gptel-dialog/body)
;;             (evil-define-key 'visual evil-markdown-mode-map (kbd "M-l") 'gpt-send-selection)))
