;; Some additions & modifications of Ellama I wrote to suit my needs
;; Ellama can be found here: https://github.com/s-kostyaev/ellama/

;; Set default LLM to use
;(setq default-llm "nollama/una-cybertron-7b-v2")
(setq default-llm "nous-hermes2")
;(setq default-llm "starling-lm")
;(setq default-llm "mistral")

;; declare the ellama package
(use-package ellama
  :init
  (setq ellama-language "English")
  (require 'llm-ollama)
  (setq ellama-provider (make-llm-ollama :chat-model default-llm :embedding-model default-llm)))

;; Set name that shows up preceding LLM responses to the name of the default LLM.
;; variable value will change when switching LLMs with ellama-select-ollama-model
(setq ellama-assistant-nick (concat default-llm " :"))

;; Some general settings ;;
(setq ellama-auto-scroll t)                 ; print text to screen as it is generated
(setq ellama-fill-paragraphs nil)           ; do not wrap text
(setq ellama-spinner-type 'rotating-line)   ; use the "rotating-line" progress indicator ["-" "\" "|" "/"]
;;
;; set characters that begin first line of prompt and first line of LLM reply
(setq ellama-prompt-prefix "## ")
(setq ellama-reply-prefix "### ")


;; Bring up a menu of ellama commands using hydra
(require 'hydra)
(defhydra ellama-menu (nil nil :foreign-keys nil :hint nil :exit t)
"Press key for command:
----------------------
s   send Selected text as a prompt in the ellama buffer
d   Define selected word
t   Translate the selected region
m   select from installed ollama Models
l   List installed ollama models
u   print cpu and ram Usage
M-k Kill ollama process
q   Quit"
  ("q" nil)
  ("s" ellama-ask-selected)
  ("d" ellama-define-word)
  ("t" ellama-translate)
  ("m" ellama-select-ollama-model)
  ("l" ollama-list)
  ("u" ollama-cpu-ram-usage)
  ("M-k" ollama-terminate))

(defun ellama-commands ()
  "Call ellama-menu and also print name of the current LLM"
  (interactive)
  (ellama-menu/body)
  (setq ellama-current-model (nth 4 (s-split " " (format "%s" ellama-provider))))
  (princ (format "%s %s" "         current model: " ellama-current-model)))

;; Some functions used in the ellama menu ;;

(defun ellama-select-ollama-model ()
  "Return LLM provider to interactively select ollama model, set ellama-assistant-nick to name of LLM
(a modification of ellama-get-ollama-local-model)"
  (interactive)
  (let ((model-name
         (completing-read "Select ollama model: "
                          (mapcar (lambda (s)
                                    (car (split-string s)))
                                  (seq-drop
                                   (process-lines ellama-ollama-binary "ls") 1)))))
   (setq ellama-provider
        (make-llm-ollama
         :chat-model model-name :embedding-model model-name))
   (setq ellama-assistant-nick (s-replace-regexp ":.*" " :" model-name))))

(defun ollama-list ()
  "list models installed by ollama, includes size & date installed"
  (interactive)
  (with-help-window "*ollama-models*"
    (with-current-buffer "*ollama-models*"
      (shell-command "ollama list" "*ollama-models*")
      (pop-to-buffer "*ollama-models*"))))

(defun ollama-cpu-ram-usage ()
"Show RAM and CPU used by Ollama processes (using shell-command and ps)"
  (interactive)
  (shell-command "ps -eo pmem,pcpu,cmd --sort=-pmem | grep ollama | sed '/grep/d' | awk '{print \"CPU: \" $2 \"%   RAM: \" $1 \"%   \" $3}'"))

(defun ollama-terminate ()
"Terminate running Ollama process (using shell-command and pkill)"
  (interactive)
  (shell-command "sudo /bin/pkill ollama"))

(defun move-to-next-md-heading ()
  "Move to next Markdown heading"
  (interactive)
  (markdown-outline-next)
  (evil-end-of-line-or-visual-line)
  (right-char))

;;;###autoload
(defun ellama-chat (prompt)
  "Send PROMPT to ellama chat with conversation history. This is a modification
of ellama-chat with line breaks adjusted for personal preference where the prompt
query text is preceded by the prefix (set to a markdown heading) and on the same
line so that the prompt text is visible as a heading when collapsing the structure
in markdown-mode. Also, PROMPT text is not printed to the buffer when in visual
state (and evil-mode thing) for when text in the buffer has been selected to be
sent to the LLM as PROMPT (must be a better way to do this, but works for now)."
  (interactive "sAsk ellama: ")
  (when (not (buffer-live-p (get-buffer ellama-buffer)))
    (get-buffer-create ellama-buffer)
    (with-current-buffer ellama-buffer
      (funcall ellama-buffer-mode)))
  (display-buffer ellama-buffer)
  (switch-to-buffer "*ellama*")
  (delete-other-windows)
  (with-current-buffer ellama-buffer
    (save-excursion
      (goto-char (point-max))
      (if
          (eq evil-state 'visual)
          (insert "\n\n" ellama-reply-prefix ellama-assistant-nick "\n")
        (insert ellama-user-nick prompt "\n\n" ellama-reply-prefix ellama-assistant-nick "\n"))
      (ellama-stream prompt
                     :session t
                     :on-done (lambda (_) (save-excursion
                                            (goto-char (point-max))
                                            (newline 2)
                                            (insert ellama-prompt-prefix))
                                (move-to-next-md-heading))))
    (goto-char (point-max))
    (evil-append 1)))

;;;###autoload
(defun ellama-send-line ()
  "Send current line as a PROMPT when chatting in the *ellama* buffer.
A modification of ellama-ask-line, but doesn't paste the prompt into the buffer
as the line has already been typed into the buffer. Any preceding # characters
(Markdown headings) are removed so they don't become part of PROMPT."
  (interactive)
  (let ((text (s-replace-regexp "^#*" "" (thing-at-point 'line))))
    (display-buffer ellama-buffer)
    (with-current-buffer ellama-buffer
      (save-excursion
        (goto-char (point-max))
        (newline 2)
        (insert ellama-reply-prefix ellama-assistant-nick)
        (newline)
        (ellama-stream text
                       :session t
                       :on-done (lambda (_)
                                  (save-excursion
                                    (goto-char (point-max))
                                    (newline 2)
                                    (insert ellama-prompt-prefix))
                                  (move-to-next-md-heading))))
      (goto-char (point-max)))))

;;;###autoload
(defun ellama-ask-selected ()
  (interactive)
  (if (get-buffer "*ellama*")
      (progn
        (setq ellama-user-nick "")
        (ellama-ask-selection)
        (switch-to-buffer "*ellama*")
        (delete-other-windows))
    (progn
      (setq ellama-user-nick ellama-prompt-prefix)
        (ellama-ask-selection))))


;;;###autoload
(defun ellama-chat-window ()
  "If an ellama chat window is already open, switch to it and maximize the window,
if no ellama window is open, call ellama-chat to initiate an interactive session."
  (interactive)
  (if (get-buffer "*ellama*")
      (progn
        (setq ellama-user-nick "")
        (switch-to-buffer "*ellama*")
        (delete-other-windows)
        (evil-append 1))
    (progn
      (setq ellama-user-nick ellama-prompt-prefix)
      (call-interactively #'ellama-chat)
      (evil-append 1))))


;; Key Mappings

(define-key evil-insert-state-map (kbd "M-l") 'ellama-commands)
(define-key evil-normal-state-map (kbd "M-l") 'ellama-commands)
(define-key evil-visual-state-map (kbd "M-l") 'ellama-commands)
;(define-key evil-visual-state-map (kbd "M-l") 'ellama-ask-selection)

(add-hook 'evil-markdown-mode-hook
         (lambda ()
           (evil-define-key 'normal evil-markdown-mode-map (kbd "M-l") 'ellama-commands)
           (evil-define-key 'insert evil-markdown-mode-map (kbd "M-l") 'ellama-commands)
           (evil-define-key 'visual evil-markdown-mode-map (kbd "M-l") 'ellama-commands)))
           ;(evil-define-key 'visual evil-markdown-mode-map (kbd "M-l") 'ellama-ask-selection)))

;(global-unset-key (kbd "M-l")) ; did not work for evil-markdown-mode
;(define-key evil-markdown-mode-map (kbd "M-l") nil) ; did not work either...

(defun ellama-return-key()
  (interactive)
  (if
      (eq evil-state 'visual)
      (call-interactively #'ellama-ask-selected)
    (call-interactively #'ellama-send-line)))

(map! :leader "l" #'ellama-chat-window "Chat with Ellama")
;(map! :leader "RET" 'ellama-send-line)
;(map! :leader "RET" #'ellama-return-key)
;(map! :localleader "RET" #'ellama-return-key)

;(after! markdown-mode-hook)
;(map! :map evil-markdown-mode-map :localleader "RET" #'ellama-return-key)

