;;; my configuration for interacting with Prolog in Emacs ;;;
;;; https://www.metalevel.at/ediprolog/

(require 'ediprolog)

(add-hook 'prolog-mode-hook
          (lambda () (define-key prolog-mode-map (kbd "<f12>") 'ediprolog-dwim)))

(global-unset-key (kbd "S-<f10>")) ;; had to clear this to access S-<f12>, which then comes up as <f10> ???


;; Define mode for file extensions
(add-to-list 'auto-mode-alist '("\\.pl\\'" . prolog-mode))

(defun open-prolog-history ()
  (interactive)
  (evil-window-vsplit)
  (evil-window-next nil)
  (switch-to-buffer "*ediprolog-history*")
  (goto-char (point-max))
  (evil-window-prev nil))

(defun ok-delete-ediprolog-consult ()
  (interactive)
  (delete-window (get-buffer-window "*ediprolog-consult*")))

(defun ediprolog-kill-process ()
  (interactive)
  (save-window-excursion
    (shell-command "pkill --signal 9 -f /usr/bin/prolog")))

(defun -ediprolog-ensure-no-trace ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward "?- trace," (+ (point) 12) t)
        (delete-backward-char 7))))

(defun -ediprolog-ensure-trace ()
  (save-excursion
    (beginning-of-line)
    (if (search-forward "?- " (+ (point) 10) t)
        (insert "trace, ")
      (message "Not on a query!"))))

(defun ediprolog-toggle-trace ()
  (interactive)
  (save-excursion
    (beginning-of-line)
    (if (search-forward "?- trace," (+ (point) 12) t)
        (delete-backward-char 7)
      (-ediprolog-ensure-trace))))

(defun ediprolog-debug-query ()
  (interactive)
  (ediprolog-toggle-trace)
  (ediprolog-query))

(defun ediprolog-eval-query ()
  (interactive)
  (-ediprolog-ensure-no-trace)
  (ediprolog-dwim))

(after! ediprolog
  (setq ediprolog-system 'scryer
        ediprolog-program "$HOME/.cargo/bin/scryer-prolog"))

(defun ok-fix-art-of-prolog-paste ()
  (interactive)
  (evil-execute-macro
   1
   (kmacro-lambda-form [?v ?a ?p
                           ?: ?s ?/ ?  ?/ return
                           ?g ?v ?: ?s ?/ ?I ?/ ?| ?/ return
                           ?g ?v ?: ?s ?/ ?! ?/ ?| ?/ return
                           ?g ?v ?: ?s ?/ ?- ?/ ?: ?- return
                           ?g ?v ?=] 0 "%d")))

;; (map! :after prolog
;;       :map prolog-inferior-mode-map
;;       :i "C-SPC" (cmd (insert ";") (comint-send-input)))
;;       :map prolog-mode-map
;;       :prefix "SPC"
;;       :n "t" 'ediprolog-toggle-trace
;;       :n "o" 'ediprolog-toplevel
;;       :n "c" 'ediprolog-consult
;;       :n "k" 'ediprolog-kill-process
;;       :n "r" 'ediprolog-remove-interactions
;;       :n "s" 'ok-to-snum
;;       :n "et" 'ediprolog-eval-query
;;       :n "ed" 'ediprolog-debug-query
;;       :n "eb" 'ediprolog-consult
;;       :n "h" 'open-prolog-history
;;       :n "a" 'ok-fix-art-of-prolog-paste
;;       :n "d" 'ok-delete-ediprolog-consult)

(defun ok-prolog-predicate-at-point ()
  (interactive)
  (save-excursion
    (let* ((beg (progn (evil-backward-WORD-begin) (point)))
           (end (progn (evil-jump-item) (point)))
           (open (progn (evil-jump-item) (point)))
           (name (buffer-substring-no-properties beg open))
           (args (buffer-substring-no-properties open (1+ end))))
      (str name "/" (length (read args))))))

(defun ok-prolog-send-string (s)
  (let ((s (str s "\n")))
    ;; Inserts it for the user to see.
    (prolog-process-insert-string (get-process "prolog") s)
    ;; Actually sends it.
    (process-send-string "prolog" s)))

(defun ok-prolog-apropos ()
  (interactive)
  (ok-prolog-send-string (str "apropos(" (read-string "Search for: ") ").")))

(defun ok-prolog-help ()
  (interactive)
  (ok-prolog-send-string (str "help(" (read-string "Help for: ") ").")))

(map! :after prolog
      :map prolog-inferior-mode-map
     ;:i "C-SPC" (cmd (ok-prolog-send-string ";"))
     ;:n "C-f" (cmd (process-send-string "prolog" " \n"))
      :map prolog-mode-map
      :prefix "<s-f8>"
      :n "jj" 'run-prolog
     ;:n "ds" (cmd (ok-prolog-send-string
     ;              (str "spy(" (ok-prolog-predicate-at-point) ").")))
     ;:n "dn" (cmd (ok-prolog-send-string "nospyall."))
      :n "eb" 'prolog-consult-buffer
      :map (prolog-inferior-mode-map prolog-mode-map)
      :n "dt" 'prolog-trace-on
      :n "do" 'prolog-trace-off
      :n "dd" 'ok-prolog-help
      :n "da" 'ok-prolog-apropos)

