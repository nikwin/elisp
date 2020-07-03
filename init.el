;; ======= Basic initializations =======

(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'load-path "~/elisp")

(setq split-height-threshold nil)
(setq split-width-threshold 160)

;; ======= Indentation =======

(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

;; ======= Color themes =======

(add-to-list 'load-path "~/elisp/color-theme-6.6.0")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)
     (color-theme-hober)))

;; ======= Spell checks =======

(setq ispell-program-name "/opt/local/bin/aspell")

(add-hook 'c++-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'js-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'python-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'lisp-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'emacs-lisp-mode-hook (lambda () (flyspell-prog-mode)))
(add-hook 'text-mode-hook (lambda () (flyspell-mode 1)))

(global-set-key (kbd "<f8>") 'flyspell-buffer)


;; ======= Org Mode =======

(add-to-list 'auto-mode-alist '("\\.org\\'" . org-mode))
(define-key global-map "\C-cr" 'org-remember)

(global-set-key (kbd "\C-c a") 'org-agenda)
(global-set-key (kbd "\C-c b") 'org-iswitchb)

(add-hook 'org-mode-hook (lambda () (flyspell-mode 1)))

;; ======= Insert a date =======

(defun insert-date (prefix)
  "Insert the current date. With prefix-argument, use ISO format. With
   two prefix arguments, write out the day and month name."
  (interactive "P")
  (let ((format (cond
		 ((not prefix) "%Y-%m-%d")
		 ((equal prefix '(4)) "%d.%m.%Y")
		 ((equal prefix '(16)) "%A, %d. %B %Y")))
	(system-time-locale "en_US"))
    (insert (format-time-string format))))

 (global-set-key (kbd "C-c d") 'insert-date)

;; ======= Version Control Modes =======


;; ======= Suppress Confirm For Killing Process =======

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  "Prevent annoying \"Active processes exist\" query when you quit Emacs."
  (flet ((process-list ())) ad-do-it))

;; ======= Recent Files =======

(require 'recentf)
(recentf-mode 1)

(global-set-key (kbd "C-x M-f") 'recentf-open-files)

;; ======= Pretty Greek =======

(defun pretty-greek ()
  (let ((greek '("alpha" "beta" "gamma" "delta" "epsilon" "zeta" "eta" "theta" "iota" "kappa" "lambda" "mu" "nu" "xi" "omicron" "pi" "rho" "sigma_final" "sigma" "tau" "upsilon" "phi" "chi" "psi" "omega")))
    (loop for word in greek
          for code = 97 then (+ 1 code)
          do  (let ((greek-char (make-char 'greek-iso8859-7 code))) 
                (font-lock-add-keywords nil
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[a-zA-Z]")
                                           (0 (progn (decompose-region (match-beginning 2) (match-end 2))
                                                     nil)))))
                (font-lock-add-keywords nil 
                                        `((,(concatenate 'string "\\(^\\|[^a-zA-Z0-9]\\)\\(" word "\\)[^a-zA-Z]")
                                           (0 (progn (compose-region (match-beginning 2) (match-end 2)
                                                                     ,greek-char)
                                                     nil)))))))))  (add-hook 'lisp-mode-hook 'pretty-greek)


(add-hook 'emacs-lisp-mode-hook 'pretty-greek)
(add-hook 'python-mode-hook 'pretty-greek)

;; ======= Synosaurus -------

(add-to-list 'load-path "~/elisp/synosaurus")
(require 'synosaurus)
(require 'synosaurus-wordnet)


;; ======= CSV Mode =======

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.bcsv$" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.gen_csv$" . csv-mode))
(add-to-list 'auto-mode-alist '("\\.archive_csv$" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)
(add-hook 'csv-mode-hook (lambda () (synosaurus-mode)))

;; ======= Uniquify =======

(require 'uniquify) 
(setq uniquify-buffer-name-style 'post-forward
      uniquify-separator "/")

;; ======= YAsnippet =======

(let ((format "t"))
    (insert (format-time-string format)))

(defun property-type-from-file-name (filename)
  (if (equal "h" (file-name-extension filename))
      "readonly"
    "readwrite"))

(defun property-type-from-text (text)
  (if (string-match "\*" text)
      "strong"
    "assign"))

(defun makeHeaderIncludes (text)
  (mapconcat
   (lambda (st) (format "#import \"%s.h\"" st))
   (sort (split-string text " ") 'string<) "\n"))

(defun makeBodyIncludes (text)
  (reduce (lambda (body check)
            (if (string-match (downcase check) (downcase body))
                (format "%s\n%sextern %s *%s;"
                        body
                        (if (string-match "extern" body) "" "\n")
                        (upcase-initials check)
                        check)
              body
              )
            )
          (list
           (makeHeaderIncludes text)
           "platformInterface" "globalParameters")))

(defun string-replace (from to string &optional re)
  "Replace all occurrences of FROM with TO in STRING.
All arguments are strings.
When optional fourth argument is non-nil, treat the from as a regular expression."
  (let ((pos 0)
        (res "")
        (from (if re from (regexp-quote from))))
    (while (< pos (length string))
      (if (setq beg (string-match from string pos))
          (progn
            (setq res (concat res
                              (substring string pos (match-beginning 0))
                              to))
            (setq pos (match-end 0)))
        (progn
          (setq res (concat res (substring string pos (length string))))
          (setq pos (length string)))))
    res))

(defun stripClass (st)
  (replace-string "*" "" (cadr (split-string st " ")))
  )

(defun makePropertyDefinitions (text)
  (mapconcat
   (lambda (st) (format "@property (nonatomic, readwrite, %s) %s;"
                   (if (string-match "\*" st) "strong" "assign")
                   st
                   ))
   (sort (split-string text ",") 'string<) "\n"
   ))

(defun makeSynthDefinitions (text)
  (mapconcat
   (lambda (st) (format "@synthesize %s = %s_;" (stripClass st) (stripClass st)))
   (sort (split-string text ",") 'string<) "\n"))

(defun makeDealloc (text)
  (concat "- (void)dealloc{\n"
          (mapconcat
           (lambda (st) (if (string-match "\*" st)
             (format "%s = nil;" (stripClass st))
             ""
             ))
           (sort (split-string text ",") 'string<) "\n"
           )
          "\n\n[super dealloc];\n}"
          )
  )

(add-to-list 'load-path "~/elisp/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "~/elisp/yasnippet/snippets")
(yas/global-mode 1)

;; ======= Undo-tree =======

(require 'undo-tree)
(global-undo-tree-mode)


;; ======= Icicles =======
; (add-to-list 'load-path "/Applications/emacs_lisp/icicles/")
; (require 'icicles)

;(icy-mode 1)

;(global-set-key (kbd "C-x C-f") 'icicle-find-file)

;; ======= Electric Pairs =======

(electric-pair-mode 1)

;; ======= Filesets =======

(filesets-init)

;; ======= Startup =======
(defun on-start-up ()
  (interactive)
  (recentf-open-files)
  (split-window-right)
  (other-window 1)
  (org-todo-list)
  (other-window 1)
  )

(on-start-up)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ======= Emerge mode =======

(setq emerge-combine-versions-template "%a%b")
(setq emerge-diff-options "--ignore-all-space")

;; ======= Personal Key Bindings =======

(setq mac-command-modifier 'meta)

(fset 'split-only-current-buffer "\C-x1\C-x3")
(global-set-key "\C-x!" 'split-only-current-buffer)
(global-set-key "\C-a" 'back-to-indentation)
(global-set-key "\M-m" 'move-beginning-of-line)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-o") 'other-frame)

(global-set-key (kbd "C-x O") 'previous-multiframe-window)
(global-set-key (kbd "C-x M-1") 'make-frame)

(defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
        (replace-match "\\1"))))
  
(defun uniquify-buffer-lines ()
  "Remove duplicate adjacent lines in the current buffer."
    (interactive)
    (uniquify-region-lines (point-min) (point-max)))

(fset 'my-get-pivotal-commit-message
   [?\M-x ?e ?g ?g ?- ?s ?t ?a ?t ?u ?s ?\C-m ?g ?\M-< ?\C-  ?\C-n ?\C-e ?\M-w ?\C-x ?b ?t ?e ?m ?p ?\C-m ?\C-y ?\C-p ?\C-a ?O ?n ?  ?\C-e ?  ?a ?s ?  ?o ?f ?  ?\C-d ?\C-e ?. ?\C-  ?\C-a ?\C-w])

(setq compilation-read-command nil)
(global-set-key (kbd "M-r") 'compile)

(setq dabbrev-case-fold-search nil)
(setq dabbrev-case-replace nil)

;; ======= Dash =======

(autoload 'dash-at-point "dash-at-point"
          "Search the word at point with Dash." t nil)
(global-set-key "\C-c\C-d" 'dash-at-point)

;; ======= JS helpers =======

(defun make-and-run-js ()
  (interactive)
  (compile "make -k")
  (do-applescript "tell application \"Google Chrome\"
    activate
    tell the active tab of its first window
        reload
    end tell
end tell")
  )

(add-hook 'js-mode-hook (lambda () (local-set-key (kbd "\C-c\C-c") 'make-and-run-js)))
(add-to-list 'auto-mode-alist '("\\.ecs$" . js-mode))

(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))

;; ======= Pomodoro =======

(require 'pomodoro)

(defun pomodoro-apple-display (msg) 
  (do-applescript (format "display notification \"%s\"" msg))
)

(add-hook 'pomodoro-message-hook 'pomodoro-apple-display)

;; ======= Marmalade =======

(require 'package)
(add-to-list 'package-archives 
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))
(package-initialize)

;; ======= JS flymake =======


;; ======= Eshell customization =======

(defun eshell-mode-hook-func ()
  (setq eshell-path-env (concat "/usr/local/bin:" eshell-path-env))
  (setenv "PATH" (concat "/usr/local/bin:" (getenv "PATH"))))

(add-hook 'eshell-mode-hook 'eshell-mode-hook-func)

(require 'exec-path-from-shell)
(exec-path-from-shell-initialize)

(setenv "PAGER" "cat")
(setenv "EDITOR" "emacsclient")

(eshell)

;; ======= LUA Mode =======

(autoload 'lua-mode "lua-mode" "Lua editing mode." t)
(add-to-list 'auto-mode-alist '("\\.lua$" . lua-mode))
(add-to-list 'interpreter-mode-alist '("lua" . lua-mode))

;; ------- Emacs client support =======

(server-start)

;; ======= Dired-X mode =======

(require 'dired-x)
(setq-default dired-omit-files-p t) ; Buffer-local variable

;; ======= Article Note =======

(defun article-note ()
  (interactive)
  (let ((month (format-time-string "%Y%m"))) 
    (find-file (format "~/Desktop/whynotgame_google/blog/notes/articleNotes%s.html" month))
    )
  (end-of-buffer)
  (insert "note")
  (yas-expand)
  (yank)
)

;; ======= Magit =======

(global-set-key (kbd "C-x g") 'magit-status)

;; ======= HTML Mode =======

(defun add-new-html-li-line ()
  (interactive)
  (electric-newline-and-maybe-indent)
  (indent-for-tab-command)
  (insert "<li></li>")
  (backward-char 5)
)
(add-hook 'html-mode-hook (lambda () (local-set-key (kbd "M-RET") 'add-new-html-li-line)))

(defun open-current-html-page ()
  (interactive)
  (do-applescript
   (format "set site to \"file://%s\"
tell application \"Google Chrome\"
    activate
    tell window 1
        set URL of active tab to site
    end tell
end tell" (file-truename (buffer-file-name)))))


(add-hook 'html-mode-hook (lambda () (local-set-key (kbd "\C-c\C-c") 'open-current-html-page)))

;; ======= Rainbow Delimiters =======

(add-hook 'prog-mode-hook #'rainbow-delimiters-mode)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rainbow-delimiters-depth-1-face ((t (:foreground "dark red"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "DarkOrange2"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "gold2"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "green2"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "DeepSkyBlue2"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "DarkOrchid2"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "VioletRed2")))))

;; ======= Ivy Mode =======

(setq ivy-use-virtual-buffers t)
(setq ivy-count-format "(%d/%d) ")

(ivy-mode 1)

;; ======= Back-up files =======

(setq
   backup-by-copying t 
   backup-directory-alist
    '(("." . "~/.saves"))
   delete-old-versions t
   kept-new-versions 6
   kept-old-versions 2)

;; ======= Projectile Mode =======

(projectile-mode)

;; ======= Projector Size =======

(defun projector-size ()
  (interactive)
  (set-face-attribute 'default nil :height 100)
  )

;; ======= Synonyms =======

(setq synonyms-file "~/elisp/mthesaur.txt")
(setq synonyms-cache-file "~/elisp/mthesaur.txt.cache")
(require 'synonyms)

;; ======= Multiple Cursors =======

(add-to-list 'load-path "~/elisp/multiple-cursors")
(require 'multiple-cursors)

(global-set-key (kbd "C-c m c") 'mc/edit-lines)

;; ======= Deadgrep =======

(add-to-list 'load-path "~/elisp/spinner")
(require 'deadgrep)

(setq deadgrep-project-root-function (lambda () "~/Desktop/syph/"))
(global-set-key (kbd "C-c C-r") 'deadgrep)
