; ======= Basic initializations =======

(tool-bar-mode -1)
(scroll-bar-mode -1)

(add-to-list 'load-path "/Applications/emacs_lisp/")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(filesets-data (quote (("hiddenCsv" (:pattern "~/Desktop/hidden/Resources/Parameters/" "^[a-zA-Z]+\\.csv$") (:tree-max-level 3)))))
 '(hg-binary "hg")
 '(ido-default-buffer-method (quote selected-window))
 '(ido-default-file-method (quote selected-window))
 '(ido-mode (quote both) nil (ido))
 '(ido-rotate-file-list-default t)
 '(indent-tabs-mode t)
 '(inhibit-startup-screen t)
 '(initial-scratch-message nil)
 '(org-agenda-files (quote ("~/Dropbox/Nikhil/orgfiles/taskList.org" "~/Desktop/nexus/tasks.org")))
 '(org-agenda-start-on-weekday nil)
 '(org-remember-templates (quote (("Personal Life" 108 "** TODO %?
   %u" "~/Dropbox/Nikhil/orgfiles/taskList.org" "Personal Life:" nil) ("Personal Work" 119 "** TODO %?
   %u" "~/Dropbox/Nikhil/orgfiles/taskList.org" "Personal Work:" nil) ("Pocket Gems Work" 112 "** TODO %?
   %u" "~/Dropbox/Nikhil/orgfiles/taskList.org" "Pocket Gems Work:" nil) ("Notes" 110 "** %?
   %u" "~/Dropbox/Nikhil/orgfiles/taskList.org" "Notes" nil))))
 '(py-python-command-args nil)
 '(standard-indent 4))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

;; ======= Indentation =======

(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)

;; ======= Color themes =======

(add-to-list 'load-path "/Applications/emacs_lisp/color-theme-6.6.0")
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

;(require 'mercurial)
(require 'egg)

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

;; ======= CSV Mode =======

(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

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

(add-to-list 'load-path "/Applications/emacs_lisp/yasnippet")
(require 'yasnippet)
(yas/initialize)
(yas/load-directory "/Applications/emacs_lisp/yasnippet/snippets")
(yas/global-mode 1)

;; ======= Undo-tree =======

(require 'undo-tree)
(global-undo-tree-mode)


;; ======= Icicles =======
 (add-to-list 'load-path "/Applications/emacs_lisp/icicles/")
 (require 'icicles)

(icy-mode 1)

;; ======= Electric Pairs =======

(electric-pair-mode 1)

;; ======= Filesets =======

(filesets-init)

;; ======= Tags =======
(require 'find-file-in-tags)
(global-set-key (kbd "C-x f") 'find-file-in-tags)
(global-set-key (kbd "M-?") 'complete-tag)

;; ======= Run Xcode =======

(defun run-xcode ()
  (interactive)
  (do-applescript "tell application \"Xcode\"
	
	activate
end tell

tell application \"System Events\"
	get system attribute \"sysv\"
	if result is greater than or equal to 4144 then -- Mac OS X 10.3.0
		if UI elements enabled then
			tell application process \"Xcode\"
				click menu item \"Run\" of menu 1 of menu bar item \"Product\" of menu bar 1
				
			end tell
		else
			beep
			display dialog \"GUI Scripting is not enabled\" & return & return & \"Open System Preferences and check Enable Access for Assistive Devices in the Universal Access preference pane, then run this script again.\" with icon stop
			if button returned of result is \"OK\" then
				tell application \"System Preferences\"
					activate
					set current pane to pane \"com.apple.preference.universalaccess\"
				end tell
			end if
		end if
	else
		beep
		display dialog \"This computer cannot run this script\" & return & return & \"The script uses GUI Scripting technology, which requires an upgrade to Mac OS X 10.3 Panther or newer.\" with icon caution buttons {\"Quit\"} default button \"Quit\"
	end if
end tell")
  )

(add-hook 'objc-mode-hook (lambda () (local-set-key (kbd "M-r") 'run-xcode)))
(add-hook 'objc-mode-hook (lambda () (local-set-key (kbd "C-c C-c") 'run-xcode)))

;; ======= ObjC-specific stuff =======

(add-to-list 'auto-mode-alist '("\\.h\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-hook 'objc-mode-hook (lambda () (flyspell-prog-mode)))

(defun objc-find-flipped-filename ()
  (interactive)
  (find-file (format "%s.%s" (file-name-sans-extension (buffer-file-name))
                     (if (equal "h" (file-name-extension (buffer-file-name)))
                         "m" "h"))))

(add-hook 'objc-mode-hook (lambda () (local-set-key (kbd "C-x C-S-f") 'objc-find-flipped-filename)))

;; ======= Autocomplete =======

(add-to-list 'load-path "/Applications/emacs_lisp/autocomplete/")
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "/Applications/emacs_lisp/autocomplete//ac-dict")
(ac-config-default)

(setq ac-modes (cons 'objc-mode ac-modes))
;(ac-flyspell-workaround)

(require 'ac-company)
(ac-company-define-source ac-source-company-xcode company-xcode)
(add-hook 'objc-mode-hook
          (lambda () 
            (add-to-list 'ac-sources 'ac-source-company-xcode)))

;; ======= Startup =======
(defun on-start-up ()
  (interactive)
  (recentf-open-files)
  (split-window-right)
  (other-window 1)
  (org-agenda-list)
  (other-window 1)
  )

(on-start-up)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)

;; ======= Emerge mode =======

(setq emerge-combine-versions-template "%a%b")
(setq emerge-diff-options "--ignore-all-space")

;; ======= E-mail =======

(defun djcb-term-start-or-switch (prg &optional use-existing)
  "* run program PRG in a terminal buffer. If USE-EXISTING is non-nil "
  " and PRG is already running, switch to that buffer instead of starting"
  " a new instance."
  (interactive)
  (let ((bufname (concat "*" prg "*")))
    (when (not (and use-existing
                 (let ((buf (get-buffer bufname)))
                   (and buf (buffer-name (switch-to-buffer bufname))))))
      (ansi-term prg prg))))

(defun mutt ()
  (interactive)
  (djcb-term-start-or-switch "/opt/local/bin/mutt" t))

(server-start)

(autoload 'post-mode "post" "mode for e-mail" t)
(add-to-list 'auto-mode-alist 
             '("\\.*mutt-*\\|.article\\|\\.followup" 
                . post-mode))

(add-hook 'post-mode-hook 
  (lambda()
    (auto-fill-mode t)    
    (setq fill-column 72)    ; rfc 1855 for usenet messages
    (require 'footnote-mode) 
    (footmode-mode t)
    (require 'boxquote)))

;; ======= Personal Key Bindings =======

(setq mac-command-modifier 'meta)

(fset 'split-only-current-buffer "\C-x1\C-x3")
(global-set-key "\C-x#" 'split-only-current-buffer)
(global-set-key "\C-a" 'back-to-indentation)
(global-set-key "\M-m" 'move-beginning-of-line)
(global-set-key (kbd "<f6>") 'linum-mode)
(global-set-key (kbd "C-c o") 'occur)
(global-set-key (kbd "C-o") 'other-frame)

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

;; ======= toe specific code =======

(setq tags-file-name "~/Desktop/enginestarterapp/")

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

;; ======= Pomodoro =======

(require 'pomodoro)

;; ======= Marmalade =======

(require 'package)
(add-to-list 'package-archives 
    '("marmalade" .
      "http://marmalade-repo.org/packages/"))
(package-initialize)

;; ======= JS flymake =======

(require 'flycheck)
(flycheck-define-checker javascript-jslint-reporter
  "A JavaScript syntax and style checker based on JSLint Reporter.

See URL `https://github.com/FND/jslint-reporter'."
  :command ("~/.emacs.d/jslint-reporter/jslint-reporter" source "--jshint")
  :error-patterns
  ((error line-start (1+ nonl) ":" line ":" column ":" (message) line-end))
  :modes (js-mode js2-mode js3-mode))
(add-hook 'js-mode-hook (lambda ()
                          (flycheck-select-checker 'javascript-jslint-reporter)
                          (flycheck-mode)))
