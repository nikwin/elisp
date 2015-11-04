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

;; ======= toe specific code =======

(setq tags-file-name "~/Desktop/tow/")

(defun tgrep ()
  (interactive)
  (let* ((regexp (grep-read-regexp)))
    (rgrep regexp "*.[mh]" "~/Desktop/tow/"))
 )
