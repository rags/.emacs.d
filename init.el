(defun load-all ()

  (custom-set-variables  
   '(desktop-save-buffer nil)
   '(desktop-load-locked-desktop t)

   '(desktop-load-locked-desktop t)
   '(desktop-save-buffer nil t)
   '(ido-create-new-buffer (quote always))
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(send-mail-function (quote mailclient-send-it))
   '(show-paren-mode t)
   '(tool-bar-mode nil)
   '(transient-mark-mode nil))
    
  (setq stack-trace-on-error t)
  (put 'narrow-to-region 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (show-paren-mode 1)
  (if (boundp 'tool-bar-mode)  (tool-bar-mode -1))
  (setq frame-title-format "%b")
  (setq x-select-enable-clipboard t)
  (recentf-mode 1)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (delete-selection-mode t) 
  (transient-mark-mode nil)
  (if (boundp 'scroll-bar-mode) (scroll-bar-mode 0))
  (column-number-mode 1)
  (display-time-mode 1)
  (winner-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)   	
  (setq mac-command-modifier 'super)
  (setq mac-control-modifier 'control)
  (global-linum-mode t)
  (menu-bar-mode 0)
  (setq vc-follow-symlinks t)
  (setq ac-auto-show-menu nil)

  (let* ((emacs.d "~/.emacs.d/") (plugins-dir  (concat emacs.d "plugins/")))
    (load-file (concat plugins-dir "cua-emul.el"))
    (load-file (concat plugins-dir "encrypt.el"))
    ;(load-file (concat plugins-dir "flymake.el"))
    ;(load-file (concat plugins-dir "paredit.el"))
    (load-file (concat plugins-dir "kill-ring-ido.el"))
    (load-file (concat plugins-dir "emacs-for-python/epy-init.el"))

    (setq load-path 
	  (append `(,emacs.d ,plugins-dir
			     ,(concat plugins-dir "nxml-mode")
			     ,(concat plugins-dir "emacs-for-python")
			     ,(concat plugins-dir "js2-highlight-vars")
			     ,(concat plugins-dir "pretty-lambdada")
				  ) load-path)))
   (add-to-list 'exec-path "/usr/local/bin")


(require 'package)
(add-to-list 'package-archives 
	     '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives 
	     '("melpa" . "http://melpa.milkbox.net/packages/"))
(package-initialize)
;; todo add more repos and install magit, ffip, smex and other common packages from ELPA
;;      delete these packages from plugins fodler

;; Install ELPA packages
(dolist (package `(sml-mode magit smex color-theme find-file-in-project ecb 
			    fuzzy-match js3-mode js2-refactor expand-region
			    mark-multiple wrap-region flymake clojure-mode 
			    auctex toggle-test ensime scala-mode2 paredit 
			    color-theme-solarized helm ac-helm))
  (if (not (package-installed-p package))
      (progn 
	(message (concat "installing package: " (symbol-name package)))
	(package-install package))))


   (autoload 'encrypt-decrypt "encrypt"
	 "Decrypt a crypted file use encrypt coding system" t)

   (require 'server)
   (require 'mydefuns)


   (make-desktop-load-non-blocking)
;   (desktop-save-mode 1)
   (desktop-save-mode 0)
   (require 'ido)
   (ido-mode t)
   (require 'find-file-in-project)
   (add-to-list 'ffip-patterns "*.yaml")
   (add-to-list 'ffip-patterns "*.css")
  
  ;(color-theme-initialize)

					;(color-theme-bharadwaj)
					;(color-theme-blippblopp)
					; (color-theme-calm-forest)
  ;(color-theme-charcoal-black)
					; (color-theme-vim-colors)
					; (color-theme-snowish)
  ;(color-theme-clarity)		
  (load-theme 'solarized-dark t)
  (set-frame-font "-unknown-Liberation Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")
  ;(set-default-font "-unknown-Inconsolata-bold-normal-normal-*-21-*-*-*-m-0-iso10646-1")
 ;(set-default-font "-unknown-FreeMono-bold-normal-normal-*-19-*-*-*-m-0-iso10646-1")
  (require 'smart-operator)
  (require 'ecb)
  (require 'window-numbering)
  (window-numbering-mode 1)
  (require 'clojure-mode)
  (require 'paredit)
  (smex-initialize) 
  (require 'js2-highlight-vars)
  (require 'js2-refactor)

(python-stuff)
(js-stuff)
(elisp-stuff)
(LaTeX-stuff)
(make-file-associations)
(add-hook 'after-make-frame-functions 'client-initialization)
(require 'pretty-lambdada)
(global-pretty-lambda-mode)
(wrap-region-mode t)
(add-hook 'scala-mode-hook 'ensime-scala-mode-hook)

(require 'helm-config)
(helm-mode 1)
(require 'auto-complete-config)
(ac-config-default)


(require 'myshortcuts)
(require 'myproject nil 'noerror)

  (custom-set-variables
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(ido-create-new-buffer (quote always))
   '(show-paren-mode t)
   '(tool-bar-mode nil)
   '(transient-mark-mode nil))
   (global-font-lock-mode 1)
   (auto-complete-mode t)
   (message "load all done")
)

(defun client-initialization (frame)
  "frame initialization ui/layout related"
  (interactive)
  (select-frame frame)
  (my-fullscreen)
  ;(make-my-layout)
  (menu-bar-mode 0)
  (global-font-lock-mode 1)
  ;(set-default-font "-unknown-FreeMono-bold-normal-normal-*-19-*-*-*-m-0-iso10646-1")
  (load-theme 'solarized-dark t)
  (set-frame-font "-unknown-Liberation Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")
  )



(load-all)
;;(if window-system (load-all) (load-quick))
;;(clojure-slime-config "/home/rags/projects/clojure/src")




