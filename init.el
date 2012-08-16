
(defun load-all ()

(custom-set-variables  
   '(desktop-save-buffer nil)
   '(desktop-load-locked-desktop t))
    
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
  
  
  

  (let* ((emacs.d "~/.emacs.d/") (plugins-dir  (concat emacs.d "plugins/")))
    (load-file (concat plugins-dir "cua-emul.el"))
    (load-file (concat plugins-dir "ido.el"))
    (load-file (concat plugins-dir "encrypt.el"))
    (load-file (concat plugins-dir "flymake.el"))
    (load-file (concat plugins-dir "paredit.el"))
    (load-file (concat plugins-dir "emacs-for-python/epy-init.el"))
    (setq load-path 
	  (append `(,emacs.d ,plugins-dir
			     ,(concat plugins-dir "color-theme")
			     ,(concat plugins-dir "nxml-mode")
			     ,(concat plugins-dir "speedbar")
			     ,(concat plugins-dir "semantic")
			     ,(concat plugins-dir "eieio")
			     ,(concat plugins-dir "ecb")
			     ,(concat plugins-dir "slime")
			     ,(concat plugins-dir "clojure-mode")
			     ,(concat plugins-dir "emacs-for-python")
			     ) load-path)))
  (add-to-list 'exec-path "/usr/local/bin")
  

  (autoload 'encrypt-decrypt "encrypt"
    "Decrypt a crypted file use encrypt coding system" t)
  
  (require 'server)
  (require 'mydefuns)

  (make-desktop-load-non-blocking)
  (desktop-save-mode 1)
  (require 'ido)
  (ido-mode t)

					;(require 'mercurial)
  (require 'color-theme)
  (color-theme-initialize)

					;(color-theme-bharadwaj)
					;(color-theme-blippblopp)
					; (color-theme-calm-forest)
  (color-theme-charcoal-black)
					; (color-theme-vim-colors)
					; (color-theme-snowish)
					;(color-theme-clarity)		
					;(set-default-font "-unknown-Liberation Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")
  (require 'smart-operator)
  (require 'ecb)
  (require 'ecb-autoloads)
  (require 'window-numbering)
  (window-numbering-mode 1)
  (require 'myshortcuts)
  (require 'clojure-mode)
  (require 'paredit)
    
  (eval-after-load "slime" 
  '(progn (slime-setup '(slime-repl))	
	(defun paredit-mode-enable () (paredit-mode 1))	
	(add-hook 'slime-mode-hook 'paredit-mode-enable)	
	(add-hook 'slime-repl-mode-hook 'paredit-mode-enable)
	(setq slime-protocol-version 'ignore)))
  (require 'slime)
  (slime-setup)
  (require 'myproject nil 'noerror)
  (add-hook 'after-make-frame-functions 'client-initialization)
  (python-stuff)
  ;;(require 'gmail)

  ;;(server-start)
  ;;(my-fullscreen)
  ;;(make-my-layout)
  
  (custom-set-variables
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(menu-bar-mode nil)
   '(ecb-options-version "2.32")
   '(ido-create-new-buffer (quote always))
   '(show-paren-mode t)
   '(tool-bar-mode nil)
   '(transient-mark-mode nil))

  
)

(defun client-initialization (frame)
  "frame initialization ui/layout related"
  (interactive)
  (select-frame frame)
  (my-fullscreen)
  (make-my-layout))

(load-all)
;;(if window-system (load-all) (load-quick))
;;(clojure-slime-config "/home/rags/projects/clojure/src")
