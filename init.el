
(defun load-all ()

  (custom-set-variables
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(menu-bar-mode nil)
   '(ecb-options-version "2.32")
   '(ido-create-new-buffer (quote always))
   '(show-paren-mode t)
   '(tool-bar-mode nil)
   '(desktop-save-buffer nil)
   '(transient-mark-mode nil)
   '(desktop-load-locked-desktop t))
  

  (put 'narrow-to-region 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (show-paren-mode 1)
  (tool-bar-mode -1)
  (setq frame-title-format "%b")
  (setq x-select-enable-clipboard t)
  (recentf-mode 1)
  (put 'downcase-region 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (delete-selection-mode t) 
  (transient-mark-mode nil)
  (scroll-bar-mode 0)
  (column-number-mode 1)
  (display-time-mode 1)
  (winner-mode 1)
  (fset 'yes-or-no-p 'y-or-n-p)   	
  
  
  (when
      (load
       (expand-file-name "~/.emacs.d/elpa/package.el"))
    (package-initialize))

  (let* ((emacs.d "~/.emacs.d/") (plugins-dir  (concat emacs.d "plugins/")))
    (load-file (concat plugins-dir "cua-emul.el"))
    (load-file (concat plugins-dir "ido.el"))
    (load-file (concat plugins-dir "encrypt.el"))
    (load-file (concat plugins-dir "paredit.el"))
    (setq load-path 
	  (append `(,emacs.d ,plugins-dir
			     ,(concat plugins-dir "color-theme")
			     ,(concat plugins-dir "nxml-mode")
			     ,(concat plugins-dir "ecb")
			     ,(concat plugins-dir "speedbar")
			     ,(concat plugins-dir "semantic")
			     ,(concat plugins-dir "eieio")
			     ,(concat plugins-dir "slime")
			     ,(concat plugins-dir "clojure-mode")
			     ) load-path)))

  (autoload 'encrypt-decrypt "encrypt"
    "Decrypt a crypted file use encrypt coding system" t)

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
  (require 'ecb)
  (require 'ecb-autoloads)
  (require 'window-numbering)
  (window-numbering-mode 1)
  (require 'mydefuns)
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

  (add-hook 'after-make-frame-functions 'client-initialization)
  
  
  ;;(require 'gmail)

  ;;(server-start)
  ;;(my-fullscreen)
  ;;(make-my-layout)
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
