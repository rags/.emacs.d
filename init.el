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
   '(visible-bell 1)
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
;    (load-file (concat plugins-dir "emacs-for-python/epy-init.el"))

    (setq load-path 
	  (append `(,emacs.d ,plugins-dir
			     ,(concat plugins-dir "nxml-mode")
			     ,(concat plugins-dir "js3-highlight-vars")
			     ,(concat plugins-dir "pretty-lambdada")
			     ,(concat plugins-dir "js3-refactor")) load-path))
    (add-to-list 'custom-theme-load-path 
		 (concat plugins-dir "emacs-color-theme-darkula")))
  (add-to-list 'exec-path "/usr/local/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))


  (require 'package)
  (add-to-list 'package-archives 
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives 
	       '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)

  ;; Install ELPA packages
  (let* ((my-packages `(sml-mode magit smex  ecb fuzzy-match js3-mode js2-refactor 
				mark-multiple flymake clojure-mode guide-key key-chord
				auctex toggle-test ensime scala-mode2 paredit 
				color-theme-solarized helm ac-helm json-mode expand-region
				git-gutter-fringe+ flycheck tern tern-auto-complete
				yasnippet ag web-beautify haskell-mode js-comint projectile
				flx-ido ido-vertical-mode smartparens visual-regexp))
	 (to-install-packages (remove-if #'package-installed-p my-packages)))

    (if to-install-packages
	(progn (message "Refresh packages...") 
	       (package-refresh-contents)
	       (dolist (package to-install-packages)      
		 (message (concat "installing package: " (symbol-name package)))
		 (package-install package))) 
      (message "Nothing new to install"))) 


  (autoload 'encrypt-decrypt "encrypt"
    "Decrypt a crypted file use encrypt coding system" t)

  (require 'server)
  (require 'mydefuns)
  (require 'smartparens-config)

  (make-desktop-load-non-blocking)
					;   (desktop-save-mode 1)
  (desktop-save-mode 0)  
  (ido-mode t)
  (ido-vertical-mode 1)
  (ido-everywhere 1)
  (flx-ido-mode 1)
  ;; disable ido faces to see flx highlights.
  (setq ido-enable-flex-matching t)
  (setq ido-use-faces nil)
  (setq flx-ido-use-faces t)
  (projectile-global-mode)
  (setq projectile-enable-caching t)
  (setq ag-highlight-search t)
					;(color-theme-initialize)
					;(color-theme-clarity)		
  (load-theme 'Darkula t)
  (set-frame-font "-unknown-Liberation Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")
					;(set-default-font "-unknown-Inconsolata-bold-normal-normal-*-21-*-*-*-m-0-iso10646-1")
					;(set-default-font "-unknown-FreeMono-bold-normal-normal-*-19-*-*-*-m-0-iso10646-1")

  (require 'ecb)
  (require 'window-numbering)
  (window-numbering-mode 1)
  (smex-initialize) 

  (yas-global-mode t)
  (my-add-to-list 'yas-snippet-dirs "~/.emacs.d/plugins/yasnippet-snippets" "~/.emacs.d/snippets")
  (yas-reload-all)
  ;(defun yas/initialize());hack old version of yas used by epy
  (load-file  "~/.emacs.d/plugins/emacs-for-python/epy-init.el")

  (guide-key-mode t)
  (setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x n" "C-c @" "C-c C-c" ))

  (add-hook 'after-make-frame-functions 'client-initialization)
  (require 'helm-config)
  (helm-mode 1)
  (require 'auto-complete-config)
  (ac-config-default)
  (key-chord-mode t)

  (require 'mymodes)
  (require 'myproject nil 'noerror)
  (require 'myshortcuts)


  (custom-set-variables
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(ido-create-new-buffer (quote always))
   '(show-paren-mode t)
   '(tool-bar-mode nil)
   '(transient-mark-mode nil)
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   '(ediff-split-window-function 'split-window-horizontally))

  (global-font-lock-mode 1)
  (global-ede-mode t)
  (auto-complete-mode t)
  (message "load all done")
  ) 

(defun client-initialization (frame)
  "frame initialization ui/layout related"
  (interactive "XFrame: ")
  (select-frame frame)
  (my-fullscreen)
					;(make-my-layout)
  (menu-bar-mode 0)
  (global-font-lock-mode 1)
					;(set-default-font "-unknown-FreeMono-bold-normal-normal-*-19-*-*-*-m-0-iso10646-1")
  (load-theme 'Darkula t)
  (set-frame-font "-unknown-Liberation Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1"))

(defun reset () (interactive) (client-initialization (selected-frame)))

(load-all)
;;(if window-system (load-all) (load-quick))
;;(clojure-slime-config "/home/rags/projects/clojure/src")




