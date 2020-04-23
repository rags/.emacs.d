(require 'cl)
(defun load-all ()
  (setenv "GPG_AGENT_INFO" nil)
  (custom-set-variables
   '(desktop-save-buffer nil)
   '(desktop-load-locked-desktop t)
   '(backup-directory-alist `(("." . "~/.saves")))
   '(backup-by-copying t)
   '(delete-old-versions t)
   '(kept-new-versions 6)
   '(kept-old-versions 2)
   '(desktop-load-locked-desktop t)
   '(desktop-save-buffer nil t)
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(send-mail-function (quote mailclient-send-it))
   '(show-paren-mode t)
   '(tool-bar-mode nil)
   '(indent-tabs-mode nil)
   '(tab-width 4)
   '(visible-bell 1)
   '(transient-mark-mode nil)
   '(show-trailing-whitespace t)
   '(stack-trace-on-error t))

  (put 'narrow-to-region 'disabled nil)
  (put 'set-goal-column 'disabled nil)
  (show-paren-mode 1)
  (if (fboundp 'tool-bar-mode)  (tool-bar-mode -1))
  (setq frame-title-format "%b")
  (setq select-enable-clipboard t)
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


  (require 'package)
  (add-to-list 'package-archives
	       '("marmalade" . "http://marmalade-repo.org/packages/"))
  (add-to-list 'package-archives
	       '("melpa" . "http://melpa.milkbox.net/packages/"))
  (package-initialize)

  (if (not (package-installed-p 'use-package))
      (progn (package-refresh-contents)
                 (package-install 'use-package)))

 (let* ((emacs.d "~/.emacs.d/") (plugins-dir  (concat emacs.d "plugins/")))
    (load-file (concat plugins-dir "cua-emul.el"))
    (load-file (concat plugins-dir "encrypt.el"))
					;(load-file (concat plugins-dir "flymake.el"))
					;(load-file (concat plugins-dir "paredit.el"))
    ;(load-file (concat plugins-dir "emacs-for-python/epy-init.el"))

    (setq load-path
	  (append `( ,plugins-dir
			     ,(concat emacs.d "lisp")
			     ,(concat plugins-dir "nxml-mode")
			     ,(concat plugins-dir "js3-highlight-vars")
			     ,(concat plugins-dir "js3-refactor")) load-path))
    (add-to-list 'custom-theme-load-path
		 (concat plugins-dir "emacs-color-theme-darkula")))

 (require 'mypackages)
  (autoload 'encrypt-decrypt "encrypt"
    "Decrypt a crypted file use encrypt coding system" t)

  (require 'server)
  (require 'mydefuns)
  (require 'smartparens-config)

  (make-desktop-load-non-blocking)
					;   (desktop-save-mode 1)
  (desktop-save-mode 0)

  (setq ag-highlight-search t)
					;(color-theme-initialize)
					;(color-theme-clarity)
  (load-theme 'manoj-dark t)
  ;(set-frame-font "-unknown-Liberation Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")
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
  ;(load-file  "~/.emacs.d/plugins/emacs-for-python/epy-init.el")

  (guide-key-mode t)

  (add-hook 'after-make-frame-functions 'client-initialization)
  (require 'auto-complete-config)
  (ac-config-default)
  (key-chord-mode t)

  (require 'mymodes)
  (require 'myproject nil 'noerror)
  (require 'myshortcuts)

  (my-add-to-list 'exec-path "/usr/local/bin" "~/bin")
  (setenv "PATH" (concat (getenv "PATH") ":/usr/local/bin"))


  (custom-set-variables
   '(inhibit-startup-screen t)
   '(initial-scratch-message nil)
   '(show-paren-mode t)
   '(tool-bar-mode nil)
   '(transient-mark-mode nil)
   '(ediff-window-setup-function 'ediff-setup-windows-plain)
   '(ediff-split-window-function 'split-window-horizontally)
   '(initial-frame-alist (quote ((fullscreen . fullscreen)))))

  (global-font-lock-mode 1)
  (global-ede-mode t)
  (auto-complete-mode t)
  (setq ivy-re-builders-alist
      '((read-file-name-internal . ivy--regex-fuzzy)
        (describe-variable . ivy--regex-ignore-order)
        (counsel-M-x . ivy--regex-ignore-order)
        (t . ivy--regex-plus)))
  (message "load all done")
  )

(defun client-initialization (frame)
  "frame initialization ui/layout related"
  (interactive "XFrame: ")
  (message "caled frame init")
  (select-frame frame)
  (toggle-frame-fullscreen)
  (make-my-layout)
  (menu-bar-mode 0)
  (global-font-lock-mode 1)
					;(set-default-font "-unknown-FreeMono-bold-normal-normal-*-19-*-*-*-m-0-iso10646-1")
  (load-theme 'manoj-dark t)
  ;;   (set-frame-font "-unknown-Liberation Mono-bold-normal-normal-*-15-*-*-*-m-0-iso10646-1")
  )

(defun reset () (interactive) (client-initialization (selected-frame)))

(load-all)
;;(if window-system (load-all) (load-quick))
;;(clojure-slime-config "/home/rags/projects/clojure/src")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(backup-by-copying t)
 '(backup-directory-alist (\` (("." . "~/.saves"))))
 '(custom-safe-themes
   (quote
    ("41c8c11f649ba2832347fe16fe85cf66dafe5213ff4d659182e25378f9cfc183" "1e7e097ec8cb1f8c3a912d7e1e0331caeed49fef6cff220be63bd2a6ba4cc365" "fc5fcb6f1f1c1bc01305694c59a1a861b008c534cae8d0e48e4d5e81ad718bc6" default)))
 '(delete-old-versions t)
 '(desktop-load-locked-desktop t)
 '(desktop-save-buffer nil t)
 '(ediff-split-window-function (quote split-window-horizontally))
 '(ediff-window-setup-function (quote ediff-setup-windows-plain))
 '(indent-tabs-mode nil)
 '(inhibit-startup-screen t)
 '(initial-frame-alist (quote ((fullscreen . fullscreen))))
 '(initial-scratch-message nil)
 '(kept-new-versions 6)
 '(kept-old-versions 2)
 '(package-selected-packages
   (quote
    (elpy ropemacs git-gutter clang-format flycheck-irony irony-eldoc irony flx cmake-ide rtags web-beautify vlf visual-regexp use-package toggle-test tern-auto-complete sml-mode smex smartparens smart-operator projectile move-text mark-multiple magit key-chord json-mode js3-mode js2-refactor js-comint haskell-mode guide-key git-gutter-fringe git-gutter-fringe+ flycheck expand-region ess ecb darcula-theme counsel color-theme-solarized clj-refactor auctex ag ac-cider)))
 '(send-mail-function (quote mailclient-send-it))
 '(show-paren-mode t)
 '(show-trailing-whitespace t)
 '(stack-trace-on-error t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(transient-mark-mode nil)
 '(visible-bell 1))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )
