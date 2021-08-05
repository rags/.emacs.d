(eval-when-compile
  (require 'use-package))
(require 'bind-key)

;; Install ELPA packages
;;TODO: remove unsed pacakges like ido, helm. use use-package with config, bind,init for all packages. So we have entore library config in one place
(let* ((my-packages '(sml-mode magit smex  ;;ecb
                               ;;javascript
                               js3-mode js2-refactor tern tern-auto-complete js-comint 
                               mark-multiple flymake
                               ;;clojure
                               clojure-mode cider clj-refactor ac-cider
                               guide-key toggle-test  paredit py-smart-operator
                               json-mode expand-region darcula-theme
                               flycheck key-chord
                               yasnippet ag web-beautify haskell-mode
                               smartparens visual-regexp move-text ess vlf flx
                               ;;c++ packages
                               rtags cmake-ide irony irony-eldoc flycheck-irony clang-format srefactor flycheck-rtags
                               ;;python
                               elpy jedi
                               ;;completions
                               ivy-hydra
                               )))
  (dolist (package my-packages)
    (eval `(use-package ,package :ensure t))))



(use-package git-gutter-fringe+
  :init (unless (fboundp 'define-fringe-bitmap)
          (defun define-fringe-bitmap (&rest _)
            nil)))

(use-package tex  :ensure auctex)
(use-package solarized-theme  :ensure t :no-require t :defer t)
(use-package counsel
             :ensure t
             :init (progn
                     (setq ivy-use-virtual-buffers t)
                     (setq enable-recursive-minibuffers t))
             :config (ivy-mode 1)
             :bind (("C-s" . swiper)
                    ("<f6>" . ivy-resume)
                    ("M-x" . counsel-M-x)
                    ("C-x C-r" . counsel-recentf)
                    ("C-x C-f" . counsel-find-file)
                    ("<f1> f" . counsel-describe-function)
                    ("<f1> v" . counsel-describe-variable)
                    ("<f1> l" . counsel-find-library)
                    ("<f2> i" . counsel-info-lookup-symbol)
                    ("<f2> u" . counsel-unicode-char)
                    ("C-c g" . counsel-git)
                    ("C-c j" . counsel-git-grep)
                    ("C-c k" . counsel-ag)
                    ("C-x b" . ivy-switch-buffer)
                    ("M-y" . counsel-yank-pop)
                    :map read-expression-map
                    ("C-r" . counsel-expression-history)))

(use-package projectile
  :ensure t
  :init (progn (setq projectile-completion-system 'ivy)
               (setq projectile-indexing-method 'native)
               (setq projectile-enable-caching t))
  :config  (projectile-global-mode)
  :bind (("C-x d" . projectile-find-dir)
         ("C-x f" . projectile-find-file)))

(use-package irony
  :hook ((c++-mode . irony-mode)
	 (c-mode . irony-mode))
  :defer t
  :config
  ;; counsel-irony setup
  (define-key irony-mode-map
      [remap completion-at-point] 'counsel-irony)
  (define-key irony-mode-map
      [remap complete-symbol] 'counsel-irony)
  (irony-cdb-autosetup-compile-options)

  (use-package irony-eldoc
    :pin melpa
    :hook (irony-mode . irony-eldoc))

  (use-package flycheck-irony
    :after flycheck
    :hook (flycheck-mode . flycheck-irony-setup))
)


(provide 'mypackages)
