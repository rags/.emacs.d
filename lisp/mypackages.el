(require 'use-package)

;; Install ELPA packages
;;TODO: remove unsed pacakges like ido, helm. use use-package with config, bind,init for all packages. So we have entore library config in one place
(let* ((my-packages `(sml-mode magit smex  ecb fuzzy-match js3-mode js2-refactor
				mark-multiple flymake clojure-mode cider clj-refactor ac-cider guide-key
				auctex toggle-test ensime paredit smart-operator
				color-theme-solarized helm ac-helm json-mode expand-region
				git-gutter-fringe+ flycheck tern tern-auto-complete key-chord
				yasnippet ag web-beautify haskell-mode js-comint projectile
				flx-ido ido-vertical-mode smartparens visual-regexp move-text ess)))
    (dolist (package my-packages)
      (use-package package :ensure t)))

(use-package counsel
             :ensure t
             :init (progn
                     (setq ivy-use-virtual-buffers t)
                     (setq enable-recursive-minibuffers t))
             :config (ivy-mode 1)
             :bind (("C-s" . swiper)
                    ("<f6>" . ivy-resume)
                    ("M-x" . counsel-M-x)
                    ("C-x C-f" . counsel-find-file)
                    ("<f1> f" . counsel-describe-function)
                    ("<f1> v" . counsel-describe-variable)
                    ("<f1> l" . counsel-find-library)
                    ("<f2> i" . counsel-info-lookup-symbol)
                    ("<f2> u" . counsel-unicode-char)
                    ("C-c g" . counsel-git)
                    ("C-c j" . counsel-git-grep)
                    ("C-c k" . counsel-ag)
                    :map read-expression-map
                    ("C-r" . counsel-expression-history)))

(provide 'mypackages)
