(require 'mydefuns)
(defun  python-stuff ()
  (epy-setup-ipython)
  (epy-setup-checker "pyflakes %f")
  (setq ropemacs-enable-autoimport 't)
  (setq ropemacs-autoimport-modules '("os" "shutil" "logging"))

  (add-hook 'python-mode-hook
	    (lambda ()
	      (infix-language-mode)
	      (local-unset-key ".")
	      (setq-default tab-width 4)
	      (setq-default python-indent-offset 4)
	      (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")
					;(hide-sublevels 1)
	      
	      (setq ropemacs-goto-def-newwin t)
	      )))

(defun make-file-associations ()
  (my-add-to-list 'auto-mode-alist 
                  '("\\.html$" . nxml-mode)
                  '("\\.xml$" .  nxml-mode) 
                  '("\\.xsd$" .  nxml-mode)
                  '("\\.js$" . js3-mode)
                  '("\\.m\\'" . octave-mode)))

(defun LaTeX-stuff ()
  (setq TeX-PDF-mode t)
  (setq TeX-output-view-style
	'(("^pdf$" "." "emacsclient %o")
	  ("^html?$" "." "firefox %o")))
  (setq TeX-view-program-selection
	'(((output-dvi style-pstricks)
	   "dvips and gv")
	  (output-dvi "xdvi")
	  (output-pdf "emacsclient")
	  (output-html "xdg-open")))
  (setq TeX-view-program-list '(("emacsclient" "emacsclient %o")))
  (add-hook 'tex-mode-hook
	    (lambda () 
	      (TeX-PDF-mode)))
  )

(defun xml-stuff ()

  (setq nxml-slash-auto-complete-flag t)
  (add-hook 'nxml-mode-hook (lambda () 
			      (programming-modes))))
(defun js-stuff()
 (require 'js3-refactor)
 (custom-set-variables 
 '(js3-auto-indent-p t)         ; it's nice for commas to right themselves.
 '(js3-enter-indents-newline t) ; don't need to push tab before typing
 '(js3-indent-on-enter-key t)   ; fix indenting before moving on
 '(js3-indent-level 4)
 '(js3-consistent-level-indent-inner-bracket t))


 (eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
 
  (autoload 'js3-mode "js3" nil t)  
  (add-hook 'json-mode-hook (lambda () 			      
			      (programming-modes) 
				  (add-hook 'before-save-hook 'web-beautify-js-buffer t t)
			      (setq outline-regexp "[^{]*{")))
  (add-hook 'js3-mode-hook
	    (lambda ()
	      (infix-language-mode)
	      (tern-mode t)
	      (flycheck-mode t)
	      (setq outline-regexp " *\\(function\\)")
		  (add-hook 'before-save-hook 'web-beautify-js-buffer t t)
	      ;; '(if (featurep 'js3-highlight-vars)
		  ;; (js3-highlight-vars-mode))
	      ))
  (setq inferior-js-program-command "node")
  (add-hook 'inferior-js-mode-hook (lambda () 
				     (ansi-color-for-comint-mode-on)
				     (add-to-list
				      'comint-preoutput-filter-functions
				      (lambda (output)
					(replace-regexp-in-string "\033\\[[0-9]+[A-Z]" "" output)))
				     (infix-language-mode t))))

(defun elisp-stuff ()
  (add-hook 'emacs-lisp-mode-hook
	    (lambda () 
	      (programming-modes)
	      (paredit-mode))))

(defun git-stuff ()  
(require 'git-gutter-fringe+)
  (global-git-gutter+-mode t)
  (setq git-gutter-fr+-side 'right-fringe)
  (set-face-foreground 'git-gutter-fr+-modified "yellow"))

(defun haskell-stuff ()
  (add-hook 'haskell-interactive-mode-hook 
	    (lambda () (infix-language-mode t) (local-unset-key ":")))
(add-hook 'haskell-mode-hook (lambda ()
				 (haskell-indent-mode t)
				 (interactive-haskell-mode t)
				 (infix-language-mode))))

(defun infix-language-mode (&optional interactivep)
  (programming-modes interactivep) 
  (smart-operator-mode t))

(defun programming-modes (&optional interactivep)
  ;(smartparens-strict-mode t)
  (smartparens-mode t)
  (show-smartparens-mode t)
  (if (not interactivep) 
      (progn (outline-minor-mode t)
	     (hs-minor-mode t)
	     (imenu-add-menubar-index)
	     (yas-minor-mode t)))
  
  (subword-mode t)
  (auto-complete-mode t)
  (setq mode-line-modes 'nil)
  (setq minor-mode-alist 'nil)
  (prettify-symbols-mode)
  ;(electric-pair-mode t)
)
   

(defun setup-programming-modes ()
  (require 'clojure-mode)
  (require 'paredit)
  (require 'js3-highlight-vars)
  (require 'js3-refactor)
  
  (python-stuff)
  (js-stuff)
  (elisp-stuff)
  (LaTeX-stuff)
  (xml-stuff)
  (git-stuff)


  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook)
  (haskell-stuff)
  (setq-default inferior-R-program-name "R")
  (make-file-associations))

(setup-programming-modes)

(provide 'mymodes)
