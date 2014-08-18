(defvar my-fullscreen-p t "Check if fullscreen is on or off")

(defun my-non-fullscreen (&optional frame)
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND restore #xf120
      (w32-send-sys-command 61728))
  (progn (set-frame-parameter frame 'width 82)
	 (set-frame-parameter frame 'fullscreen 'fullheight)))

(defun my-fullscreen (&optional frame)
  (interactive)
  (if (fboundp 'w32-send-sys-command)
      ;; WM_SYSCOMMAND maximaze #xf030
      (w32-send-sys-command 61488))
  (set-frame-parameter frame 'fullscreen 'fullboth))

(defun fullscreen ()
  (interactive)
  (setq my-fullscreen-p (not my-fullscreen-p))
  (if my-fullscreen-p
      (my-non-fullscreen)
    (my-fullscreen)))

(defun make-trasparent ()  
  (set-frame-parameter (selected-frame) 'alpha '(96 80))
  (add-to-list 'default-frame-alist '(alpha 96 80))
  )

(defun toggle-comment  ()
  "toggle comment on current line"
  (interactive)
  (if (use-region-p) 
	  (comment-or-uncomment-region (region-beginning) (region-end))
	(comment-or-uncomment-region (point-at-bol) (point-at-eol))))


(defun delete-duplicate-lines ()
  (interactive)
  (require 'cl)
  (defun num-of-lines () (count-lines (point-min) (point-max)))
  (let
      ((lines-before (num-of-lines))
       (line-num 1))
    (while (< line-num (num-of-lines))
      (goto-line line-num)
      (let ((cur-line (concat "^" (regexp-quote (buffer-substring (point-at-bol) (point-at-eol))) "$")))
	(delete-matching-lines cur-line (point-at-eol) (point-max)))
      (incf line-num))   
    (message (concat (int-to-string (- lines-before (num-of-lines))) " duplicate line deleted"))))

					;(defun duplicate-line ()
					;  (interactive)  
					;  (duplicate (point-at-bol)  (line-beginning-position 2)))
					;

(defun backward-kill-line ()
  (interactive)
  (kill-line 0))

(defun copy-line () 
  (interactive) 
  (kill-ring-save 
   (point-at-bol)  
   (line-beginning-position 2)))

(defun duplicate (beg end)
  (interactive "r")
  (let ((cur-pos (point)))
    (kill-ring-save beg end)
    (goto-char end)
    (yank)
    (goto-char cur-pos)))

(defun make-my-layout ()
  (split-window-horizontally)
  (enlarge-window-horizontally 4)
  (select-window-2)
  (split-window-vertically)
  (enlarge-window 2)
  (select-window-1))


					;(defun switch-window (n)
					;  (let ((windows (window-list)))
					;  (select-window (nth (mod n (length windows))  windows))))

(defun insert-lambda ()
  (interactive) 
  (insert (concat "("  (char-to-string (make-char 'greek-iso8859-7 107)) " () )")))

(defun reload.emacs ()
  (interactive)
  (load-file "~/.emacs.d/init.el"))

(defun toggle-selective-display ()
  (interactive)
  (set-selective-display (if selective-display nil 1)))

(defun kill-all-saved-buffers ()
  (interactive)
  (map nil (function kill-buffer)
       (remove-if (function buffer-modified-p) (buffer-list))))

(defun  python-stuff ()
  (epy-setup-ipython)
  (epy-setup-checker "pyflakes %f")
  (setq ropemacs-enable-autoimport 't)
  (setq ropemacs-autoimport-modules '("os" "shutil" "logging"))

  (add-hook 'python-mode-hook
	    (lambda ()
	      (smart-operator-mode 1)
	      (define-key smart-operator-mode-map  "." 'op-override-.)
	      (setq-default tab-width 4)
	      (setq-default python-indent-offset 4)
	      (programming-modes)
	      (setq outline-regexp " *\\(def \\|clas\\|#hea\\)")
					;(hide-sublevels 1)
	      
	      (setq ropemacs-goto-def-newwin t)
	      )))

(defun make-file-associations ()
  (add-to-list 'auto-mode-alist '("\\.m\\'" . octave-mode)))

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
  (add-to-list 'auto-mode-alist '("\\.html$" . nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.xml$" .  nxml-mode))
  (add-to-list 'auto-mode-alist '("\\.xsd$" .  nxml-mode))
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
 )

 (eval-after-load 'tern
   '(progn
      (require 'tern-auto-complete)
      (tern-ac-setup)))
 
  (autoload 'js3-mode "js3" nil t)
  (add-to-list 'auto-mode-alist '("\\.js$" . js3-mode))
  (add-hook 'json-mode-hook (lambda () 			      
			      (programming-modes) 
				  (add-hook 'before-save-hook 'web-beautify-js-buffer t t)
			      (setq outline-regexp "[^{]*{")))
  (add-hook 'js3-mode-hook
	    (lambda ()
	      (programming-modes)
	      (smart-operator-mode 1)
	      (tern-mode t)
	      (flycheck-mode t)
	      (setq outline-regexp " *\\(function\\)")
		  (add-hook 'before-save-hook 'web-beautify-js-buffer t t)
	      ;; '(if (featurep 'js3-highlight-vars)
		  ;; (js3-highlight-vars-mode))
	      )))

(defun elisp-stuff ()
  (add-hook 'emacs-lisp-mode-hook
	    (lambda () 
	      (programming-modes)
	      (paredit-mode))))

(defun programming-modes ()
  	(subword-mode t)
	(outline-minor-mode t)
	(hs-minor-mode t)
	(imenu-add-menubar-index)
	(auto-complete-mode t)
	(yas-minor-mode t)
	(setq mode-line-modes 'nil)
	(setq minor-mode-alist 'nil)
	(electric-pair-mode t))

(defun op-override-. ()
  (interactive)
  (insert "."))

(defun make-desktop-load-non-blocking ()
  (defadvice desktop-restore-file-buffer
	(around my-desktop-restore-file-buffer-advice)
	"Be non-interactive while starting a daemon."
	(if (and (daemonp)
			 (not server-process))
		(let ((noninteractive t))
		  ad-do-it)
	  ad-do-it))
  (ad-activate 'desktop-restore-file-buffer))

(defun string/ends-with (s ending)
  "Return non-nil if string S ends with ENDING."
  (cond ((>= (length s) (length ending))
	 (let ((elength (length ending)))
	   (string= (substring s (- 0 elength)) ending)))
	(t nil)))

(defun parent-dir (path)
  (let* ((truepath (cond 
		    ((string/ends-with path "/") (substring path 0 (- (length path) 1)))
		    (t path))))
    (file-name-directory truepath)))

(defun ffip-grep (&optional filters suffix)
  (my-grep (ffip-project-root) filters suffix))

(defun my-grep (dir &optional filters suffix)
  (interactive)
  (-my-grep (concat "find " dir " -type f " 
					   (cond (filters (concat "-name \"" filters "\" ")) 
						 (t ""))  
					   "-exec grep -nH -F  {} +"
					   (cond (suffix (concat " " suffix)) 
						 (t ""))))) 

(defun -my-grep (cmd)
  (grep-find (read-shell-command "Run find (like this): " cmd))) 


(provide 'mydefuns)


