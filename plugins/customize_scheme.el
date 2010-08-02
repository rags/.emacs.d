;;; .emacs - an emacs initialization file created by Bill Clementson
(require 'cl)

;;__________________________________________________________________________
;;;;    Site-Specific Variables 

;; See if we're on MS Windows or some other OS
(defvar mswindows-p (string-match "windows" (symbol-name system-type)))

;; Some file locations are relative to the HOME or the BIN directory
(defvar use-home)
(setq use-home (concat  "d:/tools/elisp"))
(defvar use-bin "d:/bin/")

;; Setup for PLT Scheme
(defvar plt-dir  "C:\Program Files\PLT")
(defvar mzscheme-program (concat plt-dir "mzscheme.exe"))
(setenv "path" (concat plt-dir ";" (getenv "path")))
(setenv "PLTHOME" plt-dir)
(setenv "PLTCOLLECTS" (concat plt-dir "collects"))
(if mswindows-p
    (progn
      (setenv "HOMEDRIVE" (subseq use-home 0 2))
      (setenv "HOMEPATH" (subseq use-home 2))))
(setq quack-pltcollect-dirs (directory-files (concat plt-dir "collects") t))

;; Specify where backup files are stored
(setq backup-directory-alist (quote ((".*" . "~/.backups"))))

;; Location of Info documentation
(setq-default Info-default-directory-list
	      (list (expand-file-name (concat use-home "info"))
		    (expand-file-name (concat (getenv "EMACS_DIR") "/info"))))

;;__________________________________________________________________________
;;;;    Initial Code Load

(require 'dired)
(require 'font-lock)
(require 'lazy-lock)
(require 'recentf)
(require 'mouse-sel)
(require 'hippie-exp)
(require 'browse-url)
(require 'comint)
(ignore-errors (require 'color-theme))
(ignore-errors (require 'ecb))

;;__________________________________________________________________________
;;;;    System Customizations 

;; Set buffer behaviour
(setq next-line-add-newlines nil)
(setq scroll-step 1)
(setq scroll-conservatively 5)

;; Enable emacs functionality that is disabled by default
(put 'eval-expression 'disabled nil)
(put 'set-goal-column 'disabled nil)
(put 'narrow-to-page 'disabled nil)
(put 'narrow-to-region 'disabled nil)
(put 'eval-expression 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'upcase-region 'disabled nil)
(setq enable-recursive-minibuffers t)

;; Misc customizations
(fset 'yes-or-no-p 'y-or-n-p)           ;replace y-e-s by y
(setq inhibit-startup-message t)        ;no splash screen
(defconst use-backup-dir t)             ;use backup directory
(defconst query-replace-highlight t)    ;highlight during query
(defconst search-highlight t)           ;highlight incremental search
(setq ls-lisp-dirs-first t)             ;display dirs first in dired
(global-font-lock-mode t)               ;colorize all buffers
(setq ecb-tip-of-the-day nil)           ;turn off ECB tips
(recentf-mode 1)                        ;recently edited files in menu

;; Conventional mouse/arrow movement & selection
(pc-selection-mode)                 
(delete-selection-mode t)           




;; Use ESC to exit popups

(defvar running-fsf    (and (string-match "^\\([0-9]+\\)\\." emacs-version)
                            (string-to-number (match-string 1 emacs-version))))
(defvar running-fsf21  (and running-fsf (>= running-fsf 21)))

(ignore-errors
  (load-library "electric-not.el"))

;; Always re-indent the top-level sexp
(defadvice indent-sexp (around indent-defun (&optional endpos))
  "Indent the enclosing defun (or top-level sexp)."
  (interactive)
  (save-excursion
    (beginning-of-defun)
    ad-do-it))

(ad-activate 'indent-sexp)

;; Ediff customizations
(defconst ediff-ignore-similar-regions t)
(defconst ediff-use-last-dir t)
(defconst ediff-diff-options " -b ")

;; Dired customizations
;(setq dired-listing-switches "-l")

(defun dired-mouse-find-file (event)
  "In dired, visit the file or directory name you double-click on (EVENT)."
  (interactive "e")
  (let (file)
    (save-excursion
      (set-buffer (window-buffer (posn-window (event-end event))))
      (save-excursion
	(goto-char (posn-point (event-end event)))
	(setq file (dired-get-filename))))
    (select-window (posn-window (event-end event)))
    (find-file (file-name-sans-versions file t))))

(defun my-dired-find-file ()
  "In dired, visit the file or directory name you are on (in the same window)."
  (interactive)
  (let (file)
    (save-excursion
      (setq file (dired-get-filename))
      (find-file (file-name-sans-versions file t)))))

(add-hook 'dired-mode-hook
	  '(lambda()
	     (define-key dired-mode-map [delete] 'dired-do-delete)
	     (define-key dired-mode-map [C-return] 'dired-find-file-other-window)
	     (define-key dired-mode-map [C-down-mouse-1] 'mouse-buffer-menu)
	     (define-key dired-mode-map [double-down-mouse-1] 'dired-mouse-find-file)))

;; Word completion customizations
(defconst dabbrev-always-check-other-buffers t)
(defconst dabbrev-abbrev-char-regexp "\\sw\\|\\s_")

(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
	try-expand-dabbrev-all-buffers
	try-expand-dabbrev-from-kill
	try-complete-file-name-partially
	try-complete-file-name
	try-complete-lisp-symbol-partially
	try-complete-lisp-symbol
	try-expand-whole-kill))

;; Set the name of the host and current path/file in title bar:
(setq frame-title-format
      (list (format "%s %%S: %%j " (system-name))
	    '(buffer-file-name "%f" (dired-directory dired-directory "%b"))))

;; Column & line numbers in mode bar
(column-number-mode t)
(line-number-mode t)	

;;__________________________________________________________________________
;;;;    Programming - Scheme

;; Specify modes for Scheme file extensions
(setq auto-mode-alist
      (append '(
		("\\.emacs$" . emacs-lisp-mode)
		("\\.scm$" . scheme-mode)
		("\\.ss$" . scheme-mode)
		("\\.sch$" . scheme-mode)
		)auto-mode-alist))

;; Start up Scheme
(global-set-key [(f5)]
		'(lambda ()
		   (interactive)
		   (require 'quack)
		   (run-scheme mzscheme-program)))

;; Scheme Hooks
(add-hook 'scheme-mode-hook
	  (lambda ()
	    (define-key scheme-mode-map [f1]
	      '(lambda ()
		 (interactive)
		 (ignore-errors
		   (let ((symbol (thing-at-point 'symbol)))
		     (info "(r5rs)")
		     (Info-index symbol)))))
	    (mapc (lambda (key-arg)
                    (define-key scheme-mode-map (car key-arg)
                      (eval `(lambda ()
                               (interactive)
                               (-test ,(cadr key-arg))))))
                  '(([(control c) (control m)] nil)
                    ([(control c) (h)]         :this)
                    ([(control c) (e)]         :expand)
                    ([(control c) (o)]         :expand-once)
                    ([(control c) (*)]         :expand*)
                    ([(control c) (p)]         :pp)))
	    (define-key scheme-mode-map [(control c) (x)] 'scheme-send-dwim)
	    (define-key scheme-mode-map [(control c) (\;)] 'insert-balanced-comments)
	    (define-key scheme-mode-map [(control c) (:)] 'remove-balanced-comments)
	    (define-key scheme-mode-map [(control c) (t)]
	      (lambda (prefix)
		(interactive "P")
		(-trace "trace" prefix)))
	    (define-key scheme-mode-map [(control c) (T)]
	      (lambda (prefix)
		(interactive "P")
		(-trace "trace-all" prefix)))
	    (imenu-add-to-menubar "Symbols")
	    (outline-minor-mode)
	    (make-local-variable 'outline-regexp)
	    (setq outline-regexp "^(.*")))

(add-hook 'Info-mode-hook
	  (lambda ()
	    (interactive)
	    (define-key Info-mode-map [(control c) (x)] 'scheme-send-dwim)))

;; Scheme-specific Functions
(defun insert-balanced-comments (arg)
  "Insert a set of balanced comments around the s-expression 
containing the point.  If this command is invoked repeatedly
(without any other command occurring between invocations), the 
comment progressively moves outward over enclosing expressions."
  (interactive "*p")
  (save-excursion
    (when (eq last-command this-command)
      (when (search-backward "#|" nil t)
        (save-excursion
          (delete-char 2)
          (while (and (< (point) (point-max)) (not (looking-at " *|#")))
            (forward-sexp))
          (replace-match ""))))
    (while (> arg 0)
      (backward-char 1)
      (cond ((looking-at ")") (incf arg))
            ((looking-at "(") (decf arg))))
    (insert "#|")
    (forward-sexp)
    (insert "|#")))

(defun remove-balanced-comments ()
  "Remove a set of balanced comments enclosing point."
  (interactive "*")
  (save-excursion
    (when (search-backward "#|" nil t)
      (delete-char 2)
      (while (and (< (point) (point-max)) (not (looking-at " *|#")))
	(forward-sexp))
      (replace-match ""))))

(defun kill-this-buffer-lisp ()
  (interactive)
  (cond
   ((eq (current-buffer) (get-buffer "*scheme*"))
    (let ((process (get-buffer "*scheme*")))
      (comint-snapshot-last-prompt)
      (process-send-string process "(exit)"))
    (sleep-for .1)
    (kill-this-buffer))
   (t (kill-this-buffer))))

(defun kill-all-process-buffers ()
  (mapc (lambda (buffer)
	  (if (get-buffer buffer)
	      (progn
		(pop-to-buffer buffer)
		(kill-this-buffer-lisp))))
	'("*scheme*"))
  (if mswindows-p
      (ignore-errors
	(progn 
	  (require 'gnuserv) 
	  (gnuserv-start t))))) 

(defun scheme-send-dwim (arg)
  "Send the appropriate forms to Scheme to be evaluated."
  (interactive "P")
  (save-excursion
    (cond 
     ;;Region selected - evaluate region
     ((not (equal mark-active nil))
      (scheme-send-region (mark) (point)))
     ;; At/after sexp - evaluate last sexp
     ((or (looking-at "\\s\)")
	  (save-excursion
	    (backward-char 1)
	    (looking-at "\\s\)")))
      (if (looking-at "\\s\)")
	  (forward-char 1)) 
      (scheme-send-last-sexp))
     ;; At/before sexp - evaluate next sexp
     ((or (looking-at "\\s\(")
	  (save-excursion
	    (forward-char 1)
	    (looking-at "\\s\(")))
      (if (looking-at "\\s\(")
	  (forward-list 1)) 
      (scheme-send-last-sexp))
     ;; Default - evaluate enclosing top-level sexp
     (t (scheme-send-definition)))
    (if arg (switch-to-scheme t))))

;; MzScheme Macro expansion
(defvar mzexpand-actions
  '(nil :this :expand :expand-once :expand* :pp))

(defvar mzexpand-cache nil)

(defun mzexpand-get-action ()
  (unless (eq (car mzexpand-cache) mzexpand-actions)
    (setq mzexpand-cache
          (cons mzexpand-actions
                (mapcar (lambda (a)
                          (list (replace-regexp-in-string
                                 "^:" "" (format "%s" a))
                                a))
                        mzexpand-actions))))
  (cdr (assoc (completing-read "Action? " mzexpand-cache nil t)
              (cdr mzexpand-cache))))

(defun -test (action)
  "Scheme syntax debugging. Uses Scheme code originally developed by
Eli Barzilay.  Actions: nil set current using sexp at point
 :this        show current
 :expand      expand current (possibly in a context)
 :expand-once expand one step
 :expand*     expand one step repeatedly
 :pp          pprint current"
  (interactive (mzexpand-get-action))
  (comint-send-string (get-buffer-process "*scheme*")
                      (format "(-test %S)" (or action 
					       (sexp-at-point))))
  (pop-to-buffer "*scheme*" t)
  (other-window 1))

;; MzScheme Trace
(defun -trace (action &optional prefix)
  (interactive)
  (let ((symb nil))
    (if (or (equal action "trace")
	    (equal action "untrace"))
	(setq symb (symbol-at-point)))
    (if prefix
	(setq action (concat "un" action)))
    (comint-send-string (get-buffer-process "*scheme*")
			(if symb
			    (format "(%s %S)" action symb)
			  (format "(%s)" action))))
  (pop-to-buffer "*scheme*" t)
  (other-window 1))

;;__________________________________________________________________________
;;;;    Programming - Elisp

(add-hook 'emacs-lisp-mode-hook
	  '(lambda ()
	     (interactive)
	     (require 'eldoc)
	     (turn-on-eldoc-mode)
	     (pretty-lambdas)
	     (define-key emacs-lisp-mode-map [(control c) (x)] 'copy-eval-dwim-lisp)
	     ;; Default to auto-indent on Enter
	     (define-key emacs-lisp-mode-map [(control j)] 'newline)
	     (define-key emacs-lisp-mode-map [(control m)] 'newline-and-indent)))

(defun pretty-lambdas ()
  (font-lock-add-keywords
   nil `(("(\\(lambda\\>\\)"
	  (0 (progn (compose-region (match-beginning 1) (match-end 1)
				    ,(make-char 'greek-iso8859-7 107))
		    nil))))))

;;__________________________________________________________________________
;;;;    Standard Key Overrides

;; Completions in minibuffer 


;; Mouse 
(global-set-key [down-mouse-2] 'imenu)

;; Disable mouse-2 event that was appending text into documents
(global-set-key [mouse-2] nil)

;; Prevent accidentally killing emacs.
(global-set-key [(control x) (control c)]
		'(lambda ()
		   (interactive)
		   (cua-kill-emacs)))

;; Close down Lisp before killing buffer
(global-set-key [f4] 'kill-this-buffer-lisp)

;; Better buffer list.
(global-set-key [(control x) (control b)] 'electric-buffer-list)

;; Common buffer/window control shortcuts
(global-set-key [f6] 'other-window)
(global-set-key [f7] 'delete-other-windows)
(global-set-key [(control f7)] 'ecb-toggle-ecb-windows)
(global-set-key [(meta f7)] 'ecb-toggle-layout)

;; Shells
(global-set-key [f12]
		'(lambda ()
		   (interactive)
		   (eshell)))

(global-set-key [(control f12)]
		'(lambda ()
		   (interactive)
		   (cond
		    (mswindows-p
		     (let ((explicit-shell-file-name
			    (expand-file-name (concat (getenv "EMACS_DIR") "/bin/cmdproxy.exe")))
			   (shell-file-name "cmdproxy.exe"))
		       (shell)))
		    (t (shell)))))

(global-set-key [(meta f12)]
		'(lambda ()
		   (interactive)
		   (let ((explicit-shell-file-name
			  (if mswindows-p
			      "bash.exe"
			    "bash"))
			 (shell-file-name
			  (if mswindows-p
			      "bash.exe"
			    "bash")))
		     (shell))))

;; Shortcuts to common functions
(global-set-key [(control c) (f)] 'find-function-at-point)
(global-set-key [(control c) (F)] 'ffap)

(global-set-key [(control c) (j)] 'join-line)
(global-set-key [(control c) (s)]
		(function
		 (lambda ()
		   (interactive)
		   (let ((arg (thing-at-point 'symbol)))
		     (search-forward arg)))))

(global-set-key [(control c) (r)]
		(function
		 (lambda ()
		  (interactive)
		  (let ((arg (thing-at-point 'symbol)))
		    (search-backward arg)))))

(global-set-key [(control c) (/)] 'hippie-expand)
(global-set-key [(control c) (g)] 'goto-line)	
(global-set-key [(control c) (a)] 'mark-whole-buffer)
  
;;__________________________________________________________________________
;;;;    MS Windows Customizations

;; Note that the cua-emul, gnuserve & cua libraries are optional
(if mswindows-p
    (progn
      ;; Ctrl-tab, Ctrl-F4, etc like Windows
      (ignore-errors
	(progn
	  (require 'cua-emul)
	  (setq cua-emul-force t)
	  (turn-on-cua-emul-mode)))      
      
      ;; Windows Execute from dired
      (define-key dired-mode-map "w"
	(function
	 (lambda ()
	   (interactive)
	   (setq w32-shellex-no-dired-hook t)
	   (require 'w32-shellex)
	   (w32-shellex-dired-on-objects))))

      ;; Start gnuserv on Windows 
      (ignore-errors
	(progn 
	  (require 'gnuserv) 
	  (setq server-done-function 'bury-buffer 
		gnuserv-frame (car (frame-list))) 
	  (gnuserv-start) 
	  ;; Open buffer in existing frame instead of creating new one... 
	  (setq gnuserv-frame (selected-frame)) 
	  (message "gnuserv started.")))))



;;;;    Customizations

(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(max-lisp-eval-depth 10000)
 '(max-specpdl-size 2000)
 '(quack-pretty-lambda-p t)
 '(tool-bar-mode nil nil (tool-bar)))

;; Overrides
(global-set-key [f6] 'other-window)

;; end of emacs.el