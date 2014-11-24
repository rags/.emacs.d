;; Part of 24.4
;; (defvar my-fullscreen-p t "Check if fullscreen is on or off")

;; (defun my-non-fullscreen (&optional frame)
;;   (interactive)
;;   (if (fboundp 'w32-send-sys-command)
;;       ;; WM_SYSCOMMAND restore #xf120
;;       (w32-send-sys-command 61728))
;;   (progn (set-frame-parameter frame 'width 82)
;; 	 (set-frame-parameter frame 'fullscreen 'fullheight)))

;; (defun my-fullscreen (&optional frame)
;;   (interactive)
;;   (if (fboundp 'w32-send-sys-command)
;;       ;; WM_SYSCOMMAND maximaze #xf030
;;       (w32-send-sys-command 61488))
;;   (set-frame-parameter frame 'fullscreen 'fullboth))

;; (defun fullscreen ()
;;   (interactive)
;;   (setq my-fullscreen-p (not my-fullscreen-p))
;;   (if my-fullscreen-p
;;       (my-non-fullscreen)
;;     (my-fullscreen)))

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


;; part of 24.4
;; (defun delete-duplicate-lines ()
;;   (interactive)
;;   (require 'cl)
;;   (defun num-of-lines () (count-lines (point-min) (point-max)))
;;   (let
;;       ((lines-before (num-of-lines))
;;        (line-num 1))
;;     (while (< line-num (num-of-lines))
;;       (goto-line line-num)
;;       (let ((cur-line (concat "^" (regexp-quote (buffer-substring (point-at-bol) (point-at-eol))) "$")))
;; 	(delete-matching-lines cur-line (point-at-eol) (point-max)))
;;       (incf line-num))   
;;     (message (concat (int-to-string (- lines-before (num-of-lines))) " duplicate line deleted"))))

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

(defun my-duplicate () (interactive) (if (use-region-p) (duplicate (region-beginning) (region-end)) (djcb-duplicate-line)))

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

;(defun projectile-grep (&optional filters suffix)
;  (my-grep (projectile-project-root) filters suffix))
;
;(defun my-grep (dir &optional filters suffix)
;  (interactive)
;  (-my-grep (concat "find " dir " -type f " 
;					   (cond (filters (concat "-name \"" filters "\" ")) 
;						 (t ""))  
;					   "-exec grep -nH -F  {} +"
;					   (cond (suffix (concat " " suffix)) 
;						 (t ""))))) 
;
;(defun -my-grep (cmd)
;  (grep-find (read-shell-command "Run find (like this): " cmd))) 
;

(defun my-add-to-list (l &rest elements)
  (dolist (element elements) (add-to-list l element)))

(defun js-require-file-and-go (filename)
  "Load a file in the javascript interpreter."
  (interactive (list (or buffer-file-truename (read-file-name "Require: "))))
  (let ((filename (expand-file-name filename)))
    (run-js inferior-js-program-command t)
    (comint-send-string inferior-js-buffer (concat "require(\"" filename "\")\n"))
    (switch-to-js inferior-js-buffer)))

(defun yank-bol ()
  (interactive)
  (beginning-of-line) 
  (yank))

(defun my-global-set-key (command &rest keys)  
  (dolist (key keys) (global-set-key key command)))

(provide 'mydefuns)


