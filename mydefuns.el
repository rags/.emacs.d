(defun fullscreen ()
      (interactive)
      (set-frame-parameter nil 'fullscreen
                           (if (frame-parameter nil 'fullscreen) nil 'fullboth)))
    
(defun delete-duplicate-lines ()
   (interactive)
   (require 'cl)
   (defun num-of-lines () (count-lines (point-min) (point-max)))
   (setq lines-before (num-of-lines))
   (setq line-num 1)
   (while (< line-num (num-of-lines))
     (goto-line line-num)
     (setq cur-line (concat "^" (regexp-quote (buffer-substring (point-at-bol) (point-at-eol))) "$"))
     (delete-matching-lines cur-line (point-at-eol) (point-max))
     (incf line-num))   
   (message (concat (int-to-string (- lines-before (num-of-lines))) " duplicate line deleted")))

(defun duplicate-line ()
  (interactive)  
  (duplicate (point-at-bol)  (line-beginning-position 2)))

(defun duplicate (beg end)
  (interactive "r")
  (setq cur-pos (point))
  (kill-ring-save beg end)
  (goto-char end)
  (yank)
  (goto-char cur-pos))

(defun make-my-layout ()
  (split-window-horizontally)
  (enlarge-window-horizontally 7)
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

(provide 'mydefuns)


