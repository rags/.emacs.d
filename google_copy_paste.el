(defun ido-my-edit-input () "bla" (interactive)
    (setq ido-current-directory 
          (concat (abbreviate-file-name ido-current-directory) ido-text ))
    (setq ido-text "")
    (ido-edit-input)
    )
  
(defun kill-all-buffers ()
  "kill all buffers, leaving *scratch* only"
  (interactive)
  (mapcar (lambda (buffer) (kill-buffer buffer))
	  (buffer-list))
  (delete-other-windows))

(defun kill-saved-buffers ()
  "kills saved buuffers"
  (interactive)
  (mapcar (lambda (buffer) (if (not (buffer-modified-p buffer)) (kill-buffer buffer)))
	  (buffer-list))
  (delete-other-windows))
