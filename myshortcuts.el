
(global-set-key [f11] 'fullscreen)
(global-set-key (kbd "C-z") 'copy-line)
(global-set-key (kbd "M-RET") 'duplicate)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "<M-f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)
(global-set-key [C-f4] 'cua-emul-kill-buffer)
(global-set-key [C-tab] 'cua-emul-next-buffer)
(global-set-key [C-S-iso-lefttab] 'cua-emul-previous-buffer)
(global-set-key (kbd "C-x C-b") 'buffer-menu)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-M-+") 'text-scale-decrease) 
(global-set-key [f1] 'toggle-selective-display)
(global-set-key (kbd "C-x M-k") 'kill-all-saved-buffers)
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)
(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)
(global-set-key (kbd "C-x f") 'find-file-in-project)
(global-set-key (kbd "C-x M-f") 'find-file-in-project)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(defalias 'rr 'replace-regexp)


(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "ESC <right>")
	      'balle-python-shift-right)
	    (define-key python-mode-map (kbd "ESC <left>")
	      'balle-python-shift-left))
	    (define-key smart-operator-mode-map  "." 'op-override-.)			
	  )

(add-hook 'scheme-mode-hook
 (lambda ()
 (define-key scheme-mode-map (kbd "C-M-/") 'insert-lambda)))

(add-hook 'doc-view-mode-hook
 (lambda ()
(define-key doc-view-mode-map (kbd "C-+") 'doc-view-enlarge)
(define-key doc-view-mode-map (kbd "C-M-+") 'doc-view-shrink))) 



(defun ido-my-keys ()
  "Add my keybindings for ido."
  (when (eq ido-cur-item 'file)
	(define-key ido-mode-map (kbd "ESC DEL") 'ido-delete-backward-updir)
	(define-key ido-mode-map (kbd "C-e") 'ido-my-edit-input)
	(define-key ido-mode-map (kbd "<backspace>") 'ido-my-edit-input)
	))

(provide 'myshortcuts)
