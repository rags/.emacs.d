
(global-set-key [f11] 'fullscreen)

(global-set-key (kbd "M-c") 'capitalize-word)
(global-set-key (kbd "C-z") 'copy-line)
(global-set-key (kbd "M-RET") 'duplicate)
(global-set-key (kbd "C-x C-r") 'recentf-open-files)
(global-set-key (kbd "<M-f4>") 'save-buffers-kill-terminal)
(global-set-key (kbd "C-x C-c") 'save-buffers-kill-terminal)
(global-set-key (kbd "<s-f4>") 'cua-emul-kill-buffer)
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
(global-set-key (kbd "C-c w") 'er/expand-region)
(global-set-key (kbd "M-y") 'kill-ring-ido)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-x C-h") 'browse-url-emacs)
(defalias 'rr 'replace-regexp)

(global-set-key (kbd "C-<") 'mark-previous-like-this)
(global-set-key (kbd "C->") 'mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mark-more-like-this) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mark-all-like-this)
(define-key python-mode-map (kbd "C-c t") 'tgt-toggle)
(global-set-key (kbd "C-?") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-?") 'ac-complete-with-helm)
(global-set-key (kbd "M-;") 'toggle-comment)


(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "ESC <right>")
	      'balle-python-shift-right)
	    (define-key python-mode-map (kbd "ESC <left>")
	      'balle-python-shift-left))
	    (define-key smart-operator-mode-map  "." 'op-override-.)

		
	  )

(add-hook 'python-mode-hook
		  (lambda () (define-key python-mode-map (kbd "M-/") 'dabbrev-expand)))

(add-hook 'scheme-mode-hook
 (lambda ()
 (define-key scheme-mode-map (kbd "C-M-/") 'insert-lambda)))




(add-hook 'doc-view-mode-hook
 (lambda ()
(define-key doc-view-mode-map (kbd "C-+") 'doc-view-enlarge)
(define-key doc-view-mode-map (kbd "C-M-+") 'doc-view-shrink))) 

(provide 'myshortcuts)
