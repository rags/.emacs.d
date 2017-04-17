(global-set-key [f1] help-map)
(global-set-key [f11] 'toggle-frame-fullscreen)

(global-set-key (kbd "M-c") 'capitalize-word)
(global-set-key (kbd "C-z") 'copy-line)
(global-set-key (kbd "M-RET") 'duplicate)
(global-set-key (kbd "C-x C-r") 'helm-recentf)
(my-global-set-key  'save-buffers-kill-terminal (kbd "C-x C-c") (kbd "<M-f4>"))
(global-set-key (kbd "<s-f4>") 'cua-emul-kill-buffer)
(global-set-key [C-f4] 'cua-emul-kill-buffer)
(global-set-key [C-tab] 'cua-emul-next-buffer)
(global-set-key [C-S-iso-lefttab] 'cua-emul-previous-buffer)
(global-set-key (kbd "C-x C-b") 'helm-buffers-list)
(global-set-key (kbd "C-+") 'text-scale-increase)
(global-set-key (kbd "C-M-+") 'text-scale-decrease) 
(global-set-key (kbd "C-x M-k") 'kill-all-saved-buffers)
(global-set-key (kbd "C-x c") 'save-buffers-kill-emacs)
(global-set-key (kbd "ESC <up>") 'move-text-up)
(global-set-key (kbd "ESC <down>") 'move-text-down)
(global-set-key (kbd "C-x d") 'projectile-find-dir)
(global-set-key (kbd "C-x f") 'projectile-find-file)
(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)
(global-set-key (kbd "C-c w") 'er/expand-region)
(global-set-key (kbd "C-c W") 'er/contract-region)
(global-set-key (kbd "M-y") 'kill-ring-ido)
(global-set-key (kbd "C-c k") 'kill-whole-line)
(global-set-key (kbd "C-x C-h") 'browse-url-emacs)


(global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
(global-set-key (kbd "C->") 'mc/mark-next-like-this)
(global-set-key (kbd "C-M-m") 'mc/mark-more-like-this-extended) ; like the other two, but takes an argument (negative is previous)
(global-set-key (kbd "C-*") 'mc/mark-all-like-this)
(global-set-key (kbd "C-c m") 'vr/mc-mark)

(global-set-key (kbd "C-?") 'ac-complete-with-helm)
(define-key ac-complete-mode-map (kbd "C-?") 'ac-complete-with-helm)
(global-set-key (kbd "M-;") 'toggle-comment)
(global-set-key (kbd "M-.") 'helm-etags-select)
(global-set-key (kbd "C-c @ t") 'hs-toggle-hiding)
(global-set-key (kbd "C-c @ a") 'hs-show-all)
(global-set-key (kbd "C-c @ c") 'hs-hide-level)
(global-set-key (kbd "C-c t") 'tgt-toggle)
(global-set-key (kbd "C-s") 'helm-occur)
(my-global-set-key  'set-rectangular-region-anchor (kbd "C-c SPC") (kbd "s-SPC") (kbd "H-SPC"))
(global-set-key  (kbd "C-M-SPC") 'mc/edit-lines)
(global-set-key (kbd "C-^") (lambda () (interactive) (join-line 0)))
(global-unset-key (kbd "C-c y"))
(my-global-set-key  'my-duplicate (kbd "C-c y") (kbd "s-d") (kbd "H-d"))
(my-global-set-key  'yank-bol (kbd "s-y") (kbd "H-y") (kbd "C-c C-y"))
(my-global-set-key  'goto-line (kbd "s-g") (kbd "H-g"))
;(global-set-key (kbd "C-c r") 'vr/query-replace)
(global-set-key (kbd "C-h") 'delete-backward-char)
(global-set-key (kbd "C-M-h") 'backward-kill-word)
(global-set-key [remap eval-expression] 'pp-eval-expression)
(global-set-key [remap eval-last-sexp] 'pp-eval-last-sexp)

(js3r-add-keybindings-with-prefix "C-c C-m")

(add-hook 'python-mode-hook
	  (lambda ()
	    (define-key python-mode-map (kbd "ESC <right>")
	      'balle-python-shift-right)
	    (define-key python-mode-map (kbd "ESC <left>")
	      'balle-python-shift-left)
        (define-key python-mode-map (kbd "M-/") 'dabbrev-expand)
        (define-key python-mode-map (kbd "C-c t") 'tgt-toggle)
        )
;	    (define-key smart-operator-mode-map  "." 'disable-smart-op)
	  )


(add-hook 'scheme-mode-hook
 (lambda ()
 (define-key scheme-mode-map (kbd "C-M-/") 'insert-lambda)))


(add-hook 'js3-mode-hook
	    (lambda ()
	       (local-set-key "\C-x\C-e" 'js-send-last-sexp)
	       (local-set-key "\C-\M-x" 'js-send-last-sexp-and-go)
	       (local-set-key "\C-cb" 'js-send-buffer)
	       (local-set-key "\C-c\C-b" 'js-send-buffer-and-go)
	       (local-set-key "\C-cl" 'js-require-file-and-go)))
(key-chord-define js3-mode-map ";;" "\C-e;")

(add-hook 'doc-view-mode-hook
 (lambda ()
(define-key doc-view-mode-map (kbd "C-+") 'doc-view-enlarge)
(define-key doc-view-mode-map (kbd "C-M-+") 'doc-view-shrink))) 
;hyper key for mac
(setq ns-function-modifier 'hyper) 

(add-hook 'clojure-mode-hook
          (lambda ()
            (define-key clojure-mode-map (kbd "C-c t") 'tgt-toggle)
            (cljr-add-keybindings-with-prefix "C-c C-m")))


;;;;;;;;;;;;;;;;;;;;;;;;; keychords ;;;;;;;;;;;;;;;;;;;

 (key-chord-define-global "uu" 'undo)
 (key-chord-define-global "xx" 'kill-whole-line)
 (key-chord-define-global "yy" 'copy-line)
; (key-chord-define-global "pp" "\C-a\C-y")
; (key-chord-define-global "dd" 'my-duplicate)
; (key-chord-define-global "gg" 'goto-line)


;;;;;;;;;;;;;;;;;;;;;;;;; aliases ;;;;;;;;;;;;;;;;;;;
(defalias 'rr 'vr/query-replace)
(defalias 'gst 'magit-status)
(defalias 'gl 'magit-log)

(move-text-default-bindings)

;;works with guide-key-mode enabled. lists the options for the prefix
(setq guide-key/guide-key-sequence '("C-x r" "C-x 4" "C-x n" "C-c @" "C-c C-c" "C-x C-k" "C-c p" "C-c r" "C-c C-r" "C-c RET" "C-c C-m"))

(provide 'myshortcuts)
