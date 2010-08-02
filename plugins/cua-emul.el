;;; cua-emul.el --- CUA style buffer-switching
;; $Id: cua-emul.el,v 1.14 2004/04/27 22:52:00 wence Exp $

;; This file is NOT part of Emacs.

;; Copyright (C) 2002, 2003 lawrence mitchell <wence@gmx.li>
;; Filename: cua-emul.el
;; Version: $Revision: 1.14 $
;; Author: lawrence mitchell <wence@gmx.li>
;; Maintainer: lawrence mitchell <wence@gmx.li>
;; Created: 2002-04-26
;; Keywords: buffer-switching convenience

;; COPYRIGHT NOTICE

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 2 of the License, or (at
;; your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public License
;; for more details. http://www.gnu.org/copyleft/gpl.html
;;
;; You should have received a copy of the GNU General Public License
;; along with GNU Emacs. If you did not, write to the Free Software
;; Foundation, Inc., 675 Mass Ave., Cambridge, MA 02139, USA.

;;; Commentary:
;; This file provides some emulation of CUA style buffer manipulation, in
;; the form of a minor mode.  In particular it provides the following (I
;;  think useful) functions:
;; `cua-emul-next-buffer' -- Switch to the next buffer in the buffer
;;                           list, like CUA style ctrl-tab.
;; `cua-emul-previous-buffer' -- Switch to the previous buffer in the
;;                               buffer list, like CUA style
;;                               ctrl-shift-tab.
;; `cua-emul-kill-buffer' -- Kill and maybe save the current buffer,
;;                           like CUA ctrl-f4.
;; `cua-emul-kill-frame' -- Kill the current frame, or
;;                          `save-buffers-kill-emacs' if the current
;;                          frame is the only one, like CUA alt-f4.
;;
;;; Compatibility:
;; Tested in Emacs 20.4 and Emacs 21.1/21.2.
;; Not tested in XEmacs, though I don't think there should be a problem.
;; The only thing you might have to worry about is that the
;; keybindings are specified in a way compatible with your Emacs.
;;
;; Installation:
;; To use this file, put it somewhere in your load-path, optionally
;; byte compile it and then add the following to your .emacs:
;; (require  'cua-emul)
;; (turn-on-cua-emul-mode)
;;
;; Alternately:
;; If you just want the functionality of the above mentioned commands,
;; but don't want the whole minor mode, the bits you have to pull out
;; are:
;; `cua-emul-delete-from-list', both the variable and the function
;; `cua-emul-invisible-buffers', and whichever of the above mentioned
;; commands you want.
;;
;; Note, by default, if you have any keybindings on keys cua-emul-mode
;; uses, they will not be overriden, if you want to force cua-emul-mode
;; to override keybindings, set the variable `cua-emul-force' to a
;; non-nil value.  If you do set `cua-emul-force', you may also want to
;; set `cua-emul-save-and-restore-keys' to a non-nil value so that
;; overriden keybindings are restored when `cua-emul-mode' is turned
;; off.
;;
;; If you like CUA keybindings a lot you might also want to look at
;;`pc-selection-mode', `pc-bindings-mode', (both included with Emacs
;; 21) and `cua-mode' by Kim F.  Storm -- see <URL:http://www.cua.dk/>.


;;; History:
;;

;;; TODO:
;; Make keybindings Emacs/XEmacs and version specific?

;;; Code:
(eval-when-compile
  (require 'cl))
;;; Customize stuff

(defgroup cua-emul nil
  "CUA style buffer-switching."
  :group 'convenience
  :prefix "cua-emul-")


(defcustom cua-emul-mode nil
  "Toggle CUA Emul Mode.

This minor mode provides some minimal CUA style buffer manipulation
keybindings, namely:
`cua-emul-next-buffer' -- switch to the next buffer, bound to ctrl-tab;

`cua-emul-previous-buffer' -- switch to the previous buffer, bound to
                              ctrl-shift-tab;

`cua-emul-kill-buffer' -- kill and optionally save the current
                          buffer, bound to ctrl-f4;

`cua-emul-kill-frame' -- kill the current frame, if this is the only
                         frame, call `save-buffers-kill-emacs', bound
                         to meta-f4.

You can customize the keybindings used by setting the variables
`cua-emul-next-buffer-key', `cua-emul-previous-buffer-key',
`cua-emul-kill-buffer-key' and `cua-emul-kill-frame-key'
appropriately.

Setting this variable directly does not take effect;
use either \\[customize] or the function `cua-emul-mode'."
  :set (lambda (symbol value)
	 (cua-emul-mode (or value 0)))
  :initialize 'custom-initialize-default
  :group 'cua-emul
  :require 'cua-emul
  :type 'boolean)


;;; User variables

(defcustom cua-emul-force nil
  "*If non-nil, CUA emul mode will unconditionally rebind keys.

The keys rebound are those defined by the variables:
`cua-emul-next-buffer-key', `cua-emul-previous-buffer-key',
`cua-emul-kill-buffer-key' and `cua-emul-kill-frame-key'."
  :group 'cua-emul
  :require 'cua-emul
  :type 'boolean)

(defcustom cua-emul-mode-hook nil
  "Hook to be run when entering, and leaving `cua-emul-mode'."
  :type 'hook
  :group 'cua-emul)

(defcustom cua-emul-next-buffer-key [(control tab)]
  "*Keybinding for `cua-emul-next-buffer'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is ctrl-tab."
  :set (lambda (symbol value)  ; yes this is ugly, but I can't think of
			       ; a better way of doing it.
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-next-buffer-key
				 'cua-emul-next-buffer nil)
	   (cua-emul-set-key value 'cua-emul-next-buffer t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-previous-buffer-key (if (featurep 'xemacs)
                                            [(control iso-left-tab)]
                                          [(control shift iso-lefttab)])
  "*Keybinding for `cua-emul-previous-buffer'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is ctrl-shift-tab."
  :set (lambda (symbol value)
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-previous-buffer-key
				 'cua-emul-previous-buffer nil)
	   (cua-emul-set-key value 'cua-emul-previous-buffer t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-kill-buffer-key [(control f4)]
  "*Keybinding for `cua-emul-kill-buffer'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is ctrl-f4."
  :set (lambda (symbol value)
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-kill-buffer-key
				 'cua-emul-kill-buffer nil)
	   (cua-emul-set-key value 'cua-emul-kill-buffer t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-kill-frame-key [(meta f4)]
  "*Keybinding for `cua-emul-kill-frame'.
If you leave this as an empty string or as the symbol nil, the key
will be unset.

This has to be a valid keybinding in your Emacs version.
The default value is meta-f4."
  :set (lambda (symbol value)
	 (if (or (null value) (equal value ""))
	     (cua-emul-unset-key cua-emul-kill-frame-key
				 'cua-emul-kill-frame nil)
	   (cua-emul-set-key value 'cua-emul-kill-frame t)))
  :initialize 'custom-initialize-default
  :type 'sexp
  :group 'cua-emul)

(defcustom cua-emul-kill-emacs-checks 'cua-emul-kill-emacs-checks
  "Function to call to check if it is really safe to `save-buffers-kill-emacs'.

This function should return non-nil if it is safe, and nil if it
isn't.

The default function, `cua-emul-kill-emacs-checks', checks to see if
Gnus is running, and if it is, calls `gnus-group-exit'."
  :type 'function
  :group 'cua-emul)

(defcustom cua-emul-invisible-buffers
  '("KILL" "*Compile-Log*" "*Completions*" ".newsrc-dribble" "buffer"
    ".bbdb")
  "*List of buffer names you don't want to see when buffer-switching."
  :type 'alist
  :group 'cua-emul)

(defcustom cua-emul-save-and-restore-keys nil
  "Whether `cua-emul-mode' should save and restore existing keybindings.

If this is non-nil, we will save and restore them."
  :type 'boolean
  :group 'cua-emul)

(defcustom cua-emul-verbose nil
  "Whether Cua Emul should print extra messages."
  :type 'boolean
  :group 'cua-emul)

;;; Internal Variables

(defconst cua-emul-key-alist
  '((cua-emul-next-buffer-key . cua-emul-next-buffer)
    (cua-emul-previous-buffer-key . cua-emul-previous-buffer)
    (cua-emul-kill-buffer-key . cua-emul-kill-buffer)
    (cua-emul-kill-frame-key . cua-emul-kill-frame))
    "Alist of keys and their associated commands.

Don't touch this variable unless you understand how the functions
`cua-emul-set-keys' and `cua-emul-unset-keys' work.  You might break
something.")

(defvar cua-emul-overriden-key-alist nil
  "Alist of keys overriden by `cua-emul-mode'.

We will try and restore these when disabling it.")

(defconst cua-emul-version
  "$Id: cua-emul.el,v 1.14 2004/04/27 22:52:00 wence Exp $"
  "CUA Emul Mode version number.")

;;; Internal Functions

;; Generally we don't want to switch to buffers like the minibuffer.
;; Here we convert the list of buffer names specified in
;; `cua-emul-invisible-buffers' into actual buffers.
(defun cua-emul-invisible-buffers ()
  "Convert the variable `cua-emul-invisible-buffers' to a list of buffers.

Then add all buffers whose name begins with \" \" to the list."
  ;; taken from the emacs wiki <URL:http://www.emacswiki.org/>
  (delete nil
          (append
           (mapcar 'get-buffer cua-emul-invisible-buffers)
           (mapcar (lambda (this-buffer)
                     (if (string-match "^ " (buffer-name this-buffer))
                         this-buffer))
                   (buffer-list)))))

;; Possibly not completely failsafe, but it works for our needs.
(defun cua-emul-delete-from-list (members list)
  "Delete MEMBERS from LIST.

Return modified list."
  (dolist (member members list)
    (setq list (delete member list))))

(defun cua-emul-buffer-list ()
  "Return a `buffer-list'.

This returns `buffer-list', minus those buffers specified by the
variable `cua-emul-invisible-buffers'."
  (cua-emul-delete-from-list
   (cua-emul-invisible-buffers) (buffer-list)))

(defun cua-emul-kill-emacs-checks ()
  "If this returns non-nil we should `save-buffers-kill-emacs'.

Called from `cua-emul-kill-frame' (q.v.).
See also the variable `cua-emul-kill-emacs-checks'."
  (or (not (featurep 'gnus))
      (not (gnus-alive-p))
      (or (gnus-group-exit) (not (gnus-alive-p)))))

(defun cua-emul-msg-buffers (buffers)
  "Display the list of BUFFERS in the mode-line.

The current buffer is display with \"<\" \">\" around it."
  (when cua-emul-verbose
    (setq buffers (mapcar #'buffer-name buffers))
    (setcar buffers (propertize (car buffers) 'face 'font-lock-warning-face))
    (message (mapconcat #'identity buffers " "))))

(defun cua-emul-set-key (key command &optional force)
  "Bind KEY globally to COMMAND if KEY is currently unbound.

If optional third argument FORCE is non-nil, override any existing key
definition.
Calls `global-set-key' (q.v.)."
  (if (and (null force) (key-binding key))
      (message "%s not rebound from `%s' to `%s'."
	       key (key-binding key) command)
    (global-set-key key command)))

(defun cua-emul-unset-key (key command &optional force)
  "Unbind KEY globally if it is currently bound to COMMAND.

If optional third argument FORCE is non-nil, unconditionally unbind KEY.
Calls `global-unset-key' (q.v.)."
  (if (or force (eq (key-binding key) command))
	(global-unset-key key)
    (message "%s not unbound" key)))

;; This restores keybindings that we might have overridden.  The
;; keymappings are stored in `cua-emul-overriden-key-alist'.
(defun cua-emul-restore-keys ()
  "Restore the keybindings we overrode."
  (let ((alist cua-emul-overriden-key-alist))
    (dolist (car alist)
      (global-set-key (car car) (cdr car))))
  (setq cua-emul-overriden-key-alist nil))

(defun cua-emul-save-keys ()
  "Save the keybindings we have overriden."
  (let ((alist cua-emul-key-alist))
    (dolist (car alist)
      (let ((key (symbol-value (car car))))
        (and (key-binding key)
             (add-to-list 'cua-emul-overriden-key-alist
                          (cons key (key-binding key))))))))

(defun cua-emul-set-keys (alist &optional force)
  ;; hmmm...does this docstring make sense to you?
  "Bind the cars (keys) of the conses of ALIST to the cdrs (commands).

For example, if ALIST's value was:
    (([(control c)] . ignore))
control-c would be bound to `ignore'.

A key is only bound if it is currently undefined, unless the optional
second argument FORCE is non-nil, in which case any existing key
definitions are overriden.
See also `cua-emul-set-key'."
  (if cua-emul-save-and-restore-keys
      (cua-emul-save-keys))
  (dolist (car alist)
    (cua-emul-set-key (symbol-value (car car)) (cdr car) force)))

(defun cua-emul-unset-keys (alist &optional force)
  ;; and does this one...?
  "Unbind the cars (keys) of the conses of ALIST.

A key is only unbound if it is currently bound to the cdr of its cons
cell, unless the optional second argument FORCE is non-nil, in which
case it is unconditionally unbound.
See also `cua-emul-unset-key'."
  (dolist (car alist)
    (cua-emul-unset-key (symbol-value (car car)) (cdr car) force))
  (if cua-emul-save-and-restore-keys
      (cua-emul-restore-keys)))


;;; User functions

(defun cua-emul-mode (&optional arg force)
  "Enable or disable CUA emul mode.

If called interactively with no prefix argument, toggle current condition
of the mode.
If ARG is positive unconditionally enable the mode, if ARG is negative
turn off the mode.

If FORCE is non-nil, override any existing keybindings that might
happen to use the ones we want to.
See the variable `cua-emul-mode' for more information."
  (interactive "P")
  (let ((keys cua-emul-key-alist)          ; alist of keys and commands
	(force (or force cua-emul-force))) ; non-nil if we want to force
                                           ; setting/unsetting of keys.
    (setq cua-emul-mode
	  (if (null arg)
	      (not cua-emul-mode)
	    (> (prefix-numeric-value arg) 0)))
    (if (interactive-p)
	(if cua-emul-mode
	    (message "CUA Emul Mode enabled.")
	  (message "CUA Emul Mode disabled.")))
    (if cua-emul-mode
	(cua-emul-set-keys keys force)
      (cua-emul-unset-keys keys force)))
  (run-hooks 'cua-emul-mode-hook))

(defun cua-emul-version (&optional arg)
  "Print Cua Emul's version number in the minibuffer.

If optional ARG is non-nil, insert in current buffer."
  (interactive "*P")
  (if arg
      (insert "\n" cua-emul-version "\n")
    (message cua-emul-version)))

(defun turn-on-cua-emul-mode ()
  "Unconditionally turn on CUA emul mode."
  (interactive)
  (cua-emul-mode 1)
  (if (interactive-p)
      (message "CUA Emul Mode enabled.")))

(defun turn-off-cua-emul-mode ()
  "Unconditionally turn off CUA emul mode."
  (interactive)
  (cua-emul-mode -1)
  (if (interactive-p)
      (message "CUA Emul Mode disabled.")))

(defun cua-emul-next-buffer ()
  "Switch to the next buffer in `buffer-list'.

This function emulates the CUA style ctrl-tab."
  (interactive)
  (bury-buffer (car (buffer-list)))
  (let ((target-buffer (car (cua-emul-buffer-list))))
    (switch-to-buffer target-buffer))
  (cua-emul-msg-buffers (cua-emul-buffer-list)))

(defun cua-emul-previous-buffer ()
  "Switch to the previous buffer in `buffer-list'.

This function emulates the CUA style ctrl-shift-tab."
  (interactive)
  (let* ((buffer-list (cua-emul-buffer-list))
	 (target-buffer (nth (1- (length buffer-list)) buffer-list)))
    (switch-to-buffer target-buffer))
  (cua-emul-msg-buffers (cua-emul-buffer-list)))

(defun cua-emul-kill-buffer ()
  "Maybe save, then kill the current buffer."
  (interactive)
  (let ((buffer (buffer-name))
        (kill t))
    (cond ((string= (buffer-name) "*Group*")
           (gnus-group-exit)
           (setq kill nil))
          ((eq major-mode 'erc-mode)
           (if (yes-or-no-p "Really close this erc buffer? ")
               (setq kill t)
               (setq kill nil)))
          ((or (null buffer-file-name) buffer-read-only)
           nil)
          ((buffer-modified-p)
           (if (yes-or-no-p "Save buffer before closing? ")
               (save-buffer)
             (set-buffer-modified-p nil))))
    (when kill
      (bury-buffer buffer)
      (kill-buffer buffer))))

(defun cua-emul-kill-frame ()
  "Kill the current frame.

If this is the only frame, check to see if Gnus is running, kill it if
it is, and then kill Emacs."
  (interactive)
  (if (cdr (frame-list))    ; non nil if there is more than one frame.
      (delete-frame)
    ;; If you run something like Gnus, or VM, you probably want to add
    ;; a check here before quitting Emacs.
    ;; this is really really really ugly....suggestions welcome. :-)
    (if (funcall cua-emul-kill-emacs-checks)
        (cua-kill-emacs))))

(defun cua-kill-emacs ()
  (interactive)
(if (y-or-n-p-with-timeout "Do you really want to exit Emacs ? " 4 nil)
		       (save-buffers-kill-emacs)))

(provide 'cua-emul)

;;; cua-emul.el ends here
