;; -*- coding: utf-8 -*-

(defvar v3-insert-state-q t "boolean value. true means insertion mode is on.")
(setq v3-insert-state-q t)

(global-set-key (kbd "<end>") 'v3-modal-switch)

(defun v3-modal-switch ()
  "switch between insertion mode and command mode."
  (interactive)
  (if v3-insert-state-q
      (progn
        (setq cursor-type 'box )
        (message "%s" "command mode on")
        (setq v3-insert-state-q nil )
        )
    (progn
      (setq cursor-type 'bar )
      (message "%s" "insert mode on")
      (setq v3-insert-state-q t )
      )
    )
  (force-window-update)
  (if v3-insert-state-q
      (progn
        (global-set-key (kbd ".") 'self-insert-command)
        (global-set-key (kbd "'") 'self-insert-command) ;
        (global-set-key (kbd ",") 'self-insert-command) ;
        (global-set-key (kbd "-") 'self-insert-command ) 
        (global-set-key (kbd "/") 'self-insert-command)
        (global-set-key (kbd "0") 'self-insert-command)
        (global-set-key (kbd "1") 'self-insert-command)
        (global-set-key (kbd "2") 'self-insert-command) 
        (global-set-key (kbd "3") 'self-insert-command) 
        (global-set-key (kbd "4") 'self-insert-command) 
        (global-set-key (kbd "5") 'self-insert-command) 
        (global-set-key (kbd "6") 'self-insert-command)   
        (global-set-key (kbd "7") 'self-insert-command) 
        (global-set-key (kbd "8") 'self-insert-command)              
        (global-set-key (kbd "9") 'self-insert-command) 
        (global-set-key (kbd ";") 'self-insert-command)
        (global-set-key (kbd "=") 'self-insert-command)
        (global-set-key (kbd "SPC") 'self-insert-command) 
        (global-set-key (kbd "[") 'self-insert-command)
        (global-set-key (kbd "\\") 'self-insert-command)
        (global-set-key (kbd "`") 'self-insert-command)

        (global-set-key (kbd "a") 'self-insert-command)
        (global-set-key (kbd "b") 'self-insert-command)
        (global-set-key (kbd "c") 'self-insert-command)
        (global-set-key (kbd "d") 'self-insert-command)
        (global-set-key (kbd "e") 'self-insert-command)
        (global-set-key (kbd "f") 'self-insert-command)
        (global-set-key (kbd "g") 'self-insert-command)
        (global-set-key (kbd "h") 'self-insert-command)
        (global-set-key (kbd "i") 'self-insert-command)
        (global-set-key (kbd "j") 'self-insert-command)
        (global-set-key (kbd "k") 'self-insert-command)
        (global-set-key (kbd "l") 'self-insert-command)
        (global-set-key (kbd "m") 'self-insert-command)
        (global-set-key (kbd "n") 'self-insert-command)
        (global-set-key (kbd "o") 'self-insert-command)
        (global-set-key (kbd "p") 'self-insert-command)
        (global-set-key (kbd "q") 'self-insert-command)
        (global-set-key (kbd "r") 'self-insert-command)
        (global-set-key (kbd "s") 'self-insert-command)
        (global-set-key (kbd "t") 'self-insert-command)
        (global-set-key (kbd "u") 'self-insert-command)
        (global-set-key (kbd "v") 'self-insert-command)
        (global-set-key (kbd "w") 'self-insert-command)
        (global-set-key (kbd "x") 'self-insert-command)
        (global-set-key (kbd "y") 'self-insert-command)
        (global-set-key (kbd "z") 'self-insert-command)
        )
    (progn
      (global-set-key (kbd ".") 'backward-kill-word)
      (global-set-key (kbd "'") 'ergoemacs-compact-uncompact-block) ;
      (global-set-key (kbd ",") 'ergoemacs-shrink-whitespaces) ;
      (global-set-key (kbd "-") 'comment-dwim)
      (global-set-key (kbd "/") nil)
      (global-set-key (kbd "0") nil)
      (global-set-key (kbd "1") nil)
      (global-set-key (kbd "2") 'delete-window)
      (global-set-key (kbd "3") 'delete-other-windows)
      (global-set-key (kbd "4") 'split-window-vertically)
      (global-set-key (kbd "5") 'query-replace)
      (global-set-key (kbd "6") 'ergoemacs-select-current-block) ;
      (global-set-key (kbd "7") 'ergoemacs-select-current-line)
      (global-set-key (kbd "8") 'ergoemacs-extend-selection)
      (global-set-key (kbd "9") 'ergoemacs-select-text-in-quote)
      (global-set-key (kbd ";") 'undo)
      (global-set-key (kbd "=") 'flyspell-buffer)
      (global-set-key (kbd "SPC") 'set-mark-command)
      (global-set-key (kbd "[") 'remove-square-brackets)
      (global-set-key (kbd "\\") 'xah-escape-quotes)
      (global-set-key (kbd "`") 'make-backup)

      (global-set-key (kbd "a") nil) ;
      (global-set-key (kbd "b") 'xah-shell-commands)
      (global-set-key (kbd "c") 'previous-line)
      (global-set-key (kbd "d") 'move-beginning-of-line)
      (global-set-key (kbd "e") 'delete-backward-char) ;
      (global-set-key (kbd "f") 'copy-file-path)
      (global-set-key (kbd "g") 'backward-word)
      (global-set-key (kbd "h") 'backward-char)

      (global-set-key (kbd "i") 'kill-line)

      (global-set-key (kbd "j") 'ergoemacs-copy-line-or-region)
      (global-set-key (kbd "k") 'yank)
      (global-set-key (kbd "l") 'xah-clean-whitespace)
      (global-set-key (kbd "m") "_")
      (global-set-key (kbd "n") 'forward-char)
      (global-set-key (kbd "o") 'other-window)
      (global-set-key (kbd "p") 'kill-word)
      (global-set-key (kbd "q") 'ergoemacs-cut-line-or-region)
      (global-set-key (kbd "r") 'forward-word)
      (global-set-key (kbd "s") 'save-buffer) ;
      (global-set-key (kbd "t") 'next-line)
      (global-set-key (kbd "u") 'delete-char) ;
      (global-set-key (kbd "v") nil)
      (global-set-key (kbd "w") nil)
      (global-set-key (kbd "x") nil)
      (global-set-key (kbd "y") 'redo)
      (global-set-key (kbd "z") nil)

      )
    )
  )
