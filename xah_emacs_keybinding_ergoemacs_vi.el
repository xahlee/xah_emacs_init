;; -*- coding: utf-8 -*-

;; a vi-like modal keybinding for emacs.
;; the keys are based on ergoemacs-mode and dvorak layout and for The truly Ergonomic Keyboard.
;; created: 2013-09-10
;; Xah Lee

;; some notes:
;; • other than being a modal input system, this design doesn't follow vi or vim's traditions. For example: there's no command such as “dd”. and there's no typing a digit followed by a command to repeat n times. This is not a vi emulation mode.
;; • Unlike vi, where you have {i, o, a} keys to go into insertion mode, but they can't switch you back to command mode. Also, the 【Esc】 key switch you back to command mode, but isn't a toggle.
;; • the only key to switch mode is 【‹toggle key›】. This ‹toggle key› is currently the 【backspace】 key.
;; • the keymap is largely compatible with ergoemacs-mode. It's based on mapping the most frequetly used command to the most easy-to-press key positions.
;; • this is currently a prototype. That is, alpha stage. Lots improvement can be made. 

;; TODO
;; • make it a proper minor mode.
;; • fine-tune lots keys in command mode. (introduce key sequence there. Consider whether {open, close, save} should be there. and some other commands such as {dired-jump, query-replace-regexp, ergoemacs-toggle-letter-case}.)
;; • support different keyboard layouts.
;; • reconsider some keybinding so it's more friendly for normal PC keyboard or Microsoft 4000.

;; i wrote this mode for myself. License is open source. Feel free to copy but please give credit.
;; if you like to see this mode go further, donate at http://ergoemacs.org/emacs/emacs.html , or buy my tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html and let me know. Thanks.

(require 'ergoemacs-mode )              ; calls some editing functions in ergoemacs-mode.


(defvar v3-insert-state-q t "boolean value. true means insertion mode is on.")
(setq v3-insert-state-q t)

(global-set-key (kbd "<delete>") 'v3-modal-switch)

(defun v3-insert-mode-init ()
  "DOCSTRING"
  (interactive)
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
  )

(defun v3-command-mode-init ()
  "DOCSTRING"
  (interactive)
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
    (global-set-key (kbd "b") nil)
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
    (global-set-key (kbd "w") 'ergoemacs-close-current-buffer)
    (global-set-key (kbd "x") 'undo)
    (global-set-key (kbd "y") 'redo)
    (global-set-key (kbd "z") nil)
    )
  )

(defun v3-modal-switch ()
  "Switch between insertion {mode, command} modes."
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
      (v3-insert-mode-init)
    (v3-command-mode-init)
    )
  )

(add-hook 'minibuffer-setup-hook 'v3-insert-mode-init)
(add-hook 'minibuffer-exit-hook 'v3-command-mode-init)
