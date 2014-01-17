;; -*- coding: utf-8 -*-

;; a vi-like modal keybinding for emacs.
;; the keys are based on ergoemacs-mode and dvorak layout and for The truly Ergonomic Keyboard.
;; created: 2013-09-10
;; Xah Lee

;; some notes:
;; • other than being a modal input system, this design doesn't follow vi or vim's traditions. For example: there's no command such as “dd”. and there's no typing a digit followed by a command to repeat n times. This is not a vi emulation mode.
;; • the keymap is largely compatible with ergoemacs-mode. It's based on mapping the most frequetly used command to the most easy-to-press key positions.
;; • created this around 2013-08. Used it daily since.

;; TODO
;; • make it a proper minor mode.
;; • fine-tune lots keys in command mode. (introduce key sequence there. Consider whether {open, close, save} should be there. and some other commands such as {dired-jump, query-replace-regexp, xah-toggle-letter-case}.)
;; • support different keyboard layouts.
;; • reconsider some keybinding so it's more friendly for normal PC keyboard or Microsoft 4000.

;; i wrote this mode for myself. License is open source. Feel free to copy but please give credit.
;; if you like to see this mode go further, donate at http://ergoemacs.org/emacs/emacs.html , or buy my tutorial http://ergoemacs.org/emacs/buy_xah_emacs_tutorial.html and let me know. Thanks.

;; (require 'ergoemacs-mode) ; calls some editing functions in ergoemacs-mode.

(defvar v3-insert-state-q t "boolean value. true means insertion mode is on.")
(setq v3-insert-state-q t)

;; to enter insert mode, press the command mode key then 【a】
(global-set-key (kbd "<home>") 'v3-command-mode-activate)
(global-set-key (kbd "<end>") 'v3-insert-mode-activate)

(defun v3-insert-mode-init ()
  "DOCSTRING"
  (interactive)
  ;; TODO use a proper keymap
  (progn
    (global-set-key (kbd "SPC") 'self-insert-command)
    (global-set-key (kbd ";") 'self-insert-command)
    (global-set-key (kbd "=") 'self-insert-command)
    (global-set-key (kbd "[") 'self-insert-command)
    (global-set-key (kbd "\\") 'self-insert-command)
    (global-set-key (kbd "`") 'self-insert-command)
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
    (global-set-key (kbd ";") 'undo)
    (global-set-key (kbd "=") nil)
    (global-set-key (kbd "[") nil)
    (global-set-key (kbd "`") nil)
    (global-set-key (kbd ".") 'backward-kill-word)
    (global-set-key (kbd "'") 'xah-compact-uncompact-block) ;
    (global-set-key (kbd ",") 'xah-shrink-whitespaces) ;
    (global-set-key (kbd "-") nil)
    (global-set-key (kbd "/") nil)
    (global-set-key (kbd "0") nil)
    (global-set-key (kbd "1") nil)
    (global-set-key (kbd "2") nil)
    (global-set-key (kbd "3") 'set-mark-command)
    (global-set-key (kbd "4") 'keyboard-quit)

    (global-set-key (kbd "3") 'xah-backward-quote)
    (global-set-key (kbd "4") 'xah-forward-quote)

; 5605    0.35%  cua-set-mark; 944    0.06%  set-mark-command
;   5064    0.31%  delete-other-windows
;   6077    0.38%  ergoemacs-M-o

    ;;  toggle-input-method

    (global-set-key (kbd "5") nil)
    (global-set-key (kbd "6") 'xah-select-current-block) ;
    (global-set-key (kbd "7") 'xah-select-current-line)
    (global-set-key (kbd "8") 'xah-extend-selection)
    (global-set-key (kbd "9") 'xah-select-text-in-quote)
    (global-set-key (kbd "a") 'open-line) ;
    (global-set-key (kbd "b") 'xah-toggle-letter-case)
    (global-set-key (kbd "c") 'previous-line)
    (global-set-key (kbd "d") 'xah-beginning-of-line-or-block)
    (global-set-key (kbd "e") 'delete-backward-char) ;
    (global-set-key (kbd "f") 'isearch-forward)
    (global-set-key (kbd "g") 'backward-word)
    (global-set-key (kbd "h") 'backward-char)
    (global-set-key (kbd "i") 'kill-line)
    (global-set-key (kbd "j") 'xah-copy-line-or-region)
    (global-set-key (kbd "k") 'yank)
    (global-set-key (kbd "l") 'recenter-top-bottom)
    (global-set-key (kbd "m") 'xah-backward-open-bracket)
    (global-set-key (kbd "n") 'forward-char)
    (global-set-key (kbd "o") 'other-window)
    (global-set-key (kbd "p") 'kill-word)
    (global-set-key (kbd "q") 'xah-cut-line-or-region)
    (global-set-key (kbd "r") 'forward-word)
    (global-set-key (kbd "s") 'xah-end-of-line-or-block) ;
    (global-set-key (kbd "t") 'next-line)
    (global-set-key (kbd "u") 'delete-char) ;
    (global-set-key (kbd "v") 'xah-forward-close-bracket)
    (global-set-key (kbd "w") nil)
    (global-set-key (kbd "x") 'xah-cycle-hyphen-underscore-space)
    (global-set-key (kbd "y") 'redo)
    (global-set-key (kbd "z") 'comment-dwim)
    )
  )

(defun v3-mode-toggle ()
  "Switch between {insertion, command} modes."
  (interactive)
  (if v3-insert-state-q
      (v3-command-mode-activate)
    (v3-insert-mode-activate)
    )
  )

(defun v3-command-mode-activate ()
  "Switch to command mode."
  (interactive)
  (setq cursor-type 'box )
  (setq v3-insert-state-q nil )
  (force-window-update) ; TODO force cursor shape change to show, but sometimes doesn't work
  (v3-command-mode-init)
  )

(defun v3-insert-mode-activate ()
  "Switch to insertion mode."
  (interactive)
  (setq cursor-type 'bar )
  (setq v3-insert-state-q t )
  (force-window-update) ; TODO force cursor shape change to show, but sometimes doesn't work
  (v3-insert-mode-init)
  )

;; when in going into minibuffer, switch to insertion mode.
(add-hook 'minibuffer-setup-hook 'v3-insert-mode-activate)
(add-hook 'minibuffer-exit-hook 'v3-command-mode-activate)

;; TODO when in shell mode, switch to insertion mode.
(add-hook 'shell-mode-hook 'v3-insert-mode-activate)

;; TODO show state in mode line
