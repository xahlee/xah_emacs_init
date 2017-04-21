;; -*- coding: utf-8; lexical-binding: t; -*-
;; Emacs settings for packages bundled with pure gnu emacs only

;; 2007-06
;;   Xah Lee
;; ∑ http://xahlee.org/


;; initial window and default window

;; (setq inhibit-splash-screen t)

(setq initial-frame-alist
      '(
        (tool-bar-lines . 0)
        (width . 106)
        (height . 60)
        (background-color . "honeydew")
        (font . "DejaVu Sans Mono-10")
        ))

(setq default-frame-alist
      '(
        (tool-bar-lines . 0)
        (width . 106)
        (height . 60)
        (background-color . "honeydew")
        (font . "DejaVu Sans Mono-10")
        ))



;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
;; (setenv "LANG" "en_US.UTF-8" )
;; (setenv "LC_ALL" "en_US.UTF-8" )


(setq initial-major-mode 'fundamental-mode)

(setq search-whitespace-regexp "[-_ \n]")

(setq make-backup-files nil)
(setq backup-by-copying t)

(setq auto-save-default nil)
;; (setq auto-save-visited-file-name t)

(setq save-interprogram-paste-before-kill t)

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://ergoemacs.org/misc/emacs_bug_cant_paste_2015.html
;; (setq x-selection-timeout 300)

(setq time-stamp-active nil)

(progn
  ;; seems pointless to warn. There's always undo.
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
)


(progn
  ;; dired
  (require 'dired-x)

  ;; make dired suggest target dir (for copy, move, …) that's in the other dired pane
  (setq dired-dwim-target t)

  ;; make dired not include 「.」 and 「..」, and use metric prefix for file size
  (when (string-equal system-type "gnu/linux")
    (setq dired-listing-switches "-Al --time-style long-iso"))

  ;; make dired allow deleting/copy whole dir
  (setq dired-recursive-copies (quote always))
  (setq dired-recursive-deletes (quote top)))


(setq x-select-enable-clipboard-manager nil)

;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)﻿
;; ;; (setq ediff-split-window-function 'split-window-horizontally)

;; (setq mouse-yank-at-point t)
;; (setq mouse-wheel-progressive-speed t)


(require 'recentf)
(recentf-mode 1)

(desktop-save-mode 1)

(blink-cursor-mode 0)

(global-auto-revert-mode 1)

(setq sentence-end-double-space nil )

;; (setq switch-to-visible-buffer nil)

(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 9)
(setq global-mark-ring-max 9)

;; set the fallback input method to Chinese for toggle-input-method
(setq default-input-method 'chinese-py) ; as of emacs 24, default is nil anyway.

(when (fboundp 'eww)
  (progn
    (defun xah-rename-eww-hook ()
      "Rename eww browser's buffer so sites open in new page."
      (rename-buffer "eww" t))
    (add-hook 'eww-mode-hook 'xah-rename-eww-hook)))

;; (progn
;;   (defun xah-turn-on-line-number ()
;;     "Turn on line number on margin."
;;     (linum-mode 1))
;;   (add-hook 'text-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'xah-elisp-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'xah-css-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'xah-html-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'xah-js-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'xah-xahk-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'xah-clojure-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'xah-php-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'python-mode-hook 'xah-turn-on-line-number)
;;   (add-hook 'racket-mode-hook 'xah-turn-on-line-number)
;; )

(progn
  ;; use variable-width font for some modes
  (defun xah-use-variable-width-font ()
    "Set current buffer to use variable-width font."
    (variable-pitch-mode 1)
    ;; (text-scale-increase 0.5 )
    )
  (add-hook 'nxml-mode-hook 'xah-use-variable-width-font)
  (add-hook 'xah-elisp-mode-hook 'xah-use-variable-width-font)
  (add-hook 'xah-js-mode-hook 'xah-use-variable-width-font)
  (add-hook 'xah-css-mode-hook 'xah-use-variable-width-font)
  (add-hook 'xah-html-mode-hook 'xah-use-variable-width-font)
  )

(progn ; minibuffer
  (setq enable-recursive-minibuffers t)

  ;; Save minibuffer history
  (savehist-mode 1)

;; big minibuffer height, for ido to show choices vertically
(setq max-mini-window-height 0.5)

  ;; minibuffer, stop cursor going into prompt
  (customize-set-variable
   'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt))))



;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))


;;; editing related

;; make cursor movement stop in between camelCase words.
(global-subword-mode 1)

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

(setq shift-select-mode nil)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(progn
  ;; interactive name completion for describe-function, describe-variable, execute-extended-command, etc.
  (icomplete-mode 1)
  ;; make icomplete prettier
  (setq icomplete-separator " ")
  ;; (setq icomplete-separator "\n")
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t))

(progn
  (require 'ido)
  ;; make buffer switch command do suggestions, also for find-file command
  (ido-mode 1)
  ;; (ido-everywhere 1)
  (when ; make ido display choices vertically
      (not (version< emacs-version "25"))
    (progn
      (make-local-variable 'ido-decorations)
      (setf (nth 2 ido-decorations) "\n")))
  (setq ido-enable-flex-matching t) ; show any name that has the chars you typed
  (setq ido-default-file-method 'selected-window) ; use current pane for newly opened file
  (setq ido-default-buffer-method 'selected-window) ; use current pane for newly switched buffer
  (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil) ; stop ido from suggesting when naming new file
  )


;; indentation, tab

(electric-indent-mode 0) ; default is on in emacs 24.4

(set-default 'tab-always-indent 'complete)

;; no mixed tabs and spaces
(setq-default indent-tabs-mode nil) ; gnu emacs 23.1, 24.4.1 default is t

;; 4 is more popular than 8.
(setq tab-width 4) ; width for display tabs. emacs 23.1 default is 8



(progn
  ;; org-mode
  ;; make “org-mode” syntax color code sections
  (setq org-src-fontify-natively t)
  (setq org-startup-folded nil)
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil))



;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "http://melpa.milkbox.net/packages/")
   t))



(progn
 ;; Make whitespace-mode with very basic background coloring for whitespaces.
  ;; http://ergoemacs.org/emacs/whitespace-mode.html
  (setq whitespace-style (quote (face spaces tabs newline space-mark tab-mark newline-mark )))

  ;; Make whitespace-mode and whitespace-newline-mode use “¶” for end of line char and “▷” for tab.
  (setq whitespace-display-mappings
        ;; all numbers are unicode codepoint in decimal. e.g. (insert-char 182 1)
        '(
          (space-mark 32 [183] [46]) ; SPACE 32 「 」, 183 MIDDLE DOT 「·」, 46 FULL STOP 「.」
          (newline-mark 10 [182 10]) ; LINE FEED,
          (tab-mark 9 [9655 9] [92 9]) ; tab
          )))



(setq browse-url-browser-function 'browse-url-firefox)



(defalias 'yes-or-no-p 'y-or-n-p)

(defalias 'lcd 'list-colors-display)
(defalias 'sl 'sort-lines)
(defalias 'ds 'desktop-save)
(defalias 'dt 'desktop-save)
(defalias 'dsm 'desktop-save-mode)

(defalias 'elm 'emacs-lisp-mode)
(defalias 'hm 'html-mode)

(defalias 'ssm 'shell-script-mode)

(defalias 'tpu-edt 'forward-char)
(defalias 'tpu-edt-on 'forward-char) ; fuck tpu-edt


(setq hippie-expand-try-functions-list
      '(
        try-expand-dabbrev
        try-expand-dabbrev-all-buffers
        ;; try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol
        try-complete-file-name-partially
        try-complete-file-name
        ;; try-expand-all-abbrevs
        ;; try-expand-list
        ;; try-expand-line
        ))

(setq use-dialog-box nil)


