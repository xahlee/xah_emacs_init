;; -*- coding: utf-8; lexical-binding: t; -*-
;; Emacs settings plain gnu emacs only
;; 2017-07-15


;; initial window and default window

(setq inhibit-startup-screen t)

(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)
            (width . 106)
            (height . 60)
            (background-color . "honeydew")
            (left . 50)
            (top . 50)))
  (setq initial-frame-alist '( (tool-bar-lines . 0))))

(setq default-frame-alist initial-frame-alist)



;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)
;; (setenv "LANG" "en_US.UTF-8" )
;; (setenv "LC_ALL" "en_US.UTF-8" )



;; for isearch-forward, make these equivalent: space newline tab hyphen underscore
(setq search-whitespace-regexp "[-_ \t\n]+")

(defun xah-toggle-search-whitespace ()
  "Set `search-whitespace-regexp' to nil or includes hyphen lowline tab newline.
Explanation: When in isearch (M-x `isearch-forward'), space key can also stand for other chars such as hyphen lowline tab newline. It depend on a regex. It's convenient. But sometimes you want literal. This command makes it easy to toggle.

Emacs Isearch Space Toggle
http://ergoemacs.org/emacs/emacs_isearch_space.html
Version 2019-02-22"
  (interactive)
  (if (string-equal search-whitespace-regexp nil)
      (progn
        (setq search-whitespace-regexp "[-_ \t\n]+")
        (message "Space set to hyphen lowline tab newline space"))
    (progn
      (setq search-whitespace-regexp nil)
      (message "Space set to literal."))))

(setq make-backup-files nil)
(setq backup-by-copying t)

(setq create-lockfiles nil)

(setq auto-save-default nil)
;; (setq auto-save-visited-file-name t)

;; (setq auto-save-file-name-transforms '((".*" "~/.emacs.d/backup/" nil)))
;; (setq auto-save-list-file-prefix "~/.emacs.d/backup/.saves-")
;; (setq backup-directory-alist '(("" . "~/.emacs.d/backup")))


(column-number-mode 1)

;; (setq time-stamp-active nil)

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
    (setq dired-listing-switches "-al --time-style long-iso"))

  ;; make dired allow deleting/copy whole dir
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))



(setq save-interprogram-paste-before-kill t)

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://ergoemacs.org/misc/emacs_bug_cant_paste_2015.html
;; (setq x-selection-timeout 300)

(setq x-select-enable-clipboard-manager nil)

;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)﻿
;; ;; (setq ediff-split-window-function 'split-window-horizontally)



(require 'recentf)
(recentf-mode 1)

(desktop-save-mode 1)

(blink-cursor-mode 0)

(global-auto-revert-mode 1)

(setq sentence-end-double-space nil )

;; (setq switch-to-visible-buffer nil)

(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 5)
(setq global-mark-ring-max 5)

;; (electric-pair-mode 1)

;; set the fallback input method to Chinese for toggle-input-method
(setq default-input-method 'chinese-py) ; as of emacs 24, default is nil anyway.

(when (fboundp 'eww)
  (defun xah-rename-eww-buffer ()
    "Rename `eww-mode' buffer so sites open in new page.
URL `http://ergoemacs.org/emacs/emacs_eww_web_browser.html'
Version 2017-11-10"
    (let (($title (plist-get eww-data :title)))
      (when (eq major-mode 'eww-mode )
        (if $title
            (rename-buffer (concat "eww " $title ) t)
          (rename-buffer "eww" t)))))

  (add-hook 'eww-after-render-hook 'xah-rename-eww-buffer))

;; (add-hook 'eww-mode-hook 'xah-rename-eww-buffer)
;; (remove-hook 'eww-mode-hook 'xah-rename-eww-buffer)

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

;; (progn
;;   ;; use variable-width font for some modes
;;   (defun xah-use-variable-width-font ()
;;     "Set current buffer to use variable-width font."
;;     (variable-pitch-mode 1)
;;     ;; (text-scale-increase 1)
;;     )
;;   (add-hook 'nxml-mode-hook 'xah-use-variable-width-font)
;;   (add-hook 'xah-elisp-mode-hook 'xah-use-variable-width-font)
;;   (add-hook 'xah-js-mode-hook 'xah-use-variable-width-font)
;;   (add-hook 'xah-css-mode-hook 'xah-use-variable-width-font)
;;   (add-hook 'xah-html-mode-hook 'xah-use-variable-width-font)
;;   )

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
;; (global-subword-mode 1)
(global-subword-mode 0)

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

(setq shift-select-mode nil)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

(progn
  ;; minibuffer enhanced completion
  (require 'icomplete)
  (icomplete-mode 1)
  ;; show choices vertically
  (setq icomplete-separator "\n")
  (setq icomplete-hide-common-prefix nil)
  (setq icomplete-in-buffer t)
  (define-key icomplete-minibuffer-map (kbd "<right>") 'icomplete-forward-completions)
  (define-key icomplete-minibuffer-map (kbd "<left>") 'icomplete-backward-completions))

(progn
  ;; make buffer switch command do suggestions, also for find-file command
  (require 'ido)
  (ido-mode 1)

  ;; show choices vertically
  (if (version< emacs-version "25")
      (progn
        (make-local-variable 'ido-separator)
        (setq ido-separator "\n"))
    (progn
      (make-local-variable 'ido-decorations)
      (setf (nth 2 ido-decorations) "\n")))

  ;; show any name that has the chars you typed
  (setq ido-enable-flex-matching t)
  ;; use current pane for newly opened file
  (setq ido-default-file-method 'selected-window)
  ;; use current pane for newly switched buffer
  (setq ido-default-buffer-method 'selected-window)
  ;; stop ido from suggesting when naming new file
  (when (boundp 'ido-minor-mode-map-entry)
    (define-key (cdr ido-minor-mode-map-entry) [remap write-file] nil)))


;; indentation, tab

(electric-indent-mode 0) ; default is on in emacs 24.4

(set-default 'tab-always-indent 'complete)

;; no mixed tabs and spaces
(setq-default indent-tabs-mode nil) ; gnu emacs 23.1, 24.4.1 default is t

;; 4 is more popular than 8.
(setq-default tab-width 4) ; width for display tabs. emacs 23.1 default is 8



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




;; (setq-default bidi-display-reordering nil)

