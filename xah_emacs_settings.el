;; -*- coding: utf-8; lexical-binding: t; -*-
;; Emacs settings plain gnu emacs only
;; 2019-11-06
;; http://ergoemacs.org/emacs/emacs_init_index.html

;; HHH___________________________________________________________________
;; initial window and default window

(setq inhibit-startup-screen t)

(if (display-graphic-p)
    (setq initial-frame-alist
          '(
            (tool-bar-lines . 0)
            (background-color . "honeydew")
            (width . 106)
            (height . 54)
            ;; (left . 50)
            ;; (top . 50)
            ))
  (setq initial-frame-alist '( (tool-bar-lines . 0))))

(setq default-frame-alist
      '(
        (tool-bar-lines . 0)
        (background-color . "honeydew")
        (width . 100)
        (height . 52)))

;; HHH___________________________________________________________________
;; UTF-8 as default encoding
(set-language-environment "UTF-8")
(set-default-coding-systems 'utf-8-unix)

;; HHH___________________________________________________________________
;; backup and file related

(defun xah-save-all-unsaved ()
  "Save all unsaved files. no ask.
Version 2019-11-05"
  (interactive)
  (save-some-buffers t ))
(add-hook 'focus-out-hook 'xah-save-all-unsaved)

(setq make-backup-files nil)
(setq backup-by-copying t)
(setq create-lockfiles nil)
(setq auto-save-default nil)

(require 'recentf)
(recentf-mode 1)

(progn
  (desktop-save-mode 1)
  (setq desktop-restore-frames nil)
  (setq desktop-auto-save-timeout 300)
  (setq desktop-globals-to-save nil)
  )

(global-auto-revert-mode 1)

;; HHH___________________________________________________________________
;; user interface

(when (version<= "26.0.50" emacs-version )
  (global-display-line-numbers-mode))

(column-number-mode 1)
(blink-cursor-mode 0)
(setq use-dialog-box nil)

(progn
  ;; no need to warn
  (put 'narrow-to-region 'disabled nil)
  (put 'narrow-to-page 'disabled nil)
  (put 'upcase-region 'disabled nil)
  (put 'downcase-region 'disabled nil)
  (put 'erase-buffer 'disabled nil)
  (put 'scroll-left 'disabled nil)
  (put 'dired-find-alternate-file 'disabled nil)
)

;; HHH___________________________________________________________________

(progn
  (require 'dired-x)
  (setq dired-dwim-target t)
  (when (string-equal system-type "gnu/linux") (setq dired-listing-switches "-al --time-style long-iso"))
  (setq dired-recursive-copies 'always)
  (setq dired-recursive-deletes 'always))

;; HHH___________________________________________________________________

(setq set-mark-command-repeat-pop t)
(setq mark-ring-max 5)
(setq global-mark-ring-max 5)

;; HHH___________________________________________________________________

;; set default font
(set-frame-font
 (cond
  ((string-equal system-type "windows-nt") ; Microsoft Windows
   (if (member "Consolas" (font-family-list))
       "Consolas"
     nil
     ))
  ((string-equal system-type "darwin") ; macOS
   (if (member "Menlo" (font-family-list))
       "Menlo-15"
     nil
     ))
  ((string-equal system-type "gnu/linux") ; linux
   (if (member "DejaVu Sans Mono" (font-family-list))
       "DejaVu Sans Mono"
     nil
     ))
  (t nil))
 t t)

;; set font for emoji
(set-fontset-font
 t
 '(#x1f300 . #x1fad0)
 (cond
  ((member "Noto Color Emoji" (font-family-list)) "Noto Color Emoji")
  ((member "Noto Emoji" (font-family-list)) "Noto Emoji")
  ((member "Segoe UI Emoji" (font-family-list)) "Segoe UI Emoji")
  ((member "Symbola" (font-family-list)) "Symbola")
  ((member "Apple Color Emoji" (font-family-list)) "Apple Color Emoji"))
 ;; Apple Color Emoji should be before Symbola, but Richard Stallman skum disabled it.
 ;; GNU Emacs Removes Color Emoji Support on the Mac
 ;; http://ergoemacs.org/misc/emacs_macos_emoji.html
 ;;
 )

(cond
 ;; specify font for chinese characters
 ((string-equal system-type "windows-nt")
  (set-fontset-font
   t
   '(#x4e00 . #x9fff)
   (cond
    ((member "Microsoft YaHei" (font-family-list)) "Microsoft YaHei")
    ((member "PMingLiU" (font-family-list)) "PMingLiU")
    ((member "SimHei" (font-family-list)) "SimHei")
    ((member "Microsoft YaHei UI" (font-family-list)) "Microsoft YaHei UI")
    ((member "MingLiU" (font-family-list)) "MingLiU")
    ((member "SimHei" (font-family-list)) "SimHei")
    ((member "DengXian" (font-family-list)) "DengXian")
    ((member "KaiTi" (font-family-list)) "KaiTi")
    ((member "SimSun" (font-family-list)) "SimSun"))))
 ((string-equal system-type "darwin")
  (cond
   ((member "Heiti SC" (font-family-list)) "Heiti SC")
   ((member "Heiti TC" (font-family-list)) "Heiti TC")
   ((member "Songti SC" (font-family-list)) "Songti SC")
   ((member "Songti TC" (font-family-list)) "Songti TC")
   ((member "Kaiti SC" (font-family-list)) "Kaiti SC")
   ((member "BiauKai" (font-family-list)) "BiauKai")))
 ((string-equal system-type "gnu/linux")
  (cond
   ((member "WenQuanYi Micro Hei" (font-family-list)) "WenQuanYi Micro Hei"))))

(progn
  ;; use variable-width font for some modes
  (defun xah-set-proportial-font ()
    "Set current buffer to use variable-width font."
    (variable-pitch-mode 1)
    (text-scale-increase 1 )
)
  (add-hook 'html-mode-hook 'xah-set-proportial-font)
  (add-hook 'nxml-mode-hook 'xah-set-proportial-font)
  (add-hook 'emacs-lisp-mode-hook 'xah-set-proportial-font)
  (add-hook 'js-mode-hook 'xah-set-proportial-font)
  (add-hook 'css-mode-hook 'xah-set-proportial-font)
  (add-hook 'xah-elisp-mode-hook 'xah-set-proportial-font)
  (add-hook 'xah-html-mode-hook 'xah-set-proportial-font)
  (add-hook 'xah-css-mode-hook 'xah-set-proportial-font)
  (add-hook 'xah-js-mode-hook 'xah-set-proportial-font)
  ;;
  )

;; HHH___________________________________________________________________

(progn
  ;; minibuffer setup
  (setq enable-recursive-minibuffers t)
  (savehist-mode 1)
  ;; big minibuffer height, for ido to show choices vertically
  (setq max-mini-window-height 0.5)
  ;; minibuffer, stop cursor going into prompt
  (customize-set-variable
   'minibuffer-prompt-properties
   (quote (read-only t cursor-intangible t face minibuffer-prompt))))

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

;; HHH___________________________________________________________________

;; remember cursor position
(if (version< emacs-version "25.0")
    (progn
      (require 'saveplace)
      (setq-default save-place t))
  (save-place-mode 1))

;; HHH___________________________________________________________________
;;; editing related

;; make typing delete/overwrites selected text
(delete-selection-mode 1)

(setq shift-select-mode nil)

;; (electric-pair-mode 1)

;; set highlighting brackets
(show-paren-mode 1)
(setq show-paren-style 'parenthesis)

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

;; 2015-07-04 bug of pasting in emacs.
;; http://debbugs.gnu.org/cgi/bugreport.cgi?bug=16737#17
;; http://ergoemacs.org/misc/emacs_bug_cant_paste_2015.html
;; (setq x-selection-timeout 300)
(setq save-interprogram-paste-before-kill t)
(setq x-select-enable-clipboard-manager nil)

;; HHH___________________________________________________________________
;; indentation, end of line

(electric-indent-mode 0)

(set-default 'tab-always-indent 'complete)

;; no mixed tab space
(setq-default indent-tabs-mode nil)
 ; gnu emacs 23.1, 24.4.1 default is t

;; 4 is more popular than 8.
(setq-default tab-width 4)

(setq sentence-end-double-space nil )

;; HHH___________________________________________________________________

;; load emacs 24's package system. Add MELPA repository.
(when (>= emacs-major-version 24)
  (require 'package)
  (add-to-list
   'package-archives
   ;; '("melpa" . "http://stable.melpa.org/packages/") ; many packages won't show if using stable
   '("melpa" . "https://melpa.org/packages/")
   t))

;; HHH___________________________________________________________________

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

;; HHH___________________________________________________________________
;; edit related

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

;; HHH___________________________________________________________________

;; convenient
(defalias 'yes-or-no-p 'y-or-n-p)
(defalias 'rs 'replace-string)
(defalias 'lcd 'list-colors-display)
(defalias 'ds 'desktop-save)

(defalias 'dsm 'desktop-save-mode)
(defalias 'elm 'emacs-lisp-mode)
(defalias 'hm 'html-mode)
(defalias 'jsm 'js-mode)
(defalias 'fm 'fundamental-mode)
(defalias 'ssm 'shell-script-mode)
(defalias 'om 'org-mode)

(defalias 'ms 'magit-status)
(defalias 'xnp 'xahsite-new-page)
(defalias 'xfc 'xah-find-count)
(defalias 'xcm 'xah-css-mode)
(defalias 'xem 'xah-elisp-mode)
(defalias 'xhm 'xah-html-mode)
(defalias 'xjm 'xah-js-mode)
(defalias 'xcm 'xah-clojure-mode)

;; no want tpu-edt
(defalias 'tpu-edt 'forward-char)
(defalias 'tpu-edt-on 'forward-char)

;; HHH___________________________________________________________________
(progn
  ;; org-mode
  (setq org-src-fontify-natively t)
  (setq org-startup-folded nil)
  (setq org-return-follows-link t)
  (setq org-startup-truncated nil)
  (setq org-startup-with-inline-images t)
  ;;
  )

;; HHH___________________________________________________________________

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
