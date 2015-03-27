;; -*- coding: utf-8 -*-
;; Emacs settings for packages bundled with pure gnu emacs only

;; 2007-06
;;   Xah Lee
;; ∑ http://xahlee.org/



;; (set-selection-coding-system nil)
;; (set-selection-coding-system 'utf-8)
;; (set-selection-coding-system 'chinese-gb18030)
;; (set-selection-coding-system 'gb18030)
;; (set-selection-coding-system 'utf-16-le)
;; (set-selection-coding-system 'utf-16le-dos)
;; (set-selection-coding-system 'raw-text)
;; selection-coding-system

;; 哈哈
;; emacs coding system
;; binary
;; chinese-gb18030
;; chinese-gb18030-dos
;; chinese-gb18030-mac
;; chinese-gb18030-unix
;; dos
;; emacs-internal
;; gb18030
;; gb18030-dos
;; gb18030-mac
;; gb18030-unix
;; mac
;; mule-utf-8
;; mule-utf-8-dos
;; mule-utf-8-mac
;; mule-utf-8-unix
;; no-conversion
;; prefer-utf-8
;; prefer-utf-8-dos
;; prefer-utf-8-mac
;; prefer-utf-8-unix
;; raw-text
;; raw-text-dos
;; raw-text-mac
;; raw-text-unix
;; unix
;; utf-16
;; utf-16-be
;; utf-16-be-dos
;; utf-16-be-mac
;; utf-16-be-unix
;; utf-16-dos
;; utf-16-le
;; utf-16-le-dos
;; utf-16-le-mac
;; utf-16-le-unix
;; utf-16-mac
;; utf-16-unix
;; utf-16be
;; utf-16be-dos
;; utf-16be-mac
;; utf-16be-unix
;; utf-16be-with-signature
;; utf-16be-with-signature-dos
;; utf-16be-with-signature-mac
;; utf-16be-with-signature-unix
;; utf-16le
;; utf-16le-dos
;; utf-16le-mac
;; utf-16le-unix
;; utf-16le-with-signature
;; utf-16le-with-signature-dos
;; utf-16le-with-signature-mac
;; utf-16le-with-signature-unix
;; utf-7
;; utf-7-dos
;; utf-7-imap
;; utf-7-imap-dos
;; utf-7-imap-mac
;; utf-7-imap-unix
;; utf-7-mac
;; utf-7-unix
;; utf-8
;; utf-8-auto
;; utf-8-auto-dos
;; utf-8-auto-mac
;; utf-8-auto-unix
;; utf-8-dos
;; utf-8-emacs
;; utf-8-emacs-dos
;; utf-8-emacs-mac
;; utf-8-emacs-unix
;; utf-8-mac
;; utf-8-unix
;; utf-8-with-signature
;; utf-8-with-signature-dos
;; utf-8-with-signature-mac
;; utf-8-with-signature-unix

(set-default-coding-systems 'utf-8-unix)



(set-background-color "honeydew")
(setq inhibit-splash-screen t)


;; (setq x-select-enable-clipboard-manager t)

;; (setq ediff-window-setup-function 'ediff-setup-windows-plain)﻿
;; ;; (setq ediff-split-window-function 'split-window-horizontally)

(setq mouse-wheel-progressive-speed nil)


;; initial window and default window

;123456789;123456789;123456789;123456789;123456789;123456789;123456789;123456789;123456789

(setq initial-frame-alist
      '(
        (width . 92)
        (height . 54)
        (background-color . "honeydew")
        ) )

(setq default-frame-alist
      '((menu-bar-lines . 1)
        (left-fringe)
        (right-fringe)
        (tool-bar-lines . 0)
        (width . 92)
        (height . 52)
        (background-color . "#eeeedd")
        ))



(winner-mode 0)
(electric-pair-mode 0)
(blink-cursor-mode 0 )
(setq sentence-end-double-space nil )
(electric-indent-mode 0) ; default is on in emacs 24.4
(global-auto-revert-mode 1)

(setq scroll-error-top-bottom t )

(setq tab-width 4)   ; width for display tabs. emacs 23.1 default is 8
(set-default 'abbrev-mode t)

(setq shift-select-mode nil)

(setq org-startup-folded nil)
(setq org-return-follows-link t)

(when (fboundp 'eww)
  (progn
    (defun xah-rename-eww-hook ()
      "Rename eww browser's buffer so sites open in new page."
      (rename-buffer "eww" t))
    (add-hook 'eww-mode-hook 'xah-rename-eww-hook)))



;; (which-function-mode 1) ; show current function in mode line

;; emacs slows down when you open a file with tens thousands lines
;; emacs freezes
(global-linum-mode 0)

;; (setq auto-save-default t)
;; (setq auto-save-visited-file-name t )

;; set the fallback input method to Chinese for toggle-input-method
(setq default-input-method 'chinese-py) ; as of emacs 24, default is nil anyway.


;; 2009-09-29 see http://groups.google.com/group/ergoemacs/msg/9eec3b455cab3ff1 and http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
; (and (= emacs-major-version 23) (defun server-ensure-safe-dir (dir) "Noop" t))



;; (setcdr (assq 'continuation fringe-indicator-alist) '(nil right-curly-arrow))


;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(abbrev-mode t)
;;  ;; '(auto-save-default nil)
;;  ;; '(initial-major-mode (quote text-mode))
;;  ;; '(initial-scratch-message "")
;;  ;; '(line-number-display-limit-width 500)
;;  ;; '(mouse-buffer-menu-mode-mult 4)
;;  ;; '(pov-run-high "+R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640 +i%s")
;;  ;; '(recentf-exclude (quote ("/ftp")))
;;  ;; '(recentf-max-menu-items 11)
;;  ;; '(recentf-max-saved-items 31)
;;  ;; '(report-emacs-bug-no-confirmation t)
;;  ;; '(report-emacs-bug-no-explanations t)
;;  ;; '(scalable-fonts-allowed t)
;;  ;; '(user-full-name "Xah Lee")
;;  ;; '(user-mail-address "xah@xahlee.org")
;;  ;; '(w32shell-add-emacs-to-path t)
;;  ;; '(w32shell-cygwin-bin "C:\\cygwin\\bin")
;;  ;; '(w32shell-msys-bin "C:\\msys\\1.0\\bin")
;;  ;; '(w32shell-shell (quote cygwin))
;;  ;; '(xlsl-mode-format-style 1)
;;  ;; '(xlsl-reference-url "http://lslwiki.net/lslwiki/wakka.php?wakka=")
;; )

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(completions-common-part ((t (:inherit default :foreground "red"))))
 '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
 '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
 )