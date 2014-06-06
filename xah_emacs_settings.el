;; -*- coding: utf-8 -*-
;; A collection of generic Emacs settings

;; 2007-06, 2011-06-12
;;   Xah Lee
;; ∑ http://xahlee.org/


;; fundamental

(set-default-coding-systems 'utf-8-unix)

(set-background-color "honeydew")
(setq inhibit-splash-screen t)


;; initial window and default window

;123456789;123456789;123456789;123456789;123456789;123456789;123456789;123456789;123456789

(setq initial-frame-alist
      '(
        (width . 92)
        (height . 54)
        ) )

(setq default-frame-alist
      '((menu-bar-lines . 1)
        (left-fringe)
        (right-fringe)
        (tool-bar-lines . 0)
        (width . 92)
        (height . 52)
        ))

;; set default font
(cond
 ((string-equal system-type "windows-nt") ; Microsoft Windows
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
    )
  )
 ((string-equal system-type "darwin")   ; Mac OS X
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  )
 ((string-equal system-type "gnu/linux") ; linux
  (when (member "DejaVu Sans Mono" (font-family-list))
    (add-to-list 'initial-frame-alist '(font . "DejaVu Sans Mono-10"))
    (add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10")))
  )
 )

;; Emacs Lisp: Determine OS, Emacs Version, Machine Host Name
;; http://ergoemacs.org/emacs/elisp_determine_OS_version.html



(winner-mode 0)
(delete-selection-mode 1)
(electric-pair-mode 0)
(blink-cursor-mode 0 )
(setq sentence-end-double-space nil )
(electric-indent-mode 0) ; default is on in emacs 24.4
(global-auto-revert-mode 1)

(setq scroll-error-top-bottom t )

(setq tab-width 1)   ; width for display tabs. emacs 23.1 default is 8
(setq ido-enable-flex-matching t)
(set-default 'abbrev-mode t)

(setq shift-select-mode nil)

(setq org-startup-folded nil)
(setq org-return-follows-link t)


;; (global-subword-mode 0)

;; (which-function-mode 1) ; show current function in mode line

;; hog emacs down when you happened to open a large file with thousands of lines
(global-linum-mode 0)

;; (setq auto-save-default t)
;; (setq auto-save-visited-file-name t )

;; (set-default cursor-type 'bar)

;; set the fallback input method to Chinese for toggle-input-method
(setq default-input-method 'chinese-py) ; as of emacs 24, default is nil anyway.

(setq page-break-lines-modes (quote (emacs-lisp-mode xah-elisp-mode compilation-mode fundamental-mode text-mode org-mode ruby-mode python-mode xah-html-mode html-mode nxml-mode )) )



;; (defun turn-spell-checking-on ()
;;   "Turn speck-mode or flyspell-mode on."
;;   (speck-mode 1)
;; ;  (flyspell-mode 1)
;;   )

;; (add-hook 'text-mode-hook 'turn-spell-checking-on)
;; (remove-hook 'text-mode-hook 'turn-spell-checking-on)


;; 2009-09-29 see http://groups.google.com/group/ergoemacs/msg/9eec3b455cab3ff1 and http://stackoverflow.com/questions/885793/emacs-error-when-calling-server-start
; (and (= emacs-major-version 23) (defun server-ensure-safe-dir (dir) "Noop" t))


;; (add-hook 'emacs-lisp-mode-hook
;;  (lambda ()
;;   (modify-syntax-entry ?- "w" emacs-lisp-mode-syntax-table )
;;  )
;; )


;; ;; by Nikolaj Schumacher. http://www.emacswiki.org/emacs/HexColour
;; (defvar hexcolor-keywords
;;   '(("#[abcdef[:digit:]]\\{6\\}"
;;      (0 (put-text-property
;;          (match-beginning 0)
;;          (match-end 0)
;;          'face (list :background
;;                      (match-string-no-properties 0)))))))

;; (defun hexcolor-add-to-font-lock ()
;;   (interactive)
;;   (font-lock-add-keywords nil hexcolor-keywords))
;; (add-hook 'css-mode-hook 'hexcolor-add-to-font-lock)
;; (add-hook 'html-mode-hook 'hexcolor-add-to-font-lock)



;; (eval-when-compile
;;   (when (boundp 'tabbar-mode)
;;     (tabbar-mode 0)
;;     ))

(when (fboundp 'ido-vertical-mode)
    (ido-vertical-mode 1)
)

(setq yas/indent-line nil)

(math-symbol-input-mode 1)

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

;; (custom-set-faces
;;  ;; custom-set-faces was added by Custom.
;;  ;; If you edit it by hand, you could mess it up, so be careful.
;;  ;; Your init file should contain only one such instance.
;;  ;; If there is more than one, they won't work right.
;;  '(completions-common-part ((t (:inherit default :foreground "red"))))
;;  '(diredp-compressed-file-suffix ((t (:foreground "#7b68ee"))))
;;  '(diredp-ignored-file-name ((t (:foreground "#aaaaaa"))))
;;  '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))))
;;  '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
;;  '(rainbow-delimiters-depth-3-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
;;  '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
;;  '(show-paren-match ((((class color) (background light)) (:background "azure2")))))

;; (add-hook 'xah-css-mode-hook 'rainbow-mode)
;; (remove-hook 'css-mode-hook 'rainbow-mode)
;; (remove-hook 'xah-css-mode-hook 'rainbow-mode)
