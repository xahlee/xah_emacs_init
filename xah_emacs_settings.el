;; -*- coding: utf-8 -*-
;; A collection of generic Emacs settings

;; 2007-06, 2011-06-12
;;   Xah Lee
;; ∑ http://xahlee.org/


(require 'dired-x)

(setq default-input-method 'chinese-py)

(setq bookmark-default-file "~/.emacs.d/bookmarks")

(setq page-break-lines-modes (quote (emacs-lisp-mode compilation-mode fundamental-mode text-mode org-mode ruby-mode python-mode xah-html-mode html-mode nxml-mode )) )

;; interactive name completion for describe-function, describe-variable, etc.
(icomplete-mode 1)

(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))
(add-to-list 'auto-mode-alist '("\\.tpl\\'" . php-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . xah-html-mode))
(add-to-list 'auto-mode-alist '("\\.html\\.erb\\'" . xah-html-mode))
;; (add-to-list 'auto-mode-alist '("\\.html\\'" . html-mode))
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . shell-script-mode))


(setq org-return-follows-link t)
; (setq auto-save-default t)

(winner-mode 1)
(cua-mode 0)
(electric-pair-mode 0)

(blink-cursor-mode 0 )
(setq cursor-type 'box)




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


(when (boundp 'tabbar-mode) (tabbar-mode 0))
(setq tab-width 1)   ; width for display tabs. emacs 23.1 default is 8

(math-symbol-input-mode 1)

(set-default 'abbrev-mode t)
(setq initial-major-mode (quote text-mode))

(setq shift-select-mode nil)
(setq yas/indent-line nil)
(setq org-startup-folded nil)
(set-default-coding-systems 'utf-8-unix)

(set-background-color "honeydew")
(setq inhibit-splash-screen t)


;; default frame

(when (string-equal system-type "windows-nt") 
  (setq initial-frame-alist '((width . 100) (height . 54)))
  (setq default-frame-alist
        '((menu-bar-lines . 1)
          (left-fringe)
          (right-fringe)
          (tool-bar-lines . 0)
          (width . 100)
          (height . 52)
          (cursor-type . box)
          ))
  )


(add-to-list 'default-frame-alist '(font . "DejaVu Sans Mono-10"))
;; (set-frame-parameter nil 'font "DejaVu Sans Mono-10")
 
;; (setcdr (assq 'continuation fringe-indicator-alist) '(nil right-curly-arrow))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(rcirc-default-nick "xahlee"))


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
;;  ;; '(menu-bar-mode t)
;;  ;; '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
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
 '(rainbow-delimiters-depth-1-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-2-face ((t (:foreground "blue"))))
 '(rainbow-delimiters-depth-3-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-4-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-5-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-6-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-7-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-8-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-depth-9-face ((t (:foreground "#8b7500"))))
 '(rainbow-delimiters-unmatched-face ((t (:foreground "red"))))
 '(show-paren-match ((((class color) (background light)) (:background "azure2")))))
