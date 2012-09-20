;; -*- coding: utf-8 -*-
;; A collection of generic Emacs settings

;; 2007-06, 2011-06-12
;;   Xah Lee
;; ∑ http://xahlee.org/


;; § ----------------------------------------


(setq org-return-follows-link t)


(require 'dired-x)

(setenv "PS1" "\\e[0;32m◆\\u@\\H \\D{%Y-%m-%d} \\A\\e[0;30m\\w\\n")

(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . shell-script-mode))


;; § ----------------------------------------

;; (defun turn-spell-checking-on ()
;;   "Turn speck-mode or flyspell-mode on."
;;   (speck-mode 1)
;; ;  (flyspell-mode 1)
;;   )

;; (add-hook 'text-mode-hook 'turn-spell-checking-on)
;; (remove-hook 'text-mode-hook 'turn-spell-checking-on)


;; § ----------------------------------------
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


;; § ----------------------------------------
(when (boundp 'tabbar-mode) (tabbar-mode 0))

(math-symbol-input-mode 1)

(setq shift-select-mode nil)
(setq yas/indent-line nil)
(setq org-startup-folded nil)
(set-default-coding-systems 'utf-8-unix)


;; § ----------------------------------------
;; default frame

(setq initial-frame-alist '((width . 100) (height . 54)))

(setq default-frame-alist
      '((menu-bar-lines . 1)
        (left-fringe)
        (right-fringe)
        (tool-bar-lines . 0)
        (width . 100)
        (height . 52)
        ))

;; (setcdr (assq 'continuation fringe-indicator-alist) '(nil right-curly-arrow))


;; § ----------------------------------------
(custom-set-variables
  ;; custom-set-variables was added by Custom.
  ;; If you edit it by hand, you could mess it up, so be careful.
  ;; Your init file should contain only one such instance.
  ;; If there is more than one, they won't work right.
 '(abbrev-mode t)
 '(ange-ftp-try-passive-mode nil)
 '(auto-save-default nil)
 '(blink-cursor-mode nil)
 '(case-fold-search t)
 '(comment-column 2)
 '(describe-char-unidata-list (quote (name old-name general-category canonical-combining-class bidi-class decomposition decimal-digit-value digit-value numeric-value mirrored iso-10646-comment uppercase lowercase titlecase)))
 '(dictionary-default-dictionary "web1913")
 '(dictionary-default-popup-strategy "lev")
 '(dictionary-use-single-buffer t)
 '(ffap-newfile-prompt t)
 '(font-lock-maximum-decoration 2)
 '(fringe-mode nil nil (fringe))
 '(indicate-empty-lines nil)
 '(initial-major-mode (quote text-mode))
 '(initial-scratch-message "")
 '(line-number-display-limit-width 500)
 '(menu-bar-mode t)
 '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
 '(mode-require-final-newline nil)
 '(mouse-buffer-menu-mode-mult 4)
 '(pov-run-high "+R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640 +i%s")
 '(recentf-exclude (quote ("/ftp")))
 '(recentf-max-menu-items 11)
 '(recentf-max-saved-items 31)
 '(report-emacs-bug-no-confirmation t)
 '(report-emacs-bug-no-explanations t)
 '(scalable-fonts-allowed t)
 '(show-paren-mode t)
 '(speck-engine (quote Hunspell))
 '(speck-hunspell-language-options (quote (("da" utf-8 nil t nil) ("de" iso-8859-1 nil t nil) ("en" utf-8 nil nil nil) ("fr" iso-8859-1 nil nil nil) ("it" iso-8859-1 nil nil nil) ("ru" koi8-r nil nil nil))))
 '(tool-bar-mode nil nil (tool-bar))
 '(user-full-name "Xah Lee")
 '(user-mail-address "xah@xahlee.org")
 '(w32shell-add-emacs-to-path t)
 '(w32shell-cygwin-bin "C:\\cygwin\\bin")
 '(w32shell-msys-bin "C:\\msys\\1.0\\bin")
 '(w32shell-shell (quote cygwin))
 '(w3m-user-agent "Mozilla/5.0 (Windows; U; Windows NT 5.1; en-US; rv:1.9.0.6) Gecko/2009011913 Firefox/3.0.6")
 '(weblogger-server-password "")
 '(weblogger-server-url "http://www.blogger.com/")
 '(weblogger-weblog-id -1)
 '(xlsl-mode-format-style 1)
 '(xlsl-reference-url "http://lslwiki.net/lslwiki/wakka.php?wakka="))
