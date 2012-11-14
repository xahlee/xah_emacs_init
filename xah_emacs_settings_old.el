;; -*- coding: utf-8 -*-
;; A collection of generic Emacs settings

;; 2007-06, 2011-06-12
;;   Xah Lee
;; ∑ http://xahlee.org/




(setq org-return-follows-link t)


(require 'dired-x)

(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . shell-script-mode))



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

(math-symbol-input-mode 1)
(abbrev-mode 1)

(setq shift-select-mode nil)
(setq yas/indent-line nil)
(setq org-startup-folded nil)
(set-default-coding-systems 'utf-8-unix)


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


;; (custom-set-variables
;;   ;; custom-set-variables was added by Custom.
;;   ;; If you edit it by hand, you could mess it up, so be careful.
;;   ;; Your init file should contain only one such instance.
;;   ;; If there is more than one, they won't work right.
;;  '(abbrev-mode t)
;;  '(auto-save-default nil)
;;  '(initial-major-mode (quote text-mode))
;;  '(initial-scratch-message "")
;;  '(line-number-display-limit-width 500)
;;  '(menu-bar-mode t)
;;  '(minibuffer-prompt-properties (quote (read-only t point-entered minibuffer-avoid-prompt face minibuffer-prompt)))
;;  '(mouse-buffer-menu-mode-mult 4)
;;  '(pov-run-high "+R2 +A0.1 +J1.2 +Am2 +Q9 +H480 +W640 +i%s")
;;  '(recentf-exclude (quote ("/ftp")))
;;  '(recentf-max-menu-items 11)
;;  '(recentf-max-saved-items 31)
;;  '(report-emacs-bug-no-confirmation t)
;;  '(report-emacs-bug-no-explanations t)
;;  '(scalable-fonts-allowed t)
;;  '(user-full-name "Xah Lee")
;;  '(user-mail-address "xah@xahlee.org")
;;  '(w32shell-add-emacs-to-path t)
;;  '(w32shell-cygwin-bin "C:\\cygwin\\bin")
;;  '(w32shell-msys-bin "C:\\msys\\1.0\\bin")
;;  '(w32shell-shell (quote cygwin))
;;  '(xlsl-mode-format-style 1)
;;  '(xlsl-reference-url "http://lslwiki.net/lslwiki/wakka.php?wakka="))
