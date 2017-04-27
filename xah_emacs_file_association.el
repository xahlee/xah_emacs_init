;; -*- coding: utf-8; lexical-binding: t; -*-

;; 2014-05-30
;;   Xah Lee
;; ∑ http://xahlee.org/


(add-to-list 'auto-mode-alist '("\\.txt\\'" . fundamental-mode))

(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.php\\'" . xah-php-mode))
(add-to-list 'magic-mode-alist '("<\\?php" . xah-php-mode) )

;; open pdf files in hex mode
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . fundamental-mode))

;; open svg files in xml mode, because emacs crashes
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))

(autoload 'vimrc-mode "vimrc-mode" "loads vimrc-mode" "INTERACTIVE")

(when (fboundp 'vimrc-mode)
  (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))
)

;; apache per dir config file
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-unix-mode))
