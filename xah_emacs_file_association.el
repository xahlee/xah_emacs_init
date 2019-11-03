;; -*- coding: utf-8; lexical-binding: t; -*-

;; 2014-05-30
;;   Xah Lee
;; ∑ http://xahlee.org/

(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

;; open pdf files in hex mode
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . fundamental-mode))

;; open svg files in xml mode, because emacs crashes
(add-to-list 'auto-mode-alist '("\\.svg\\'" . nxml-mode))

;; apache per dir config file
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-unix-mode))

(when (fboundp 'xah-text-mode)
  (add-to-list 'auto-mode-alist '("\\.txt\\'" . xah-text-mode)))

(when (fboundp 'xah-php-mode)
  (add-to-list 'auto-mode-alist '("\\.php\\'" . xah-php-mode))
  (add-to-list 'magic-mode-alist '("<\\?php" . xah-php-mode)))

