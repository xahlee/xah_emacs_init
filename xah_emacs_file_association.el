;; -*- coding: utf-8 -*-

;; 2014-05-30
;;   Xah Lee
;; ∑ http://xahlee.org/

(add-to-list 'auto-mode-alist '("\\.py3\\'" . python-mode))

(add-to-list 'auto-mode-alist '("\\.clj\\'" . xah-clojure-mode))

;; (autoload 'xah-js-mode "xah-js-mode" "load xah-js-mode for JavaScript file" t)
(add-to-list 'auto-mode-alist '("\\.js\\'" . xah-js-mode))

;(autoload 'xah-css-mode "xah-css-mode" "load xah-css-mode for CSS file" t)
(add-to-list 'auto-mode-alist '("\\.css\\'" . xah-css-mode))

(add-to-list 'auto-mode-alist '("\\.html\\'" . xah-html-mode))

(add-to-list 'auto-mode-alist '("\\.php\\'" . xah-php-mode))
(add-to-list 'magic-mode-alist '("<\\?php" . xah-php-mode) )

;; open pdf files in hex mode
(add-to-list 'auto-mode-alist '("\\.pdf\\'" . fundamental-mode))

(autoload 'vimrc-mode "vimrc-mode" "loads vimrc-mode" "INTERACTIVE")

(when (fboundp 'vimrc-mode)
  (add-to-list 'auto-mode-alist '(".vim\\(rc\\)?$" . vimrc-mode))
)

;; apache per dir config file
(add-to-list 'auto-mode-alist '("\\.htaccess\\'" . conf-unix-mode))
