;; -*- coding: utf-8 -*-

(ergoemacs-theme-component ergoemacs-xah ()
  "xah's menu key set"
  :layout "dvorak"

  (global-set-key (kbd "<menu> SPC") (kbd "_"))
  ;; …

  (global-set-key (kbd "<menu> n 3") 'whitespace-mode)
  (global-set-key (kbd "<menu> n 4") 'linum-mode)
  (global-set-key (kbd "<menu> n 5") 'visual-line-mode)
  (global-set-key (kbd "<menu> n 6") 'calendar)
  (global-set-key (kbd "<menu> n 7") 'calc)
  (global-set-key (kbd "<menu> n 8") 'shell)
  (global-set-key (kbd "<menu> n 9") 'shell-command)
  (global-set-key (kbd "<menu> n 0") 'shell-command-on-region)
  (global-set-key (kbd "<menu> n b") 'toggle-debug-on-error)
  (global-set-key (kbd "<menu> n c") 'toggle-case-fold-search)
  (global-set-key (kbd "<menu> n e") 'eshell)
  (global-set-key (kbd "<menu> n h") 'widen)
  (global-set-key (kbd "<menu> n f") ctl-x-5-map) ; frame
  (global-set-key (kbd "<menu> n n") 'narrow-to-region)
  (global-set-key (kbd "<menu> n s") 'flyspell-buffer)
  (global-set-key (kbd "<menu> n t") 'narrow-to-defun)

)

