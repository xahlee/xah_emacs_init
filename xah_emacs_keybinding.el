;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.

;; jump points to other files
;; • xah_emacs_keybinding.el
;; • xah_emacs_unicode_input.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_mouse_binding.el
;; • xah_emacs_alias.el
;; • xah_emacs_abbr.el

;; • 〈Dvorak Keyboard Layout〉 http://xahlee.info/comp/dvorak_keyboard_layout.html
;; • 〈ErgoEmacs Keybinding〉 http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html
;; • 〈xah_autohotkey.ahk〉 http://xahlee.info/mswin/autohotkey.html
;; • 〈Emacs: How to define Hyper ＆ Super Keys〉 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; • 〈Emacs: Remapping Keys Using key-translation-map〉 http://ergoemacs.org/emacs/emacs_key-translation-map.html
;; • 〈Emacs: Add Custom Keys to Enhance Productivity〉 http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html

;; Xah Lee
;; created: 2007-06.


;; generic


(define-key key-translation-map (kbd "<apps>") (kbd "<menu>"))
(define-key key-translation-map (kbd "<f7>") (kbd "<menu>"))

;; in linux, <lwindow> is Super is s
;; linux, the menu/apps key is <menu>

;; windows, the menu/apps key is <apps>

(global-set-key (kbd "<menu>") 'backward-char)

(global-set-key (kbd "M-7") 'ergoemacs-select-text-in-quote)
(global-set-key (kbd "C-8") 'ergoemacs-extend-selection)

;; (global-set-key (kbd "<f8>") ctl-x-map)
;; (global-set-key (kbd "<f8>") mode-specific-map)


(global-set-key (kbd "<f8>") 'delete-other-windows)
(global-set-key (kbd "<f9>") 'ergoemacs-move-cursor-next-pane)
;; (global-set-key (kbd "<f8>") 'ergoemacs-switch-to-next-frame)
;; (global-set-key (kbd "<f9>") 'split-window-vertically)
(global-set-key (kbd "<f10>") 'ergoemacs-close-current-buffer)
(global-set-key (kbd "<f11>") 'ergoemacs-next-user-buffer)
(global-set-key (kbd "<f12>") 'ergoemacs-previous-user-buffer)

;; (global-set-key (kbd "<f8>") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'beep)



(global-set-key (kbd "M-m") 'hippie-expand)


(global-set-key (kbd "<f10>") 'ergoemacs-close-current-buffer)

(define-prefix-command 'xah-keymap)
(global-set-key (kbd "<menu>") 'xah-keymap)

(global-set-key (kbd "<menu> <menu>") 'smex)

;; e is for unicode insert
;; t is mode-specific
(global-set-key (kbd "<menu> '") 'replace-straight-quotes)
(global-set-key (kbd "<menu> ,") 'remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<menu> .") 'convert-english-chinese-punctuation)
(global-set-key (kbd "<menu> 1") 'copy-to-register-1)
(global-set-key (kbd "<menu> 2") 'paste-from-register-1)
(global-set-key (kbd "<menu> 3") 'query-replace)
(global-set-key (kbd "<menu> 4") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<menu> 5") 'recentf-open-files)
(global-set-key (kbd "<menu> 6") 'run-current-file) ;; immediate, but dangerous
(global-set-key (kbd "<menu> 7") 'xah-open-file-at-cursor)
(global-set-key (kbd "<menu> 8") 'dired-jump)
(global-set-key (kbd "<menu> 9") 'ispell-word)
(global-set-key (kbd "<menu> <delete>") 'delete-current-file)
(global-set-key (kbd "<menu> =") 'repeat-complex-command)
(global-set-key (kbd "<menu> [") 'remove-square-brackets)
(global-set-key (kbd "<menu> \\") 'escape-quotes)
(global-set-key (kbd "<menu> ]") 'indent-region)
(global-set-key (kbd "<menu> `") 'make-backup)
(global-set-key (kbd "<menu> b") 'flyspell-buffer)
(global-set-key (kbd "<menu> d") 'ergoemacs-open-in-desktop)
(global-set-key (kbd "<menu> f") 'copy-file-path)
(global-set-key (kbd "<menu> i d") 'insert-date)
(global-set-key (kbd "<menu> i r h") 'insert-random-hex)
(global-set-key (kbd "<menu> i r n") 'insert-random-number)
(global-set-key (kbd "<menu> i r s") 'insert-random-string)
(global-set-key (kbd "<menu> i r u") 'insert-random-uuid)
(global-set-key (kbd "<menu> i t") 'insert-date-time)
(global-set-key (kbd "<menu> m c") 'calc)
(global-set-key (kbd "<menu> m e") 'emacs-lisp-mode)
(global-set-key (kbd "<menu> m h") 'xah-html-mode)
(global-set-key (kbd "<menu> m o") 'org-mode)
(global-set-key (kbd "<menu> m s") 'shell)
(global-set-key (kbd "<menu> m t") 'text-mode)
(global-set-key (kbd "<menu> m v") 'visual-line-mode)
(global-set-key (kbd "<menu> m w") 'whitespace-mode)
(global-set-key (kbd "<menu> o") 'xah-open-file-fast)
(global-set-key (kbd "<menu> r f") 'xah-find-text)
(global-set-key (kbd "<menu> r q") 'query-replace-regexp)
(global-set-key (kbd "<menu> r r") 'xah-find-replace-text)
(global-set-key (kbd "<menu> s") 'shell-command)
(global-set-key (kbd "<menu> w") 'delete-trailing-whitespace)
(global-set-key (kbd "<menu> x") 'xah-cite)
(global-set-key (kbd "<menu> z") 'title-case-string-region-or-line)



;; special keys

(global-set-key (kbd "<insert>") 'ergoemacs-switch-to-next-frame)




;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown


(global-set-key (kbd "M-\"") 'xah-compact-uncompact-block)

(global-set-key (kbd "M-2") 'cycle-hyphen-underscore-space)


;; mode-specific

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."

  (local-set-key (kbd "<menu> e 0") 'dehtmlize-text)
  (local-set-key (kbd "<menu> e 5") 'mark-unicode)
  (local-set-key (kbd "<menu> e 6") 'browse-url-of-buffer)
  (local-set-key (kbd "<menu> e 7") 'htmlize-or-dehtmlize-pre-block)
  (local-set-key (kbd "<menu> e 8") 'get-pre-block-make-new-file)
  (local-set-key (kbd "<menu> e 9") 'code-bracket-to-html-tag)
  (local-set-key (kbd "<menu> e <delete>") 'sgml-delete-tag)
  (local-set-key (kbd "<menu> e <left>") 'sgml-skip-tag-backward)
  (local-set-key (kbd "<menu> e <right>") 'sgml-skip-tag-forward)
  (local-set-key (kbd "<menu> e C-6") 'xah-browse-url-of-buffer)
  (local-set-key (kbd "<menu> e a") 'xah-annotate)
  (local-set-key (kbd "<menu> e b") 'make-blogger-entry)
  (local-set-key (kbd "<menu> e c") 'make-citation)
  (local-set-key (kbd "<menu> e d") 'insert-date-tag)
  (local-set-key (kbd "<menu> e e") 'wrap-html-tag)
  (local-set-key (kbd "<menu> e f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<menu> e i") 'insert-tag)
  (local-set-key (kbd "<menu> e k") 'htmlize-keyboard-shortcut-notation)
  (local-set-key (kbd "<menu> e l 6") 'source-linkify)
  (local-set-key (kbd "<menu> e l c") 'chinese-linkify)
  (local-set-key (kbd "<menu> e l d") 'perldoc-ref-linkify)
  (local-set-key (kbd "<menu> e l e") 'emacs-ref-linkify)
  (local-set-key (kbd "<menu> e l f") 'full-size-img-linkify)
  (local-set-key (kbd "<menu> e l i") 'image-linkify)
  (local-set-key (kbd "<menu> e l j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<menu> e l l") 'listify-block)
  (local-set-key (kbd "<menu> e l p") 'php-ref-linkify)
  (local-set-key (kbd "<menu> e l t") 'word-etymology-linkify)
  (local-set-key (kbd "<menu> e l u") 'wrap-url)
  (local-set-key (kbd "<menu> e l w") 'wikipedia-linkify)
  (local-set-key (kbd "<menu> e l z") 'amazon-linkify)
  (local-set-key (kbd "<menu> e m") 'xah-make-atom-entry)
  (local-set-key (kbd "<menu> e p") 'add-paragraph-tag)
  (local-set-key (kbd "<menu> e r ,") 'replace-html-chars-to-unicode)
  (local-set-key (kbd "<menu> e r .") 'replace-html-chars-to-entities)
  (local-set-key (kbd "<menu> e r 3") 'xah-update-title)
  (local-set-key (kbd "<menu> e r 4") 'xah-update-article-timestamp)
  (local-set-key (kbd "<menu> e r k") 'emacs-to-windows-kbd-notation)
  (local-set-key (kbd "<menu> e r m") 'make-html-table)
  (local-set-key (kbd "<menu> e r q") 'curly-quotes-to-emacs-function-tag)
  (local-set-key (kbd "<menu> e r t") 'title-bracket-to-html-tag)
  (local-set-key (kbd "<menu> e u") 'xah-all-linkify)

  )
(add-hook 'html-mode-hook 'xah-html-mode-keys)
(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)
(add-hook 'nxml-mode-hook 'xah-html-mode-keys)
;; (unload-feature 'sgml-mode)
;; (remove-hook 'html-mode-hook 'xah-html-mode-keys)

(defun xah-rcirc-mode-keys ()
  "my keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<f12>") 'rcirc-insert-next-input)
  )
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(defun xah-org-mode-keys ()
  "my keybindings for `org-mode'.
For `org-mode-hook'."
  (local-set-key (kbd "<M-up>") 'org-metaup)
  (local-set-key (kbd "<M-down>") 'org-metadown)
  (local-set-key (kbd "<M-left>") 'org-metaleft)
  (local-set-key (kbd "<M-right>") 'org-metaright)
  )
(add-hook 'org-mode-hook 'xah-org-mode-keys)

(defun xah-Info-mode-keys ()
  "my keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<menu> e 6") 'xah-view-emacs-manual-in-browser)
  )
(add-hook 'Info-mode-hook 'xah-Info-mode-keys)

(defun xah-nxml-mode-keys ()
  "my keybindings for `nxml-mode'.
For `nxml-mode-hook'."
  (local-set-key (kbd "<M-up>") 'nxml-backward-up-element)
  (local-set-key (kbd "<M-down>") 'nxml-down-element)
  )
(add-hook 'nxml-mode-hook 'xah-nxml-mode-keys)

(defun xah-dired-mode-keys ()
  "Modify keymaps used by `dired'."
;;  (define-key dired-mode-map (kbd "<return>") 'dired-find-alternate-file) ; was dired-advertised-find-file
;;  (define-key dired-mode-map (kbd "^") (lambda () (interactive) (find-alternate-file ".."))) ; was dired-up-directory
  (define-key dired-mode-map (kbd "M-$") nil) ; was dired-up-directory
  (when (>= emacs-major-version 23)
;    (define-key dired-mode-map (kbd "M-s") 'isearch-forward)
;    (define-key dired-mode-map (kbd "M-S") 'isearch-backward)
    (define-key dired-mode-map (kbd "<menu> e 8") 'wdired-change-to-wdired-mode)
    )
  )

(add-hook 'dired-mode-hook 'xah-dired-mode-keys)


;;;; misc

;; (load (fullpath-relative-to-current-file "xah_emacs_keybinding_unset_keys.el"))

    ;; ("<f1> 6" lookup-all-dictionaries)
    ;; ("<f1> 7" lookup-google)
    ;; ("<f1> 8" lookup-wikipedia)
    ;; ("<f1> 9" lookup-word-definition)
    ;; ("<f1> 0" lookup-answers.com)
    ;; ("<f1> [" lookup-word-dict-org)
    ;; ("<f1> ]" lookup-wiktionary)

(global-set-key (kbd "<f1> <f2>") 'helm-mini)

;; (load (fullpath-relative-to-current-file "xah_emacs_keybinding_number_pad"))
(load (fullpath-relative-to-current-file "xah_emacs_keybinding_number_pad_number"))
;; (load (fullpath-relative-to-current-file "xah_emacs_keybinding_truly_ergonomic"))
(load (fullpath-relative-to-current-file "xah_emacs_unicode_input"))

;; remove ErgoEmacs's changing font size
(global-unset-key (kbd "C-+") )
(global-unset-key (kbd "C--") )
(global-unset-key (kbd "C-0") ) ; text-scale-normal-size
(global-unset-key (kbd "M-5") )
(global-unset-key (kbd "M-%") )


(defun toggle-menu-key ()
  "toggle the value of `w32-apps-modifier' between 'meta and 'nil"
  (interactive)
  (if (eq w32-apps-modifier 'meta)
        (progn (setq w32-apps-modifier 'nil))
      (progn (setq w32-apps-modifier 'meta) )
      ))


;; c:/Users/h3/web/ergoemacs_org/emacs/gnu_emacs_keybinding_C-x.txt

;; some idea about command categories, in context to chosing keys for them

;; • whether a command has immediate effect, no prompt. e.g. shell vs delete-matching-lines
;; • whether a command has is safe to run by mistake. e.g. whitespace-mode vs eval-buffer
;; • whether a command is frequently needed (few times a hour). e.g.

;; idea about key groups
;; all should be sequence of single keys. 2 to 3 keys. All should start with F7. And all commands should be globally useful.
;; • 2 keys vs 3 keys
;; • whether the key ends in a digit key 0 to 9. These probably should be most frequently used, or immediate effect.

;; command that are not immediate (has prompt) probably should not have a key.
