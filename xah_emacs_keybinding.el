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

;; in linux, <lwindow> is Super 「s」,  while the menu key is 「<menu>」
(cond
 ((string-equal system-type "windows-nt")
  (global-set-key (kbd "<lwindow>") 'smex)
  )
 ((string-equal system-type "darwin")
  t )
 ((string-equal system-type "gnu/linux")
  t ) )

(global-set-key (kbd "C-7") 'ergoemacs-select-text-in-quote)
(global-set-key (kbd "C-8") 'ergoemacs-extend-selection)

;(global-set-key (kbd "<f9>") 'ergoemacs-switch-to-next-frame)
;(global-set-key (kbd "<f10>") 'ergoemacs-close-current-buffer)
(global-set-key (kbd "<f11>") 'ergoemacs-next-user-buffer)
(global-set-key (kbd "<f12>") 'ergoemacs-previous-user-buffer)

(global-set-key (kbd "<f8>") ctl-x-map)
;; (global-set-key (kbd "<f8>") mode-specific-map)

;; (global-set-key (kbd "<f8>") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'beep)



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


(global-set-key (kbd "M-m") 'hippie-expand)


(global-set-key (kbd "<f10>") 'ergoemacs-close-current-buffer)

(define-prefix-command 'xah-keymap)
(global-set-key (kbd "<lwindow>") 'xah-keymap)

(global-set-key (kbd "<lwindow> <lwindow>") 'smex)

;; e is for unicode insert
;; t is mode-specific
(global-set-key (kbd "<lwindow> '") 'replace-straight-quotes)
(global-set-key (kbd "<lwindow> ,") 'remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<lwindow> .") 'convert-english-chinese-punctuation)
(global-set-key (kbd "<lwindow> 3") 'query-replace)
(global-set-key (kbd "<lwindow> 4") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<lwindow> 5") 'recentf-open-files)
(global-set-key (kbd "<lwindow> 6") 'run-current-file) ;; immediate, but dangerous
(global-set-key (kbd "<lwindow> 7") 'xah-open-file-at-cursor)
(global-set-key (kbd "<lwindow> 8") 'dired-jump)
(global-set-key (kbd "<lwindow> 9") 'ispell-word)
(global-set-key (kbd "<lwindow> <delete>") 'delete-current-file)
(global-set-key (kbd "<lwindow> =") 'repeat-complex-command)
(global-set-key (kbd "<lwindow> [") 'remove-square-brackets)
(global-set-key (kbd "<lwindow> \\") 'escape-quotes)
(global-set-key (kbd "<lwindow> `") 'make-backup)
(global-set-key (kbd "<lwindow> b") 'flyspell-buffer)
(global-set-key (kbd "<lwindow> c") 'copy-to-register-1)
(global-set-key (kbd "<lwindow> d") 'ergoemacs-open-in-desktop)
(global-set-key (kbd "<lwindow> f") 'copy-file-path)
(global-set-key (kbd "<lwindow> i d") 'insert-date)
(global-set-key (kbd "<lwindow> i r h") 'insert-random-hex)
(global-set-key (kbd "<lwindow> i r n") 'insert-random-number)
(global-set-key (kbd "<lwindow> i r s") 'insert-random-string)
(global-set-key (kbd "<lwindow> i r u") 'insert-random-uuid)
(global-set-key (kbd "<lwindow> i t") 'insert-date-time)
(global-set-key (kbd "<lwindow> m c") 'calc)
(global-set-key (kbd "<lwindow> m e") 'emacs-lisp-mode)
(global-set-key (kbd "<lwindow> m h") 'xah-html-mode)
(global-set-key (kbd "<lwindow> m o") 'org-mode)
(global-set-key (kbd "<lwindow> m s") 'shell)
(global-set-key (kbd "<lwindow> m t") 'text-mode)
(global-set-key (kbd "<lwindow> m v") 'visual-line-mode)
(global-set-key (kbd "<lwindow> m w") 'whitespace-mode)
(global-set-key (kbd "<lwindow> o") 'xah-open-file-fast)
(global-set-key (kbd "<lwindow> p") 'paste-from-register-1)
(global-set-key (kbd "<lwindow> r f") 'xah-find-text)
(global-set-key (kbd "<lwindow> r r") 'xah-find-replace-text)
(global-set-key (kbd "<lwindow> r q") 'query-replace-regexp)
(global-set-key (kbd "<lwindow> s") 'shell-command)
(global-set-key (kbd "<lwindow> w") 'delete-trailing-whitespace)
(global-set-key (kbd "<lwindow> x") 'xah-cite)
(global-set-key (kbd "<lwindow> z") 'title-case-string-region-or-line)



(global-set-key (kbd "<f7>") 'xah-keymap)

(global-set-key (kbd "<f7> <f7>") 'smex)

;; y is for unicode insert
;; t is mode-specific
(global-set-key (kbd "<f7> '") 'replace-straight-quotes)
(global-set-key (kbd "<f7> ,") 'remove-punctuation-trailing-redundant-space)
(global-set-key (kbd "<f7> .") 'convert-english-chinese-punctuation)
(global-set-key (kbd "<f7> 3") 'query-replace)
(global-set-key (kbd "<f7> 4") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<f7> 5") 'recentf-open-files)
(global-set-key (kbd "<f7> 6") 'run-current-file) ;; immediate, but dangerous
(global-set-key (kbd "<f7> 7") 'xah-open-file-at-cursor)
(global-set-key (kbd "<f7> 8") 'dired-jump)
(global-set-key (kbd "<f7> 9") 'ispell-word)
(global-set-key (kbd "<f7> <delete>") 'delete-current-file)
(global-set-key (kbd "<f7> =") 'repeat-complex-command)
(global-set-key (kbd "<f7> [") 'remove-square-brackets)
(global-set-key (kbd "<f7> \\") 'escape-quotes)
(global-set-key (kbd "<f7> `") 'make-backup)
(global-set-key (kbd "<f7> b") 'flyspell-buffer)
(global-set-key (kbd "<f7> c") 'copy-to-register-1)
(global-set-key (kbd "<f7> d") 'ergoemacs-open-in-desktop)
(global-set-key (kbd "<f7> f") 'copy-file-path)
(global-set-key (kbd "<f7> i d") 'insert-date)
(global-set-key (kbd "<f7> i r h") 'insert-random-hex)
(global-set-key (kbd "<f7> i r n") 'insert-random-number)
(global-set-key (kbd "<f7> i r s") 'insert-random-string)
(global-set-key (kbd "<f7> i r u") 'insert-random-uuid)
(global-set-key (kbd "<f7> i t") 'insert-date-time)
(global-set-key (kbd "<f7> m c") 'calc)
(global-set-key (kbd "<f7> m e") 'emacs-lisp-mode)
(global-set-key (kbd "<f7> m h") 'xah-html-mode)
(global-set-key (kbd "<f7> m o") 'org-mode)
(global-set-key (kbd "<f7> m s") 'shell)
(global-set-key (kbd "<f7> m t") 'text-mode)
(global-set-key (kbd "<f7> m v") 'visual-line-mode)
(global-set-key (kbd "<f7> m w") 'whitespace-mode)
(global-set-key (kbd "<f7> o") 'xah-open-file-fast)
(global-set-key (kbd "<f7> p") 'paste-from-register-1)
(global-set-key (kbd "<f7> r f") 'xah-find-text)
(global-set-key (kbd "<f7> r r") 'xah-find-replace-text)
(global-set-key (kbd "<f7> r q") 'query-replace-regexp)
(global-set-key (kbd "<f7> s") 'shell-command)
(global-set-key (kbd "<f7> w") 'delete-trailing-whitespace)
(global-set-key (kbd "<f7> x") 'xah-cite)
(global-set-key (kbd "<f7> z") 'title-case-string-region-or-line)


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

  (local-set-key (kbd "<lwindow> t &") 'replace-html-characters)

  (local-set-key (kbd "<lwindow> t 0") 'dehtmlize-text)

  (local-set-key (kbd "<lwindow> t 5") 'mark-unicode)
  (local-set-key (kbd "<lwindow> t 6") 'browse-url-of-buffer)
  (local-set-key (kbd "<lwindow> t 7") 'htmlize-or-dehtmlize-pre-block)
  (local-set-key (kbd "<lwindow> t 8") 'get-pre-block-make-new-file)
  (local-set-key (kbd "<lwindow> t 9") 'code-bracket-to-html-tag)
  (local-set-key (kbd "<lwindow> t <delete>") 'sgml-delete-tag)
  (local-set-key (kbd "<lwindow> t <left>") 'sgml-skip-tag-backward)
  (local-set-key (kbd "<lwindow> t <right>") 'sgml-skip-tag-forward)
  (local-set-key (kbd "<lwindow> t C-6") 'xah-browse-url-of-buffer)
  (local-set-key (kbd "<lwindow> t a") 'xah-annotate)
  (local-set-key (kbd "<lwindow> t b") 'make-blogger-entry)
  (local-set-key (kbd "<lwindow> t c") 'make-citation)
  (local-set-key (kbd "<lwindow> t d") 'insert-date-tag)
  (local-set-key (kbd "<lwindow> t e") 'wrap-html-tag)
  (local-set-key (kbd "<lwindow> t f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<lwindow> t i") 'insert-tag)
  (local-set-key (kbd "<lwindow> t k") 'htmlize-keyboard-shortcut-notation)
  (local-set-key (kbd "<lwindow> t l 6") 'source-linkify)
  (local-set-key (kbd "<lwindow> t l c") 'chinese-linkify)
  (local-set-key (kbd "<lwindow> t l d") 'perldoc-ref-linkify)
  (local-set-key (kbd "<lwindow> t l e") 'emacs-ref-linkify)
  (local-set-key (kbd "<lwindow> t l f") 'full-size-img-linkify)
  (local-set-key (kbd "<lwindow> t l i") 'image-linkify)
  (local-set-key (kbd "<lwindow> t l j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<lwindow> t l l") 'listify-block)
  (local-set-key (kbd "<lwindow> t l p") 'php-ref-linkify)
  (local-set-key (kbd "<lwindow> t l t") 'word-etymology-linkify)
  (local-set-key (kbd "<lwindow> t l u") 'wrap-url)
  (local-set-key (kbd "<lwindow> t l w") 'wikipedia-linkify)
  (local-set-key (kbd "<lwindow> t l z") 'amazon-linkify)
  (local-set-key (kbd "<lwindow> t m") 'xah-make-atom-entry)
  (local-set-key (kbd "<lwindow> t p") 'add-paragraph-tag)

  (local-set-key (kbd "<lwindow> t r 3") 'xah-update-title)
  (local-set-key (kbd "<lwindow> t r 4") 'xah-update-article-timestamp)
  (local-set-key (kbd "<lwindow> t r t") 'title-bracket-to-html-tag)
  (local-set-key (kbd "<lwindow> t r k") 'emacs-to-windows-kbd-notation)
  (local-set-key (kbd "<lwindow> t r q") 'curly-quotes-to-emacs-function-tag)
  (local-set-key (kbd "<lwindow> t r m") 'make-html-table)

  (local-set-key (kbd "<lwindow> t u") 'xah-all-linkify)
  (local-set-key (kbd "<lwindow> t ＆") 'replace-html-characters-to-unicode)

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
  (local-set-key (kbd "<lwindow> t 6") 'xah-view-emacs-manual-in-browser)
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
    (define-key dired-mode-map (kbd "<lwindow> t 8") 'wdired-change-to-wdired-mode)
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
(load (fullpath-relative-to-current-file "xah_emacs_keybinding_truly_ergonomic"))
(load (fullpath-relative-to-current-file "xah_emacs_unicode_input"))

;; remove ErgoEmacs's changing font size
(global-unset-key (kbd "C-+") )
(global-unset-key (kbd "C--") )
(global-unset-key (kbd "C-0") ) ; text-scale-normal-size
(global-unset-key (kbd "M-5") )
(global-unset-key (kbd "M-%") )
