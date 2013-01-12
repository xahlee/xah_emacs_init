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

(global-set-key (kbd "<f7> 3") 'repeat-complex-command)
(global-set-key (kbd "<f7> 4") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<f7> 5") 'recentf-open-files)

(global-set-key (kbd "<f7> 6") 'ergoemacs-open-in-desktop)
(global-set-key (kbd "<f7> 7") 'xah-open-file-at-cursor)
(global-set-key (kbd "<f7> 8") 'dired-jump)
(global-set-key (kbd "<f7> 9") 'set-input-method-to-chinese)
(global-set-key (kbd "<f7> <delete>") 'delete-current-file)

(global-set-key (kbd "<f7> o") 'xah-open-file-fast)

(global-set-key (kbd "<f7> f") 'copy-file-path)
(global-set-key (kbd "<f7> s") 'ispell-word)
(global-set-key (kbd "<f7> t") 'title-case-string-region-or-line)
(global-set-key (kbd "<f7> w") 'delete-trailing-whitespace)

(global-set-key (kbd "<f7> \\") 'escape-quotes)
(global-set-key (kbd "<f7> '") 'replace-straight-quotes)
(global-set-key (kbd "<f7> `") 'make-backup)

(global-set-key (kbd "<f7> [") 'remove-square-brackets)

;; all immediate, safe.
(global-set-key (kbd "<f7> <f7> 1") 'rainbow-mode)
(global-set-key (kbd "<f7> <f7> 2") 'visual-line-mode)
(global-set-key (kbd "<f7> <f7> 3") 'global-linum-mode)
(global-set-key (kbd "<f7> <f7> 4") 'whitespace-mode)
(global-set-key (kbd "<f7> <f7> 5") 'flyspell-buffer)
(global-set-key (kbd "<f7> <f7> 7") 'calc)
(global-set-key (kbd "<f7> <f7> 8") 'shell)
(global-set-key (kbd "<f7> <f7> 9") 'calendar)

;; all immediate, but dangerous
(global-set-key (kbd "<f7> <f5> 5") 'eval-last-sexp)
(global-set-key (kbd "<f7> <f5> 6") 'run-current-file)
(global-set-key (kbd "<f7> <f5> 7") 'eval-buffer)
(global-set-key (kbd "<f7> <f5> 8") 'eval-defun)
(global-set-key (kbd "<f7> <f5> 9") 'eval-region)

(global-set-key (kbd "<f7> <f6> 0") 'delete-non-matching-lines)
(global-set-key (kbd "<f7> <f6> 5") 'xah-find-text-regex)
(global-set-key (kbd "<f7> <f6> 6") 'xah-find-text)
(global-set-key (kbd "<f7> <f6> 7") 'shell-command)
(global-set-key (kbd "<f7> <f6> 8") 'list-matching-lines)
(global-set-key (kbd "<f7> <f6> 9") 'delete-matching-lines)

(global-set-key (kbd "<f7> <f4> 3") 'xah-html-mode)
(global-set-key (kbd "<f7> <f4> 4") 'html-mode)
(global-set-key (kbd "<f7> <f4> 5") 'emacs-lisp-mode)
(global-set-key (kbd "<f7> <f4> 6") 'org-mode)
(global-set-key (kbd "<f7> <f4> 7") 'text-mode)
(global-set-key (kbd "<f7> <f4> 8") 'shell-script-mode)

(global-set-key (kbd "<f7> i 1") 'insert-random-number)
(global-set-key (kbd "<f7> i 2") 'insert-random-string)
(global-set-key (kbd "<f7> i 3") 'insert-random-hex)
(global-set-key (kbd "<f7> i 4") 'insert-random-uuid)
(global-set-key (kbd "<f7> i d") 'insert-date)
(global-set-key (kbd "<f7> i t") 'insert-date-time)

(define-prefix-command 'xah-win-keymap)
(global-set-key (kbd "<lwindow>") 'xah-win-keymap)

(global-set-key (kbd "<lwindow> <lwindow>") 'smex)

(global-set-key (kbd "<lwindow> '") 'replace-straight-quotes)
(global-set-key (kbd "<lwindow> 3") 'repeat-complex-command)
(global-set-key (kbd "<lwindow> 4") 'xah-open-file-from-clipboard)
(global-set-key (kbd "<lwindow> 5") 'recentf-open-files)
(global-set-key (kbd "<lwindow> 6") 'run-current-file) ;; immediate, but dangerous
(global-set-key (kbd "<lwindow> 7") 'xah-open-file-at-cursor)
(global-set-key (kbd "<lwindow> 8") 'dired-jump)
(global-set-key (kbd "<lwindow> <delete>") 'delete-current-file)
(global-set-key (kbd "<lwindow> <f6> 7") 'shell-command)
(global-set-key (kbd "<lwindow> [") 'remove-square-brackets)
(global-set-key (kbd "<lwindow> \\") 'escape-quotes)
(global-set-key (kbd "<lwindow> `") 'make-backup)
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
(global-set-key (kbd "<lwindow> m f b") 'flyspell-buffer)
(global-set-key (kbd "<lwindow> m s") 'shell)
(global-set-key (kbd "<lwindow> m v") 'visual-line-mode)
(global-set-key (kbd "<lwindow> m w") 'whitespace-mode)
(global-set-key (kbd "<lwindow> o") 'xah-open-file-fast)
(global-set-key (kbd "<lwindow> p") 'paste-from-register-1)
(global-set-key (kbd "<lwindow> s") 'ispell-word)
(global-set-key (kbd "<lwindow> t") 'title-case-string-region-or-line)
(global-set-key (kbd "<lwindow> w") 'delete-trailing-whitespace)



;; special keys

(global-set-key (kbd "<insert>") 'ergoemacs-switch-to-next-frame)




;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; Ctrl+Alt+PageDown



(global-set-key (kbd "M-\"") 'xah-compact-uncompact-block)

(global-set-key (kbd "M-2") 'cycle-hyphen-underscore-space)


;; mode-specific

;; (when (boundp 'ergoemacs-keymap)
;; (define-key ergoemacs-keymap (kbd "M-m") 'hippie-expand)
;; ;; (define-key ergoemacs-keymap (kbd "M-G") nil) ; was backward-paragraph
;; ;; (define-key ergoemacs-keymap (kbd "M-R") nil) ; was forward-paragraph

;; (define-key ergoemacs-keymap (kbd "M-;") nil) ; undo
;; (define-key ergoemacs-keymap (kbd "M-:") nil) ; redo, undo-tree-redo
;; (define-key ergoemacs-keymap (kbd "M-q") nil) ; cut
;; (define-key ergoemacs-keymap (kbd "M-Q") nil) ; cut-all
;; ;(define-key ergoemacs-keymap (kbd "M-j") nil) ; copy
;; ;(define-key ergoemacs-keymap (kbd "M-k") nil) ; paste, yank

;; ;; (define-key ergoemacs-keymap (kbd "M-o") nil) ; move-cursor-next-pane

;; ;(define-key ergoemacs-keymap (kbd "M-,") nil) ; shrink-whitespaces
;; ;(define-key ergoemacs-keymap (kbd "M-'") nil) ; compact-uncompact-block
;; )

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  (local-set-key (kbd "<f6> p") 'add-paragraph-tag)
  (local-set-key (kbd "<f6> u") 'xah-all-linkify)
  (local-set-key (kbd "<f6> e") 'wrap-html-tag)
  (local-set-key (kbd "<f6> i") 'insert-tag)
  (local-set-key (kbd "<f6> a") 'xah-annotate)
  (local-set-key (kbd "<f6> d") 'insert-date-tag)

  (local-set-key (kbd "<f6> <f5> 6") 'source-linkify)
  (local-set-key (kbd "<f6> <f5> e") 'emacs-ref-linkify)
  (local-set-key (kbd "<f6> <f5> d") 'perldoc-ref-linkify)
  (local-set-key (kbd "<f6> <f5> p") 'php-ref-linkify)
  (local-set-key (kbd "<f6> <f5> t") 'word-etymology-linkify)
  (local-set-key (kbd "<f6> <f5> c") 'chinese-linkify)
  (local-set-key (kbd "<f6> <f5> w") 'wikipedia-linkify)
  (local-set-key (kbd "<f6> <f5> z") 'amazon-linkify)
  (local-set-key (kbd "<f6> <f5> i") 'image-linkify)
  (local-set-key (kbd "<f6> <f5> j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<f6> <f5> f") 'full-size-img-linkify)

  (local-set-key (kbd "<f6> <f5> u") 'wrap-url)

  (local-set-key (kbd "<f6> <delete>") 'sgml-delete-tag)
  (local-set-key (kbd "<f6> <left>") 'sgml-skip-tag-backward)
  (local-set-key (kbd "<f6> <right>") 'sgml-skip-tag-forward)

  (local-set-key (kbd "<f6> l") 'listify-block)
  (local-set-key (kbd "<f6> k") 'htmlize-keyboard-shortcut-notation)
  (local-set-key (kbd "<f6> c") 'make-citation)
  (local-set-key (kbd "<f6> b") 'make-blogger-entry)
  (local-set-key (kbd "<f6> m") 'xah-make-atom-entry)
  (local-set-key (kbd "<f6> t") 'make-html-table)
  (local-set-key (kbd "<f6> f") 'xah-copy-url-current-file)

  (local-set-key (kbd "<f6> ＆") 'replace-html-characters-to-unicode)
  (local-set-key (kbd "<f6> &") 'replace-html-characters)

  (local-set-key (kbd "<f6> 1") 'curly-quotes-to-emacs-function-tag)
  (local-set-key (kbd "<f6> 2") 'title-bracket-to-html-tag)
  (local-set-key (kbd "<f6> 3") 'emacs-to-windows-kbd-notation)
  (local-set-key (kbd "<f6> 5") 'mark-unicode)

  (local-set-key (kbd "<f6> 6") 'browse-url-of-buffer)
  (local-set-key (kbd "<f6> C-6") 'xah-browse-url-of-buffer)

  (local-set-key (kbd "<f6> 7") 'htmlize-or-dehtmlize-pre-block)
  (local-set-key (kbd "<f6> 0") 'dehtmlize-text)
  (local-set-key (kbd "<f6> 8") 'get-pre-block-make-new-file)
  (local-set-key (kbd "<f6> 9") 'code-bracket-to-html-tag)

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
  (local-set-key (kbd "<f6> 6") 'xah-view-emacs-manual-in-browser)
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
    (define-key dired-mode-map (kbd "<f6> 8") 'wdired-change-to-wdired-mode)
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
