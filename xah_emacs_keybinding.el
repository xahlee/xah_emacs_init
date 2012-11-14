;; -*- coding: utf-8 -*-
;; xah's emacs keybinding.
;; • xah_emacs_keybinding.el
;; • xah_emacs_unicode_input.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_mouse_binding.el
;; • xah_emacs_alias.el
;; • xah_emacs_abbr.el
;; • 〈Dvorak Keyboard Layout〉 http://xahlee.info/comp/dvorak_keyboard_layout.html
;; • 〈ErgoEmacs Keybinding〉 http://ergoemacs.org/emacs/ergonomic_emacs_keybinding.html
;; • 〈xah_autohotkey.ahk〉 http://xahlee.org/mswin/xah_autohotkey.ahk
;; • 〈Emacs: How to define Hyper ＆ Super Keys〉 http://ergoemacs.org/emacs/emacs_hyper_super_keys.html
;; • 〈Emacs: Remapping Keys Using key-translation-map〉 http://ergoemacs.org/emacs/emacs_key-translation-map.html
;; • 〈Emacs: Add Custom Keys to Enhance Productivity〉 http://ergoemacs.org/emacs/emacs_useful_user_keybinding.html

;; Xah Lee
;; created: 2007-06.



(load (fullpath-relative-to-current-file "xah_emacs_unset_keys"))


;; generic

;; in linux, <lwindow> is Super 「s」,  while the menu key is 「<menu>」
(global-set-key (kbd "<lwindow>") 'set-mark-command)

(define-key global-map (kbd "<escape>") 'keyboard-quit)

(global-set-key (kbd "C-7") 'select-text-in-quote)
(global-set-key (kbd "C-8") 'extend-selection)


;(global-set-key (kbd "<f8>") ctl-x-map)
;; (global-set-key (kbd "<f8>") mode-specific-map)

;; (global-set-key (kbd "<f8>") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'set-mark-command)
;; (global-set-key (kbd "M-SPC") 'beep)



;; c:/Users/h3/web/ergoemacs_org/emacs/gnu_emacs_keybinding_C-x.txt

;(global-set-key (kbd "<f7> 2") 'shrink-whitespaces)
;(global-set-key (kbd "<f7> c") 'compact-uncompact-block)

;; immediate effect on text, non-
;; mode vs non-mode
;; very frequently needed, non-

(global-set-key (kbd "<f7> 1") 'xah-fix-number-items-block)

(global-set-key (kbd "<f7> 7") 'xah-open-file-at-cursor)
(global-set-key (kbd "<f7> 8") 'dired-jump)
(global-set-key (kbd "<f7> <delete>") 'delete-current-file)

(global-set-key (kbd "<f7> f") 'copy-file-path)
(global-set-key (kbd "<f7> s") 'ispell-word)
(global-set-key (kbd "<f7> t") 'title-case-string-region-or-line)
(global-set-key (kbd "<f7> w") 'delete-trailing-whitespace)

(global-set-key (kbd "<f7> \\") 'escape-quotes)
(global-set-key (kbd "<f7> '") 'replace-straight-quotes)
(global-set-key (kbd "<f7> `") 'make-backup)

(global-set-key (kbd "<f7> [") 'remove-square-brackets)

(global-set-key (kbd "<f7> <f5> 6") 'run-current-file)
; (global-set-key (kbd "<f7> <f5> 6") 'eval-buffer)
(global-set-key (kbd "<f7> <f5> 7") 'eval-last-sexp)
(global-set-key (kbd "<f7> <f5> 8") 'eval-region)
(global-set-key (kbd "<f7> <f5> 4") 'eval-expression)
(global-set-key (kbd "<f7> <f5> 3") 'eval-defun)

;; commands
(global-set-key (kbd "<f7> <f6> 5") 'xah-find-text-regex)
(global-set-key (kbd "<f7> <f6> 6") 'xah-find-text)
(global-set-key (kbd "<f7> <f6> 7") 'shell-command)
(global-set-key (kbd "<f7> <f6> 8") 'list-matching-lines)

(global-set-key (kbd "<f7> <f6> 9") 'delete-matching-lines)
(global-set-key (kbd "<f7> <f6> 0") 'delete-non-matching-lines)

(global-set-key (kbd "<f7> <f7> 3") 'flyspell-buffer)
(global-set-key (kbd "<f7> <f7> 1") 'global-linum-mode)
(global-set-key (kbd "<f7> <f7> 2") 'rainbow-mode)
(global-set-key (kbd "<f7> <f7> 5") visual-line-mode)
(global-set-key (kbd "<f7> <f7> 6") 'whitespace-mode)
(global-set-key (kbd "<f7> <f7> 7") 'shell)
(global-set-key (kbd "<f7> <f7> 8") 'calc)
(global-set-key (kbd "<f7> <f7> 9") 'calendar)

(global-set-key (kbd "<f7> <f7> h") 'xah-html-mode)
(global-set-key (kbd "<f7> <f7> e") 'emacs-lisp-mode)
(global-set-key (kbd "<f7> <f7> o") 'org-mode)
(global-set-key (kbd "<f7> <f7> t") 'text-mode)

(global-set-key (kbd "<f7> i 1") 'insert-random-number)
(global-set-key (kbd "<f7> i 2") 'insert-random-string)
(global-set-key (kbd "<f7> i 3") 'insert-random-hex)
(global-set-key (kbd "<f7> i 4") 'insert-random-uuid)
(global-set-key (kbd "<f7> i d") 'insert-date)
(global-set-key (kbd "<f7> i t") 'insert-date-time)


(global-set-key (kbd "C-3") 'copy-to-register-1)
(global-set-key (kbd "C-4") 'paste-from-register-1)


;; special keys

(global-set-key (kbd "<insert>") 'switch-to-next-frame)


;; NUMBERIC KEYPAD. nice number pad conveniences as extra function keys

(global-set-key (kbd "<kp-subtract>") 'close-current-buffer)
(global-set-key (kbd "<kp-divide>") 'previous-user-buffer)
(global-set-key (kbd "<kp-multiply>") 'next-user-buffer)

(global-set-key (kbd "<kp-decimal>") 'other-window)
(global-set-key (kbd "<kp-0>") 'delete-window)
(global-set-key (kbd "<kp-1>") 'delete-other-windows)
(global-set-key (kbd "<kp-2>") 'split-window-vertically)
(global-set-key (kbd "<kp-3>") 'xah-open-file-at-cursor)
(global-set-key (kbd "<C-kp-3>") 'xah-open-file-from-clipboard)

(global-set-key (kbd "<C-kp-5>") 'cycle-font-2)
(global-set-key (kbd "<C-kp-6>") 'cycle-font-forward)
(global-set-key (kbd "<C-kp-4>") 'cycle-font-backward)

(global-set-key (kbd "<kp-5>") 'save-buffer)
(global-set-key (kbd "<kp-6>") 'repeat-complex-command)

(global-set-key (kbd "<kp-7> <kp-1>") (lambda () "" (interactive) (find-file "~/web/xahlee_info/js/blog.html")))
(global-set-key (kbd "<kp-7> <kp-2>") (lambda () "" (interactive) (find-file "~/web/xahlee_info/comp/blog.html")))
(global-set-key (kbd "<kp-7> <kp-3>") (lambda () "" (interactive) (find-file "~/web/ergoemacs_org/emacs/blog.html")))
(global-set-key (kbd "<kp-7> <kp-4>") (lambda () "" (interactive) (find-file "~/Dropbox/twitter tweets.txt")))
(global-set-key (kbd "<kp-7> <kp-5>") (lambda () "" (interactive) (find-file "~/git/xah_emacs_init/xah_emacs_keybinding.el")))

(global-set-key (kbd "<kp-7> <kp-6>") 'open-in-desktop)
(global-set-key (kbd "<kp-7> <kp-7>") 'bookmark-bmenu-list)
(global-set-key (kbd "<kp-7> <kp-8>") 'ibuffer)
(global-set-key (kbd "<kp-7> <kp-9>") 'recentf-open-files)

(global-set-key (kbd "<kp-9>") 'isearch-forward)
(global-set-key (kbd "<C-kp-9>") 'isearch-backward)

(defun xah-isearch-hook ()
  "Hook for `isearch-mode-hook'"
  (define-key isearch-mode-map (kbd "<C-kp-9>") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<kp-9>") 'isearch-repeat-forward)
  )
(add-hook 'isearch-mode-hook 'xah-isearch-hook)

(global-set-key (kbd "<C-kp-0>") 'tags-loop-continue)



(global-set-key (kbd "<C-M-prior>") 'backward-page) ; gnu emacs C-x [
(global-set-key (kbd "<C-M-next>") 'forward-page)   ; gnu emacs C-x ]



(global-set-key (kbd "M-\"") 'xah-compact-uncompact-block)

(global-set-key (kbd "M-2") 'cycle-hyphen-underscore-space)


;; mode-specific

(when (boundp 'ergoemacs-keymap)
(define-key ergoemacs-keymap (kbd "M-m") 'hippie-expand)
;; (define-key ergoemacs-keymap (kbd "M-G") nil) ; was backward-paragraph
;; (define-key ergoemacs-keymap (kbd "M-R") nil) ; was forward-paragraph

(define-key ergoemacs-keymap (kbd "M-;") nil) ; undo
(define-key ergoemacs-keymap (kbd "M-:") nil) ; redo, undo-tree-redo
(define-key ergoemacs-keymap (kbd "M-q") nil) ; cut
(define-key ergoemacs-keymap (kbd "M-Q") nil) ; cut-all
;(define-key ergoemacs-keymap (kbd "M-j") nil) ; copy
;(define-key ergoemacs-keymap (kbd "M-k") nil) ; paste, yank

;; (define-key ergoemacs-keymap (kbd "M-o") nil) ; move-cursor-next-pane

;(define-key ergoemacs-keymap (kbd "M-,") nil) ; shrink-whitespaces
;(define-key ergoemacs-keymap (kbd "M-'") nil) ; compact-uncompact-block
)

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
  (local-set-key (kbd "<f6> <f5> f") 'full-size-img-linkify)

  (local-set-key (kbd "<f6> <f5> u") 'wrap-url)

  (local-set-key (kbd "<kp-8>") 'browse-url-of-buffer)
  (local-set-key (kbd "<C-kp-8>") 'xah-browse-url-of-buffer)

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

  (local-set-key (kbd "<f6> 7") 'htmlize-text)
  (local-set-key (kbd "<f6> 8") 'dehtmlize-pre-block)
  )
(add-hook 'html-mode-hook 'xah-html-mode-keys)
(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)
(add-hook 'nxml-mode-hook 'xah-html-mode-keys)
;; (unload-feature 'sgml-mode)
;; (remove-hook 'html-mode-hook 'xah-html-mode-keys)

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
  (local-set-key (kbd "<kp-8>") 'xah-view-emacs-manual-in-browser)
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

    (define-key dired-mode-map (kbd "<f6> 6") '2png)
    (define-key dired-mode-map (kbd "<f6> 5") '2jpg)
    (define-key dired-mode-map (kbd "<f6> 4") 'scale-image)
    )
  )

(add-hook 'dired-mode-hook 'xah-dired-mode-keys)


;;;; misc

;; 2011-06-02 gnu emacs on mac fix. On GNU Emacs 23.1.1 compiled for Mac, the Del key in emacs is reported as <kp-delete>, and is bound to delete-backward-char. Idiotic.
(when (string-equal system-type "darwin")
  (global-set-key (kbd "<kp-delete>") 'delete-char)
)


;; (defun open-in-external-app-some ()
;;   "for find-file-hook, open image files in external app.
;; Or PDF files, etc."
;;   (interactive)
;;   (let ( (fSuffix (file-name-extension (buffer-file-name))) )
;;     ;; fSuffix may be nil
;;     (if fSuffix
;;         (when (string-match "jpg\\|jpeg\\|png\\|gif\\|pdf" fSuffix)
;;           (open-in-external-app) )
;;       nil ) ) )

;; (add-hook 'find-file-hook 'open-in-external-app-some)
