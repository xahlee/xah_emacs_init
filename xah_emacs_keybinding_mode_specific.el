;;-*- coding: utf-8 -*-
;; 2013-09-02

(progn
  (require 'dired )
  (when (>= emacs-major-version 23)
    (define-key dired-mode-map (kbd "<delete> h") 'dired-hide-details-mode)
    (define-key dired-mode-map (kbd "`") 'dired-hide-details-mode)
    (define-key dired-mode-map (kbd "6") 'dired-up-directory)
    ))

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."

  (define-key xah-html-keymap (kbd "<f5>") 'xah-browse-url-of-buffer)

  (define-key xah-html-single-keys-keymap (kbd "1") 'xah-html-get-precode-make-new-file)
  (define-key xah-html-single-keys-keymap (kbd "2") 'xah-html-toggle-syntax-coloring-markup)

  (define-key xah-html-single-keys-keymap (kbd "e") 'xah-make-atom-entry)
  (define-key xah-html-single-keys-keymap (kbd "h") 'xah-all-linkify)
  (define-key xah-html-single-keys-keymap (kbd "u") 'xahsite-update-article-timestamp)
  (define-key xah-html-single-keys-keymap (kbd "n") 'xah-add-reference-span-tag)
  (define-key xah-html-single-keys-keymap (kbd "i") 'xah-html-insert-date-tag)
  (define-key xah-html-single-keys-keymap (kbd "f") 'xah-html-image-linkify)

  (define-key xah-html-single-keys-keymap (kbd "SPC b") 'xah-make-blogger-entry)
  (define-key xah-html-single-keys-keymap (kbd "SPC c") 'xah-angle-brackets-to-html)
  (define-key xah-html-single-keys-keymap (kbd "SPC p") 'xah-copy-url-current-file)
  (define-key xah-html-single-keys-keymap (kbd "SPC r d") 'xah-html-perldoc-ref-linkify)
  (define-key xah-html-single-keys-keymap (kbd "SPC r e") 'xah-html-emacs-ref-linkify)
  (define-key xah-html-single-keys-keymap (kbd "SPC r f") 'xah-html-full-size-img-linkify)
  (define-key xah-html-single-keys-keymap (kbd "SPC r g") 'xah-clojure-word-ref-linkify)
  (define-key xah-html-single-keys-keymap (kbd "SPC r j") 'xah-image-file-to-html-figure-tag)
  (define-key xah-html-single-keys-keymap (kbd "SPC r p") 'xah-html-php-ref-linkify)
  (define-key xah-html-single-keys-keymap (kbd "SPC r r") 'xah-add-to-related-links)
  (define-key xah-html-single-keys-keymap (kbd "SPC r z") 'xah-amazon-linkify)

  (define-key xah-html-single-keys-keymap (kbd "w a") 'xah-words-annotate)
  (define-key xah-html-single-keys-keymap (kbd "w e") 'xah-words-bold-word)
  (define-key xah-html-single-keys-keymap (kbd "w c") 'xah-words-chinese-linkify)
  (define-key xah-html-single-keys-keymap (kbd "w m") 'xah-words-move-word-to-page)
  (define-key xah-html-single-keys-keymap (kbd "w t") 'xah-words-word-etymology-linkify)

  (define-key xah-html-single-keys-keymap (kbd "w n") 'xah-words-new-word-entry )
  (define-key xah-html-single-keys-keymap (kbd "w i") 'xah-words-insert-word-entry )
  (define-key xah-html-single-keys-keymap (kbd "w d") 'xah-words-add-definition )
  (define-key xah-html-single-keys-keymap (kbd "w s") 'xah-words-add-source )
  (define-key xah-html-single-keys-keymap (kbd "w c") 'xah-words-add-comment )
  (define-key xah-html-single-keys-keymap (kbd "w g") 'xah-words-search-next-unbold )
  (define-key xah-html-single-keys-keymap (kbd "w p") 'xah-words-query-find-then-bold )
  (define-key xah-html-single-keys-keymap (kbd "w u") 'xah-words-find-word-usage )

  (define-key xah-html-single-keys-keymap (kbd "SPC z a") 'xah-html-insert-keywords-tag)
  (define-key xah-html-single-keys-keymap (kbd "SPC z b") 'xah-html-insert-lyrics-header)
  (define-key xah-html-single-keys-keymap (kbd "SPC z c") 'xah-html-insert-lyrics-table)
  (define-key xah-html-single-keys-keymap (kbd "SPC z d") 'xah-html-insert-screen-filler)
  (define-key xah-html-single-keys-keymap (kbd "SPC z f") 'xah-html-insert-midi)

  )

(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)

(defun xah-css-mode-setup ()
  "Modify keymaps used by `xah-css-mode'."
  (require 'xah-css-mode)
  (define-key xah-css-single-keys-map (kbd "s") 'xah-sync-css)
  )
(add-hook 'xah-css-mode-hook 'xah-css-mode-setup)

(defun xah-org-mode-setup ()
  "Modify keymaps used by `org-mode'."
  (local-set-key (kbd "<C-tab>") 'xah-next-user-buffer))
(add-hook 'org-mode-hook 'xah-org-mode-setup)

(defun xah-racket-mode-setup ()
  "for 'racket-mode-hook'"
  (local-set-key (kbd "C-c C-h") 'racket-describe)
)
(add-hook 'racket-mode-hook 'xah-racket-mode-setup)

(defun xah-go-mode-setup ()
  "for `go-mode'"
  (local-set-key (kbd "<delete>") nil)
  (local-set-key (kbd "<delete> <delete>") 'gofmt)
)
(add-hook 'go-mode-hook 'xah-go-mode-setup)



(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<M-f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<M-f12>") 'rcirc-insert-next-input))
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(setq rcirc-default-nick "mid2")



(defun xah-eval-defun ()
  "like `eval-defun' but doesn't need proper indentation for it to work.
Still, the code isn't 100% correct.
"
  (interactive)
  (save-excursion
    (search-backward "(defun")
    ;;    (mark-sexp)
    ;;    (eval-region (region-beginning) (region-end))
    (forward-sexp)
    (call-interactively 'eval-last-sexp)))
