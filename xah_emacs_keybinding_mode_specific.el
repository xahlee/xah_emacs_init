;;-*- coding: utf-8 -*-
;; 2013-09-02

(progn
  (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-F") 'isearch-repeat-backward)
;  (define-key isearch-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
  (define-key isearch-mode-map (kbd "<next>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<prior>") 'isearch-repeat-backward)
  )

(progn
  (define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
  (define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
  (define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
  (define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element)
  )


(progn
  (require 'dired )

  ;; (define-key dired-mode-map (kbd "M-$") nil) ; was dired-up-directory
  ;; (local-set-key (kbd "6") 'dired-up-directory)
  ;; (define-key dired-mode-map (kbd "M-g") 'backward-word)
  ;; (define-key dired-mode-map (kbd "M-c") 'previous-line)
  ;; (define-key dired-mode-map (kbd "C-o") 'ido-find-file)
  (define-key dired-mode-map (kbd "o") 'other-window)
  (define-key dired-mode-map (kbd "1") 'xah-previous-user-buffer)
  (define-key dired-mode-map (kbd "2") 'delete-window)
  (define-key dired-mode-map (kbd "3") 'delete-other-windows)
  (define-key dired-mode-map (kbd "4") 'split-window-vertically)
  (define-key dired-mode-map (kbd "C-o") 'find-file)

  (when (>= emacs-major-version 23)
    ;;    (define-key dired-mode-map (kbd "M-s") 'isearch-forward)
    ;;    (define-key dired-mode-map (kbd "M-S") 'isearch-backward)
    ;; (define-key dired-mode-map (kbd "<tab>") (make-keymap))
    (define-key dired-mode-map (kbd "<delete> t") 'wdired-change-to-wdired-mode) ; emacs 23 or later only
    ))

(defun xah-help-mode-setup ()
  "for help-mode-hook."
  (local-set-key (kbd "g") 'backward-word)
  (save-current-buffer
    (set-buffer "*Help*" )
    (local-unset-key (kbd "r"))
    (local-unset-key (kbd "8"))
    (local-unset-key (kbd "2"))
    (local-unset-key (kbd "3"))
    (local-unset-key (kbd "4"))
    ) )
(add-hook 'help-mode-hook 'xah-help-mode-setup)

(defun xah-help-mode-setup ()
  "for help-mode-hook."
  (local-set-key (kbd "g") 'backward-word)
  (save-current-buffer
    (set-buffer "*Help*" )
    (local-unset-key (kbd "r"))
    (local-unset-key (kbd "8"))
    (local-unset-key (kbd "2"))
    (local-unset-key (kbd "3"))
    (local-unset-key (kbd "4"))
    ) )
(add-hook 'help-mode-hook 'xah-help-mode-setup)

(defun xah-Man-mode-keys ()
  "my keybindings."
  (local-set-key (kbd "1") 'xah-previous-user-buffer)
  (local-set-key (kbd "2") 'delete-window)
  (local-set-key (kbd "3") 'delete-other-windows)
  (local-set-key (kbd "4") 'split-window-vertically)
  (local-set-key (kbd "6") 'xah-select-current-block)
  (local-set-key (kbd "8") 'xah-extend-selection)
  )
(add-hook 'Man-mode-hook 'xah-Man-mode-keys)



(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."

  (define-key xhm-keymap (kbd "<delete>") xhm-single-keys-keymap)

  (local-set-key (kbd "<backspace>") 'delete-backward-char)

  (local-set-key (kbd "<delete> 4") 'xahsite-update-article-timestamp)
  (local-set-key (kbd "<delete> 5") 'xhm-mark-unicode)

  (local-set-key (kbd "<delete> SPC b") 'xah-angle-brackets-to-html)
  (local-set-key (kbd "<delete> SPC c") 'xwe-chinese-linkify)
  (local-set-key (kbd "<delete> SPC d") 'xah-html-perldoc-ref-linkify)
  (local-set-key (kbd "<delete> SPC e") 'xah-html-emacs-ref-linkify)
  (local-set-key (kbd "<delete> SPC f") 'xah-html-full-size-img-linkify)
  (local-set-key (kbd "<delete> SPC j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<delete> SPC p") 'xah-html-php-ref-linkify)
  (local-set-key (kbd "<delete> SPC r") 'xah-add-to-related-links)
  (local-set-key (kbd "<delete> SPC t") 'xwe-word-etymology-linkify)
  (local-set-key (kbd "<delete> SPC z") 'amazon-linkify)

  (local-set-key (kbd "<delete> a") 'xwe-annotate)
  (local-set-key (kbd "<delete> b") 'make-blogger-entry)
  (local-set-key (kbd "<delete> c") 'xah-brackets-to-html)
  (local-set-key (kbd "<delete> d") 'xah-html-insert-date-tag)
  (local-set-key (kbd "<delete> e") 'xah-make-atom-entry)
  (local-set-key (kbd "<delete> f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<delete> g") 'xah-browse-url-of-buffer)
  (local-set-key (kbd "<delete> h") 'xah-all-linkify)
  (local-set-key (kbd "<delete> i") 'xah-html-image-linkify)
  (local-set-key (kbd "<delete> n") 'xah-ref-span-tag)
  (local-set-key (kbd "<delete> z a") 'xah-html-insert-keywords-tag)
  (local-set-key (kbd "<delete> z b") 'xah-html-insert-lyrics-header)
  (local-set-key (kbd "<delete> z c") 'xah-html-insert-lyrics-table)
  (local-set-key (kbd "<delete> z d") 'xah-html-insert-screen-filler)
  (local-set-key (kbd "<delete> z f") 'xah-html-insert-midi)

)

(add-hook 'html-mode-hook 'xah-html-mode-keys)
(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)
(add-hook 'nxml-mode-hook 'xah-html-mode-keys)

(defun xah-css-mode-setup ()
  "Modify keymaps used by `xah-css-mode'."
  (local-set-key (kbd "<delete> s") 'xah-sync-css))

(add-hook 'xah-css-mode-hook 'xah-css-mode-setup)



(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<f12>") 'rcirc-insert-next-input)
  )
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(defun xah-Info-mode-keys ()
  "Modify keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<delete> g") 'xah-view-emacs-manual-in-browser)
  (local-set-key (kbd "<mouse-8>") 'Info-history-back)
  )
(add-hook 'Info-mode-hook 'xah-Info-mode-keys)

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
    (call-interactively 'eval-last-sexp)
    )
  )
