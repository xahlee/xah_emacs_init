;;-*- coding: utf-8 -*-
;; 2013-09-02


(defun xah-isearch-mode-keys ()
  "xah keybindings for `isearch-mode'.
For `isearch-mode-hook'."
  (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-F") 'isearch-repeat-backward)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-repeat-backward)
;  (define-key isearch-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)

  (define-key isearch-mode-map (kbd "<next>") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "<prior>") 'isearch-repeat-backward)
  )
(add-hook 'isearch-mode-hook 'xah-isearch-mode-keys )

(defun xah-comint-keys ()
  "xah keybindings for `comint-mode-hook'."
  (define-key comint-mode-map (kbd "M-g") 'backward-word)
  (define-key comint-mode-map (kbd "M-r") 'forward-word)
  (define-key comint-mode-map (kbd "M-h") 'backward-char)
  (define-key comint-mode-map (kbd "M-n") 'forward-char)
  (define-key comint-mode-map (kbd "M-t") 'next-line)
  (define-key comint-mode-map (kbd "M-c") 'previous-line)
  (define-key comint-mode-map (kbd "M-e") 'delete-backward-char)
  (define-key comint-mode-map (kbd "M-u") 'delete-char)
  (define-key comint-mode-map (kbd "M-.") 'backward-kill-word)
  (define-key comint-mode-map (kbd "M-p") 'kill-word)
  (define-key comint-mode-map (kbd "M-d") 'move-beginning-of-line)
  (define-key comint-mode-map (kbd "M-q") 'xah-cut-line-or-region)
  (define-key comint-mode-map (kbd "M-j") 'xah-copy-line-or-region)
  (define-key comint-mode-map (kbd "M-k") 'yank)
  )
(add-hook 'comint-mode-hook 'xah-comint-keys )
(add-hook 'minibuffer-inactive-mode-hook 'xah-comint-keys )

(progn 
  (define-key minibuffer-local-map (kbd "M-g") 'backward-word)
  (define-key minibuffer-local-map (kbd "M-r") 'forward-word)
  (define-key minibuffer-local-map (kbd "M-h") 'backward-char)
  (define-key minibuffer-local-map (kbd "M-n") 'forward-char)
  (define-key minibuffer-local-map (kbd "M-t") 'next-line)
  (define-key minibuffer-local-map (kbd "M-c") 'previous-line)
  (define-key minibuffer-local-map (kbd "M-e") 'delete-backward-char)
  (define-key minibuffer-local-map (kbd "M-u") 'delete-char)
  (define-key minibuffer-local-map (kbd "M-.") 'backward-kill-word)
  (define-key minibuffer-local-map (kbd "M-p") 'kill-word)
  (define-key minibuffer-local-map (kbd "M-d") 'move-beginning-of-line)
  (define-key minibuffer-local-map (kbd "M-q") 'xah-cut-line-or-region)
  (define-key minibuffer-local-map (kbd "M-j") 'xah-copy-line-or-region)
  (define-key minibuffer-local-map (kbd "M-k") 'yank)

;; add back some bindings for commands whose binding we displaced
(define-key minibuffer-local-map (kbd "<f11>") 'previous-history-element)
(define-key minibuffer-local-map (kbd "<f12>") 'next-history-element)
(define-key minibuffer-local-map (kbd "S-<f11>") 'previous-matching-history-element)
(define-key minibuffer-local-map (kbd "S-<f12>") 'next-matching-history-element)
)


(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."
  ;; .p gc
  ;; eu ht

  (local-set-key (kbd "<C-left>") 'xhm-skip-tag-backward)
  (local-set-key (kbd "<C-right>") 'xhm-skip-tag-forward)

  (local-set-key (kbd "<tab> <backspace>") 'xhm-remove-html-tags)
  (local-set-key (kbd "<tab> <return>") 'xhm-insert-br-tag)
  (local-set-key (kbd "<tab> -") 'xhm-insert-hr-tag)
  (local-set-key (kbd "<tab> .") 'xhm-lines-to-html-list)
  (local-set-key (kbd "<tab> '") nil)
  (local-set-key (kbd "<tab> ,") nil)
  (local-set-key (kbd "<tab> 1") nil)
  (local-set-key (kbd "<tab> 2") nil)
  (local-set-key (kbd "<tab> 3") 'xhm-update-title)
  (local-set-key (kbd "<tab> 4") 'xahsite-update-article-timestamp)
  (local-set-key (kbd "<tab> 5") 'mark-unicode)
  (local-set-key (kbd "<tab> 6") 'xhm-html-to-text)
  (local-set-key (kbd "<tab> 7") 'xhm-htmlize-or-de-precode)
  (local-set-key (kbd "<tab> 8") 'xhm-get-precode-make-new-file)
  (local-set-key (kbd "<tab> a") 'xwe-annotate)
  (local-set-key (kbd "<tab> b") 'make-blogger-entry)
  (local-set-key (kbd "<tab> c") 'xhm-make-citation)
  (local-set-key (kbd "<tab> d") 'insert-date-tag)
  (local-set-key (kbd "<tab> e") 'xhm-wrap-html-tag)

  (local-set-key (kbd "<tab> f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<tab> g") 'xah-browse-url-of-buffer) ; 5401    0.33%  xah-browse-url-of-buffer

  (local-set-key (kbd "<tab> h") 'xah-all-linkify)
  (local-set-key (kbd "<tab> i") 'image-linkify)
  (local-set-key (kbd "<tab> j") 'nil)
  (local-set-key (kbd "<tab> k") 'xhm-htmlize-keyboard-shortcut-notation)
  (local-set-key (kbd "<tab> l 3") 'xhm-source-url-linkify)
  (local-set-key (kbd "<tab> l c") 'xwe-chinese-linkify)

  (local-set-key (kbd "<tab> l d") 'perldoc-ref-linkify)
  (local-set-key (kbd "<tab> l e") 'emacs-ref-linkify)
  (local-set-key (kbd "<tab> l f") 'full-size-img-linkify)
  (local-set-key (kbd "<tab> l j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<tab> l p") 'php-ref-linkify)
  (local-set-key (kbd "<tab> l t") 'xwe-word-etymology-linkify)
  (local-set-key (kbd "<tab> l s") 'xhm-make-link-defunct)
  (local-set-key (kbd "<tab> l u") 'xhm-wrap-url)
  (local-set-key (kbd "<tab> l w") 'xhm-wikipedia-linkify)
  (local-set-key (kbd "<tab> l z") 'amazon-linkify)
  (local-set-key (kbd "<tab> m") 'xhm-pre-source-code)
  (local-set-key (kbd "<tab> n") 'nil)
  (local-set-key (kbd "<tab> o") 'nil)
  (local-set-key (kbd "<tab> p") 'xhm-wrap-p-tag)
  (local-set-key (kbd "<tab> q") 'nil)
  (local-set-key (kbd "<tab> r ,") 'xhm-replace-html-chars-to-unicode)
  (local-set-key (kbd "<tab> r .") 'xhm-replace-html-&<>-to-entities)
  (local-set-key (kbd "<tab> r c") 'code-bracket-to-html-tag)
  (local-set-key (kbd "<tab> r e") 'xhm-htmlize-elisp-keywords)
  (local-set-key (kbd "<tab> r k") 'xhm-emacs-to-windows-kbd-notation)
  (local-set-key (kbd "<tab> r m") 'xhm-make-html-table)
  (local-set-key (kbd "<tab> r t") 'title-bracket-to-html-tag)
  (local-set-key (kbd "<tab> s") 'nil)
  (local-set-key (kbd "<tab> t a") 'xah-make-atom-entry)
  (local-set-key (kbd "<tab> t l") 'xah-add-to-related-links)
  (local-set-key (kbd "<tab> t r") 'xhm-rename-html-inline-image)
  (local-set-key (kbd "<tab> t u") 'xhm-extract-url)
  (local-set-key (kbd "<tab> u") nil)
  (local-set-key (kbd "<tab> v") 'nil)
  (local-set-key (kbd "<tab> w") (lambda () (interactive) (xhm-wrap-html-tag "b" "w")))
  (local-set-key (kbd "<tab> x") 'nil)
  (local-set-key (kbd "<tab> y") 'nil)
  (local-set-key (kbd "<tab> z") 'nil)

)

(add-hook 'html-mode-hook 'xah-html-mode-keys)
(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)
(add-hook 'nxml-mode-hook 'xah-html-mode-keys)


(progn
  (require 'dired )

  ;; (define-key dired-mode-map (kbd "M-$") nil) ; was dired-up-directory
  ;; (local-set-key (kbd "6") 'dired-up-directory)
  (define-key dired-mode-map (kbd "M-g") 'backward-word)
  (define-key dired-mode-map (kbd "M-c") 'previous-line)
  (define-key dired-mode-map (kbd "o") 'other-window)
  (define-key dired-mode-map (kbd "1") 'xah-previous-user-buffer)
  (define-key dired-mode-map (kbd "2") 'delete-window)
  (define-key dired-mode-map (kbd "3") 'delete-other-windows)
  (define-key dired-mode-map (kbd "4") 'split-window-vertically)

  (when (>= emacs-major-version 23)
 ;;    (define-key dired-mode-map (kbd "M-s") 'isearch-forward)
 ;;    (define-key dired-mode-map (kbd "M-S") 'isearch-backward)
    (define-key dired-mode-map (kbd "<tab> 8") 'wdired-change-to-wdired-mode) ; emacs 23 or later only
    )
  )

(defun xah-perl-modes-keys ()
  "Modify keymaps."
  ;; (local-set-key (kbd "o") 'magit-status-mode)
  ;; if in command mode, make ; do undo
)
(add-hook 'cperl-mode-hook 'xah-perl-modes-keys)

(defun xah-magit-mode-keys ()
  "Modify keymaps."
  (local-set-key (kbd "<tab> <tab>") 'magit-toggle-section)
  (local-set-key (kbd "o") 'magit-status-mode)
  (local-set-key (kbd "1") 'xah-previous-user-buffer)
  (local-set-key (kbd "2") 'delete-window)
  (local-set-key (kbd "3") 'delete-other-windows)
  (local-set-key (kbd "4") 'split-window-vertically)
)
(add-hook 'magit-mode-hook 'xah-magit-mode-keys)

(defun xah-Man-mode-keys ()
  "my keybindings."
  (local-set-key (kbd "1") 'xah-previous-user-buffer)
  (local-set-key (kbd "2") 'delete-window)
  (local-set-key (kbd "3") 'delete-other-windows)
  (local-set-key (kbd "4") 'split-window-vertically)
  )
(add-hook 'Man-mode-hook 'xah-Man-mode-keys)






;; (defun xah-cperl-mode-keys ()
;;   "Modify keymaps used by cperl-mode."
;;   (local-set-key (kbd ")") 'self-insert-command)
;;   (local-set-key (kbd "]") 'self-insert-command)
;; )
;; (add-hook 'cperl-mode-hook 'xah-cperl-mode-keys)

;; ;(setq mybuf (get-buffer-create "*show commands*"))
;; (defun xx ()
;;   "tttttt"
;;   (let ()
;;     (princ (format "%s\n" last-command) mybuf )
;;     )
;;   ;;(message "%s" last-command)
;;   )
;; (add-hook 'post-command-hook 'xx)
;; (remove-hook 'post-command-hook 'xx)

(defun xah-elisp-mode-keys ()
  "Modify keymaps used by lisp mode."
  ;; .p gc
  ;; eu ht

  (local-set-key (kbd "<tab> t") 'eval-last-sexp)
  (local-set-key (kbd "<tab> f") 'xah-eval-defun)
)

(add-hook 'xah-elisp-mode-hook 'xah-elisp-mode-keys)
(add-hook 'emacs-lisp-mode-hook 'xah-elisp-mode-keys)



(defun xah-help-mode-keys ()
  "Modify keymaps"
  (local-set-key (kbd "g") 'backward-word)
)
(add-hook 'help-mode-hook 'xah-help-mode-keys)

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
  (local-set-key (kbd "M-h") 'backward-char)
  )
(add-hook 'org-mode-hook 'xah-org-mode-keys)

(defun xah-Info-mode-keys ()
  "my keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<tab> g") 'xah-view-emacs-manual-in-browser)
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
