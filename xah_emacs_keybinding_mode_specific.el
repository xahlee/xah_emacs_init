;;-*- coding: utf-8 -*-
;; 2013-09-02

(defun xah-isearch-mode-keys ()
  "xah keybindings for `isearch-mode'.
For `isearch-mode-hook'."
  (define-key isearch-mode-map (kbd "M-f") 'isearch-repeat-forward)
  (define-key isearch-mode-map (kbd "M-F") 'isearch-repeat-backward)
;  (define-key isearch-mode-map (kbd "<escape>") 'minibuffer-keyboard-quit)
  (define-key isearch-mode-map (kbd "<f11>") 'isearch-ring-retreat)
  (define-key isearch-mode-map (kbd "<f12>") 'isearch-ring-advance)
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
;; • 〈Dvorak Keyboard Layout〉 http://xahlee.info/comp/dvorak_keyboard_layout.html

  (local-set-key (kbd "<tab> 4") 'xahsite-update-article-timestamp)
  (local-set-key (kbd "<tab> 5") 'xah-mark-unicode)
  (local-set-key (kbd "<tab> a") 'xwe-annotate)
  (local-set-key (kbd "<tab> b") 'make-blogger-entry)
  (local-set-key (kbd "<tab> d") 'insert-date-tag)

  (local-set-key (kbd "<tab> f") 'xah-copy-url-current-file)
  (local-set-key (kbd "<tab> g") 'xah-browse-url-of-buffer) ; 5401    0.33%  xah-browse-url-of-buffer

  (local-set-key (kbd "<tab> h") 'xah-all-linkify)
  (local-set-key (kbd "<tab> i") 'image-linkify)
  (local-set-key (kbd "<tab> l c") 'xwe-chinese-linkify)

  (local-set-key (kbd "<tab> l d") 'perldoc-ref-linkify)
  (local-set-key (kbd "<tab> l e") 'emacs-ref-linkify)
  (local-set-key (kbd "<tab> l f") 'full-size-img-linkify)
  (local-set-key (kbd "<tab> l j") 'image-file-to-html-figure-tag)
  (local-set-key (kbd "<tab> l p") 'php-ref-linkify)
  (local-set-key (kbd "<tab> l t") 'xwe-word-etymology-linkify)
  (local-set-key (kbd "<tab> l z") 'amazon-linkify)
  (local-set-key (kbd "<tab> n") 'xah-ref-span-tag)
  (local-set-key (kbd "<tab> r c") 'xah-bracket→html)
  (local-set-key (kbd "<tab> r t") 'xah-title-bracket→html)
  (local-set-key (kbd "<tab> t a") 'xah-make-atom-entry)
  (local-set-key (kbd "<tab> t l") 'xah-add-to-related-links)

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
  (define-key dired-mode-map (kbd "C-o") 'ido-find-file)
  (define-key dired-mode-map (kbd "o") 'other-window)
  (define-key dired-mode-map (kbd "1") 'xah-previous-user-buffer)
  (define-key dired-mode-map (kbd "2") 'delete-window)
  (define-key dired-mode-map (kbd "3") 'delete-other-windows)
  (define-key dired-mode-map (kbd "4") 'split-window-vertically)

  (when (>= emacs-major-version 23)
 ;;    (define-key dired-mode-map (kbd "M-s") 'isearch-forward)
 ;;    (define-key dired-mode-map (kbd "M-S") 'isearch-backward)
    ;; (define-key dired-mode-map (kbd "<tab>") (make-keymap))
    (define-key dired-mode-map (kbd "<tab>") nil)
    (define-key dired-mode-map (kbd "<tab> 8") 'wdired-change-to-wdired-mode) ; emacs 23 or later only
    )
  )

(defun xah-help-mode-setup ()
  "for help-mode-hook."
  (save-current-buffer
    (set-buffer "*Help*" )
    (local-unset-key (kbd "r"))
    (local-unset-key (kbd "8"))
    ) )
(add-hook 'help-mode-hook 'xah-help-mode-setup)
(remove-hook 'help-mode-hook 'xah-help-mode-keys)

(defun xah-perl-modes-keys ()
  "Modify keymaps."
  ;; (local-set-key (kbd "o") 'magit-status-mode)
  ;; if in command mode, make ; do undo
)
(add-hook 'cperl-mode-hook 'xah-perl-modes-keys)

(defun xah-magit-mode-keys ()
  "Modify keymaps."
  ;; (local-set-key (kbd "o") 'magit-status-mode)
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
  (local-set-key (kbd "6") 'xah-select-current-block)
  (local-set-key (kbd "8") 'xah-extend-selection)
  )
(add-hook 'Man-mode-hook 'xah-Man-mode-keys)

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

(defun xah-help-mode-keys ()
  "Modify keymaps"
  (local-set-key (kbd "g") 'backward-word)
)
(add-hook 'help-mode-hook 'xah-help-mode-keys)

;; (unload-feature 'sgml-mode)
;; (remove-hook 'html-mode-hook 'xah-html-mode-keys)



(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<f12>") 'rcirc-insert-next-input)
  )
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(defun xah-org-mode-keys ()
  "Modify keybindings for `org-mode'.
For `org-mode-hook'."
  (local-set-key (kbd "<M-up>") 'org-metaup)
  (local-set-key (kbd "<M-down>") 'org-metadown)
  (local-set-key (kbd "<M-left>") 'org-metaleft)
  (local-set-key (kbd "<M-right>") 'org-metaright)
  (local-set-key (kbd "M-h") 'backward-char)
  (local-set-key (kbd "C-a") 'mark-whole-buffer)

  )
(add-hook 'org-mode-hook 'xah-org-mode-keys)

(defun xah-Info-mode-keys ()
  "Modify keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<menu> u g") 'xah-view-emacs-manual-in-browser)
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
