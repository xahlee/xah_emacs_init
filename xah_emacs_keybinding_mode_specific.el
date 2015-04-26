;;-*- coding: utf-8 -*-
;; 2013-09-02

(defun xah-html-mode-keys ()
  "Modify keymaps used by `html-mode'."

  (define-key xhm-single-keys-keymap (kbd "e") 'xah-make-atom-entry)
  (define-key xhm-single-keys-keymap (kbd "g") 'xah-browse-url-of-buffer)
  (define-key xhm-single-keys-keymap (kbd "h") 'xah-all-linkify)

  (define-key xhm-single-keys-keymap (kbd "SPC a") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC b") 'xah-make-blogger-entry)
  (define-key xhm-single-keys-keymap (kbd "SPC c") 'xah-angle-brackets-to-html)
  (define-key xhm-single-keys-keymap (kbd "SPC d") 'xah-html-insert-date-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC e") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC f") 'xah-copy-url-current-file)
  (define-key xhm-single-keys-keymap (kbd "SPC g") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC h") 'xah-html-image-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC i") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC j") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC k") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC l") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC m") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC n") 'xah-add-reference-span-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC o") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC p") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC q") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC r d") 'xah-html-perldoc-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r e") 'xah-html-emacs-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r f") 'xah-html-full-size-img-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r g") 'xah-clojure-word-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r j") 'xah-image-file-to-html-figure-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC r p") 'xah-html-php-ref-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC r r") 'xah-add-to-related-links)
  (define-key xhm-single-keys-keymap (kbd "SPC r z") 'xah-amazon-linkify)
  (define-key xhm-single-keys-keymap (kbd "SPC s") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC u") 'xahsite-update-article-timestamp)
  (define-key xhm-single-keys-keymap (kbd "SPC v") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC w") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC x") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC y") nil)
  (define-key xhm-single-keys-keymap (kbd "SPC z") nil)

  (define-key xhm-single-keys-keymap (kbd "w a") 'xwe-annotate)
  (define-key xhm-single-keys-keymap (kbd "w e") 'xwe-bold-word)
  (define-key xhm-single-keys-keymap (kbd "w c") 'xwe-chinese-linkify)
  (define-key xhm-single-keys-keymap (kbd "w m") 'xwe-move-word-to-page)
  (define-key xhm-single-keys-keymap (kbd "w t") 'xwe-word-etymology-linkify)

  (define-key xhm-single-keys-keymap (kbd "w n") 'xwe-new-word-entry )
  (define-key xhm-single-keys-keymap (kbd "w i") 'xwe-insert-word-entry )
  (define-key xhm-single-keys-keymap (kbd "w d") 'xwe-add-definition )
  (define-key xhm-single-keys-keymap (kbd "w s") 'xwe-add-source )
  (define-key xhm-single-keys-keymap (kbd "w c") 'xwe-add-comment )
  (define-key xhm-single-keys-keymap (kbd "w g") 'xwe-search-next-unbold )
  (define-key xhm-single-keys-keymap (kbd "w p") 'xwe-query-find-then-bold )
  (define-key xhm-single-keys-keymap (kbd "w u") 'xwe-find-word-usage )

  (define-key xhm-single-keys-keymap (kbd "SPC z a") 'xah-html-insert-keywords-tag)
  (define-key xhm-single-keys-keymap (kbd "SPC z b") 'xah-html-insert-lyrics-header)
  (define-key xhm-single-keys-keymap (kbd "SPC z c") 'xah-html-insert-lyrics-table)
  (define-key xhm-single-keys-keymap (kbd "SPC z d") 'xah-html-insert-screen-filler)
  (define-key xhm-single-keys-keymap (kbd "SPC z f") 'xah-html-insert-midi)

  )

(add-hook 'xah-html-mode-hook 'xah-html-mode-keys)

(defun xah-css-mode-setup ()
  "Modify keymaps used by `xah-css-mode'."
  (require 'xah-css-mode)
  (define-key xcm-single-keys-keymap (kbd "s") 'xah-sync-css)
  )
(add-hook 'xah-css-mode-hook 'xah-css-mode-setup)



(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<f12>") 'rcirc-insert-next-input)
  )
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)



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



(defun xah-Info-mode-keys ()
  "Modify keybindings for `Info-mode'.
For `Info-mode-hook'."
  (local-set-key (kbd "<menu> e g") 'xah-view-emacs-manual-in-browser)
  (local-set-key (kbd "<mouse-8>") 'Info-history-back)

  ;; (define-prefix-command 'xah-Info-mode-keymap)
  ;; (let ((ξkmap (make-keymap)))
  ;;         (define-key ξkmap (kbd "<menu> e") nil)
  ;;         (define-key ξkmap (kbd "<menu> e .") 'beginning-of-buffer)
  ;;         (define-key ξkmap (kbd "<menu> e  ") 'Info-scroll-up)
  ;;         (define-key ξkmap [?\S-\ ] 'Info-scroll-down)
  ;;         (define-key ξkmap "\C-m" 'Info-follow-nearest-node)
  ;;         (define-key ξkmap "\t" 'Info-next-reference)
  ;;         (define-key ξkmap "\e\t" 'Info-prev-reference)
  ;;         (define-key ξkmap [backtab] 'Info-prev-reference)
  ;;         (define-key ξkmap (kbd "<menu> e 1") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 2") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 3") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 4") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 5") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 6") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 7") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 8") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 9") 'Info-nth-menu-item)
  ;;         (define-key ξkmap (kbd "<menu> e 0") 'undefined)
  ;;         (define-key ξkmap (kbd "<menu> e ?") 'Info-summary)
  ;;         (define-key ξkmap (kbd "<menu> e ]") 'Info-forward-node)
  ;;         (define-key ξkmap (kbd "<menu> e [") 'Info-backward-node)
  ;;         (define-key ξkmap (kbd "<menu> e <") 'Info-top-node)
  ;;         (define-key ξkmap (kbd "<menu> e >") 'Info-final-node)
  ;;         (define-key ξkmap (kbd "<menu> e b") 'beginning-of-buffer)
  ;;         (put 'beginning-of-buffer :advertised-binding "b")
  ;;         (define-key ξkmap (kbd "<menu> e d") 'Info-directory)
  ;;         (define-key ξkmap (kbd "<menu> e e") 'end-of-buffer)
  ;;         (define-key ξkmap (kbd "<menu> e f") 'Info-follow-reference)
  ;;         (define-key ξkmap (kbd "<menu> e g") 'Info-goto-node)
  ;;         (define-key ξkmap (kbd "<menu> e h") 'Info-help)
  ;;         ;; This is for compatibility with standalone info (>~ version 5.2).
  ;;         ;; Though for some time, standalone info had H and h reversed.
  ;;         ;; See <http://debbugs.gnu.org/16455>.
  ;;         (define-key ξkmap (kbd "<menu> e H") 'describe-mode)
  ;;         (define-key ξkmap (kbd "<menu> e i") 'Info-index)
  ;;         (define-key ξkmap (kbd "<menu> e I") 'Info-virtual-index)
  ;;         (define-key ξkmap (kbd "<menu> e l") 'Info-history-back)
  ;;         (define-key ξkmap (kbd "<menu> e L") 'Info-history)
  ;;         (define-key ξkmap (kbd "<menu> e m") 'Info-menu)
  ;;         (define-key ξkmap (kbd "<menu> e n") 'Info-next)
  ;;         (define-key ξkmap (kbd "<menu> e p") 'Info-prev)
  ;;         (define-key ξkmap (kbd "<menu> e q") 'Info-exit)
  ;;         (define-key ξkmap (kbd "<menu> e r") 'Info-history-forward)
  ;;         (define-key ξkmap (kbd "<menu> e s") 'Info-search)
  ;;         (define-key ξkmap (kbd "<menu> e S") 'Info-search-case-sensitively)
  ;;         (define-key ξkmap "\M-n" 'clone-buffer)
  ;;         (define-key ξkmap (kbd "<menu> e t") 'Info-top-node)
  ;;         (define-key ξkmap (kbd "<menu> e T") 'Info-toc)
  ;;         (define-key ξkmap (kbd "<menu> e u") 'Info-up)
  ;;         ;; `w' for consistency with `dired-copy-filename-as-kill'.
  ;;         (define-key ξkmap (kbd "<menu> e w") 'Info-copy-current-node-name)
  ;;         (define-key ξkmap (kbd "<menu> e c") 'Info-copy-current-node-name)
  ;;         ;; `^' for consistency with `dired-up-directory'.
  ;;         (define-key ξkmap (kbd "<menu> e ^") 'Info-up)
  ;;         (define-key ξkmap (kbd "<menu> e ,") 'Info-index-next)
  ;;         (define-key ξkmap "\177" 'Info-scroll-down)
  ;;         (define-key ξkmap [mouse-2] 'Info-mouse-follow-nearest-node)
  ;;         (define-key ξkmap [down-mouse-2] 'ignore) ;Override potential global binding.
  ;;         (define-key ξkmap [follow-link] 'mouse-face)
  ;;         (define-key ξkmap [XF86Back] 'Info-history-back)
  ;;         (define-key ξkmap [XF86Forward] 'Info-history-forward)
  ;;         ξkmap)

  )
(add-hook 'Info-mode-hook 'xah-Info-mode-keys)
