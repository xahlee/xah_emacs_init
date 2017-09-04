;; -*- coding: utf-8; lexical-binding: t; -*-

;; xah-fly-keys move to http://ergoemacs.org/misc/ergoemacs_vi_mode.html

;; (when (fboundp 'smex)
;;   (define-key xah-fly-key-map (kbd "<menu>") 'smex)
;;   )

;; (define-key xah-fly-key-map (kbd "<menu>") 'xah-fly-mode-toggle)

;; (define-key xah-fly-key-map (kbd "<home>") xah-insertion-keymap)
;; (define-key xah-fly-key-map (kbd "DEL") 'xah-fly-command-mode-activate)
;; (define-key xah-fly-key-map (kbd "<backspace>") 'xah-fly-command-mode-activate)

(global-set-key (kbd "<end>") 'xah-user-keymap)

(define-key xah-fly-leader-key-map (kbd "1") 'xah-open-file-at-cursor)

;; (global-set-key (kbd "<end> 3") 'xah-remove-wikipedia-link)
;; (global-set-key (kbd "<end> 4") 'xah-remove-all-wikipedia-link)


;; (setq visible-bell nil)

;; ring-bell-function

 ;; (setq ring-bell-function (lambda ()
                            ;; (play-sound-file "/this/is/my/errorsound.au")))

;; (setq ring-bell-function 'ding)

;; (setq ring-bell-function nil)

(define-key key-translation-map (kbd "ESC") (kbd "C-g"))

;; (define-key key-translation-map (kbd "<delete>") (kbd "C-c C-c"))

;; (current-input-mode)
;; (t nil t 7)

;; (let ((x (current-input-mode)))
;;   (set-input-mode
;;    (nth 0 x)
;;    (nth 1 x)
;;    (nth 2 x)
;;    27))

;; (global-set-key (kbd "<prior>") 'scroll-down-command)
;; (global-set-key (kbd "<next>") 'scroll-up-command)

;; (global-set-key (kbd "<prior>") 'hippie-expand)
;; (global-set-key (kbd "<next>") 'xah-toggle-letter-case)

(global-set-key (kbd "<S-prior>") 'scroll-down) ; page up
(global-set-key (kbd "<S-next>") 'scroll-up) ; page down

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page) ; Ctrl+Alt+PageUp
(global-set-key (kbd "<C-M-next>") 'forward-page) ; Ctrl+Alt+PageDown

;; 'xah-cycle-font-2
;; 'xah-cycle-font-next
;; 'xah-cycle-font-previous

(when (fboundp 'xah-html-mode)

    (define-key xah-html-mode-map (kbd "<delete>") xah-html-mode-no-chord-map)

    (defun xah-html-mode-keys ()
      "Modify keymaps used by `html-mode'."

      (define-key xah-html-mode-map (kbd "<f5>") 'xah-browse-url-of-buffer)

      (define-key xah-html-mode-no-chord-map (kbd "e") 'xah-make-atom-entry)
      (define-key xah-html-mode-no-chord-map (kbd "u") 'xahsite-update-article-timestamp)
      (define-key xah-html-mode-no-chord-map (kbd "s") 'xah-insert-reference-span-tag)
      (define-key xah-html-mode-no-chord-map (kbd "i") 'xah-html-insert-date-tag)

      (define-key xah-html-mode-no-chord-map (kbd "<delete>") 'xah-browse-url-of-buffer)

      (define-key xah-html-mode-no-chord-map (kbd "SPC") nil)
      (define-key xah-html-mode-no-chord-map (kbd "SPC b") 'xah-make-blogger-entry)
      (define-key xah-html-mode-no-chord-map (kbd "SPC c") 'xah-angle-brackets-to-html)
      (define-key xah-html-mode-no-chord-map (kbd "SPC e") 'xah-html-full-size-img-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC p") 'xah-copy-url-current-file)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r d") 'xah-html-perldoc-ref-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r e") 'xah-html-emacs-ref-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r g") 'xah-clojure-word-ref-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "SPC r j") 'xah-html-image-figure-linkify)

      (define-key xah-html-mode-no-chord-map (kbd "SPC r r") 'xah-add-to-related-links)

      (define-key xah-html-mode-no-chord-map (kbd "j") nil)

      (define-key xah-html-mode-no-chord-map (kbd "j a") 'xah-words-annotate)
      (define-key xah-html-mode-no-chord-map (kbd "j e") 'xah-words-bold-word)
      (define-key xah-html-mode-no-chord-map (kbd "j c") 'xah-words-chinese-linkify)
      (define-key xah-html-mode-no-chord-map (kbd "j m") 'xah-words-move-word-to-page)
      (define-key xah-html-mode-no-chord-map (kbd "j t") 'xah-words-word-etymology-linkify)

      (define-key xah-html-mode-no-chord-map (kbd "j n") 'xah-words-new-word-entry )
      (define-key xah-html-mode-no-chord-map (kbd "j i") 'xah-words-insert-word-entry )
      (define-key xah-html-mode-no-chord-map (kbd "j d") 'xah-words-add-definition )
      (define-key xah-html-mode-no-chord-map (kbd "j s") 'xah-words-add-source )
      (define-key xah-html-mode-no-chord-map (kbd "j c") 'xah-words-add-comment )
      (define-key xah-html-mode-no-chord-map (kbd "j g") 'xah-words-search-next-unbold )
      (define-key xah-html-mode-no-chord-map (kbd "j p") 'xah-words-query-find-then-bold )

      (define-key xah-html-mode-no-chord-map (kbd "SPC z b") 'xah-html-insert-lyrics-header)
      (define-key xah-html-mode-no-chord-map (kbd "SPC z f") 'xah-html-insert-midi))

    (add-hook 'xah-html-mode-hook 'xah-html-mode-keys))

(progn
  (define-prefix-command 'xah-user-keymap)

  (define-key xah-user-keymap (kbd "SPC") 'xah-dump-keymap)
  (define-key xah-user-keymap (kbd "<end>") 'xah-insert-low-line)
  (define-key xah-user-keymap (kbd "DEL") nil)
  (define-key xah-user-keymap (kbd "RET") 'xah-insert-word-3)

  (define-key xah-user-keymap (kbd ".") 'xah-title-case-region-or-line)
  (define-key xah-user-keymap (kbd "'") 'xah-replace-straight-quotes)

  (define-key xah-user-keymap (kbd "8") 'xah-find-count)
  (define-key xah-user-keymap (kbd "9") 'xah-find-replace-text-regex)
  (define-key xah-user-keymap (kbd "0") 'xah-find-text-regex)

  (define-key xah-user-keymap (kbd "a") 'xah-toggle-previous-letter-case)
  (define-key xah-user-keymap (kbd "b") nil)
  (define-key xah-user-keymap (kbd "c") 'xah-cite)
  (define-key xah-user-keymap (kbd "e") xah-fly-e-keymap)
  (define-key xah-user-keymap (kbd "f b") 'xah-dired-crop-image)
  (define-key xah-user-keymap (kbd "f c") 'xah-dired-2jpg)
  (define-key xah-user-keymap (kbd "f d") 'xah-image-autocrop)
  (define-key xah-user-keymap (kbd "f e") 'xah-dired-show-metadata)
  (define-key xah-user-keymap (kbd "f f") 'xah-dired-scale-image)
  (define-key xah-user-keymap (kbd "f g") 'xah-dired-2drawing)
  (define-key xah-user-keymap (kbd "f n") 'xah-open-in-gimp)
  (define-key xah-user-keymap (kbd "f r") 'xah-dired-2png)
  (define-key xah-user-keymap (kbd "f u") 'xah-dired-remove-all-metadata)

  (define-key xah-user-keymap (kbd "g") nil)
  (define-key xah-user-keymap (kbd "h") nil)

  (define-key xah-user-keymap (kbd "i c") 'xah-insert-random-number)
  (define-key xah-user-keymap (kbd "i h") 'xah-insert-random-string)
  (define-key xah-user-keymap (kbd "i t") 'xah-insert-random-hex)
  (define-key xah-user-keymap (kbd "i n") 'xah-insert-random-uuid)

  (define-key xah-user-keymap (kbd "j") 'xah-interactive-abbrev)
  (define-key xah-user-keymap (kbd "k") 'xah-find-replace-text)

  (define-key xah-user-keymap (kbd "m") 'xah-move-image-file)
  (define-key xah-user-keymap (kbd "n") nil)
  (define-key xah-user-keymap (kbd "o") 'xah-open-file-from-clipboard)
  (define-key xah-user-keymap (kbd "p") 'xah-find-text)
  (define-key xah-user-keymap (kbd "q") 'xah-replace-BOM-mark-etc)

  (define-key xah-user-keymap (kbd "r '") 'xah-latex-to-unicode)
  (define-key xah-user-keymap (kbd "r ,") 'xah-remove-punctuation-trailing-redundant-space )
  (define-key xah-user-keymap (kbd "r .") 'xah-convert-english-chinese-punctuation)
  (define-key xah-user-keymap (kbd "r [") 'xah-remove-square-brackets)
  (define-key xah-user-keymap (kbd "r b") 'xah-change-bracket-pairs)
  
  (define-key xah-user-keymap (kbd "r d") 'xah-fix-datetime-stamp)
  (define-key xah-user-keymap (kbd "r g") 'xah-convert-latin-alphabet-gothic)
  
  (define-key xah-user-keymap (kbd "r p") 'xah-convert-asian/ascii-space)
  (define-key xah-user-keymap (kbd "r p") 'xah-replace-profanity)
  (define-key xah-user-keymap (kbd "r t") 'xah-twitterfy)
  (define-key xah-user-keymap (kbd "r w") 'xah-convert-fullwidth-chars)
  (define-key xah-user-keymap (kbd "r x") 'xah-remove-quotes-or-brackets)

  (define-key xah-user-keymap (kbd "s") nil)
  (define-key xah-user-keymap (kbd "t") nil)
  (define-key xah-user-keymap (kbd "u") 'nil)
  (define-key xah-user-keymap (kbd "v") nil)
  (define-key xah-user-keymap (kbd "w") nil)
  (define-key xah-user-keymap (kbd "y") nil)
  (define-key xah-user-keymap (kbd "z") nil))

(progn
  ;; command dump. temp, rare, or whatever. put them here to have a key for now. worry later
  (define-prefix-command 'xah-dump-keymap)

  (define-key xah-dump-keymap (kbd "a") nil)
  (define-key xah-dump-keymap (kbd "b") nil)
  (define-key xah-dump-keymap (kbd "c") 'xah-css-mode)
  (define-key xah-dump-keymap (kbd "d") nil)
  (define-key xah-dump-keymap (kbd "e") 'xah-elisp-mode)
  (define-key xah-dump-keymap (kbd "f") nil)
  (define-key xah-dump-keymap (kbd "g") nil)
  (define-key xah-dump-keymap (kbd "h") 'xah-html-mode)
  (define-key xah-dump-keymap (kbd "i") nil)
  (define-key xah-dump-keymap (kbd "j") 'xah-js-mode)
  (define-key xah-dump-keymap (kbd "k") nil)
  (define-key xah-dump-keymap (kbd "l") 'xah-scan-list)

  (define-key xah-dump-keymap (kbd "l") nil)
  (define-key xah-dump-keymap (kbd "m") nil)
  (define-key xah-dump-keymap (kbd "n") nil)
  (define-key xah-dump-keymap (kbd "o") nil)
  (define-key xah-dump-keymap (kbd "p") nil)
  (define-key xah-dump-keymap (kbd "p") nil)
  (define-key xah-dump-keymap (kbd "q") nil)
  (define-key xah-dump-keymap (kbd "r") nil)
  (define-key xah-dump-keymap (kbd "s") nil)
  (define-key xah-dump-keymap (kbd "t") 'xah-clojure-mode)
  (define-key xah-dump-keymap (kbd "u") nil)
  (define-key xah-dump-keymap (kbd "v") nil)
  (define-key xah-dump-keymap (kbd "w") nil)
  (define-key xah-dump-keymap (kbd "x") nil)
  (define-key xah-dump-keymap (kbd "y") nil)
  (define-key xah-dump-keymap (kbd "z") nil))

;; 2015-08-22 add these somewhere
;; 'xah-toggle-read-novel-mode
;; 'xah-toggle-margin-right
;; 'xah-toggle-line-spacing

;; (progn
;;   (define-key key-translation-map (kbd "<menu> SPC <up>") (kbd "↑"))
;;   (define-key key-translation-map (kbd "<menu> SPC <down>") (kbd "↓"))
;;   (define-key key-translation-map (kbd "<menu> SPC <left>") (kbd "←"))
;;   (define-key key-translation-map (kbd "<menu> SPC <right>") (kbd "→"))
;;   (define-key key-translation-map (kbd "<menu> SPC 4") (kbd "◆"))
;;   (define-key key-translation-map (kbd "<menu> SPC 2") (kbd "＆"))
;;   (define-key key-translation-map (kbd "<menu> SPC 1") (kbd "•"))
;;   (define-key key-translation-map (kbd "<menu> SPC .") (kbd "…"))
;;   (define-key key-translation-map (kbd "<menu> SPC 3") (kbd "-"))
;;   (define-key key-translation-map (kbd "<menu> SPC 9") (kbd "_")))
