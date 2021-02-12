;; -*- coding: utf-8; lexical-binding: t; -*-

;; keys for moving to prev/next code section (form feed; ^L)
(global-set-key (kbd "<C-M-prior>") 'backward-page)
(global-set-key (kbd "<C-M-next>") 'forward-page)

;; HHH___________________________________________________________________ 

(when (boundp 'xah-fly-key-map)

  (define-key xah-fly-leader-key-map (kbd "SPC") 'xah-user-keymap)
  (define-key key-translation-map (kbd "ESC") (kbd "C-g"))

  (global-set-key (kbd "C-b") 'xah-cycle-hyphen-underscore-space)

  (global-set-key (kbd "<end>") 'xah-fly-command-mode-activate)

  (define-key xah-fly-h-keymap (kbd "t") 'xah-lookup-web)
  (define-key xah-fly-h-keymap (kbd "w") 'xah-lookup-word-definition)

  (defun xah-xfk-add ()
    "addon for `xah-fly-command-mode-activate-hook'
Version 2020-04-09"
    (interactive)
    (xah-fly--define-keys
     xah-fly-key-map
     '(
       ;; the first element of cons cell is dvorak key
       ("3" . nil ) ; got pain on this with kinesis advantage keyboard ;; Xah Lee Emacs Pinky 2020 http://ergoemacs.org/emacs/emacs_pinky_2020.html
       ("4" . nil )
       ;; workaround
       ("-" . delete-other-windows) ; for dvorak on kinesis advantage keyboard http://xahlee.info/kbd/keyboard_kinesis.html
       ("\\" . split-window-below)  ; for kinesis advantage keyboard
       ("]" . delete-other-windows) ; for dvorak on kinesis advantage keyboard http://xahlee.info/kbd/keyboard_kinesis.html
       ("[" . split-window-below ) ; for dvorak on kinesis advantage keyboard
       ;;
       )))
  (add-hook 'xah-fly-command-mode-activate-hook 'xah-xfk-add)
  ;;
  )

;; HHH___________________________________________________________________


;; kinesis
;; (define-key key-translation-map (kbd "<kp-delete>") (kbd "<delete>"))

;; HHH___________________________________________________________________ 

(when (string-equal system-type "darwin")
  (define-key key-translation-map (kbd "<deletechar>") (kbd "<delete>"))
  (global-set-key (kbd "M--") 'xah-cycle-hyphen-underscore-space)
  (global-set-key (kbd "s-w") 'xah-close-current-buffer)
  (global-set-key (kbd "s-r") 'xah-html-browse-url-of-buffer)
  (global-set-key (kbd "s-T") 'xah-open-last-closed)
  (global-set-key (kbd "s-t") 'xah-new-empty-buffer)
  (global-set-key (kbd "s-n") 'xah-new-empty-buffer)
  (global-set-key (kbd "s-[") 'xah-previous-user-buffer)
  (global-set-key (kbd "s-]") 'xah-next-user-buffer)
  (global-set-key (kbd "<M-s-left>") 'xah-previous-user-buffer)
  (global-set-key (kbd "<M-s-right>") 'xah-next-user-buffer)
  (global-set-key (kbd "s-=") 'text-scale-increase)
  (global-set-key (kbd "s--") 'text-scale-decrease)
  (global-set-key (kbd "C-s-f") 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f1>") 'toggle-frame-fullscreen)
  (global-set-key (kbd "<f2>") 'xah-cut-line-or-region)
  (global-set-key (kbd "<f3>") 'xah-copy-line-or-region))

;; HHH___________________________________________________________________ 

;; (current-input-mode)
;; (t nil t 7)

;; (let ((x (current-input-mode)))
;;   (set-input-mode
;;    (nth 0 x)
;;    (nth 1 x)
;;    (nth 2 x)
;;    27))

(progn
  ;; command dump. temp, rare, or whatever. put them here to have a key for now. worry later
  (define-prefix-command 'xah-dump-keymap)
  (define-key xah-dump-keymap (kbd "c") 'xah-css-mode)
  (define-key xah-dump-keymap (kbd "e") 'xah-elisp-mode)
  (define-key xah-dump-keymap (kbd "h") 'xah-html-mode)
  (define-key xah-dump-keymap (kbd "j") 'xah-js-mode)
  (define-key xah-dump-keymap (kbd "l") 'xah-scan-list)
  (define-key xah-dump-keymap (kbd "t") 'xah-clojure-mode)
  ;;
  )

(progn
  (define-prefix-command 'xah-user-keymap)

  (define-key xah-user-keymap (kbd "SPC") xah-dump-keymap)
  ;; '
  (define-key xah-user-keymap (kbd ".") 'xah-title-case-region-or-line)
  (define-key xah-user-keymap (kbd "8") 'xah-find-count)
  (define-key xah-user-keymap (kbd "9") 'xah-find-replace-text-regex)
  (define-key xah-user-keymap (kbd "0") 'xah-find-text-regex)
  (define-key xah-user-keymap (kbd "1") 'xah-insert-word-1)
  (define-key xah-user-keymap (kbd "2") 'xah-insert-word-2)
  ;; a
  (define-key xah-user-keymap (kbd "b") 'xah-toggle-previous-letter-case)
  (define-key xah-user-keymap (kbd "c") 'xah-cite)
  (define-key xah-user-keymap (kbd "d") 'xah-reformat-to-sentence-lines)
  (define-key xah-user-keymap (kbd "e") 'xah-add-period-to-line-end)
  ;; f
  (define-key xah-user-keymap (kbd "g") 'xah-replace-straight-quotes)
  ;; h
  (define-key xah-user-keymap (kbd "i c") 'xah-insert-random-number)
  (define-key xah-user-keymap (kbd "i h") 'xah-insert-random-hex)
  (define-key xah-user-keymap (kbd "i t") 'xah-insert-random-string)
  (define-key xah-user-keymap (kbd "i n") 'xah-insert-random-uuid)
  (define-key xah-user-keymap (kbd "j") 'xah-interactive-abbrev)
  (define-key xah-user-keymap (kbd "k") 'xah-find-replace-text)
  ;; l m
  ;; n
  (define-key xah-user-keymap (kbd "o") 'xah-open-file-from-clipboard)
  (define-key xah-user-keymap (kbd "p") 'xah-find-text)
  (define-key xah-user-keymap (kbd "q") 'xah-replace-invisible-char)
  ;; r s t
  (define-key xah-user-keymap (kbd "t") 'xah-math-input-change-to-symbol)
  (define-key xah-user-keymap (kbd "u ,") 'xah-remove-punctuation-trailing-redundant-space )
  (define-key xah-user-keymap (kbd "u .") 'xah-convert-english-chinese-punctuation)
  (define-key xah-user-keymap (kbd "u [") 'xah-remove-square-brackets)
  (define-key xah-user-keymap (kbd "u b") 'xah-change-bracket-pairs)
  (define-key xah-user-keymap (kbd "u d") 'xah-fix-datetime)
  (define-key xah-user-keymap (kbd "u g") 'xah-convert-latin-alphabet-gothic)
  (define-key xah-user-keymap (kbd "u p") 'xah-convert-asian/ascii-space)
  (define-key xah-user-keymap (kbd "u p") 'xah-replace-profanity)
  (define-key xah-user-keymap (kbd "u t") 'xah-twitterfy)
  (define-key xah-user-keymap (kbd "u w") 'xah-convert-fullwidth-chars)
  (define-key xah-user-keymap (kbd "u x") 'xah-remove-quotes-or-brackets)
  ;;  v w x y z
  )

;; 2015-08-22 add these somewhere
;; 'xah-toggle-read-novel-mode
;; 'xah-toggle-margin-right
;; 'xah-toggle-line-spacing

;; HHH___________________________________________________________________

(when (fboundp 'go-mode)
  (defun xah-gofmt ()
    "Reformat current file by calling shell command gofmt.
URL `http://ergoemacs.org/misc/emacs_go-mode_gofmt_diff.html'
Version 2021-01-15"
    (interactive)
    (let ((xfname (buffer-file-name)))
      (when xfname
        (when (buffer-modified-p )
          (save-buffer))
        (shell-command (format "gofmt -w %s" xfname)))))
  (defun xah-config-go-mode ()
    "config go-mode. Version 2021-01-15"
    (interactive)
    (define-prefix-command 'xah-golang-leader-map)
    (define-key xah-golang-leader-map (kbd "c") 'xah-gofmt)
    (define-key xah-golang-leader-map (kbd "j") 'godef-jump)
    (define-key go-mode-map (kbd "<delete>") xah-golang-leader-map))
  (add-hook 'go-mode-hook 'xah-config-go-mode))

(progn
  (require 'dired )
  (defun xah-config-dired ()
    "Version 2021-01-17"
    (interactive)
    (define-prefix-command 'xah-dired-leader-map)
    (define-key xah-dired-leader-map (kbd "d") 'xah-dired-image-autocrop)
    (define-key xah-dired-leader-map (kbd "e") 'xah-dired-show-metadata)
    (define-key xah-dired-leader-map (kbd "g") 'xah-dired-2drawing)
    (define-key xah-dired-leader-map (kbd "h") 'xah-dired-scale-image)
    (define-key xah-dired-leader-map (kbd "n") 'xah-dired-2png)
    (define-key xah-dired-leader-map (kbd "p") 'xah-dired-open-in-gimp)
    (define-key xah-dired-leader-map (kbd "t") 'xah-dired-2jpg)
    (define-key xah-dired-leader-map (kbd "u") 'xah-dired-remove-all-metadata)
    (define-key xah-dired-leader-map (kbd ".") 'xah-dired-optimize-png)
    (define-key dired-mode-map (kbd "<delete>") xah-dired-leader-map)
    ;;
    (define-key dired-mode-map (kbd "-") 'xah-dired-rename-space-to-underscore)
    (define-key dired-mode-map (kbd "s") 'xah-dired-sort)
    ;;
    )
  (add-hook 'dired-mode-hook 'xah-config-dired)
  ;;
  )

(progn
  (require 'image-mode )
  (defun xah-config-image-mode ()
    "Version 2021-01-17"
    (interactive)
    (define-prefix-command 'xah-image-mode-leader-map)
    (define-key xah-image-mode-leader-map (kbd "d") 'xah-dired-image-autocrop)
    (define-key xah-image-mode-leader-map (kbd "e") 'xah-dired-show-metadata)
    (define-key xah-image-mode-leader-map (kbd "g") 'xah-dired-2drawing)
    (define-key xah-image-mode-leader-map (kbd "h") 'xah-dired-scale-image)
    (define-key xah-image-mode-leader-map (kbd "n") 'xah-dired-2png)
    (define-key xah-image-mode-leader-map (kbd "p") 'xah-dired-open-in-gimp)
    (define-key xah-image-mode-leader-map (kbd "t") 'xah-dired-2jpg)
    (define-key xah-image-mode-leader-map (kbd "u") 'xah-dired-remove-all-metadata)
    (define-key xah-image-mode-leader-map (kbd ".") 'xah-dired-optimize-png)
    (define-key image-mode-map (kbd "<delete>") xah-image-mode-leader-map)
    (define-key image-mode-map (kbd "<left>") 'image-previous-file)
    (define-key image-mode-map (kbd "<right>") 'image-next-file)
    (define-key image-mode-map (kbd "<wheel-up>") 'image-previous-file)
    (define-key image-mode-map (kbd "<wheel-down>") 'image-next-file)
    ;;
    )
  (add-hook 'image-mode-hook 'xah-config-image-mode)
  ;;
  )

(when (fboundp 'xah-html-mode)
  (progn
    (define-key xah-html-leader-map (kbd "SPC s") 'xah-insert-reference-span-tag)
    (define-key xah-html-leader-map (kbd "SPC e") 'xah-atom-new-entry)
    (define-key xah-html-leader-map (kbd "SPC m") 'xah-move-image-file)
    (define-key xah-html-leader-map (kbd "SPC u") 'xah-update-article-timestamp)
    (define-key xah-html-leader-map (kbd "SPC t") 'xah-html-wrap-big-tag)
    (define-key xah-html-leader-map (kbd "SPC c") 'xah-angle-brackets-to-html)
    (define-key xah-html-leader-map (kbd "SPC p") 'xah-copy-url-current-file)
    (define-key xah-html-leader-map (kbd "SPC r d") 'xah-html-perldoc-ref-linkify)
    (define-key xah-html-leader-map (kbd "SPC r e") 'xah-html-emacs-ref-linkify)
    (define-key xah-html-leader-map (kbd "SPC r g") 'xah-clojure-word-ref-linkify)
    (define-key xah-html-leader-map (kbd "SPC r r") 'xah-add-to-related-links)
    (define-key xah-html-leader-map (kbd "o") nil)
    (define-key xah-html-leader-map (kbd "o a") 'xah-words-annotate)
    (define-key xah-html-leader-map (kbd "o e") 'xah-words-bold-word)
    (define-key xah-html-leader-map (kbd "o c") 'xah-words-chinese-linkify)
    (define-key xah-html-leader-map (kbd "o m") 'xah-words-move-word-to-page)
    (define-key xah-html-leader-map (kbd "o t") 'xah-words-word-etymology-linkify)
    (define-key xah-html-leader-map (kbd "o n") 'xah-words-new-word-entry )
    (define-key xah-html-leader-map (kbd "o i") 'xah-words-insert-word-entry )
    (define-key xah-html-leader-map (kbd "o d") 'xah-words-add-definition )
    (define-key xah-html-leader-map (kbd "o s") 'xah-words-add-source )
    (define-key xah-html-leader-map (kbd "o c") 'xah-words-add-comment )
    (define-key xah-html-leader-map (kbd "o g") 'xah-words-search-next-unbold )
    (define-key xah-html-leader-map (kbd "o p") 'xah-words-query-find-then-bold ))
  (defun xah-config-xah-html-mode ()
    "Version 2021-01-15"
    (interactive)
    (define-key xah-html-mode-map (kbd "<delete>") xah-html-leader-map)
    (when (string-equal system-type "windows-nt")
      (define-key xah-html-mode-map (kbd "C-r") 'xah-html-browse-url-of-buffer)))
  (add-hook 'xah-html-mode-hook 'xah-config-xah-html-mode))

(when (boundp 'xah-elisp-mode-map)
  (define-key xah-elisp-mode-map (kbd "<delete>") xah-elisp-mode-no-chord-map))

(when (boundp 'xah-css-mode-no-chord-map)
  (define-key xah-css-mode-map (kbd "<delete>") xah-css-mode-no-chord-map))

(when (boundp 'xah-clojure-mode-map)
    (define-key xah-clojure-mode-map (kbd "<delete>") xah-clojure-mode-no-chord-map))



(progn
  (require 'info )
  (define-key Info-mode-map (kbd "<f5>") 'xah-view-emacs-manual-in-browser)
  (define-key Info-mode-map (kbd "C-r") 'xah-view-emacs-manual-in-browser))


(when (boundp 'org-mode-hook)
  (defun xah-org-mode-setup ()
    "Modify keymaps used by `org-mode'."
    (local-set-key (kbd "<C-tab>") 'xah-next-user-buffer))
  (add-hook 'org-mode-hook 'xah-org-mode-setup))

;; HHH___________________________________________________________________

(when (boundp 'tuareg-mode-map)
  (define-key tuareg-mode-map (kbd "<backspace>") nil)
  (define-key tuareg-mode-map (kbd "DEL") nil))

;; HHH___________________________________________________________________


(defun xah-rcirc-mode-keys ()
  "Modify keybindings for `rcirc'.
For `rcirc-mode-hook'."
  (local-set-key (kbd "<M-f11>") 'rcirc-insert-prev-input)
  (local-set-key (kbd "<M-f12>") 'rcirc-insert-next-input))
(add-hook 'rcirc-mode-hook 'xah-rcirc-mode-keys)

(setq rcirc-default-nick "mid2")

;; HHH___________________________________________________________________

(when (boundp 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map))

