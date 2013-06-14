;;-*- coding: utf-8 -*-
;; emacs customization. keys for inserting brackets and unicode (Dvorak layout).
;; • xah_emacs_keybinding.el
;; • xah_emacs_unicode_input.el
;; • xah_emacs_hyper_super_setup.el
;; • xah_emacs_insert_pairs.el
;; • 〈Emacs Unicode Math Symbols Input Mode (xmsi-mode)〉 http://ergoemacs.org/emacs/xmsi-math-symbols-input.html
;; • 〈Matching Brackets in Unicode〉 http://xahlee.info/comp/unicode_matching_brackets.html
;; • 〈Computing Symbols in Unicode〉 http://xahlee.info/comp/unicode_computing_symbols.html
;; Xah Lee
;; 2007-10
;; ∑ http://xahlee.org/


;; problem keys
;; for qwerty: () [] {} -_ =+
;; for dvorak: () [] {} /? =+

;; (define-prefix-command 'xah-unicode-keymap)
;; (global-set-key (kbd "<menu> u") xah-unicode-keymap)

(defvar xah-unicode-list nil "alist of Unicode symbols. first element is unicode, second element is a string used as key shortcut in ido-completing-read")
(setq xah-unicode-list
      '(
        ("◇" . "3" )
        ("◆" . "4" )
        ("¤" . "2" )
        ("…" . "l" )
        (" " . "s" )
        ("、" . "," )
        ("•" . "8" )
        ("🎶" . "5" )
        ("—" . "-" )
        ("＆" . "7" )
        ("↓" . "at")
        ("←" . "ah")
        ("→" . "an")
        ("↑" . "ac")
        ) )

(defun xah-insert-unicode ()
  "insert a unicode"
  (interactive)
  (let (gotThis)
    (setq gotThis
          (ido-completing-read "insert:" (mapcar (lambda (x) (concat (car x) (cdr x))) xah-unicode-list)) )
    (insert (car (assoc (substring gotThis 0 1) xah-unicode-list)))
    )
  )

