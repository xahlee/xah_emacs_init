;;-*- coding: utf-8 -*-
;; emacs customization.
;; • 〈Emacs Unicode Math Symbols Input Mode (xmsi-mode)〉 http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;; Xah Lee
;; 2011-05-27
;; ∑ http://xahlee.org/



;;;; matching pairs

(defun xah-insert-bracket-pair (φleftBracket φrightBracket)
  "Insert a matching bracket.

If there's a text selection, insert brackets around it.
If there's no text selection:
  If cursor is on alphanumeric char or hyphen or understore, insert brackets around current word.
  else, insert brackets.

The argument φleftBracket φrightBracket are strings."
  (if (region-active-p)
      (progn
        (let (
              (p1 (region-beginning))
              (p2 (region-end))
              )
          (goto-char p2)
          (insert φrightBracket)
          (goto-char p1)
          (insert φleftBracket)
          (goto-char (+ p2 2))
          ))
    (progn ; no text selection
                                        ;(= (point) (point-max) )
      (if (looking-at "[-A-Za-z0-9]")
          (progn
            (let* (
                   (bds (unit-at-cursor 'word))
                   (p1 (elt bds 1))
                   (p2 (elt bds 2))
                   )
              (goto-char p2)
              (insert φrightBracket)
              (goto-char p1)
              (insert φleftBracket)
              (goto-char (+ p2 (length φleftBracket)))
              )
            )
        (progn
          (insert φleftBracket φrightBracket)
          (search-backward φrightBracket ) )) )))

;; (insert-parentheses)

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )
(defun xah-insert-greater-less () (interactive) (xah-insert-bracket-pair "<" ">") )

(defun xah-insert-single-angle-quote‹› () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote«» () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-double-curly-quote“” () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-single-curly-quote‘’ () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-double-straight-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-single-straight-quote () (interactive) (xah-insert-bracket-pair "'" "'") )

(defun xah-insert-corner-bracket「」 () (interactive) (xah-insert-bracket-pair "「" "」") )
(defun xah-insert-white-corner-bracket『』 () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket〈〉 () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket《》 () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket〖〗 () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket【】 () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket〔〕 () (interactive) (xah-insert-bracket-pair "〔" "〕") )
