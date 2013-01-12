;;-*- coding: utf-8 -*-
;; emacs customization.
;; • 〈Emacs Unicode Math Symbols Input Mode (xmsi-mode)〉 http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;; Xah Lee
;; 2011-05-27
;; ∑ http://xahlee.org/



;;;; matching pairs

(defun insert-bracket-pair (leftBracket rightBracket)
  "Insert a matching bracket.
If region is not active, place the cursor between them.
If region is active, insert around the region, place the cursor after the right bracket.

The argument leftBracket rightBracket are strings."
  (if (region-active-p)
      (let (
            (p1 (region-beginning))
            (p2 (region-end))
            )
        (goto-char p2)
        (insert rightBracket)
        (goto-char p1)
        (insert leftBracket)
        (goto-char (+ p2 2))
        )
    (progn
      (insert leftBracket rightBracket)
      (backward-char 1) ) )
  )

;; (insert-parentheses)

(defun insert-pair-paren () (interactive) (insert-bracket-pair "(" ")") )
(defun insert-pair-bracket () (interactive) (insert-bracket-pair "[" "]") )
(defun insert-pair-brace () (interactive) (insert-bracket-pair "{" "}") )

(defun insert-pair-single-angle-quote‹› () (interactive) (insert-bracket-pair "‹" "›") )
(defun insert-pair-double-angle-quote«» () (interactive) (insert-bracket-pair "«" "»") )
(defun insert-pair-double-curly-quote“” () (interactive) (insert-bracket-pair "“" "”") )
(defun insert-pair-single-curly-quote‘’ () (interactive) (insert-bracket-pair "‘" "’") )
(defun insert-pair-double-straight-quote () (interactive) (insert-bracket-pair "\"" "\"") )
(defun insert-pair-single-straight-quote () (interactive) (insert-bracket-pair "'" "'") )

(defun insert-pair-corner-bracket「」 () (interactive) (insert-bracket-pair "「" "」") )
(defun insert-pair-white-corner-bracket『』 () (interactive) (insert-bracket-pair "『" "』") )
(defun insert-pair-angle-bracket〈〉 () (interactive) (insert-bracket-pair "〈" "〉") )
(defun insert-pair-double-angle-bracket《》 () (interactive) (insert-bracket-pair "《" "》") )
(defun insert-pair-white-lenticular-bracket〖〗 () (interactive) (insert-bracket-pair "〖" "〗") )
(defun insert-pair-black-lenticular-bracket【】 () (interactive) (insert-bracket-pair "【" "】") )
(defun insert-pair-tortoise-shell-bracket〔〕 () (interactive) (insert-bracket-pair "〔" "〕") )
