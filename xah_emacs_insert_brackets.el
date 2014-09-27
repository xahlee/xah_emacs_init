;;-*- coding: utf-8 -*-
;; emacs customization.
;; 〈Emacs Lisp: Insert Brackets by Pair〉 http://ergoemacs.org/emacs/elisp_insert_brackets_by_pair.html
;; 〈Emacs Unicode Math Symbols Input Mode (xmsi-mode)〉 http://ergoemacs.org/emacs/xmsi-math-symbols-input.html

;; Xah Lee
;; 2011-05-27
;; ∑ http://xahlee.org/



(defun xah-insert-bracket-pair (φleft-bracket φright-bracket)
  "Insert a matching bracket and place cursor in between.

If there's a text selection, insert brackets around it.
If there's no text selection:
  If the char before cursor is alphanumeric, insert brackets around current word.
  else, insert brackets at cursor position.

Alphanumeric char here includes hyphen and underscore.

The argument φleft-bracket φright-bracket are strings."
  (if (use-region-p)
      (progn
        (let (
              (p1 (region-beginning))
              (p2 (region-end)))
          (goto-char p2)
          (insert φright-bracket)
          (goto-char p1)
          (insert φleft-bracket)
          (goto-char (+ p2 2))))
    (progn ; no text selection
      (if (or
           (looking-at "[_-A-Za-z0-9]"))
          (progn
            (let ( p1 p2)
              (skip-chars-backward "-A-Za-z0-9")
              (setq p1 (point))
              (skip-chars-forward "-A-Za-z0-9")
              (setq p2 (point))
              (goto-char p2)
              (insert φright-bracket)
              (goto-char p1)
              (insert φleft-bracket)
              (goto-char (+ p2 (length φleft-bracket)))))
        (progn
          (insert φleft-bracket φright-bracket)
          (search-backward φright-bracket ))))))

;; (insert-parentheses)

(defun xah-insert-paren () (interactive) (xah-insert-bracket-pair "(" ")") )
(defun xah-insert-bracket () (interactive) (xah-insert-bracket-pair "[" "]") )
(defun xah-insert-brace () (interactive) (xah-insert-bracket-pair "{" "}") )
(defun xah-insert-greater-less () (interactive) (xah-insert-bracket-pair "<" ">") )

(defun xah-insert-single-angle-quote‹› () (interactive) (xah-insert-bracket-pair "‹" "›") )
(defun xah-insert-double-angle-quote«» () (interactive) (xah-insert-bracket-pair "«" "»") )
(defun xah-insert-double-curly-quote“” () (interactive) (xah-insert-bracket-pair "“" "”") )
(defun xah-insert-curly-single-quote‘’ () (interactive) (xah-insert-bracket-pair "‘" "’") )
(defun xah-insert-ascii-double-quote () (interactive) (xah-insert-bracket-pair "\"" "\"") )
(defun xah-insert-ascii-single-quote () (interactive) (xah-insert-bracket-pair "'" "'") )
(defun xah-insert-emacs-quote () (interactive) (xah-insert-bracket-pair "`" "'") )
(defun xah-insert-corner-bracket「」 () (interactive) (xah-insert-bracket-pair "「" "」") )
(defun xah-insert-white-corner-bracket『』 () (interactive) (xah-insert-bracket-pair "『" "』") )
(defun xah-insert-angle-bracket〈〉 () (interactive) (xah-insert-bracket-pair "〈" "〉") )
(defun xah-insert-double-angle-bracket《》 () (interactive) (xah-insert-bracket-pair "《" "》") )
(defun xah-insert-white-lenticular-bracket〖〗 () (interactive) (xah-insert-bracket-pair "〖" "〗") )
(defun xah-insert-black-lenticular-bracket【】 () (interactive) (xah-insert-bracket-pair "【" "】") )
(defun xah-insert-tortoise-shell-bracket〔〕 () (interactive) (xah-insert-bracket-pair "〔" "〕") )
