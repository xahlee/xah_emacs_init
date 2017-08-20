;;; xah-text-mode.el --- Major mode for plain text. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 0.1.20170820
;; Created: 29 April 2017
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 3.

;;; Commentary:

;; Major mode for editing text code. same enhancement for xah


;;; Code:

(defvar xah-text-mode-hook nil "Standard hook for `xah-text-mode'")



(defface xah-text-fc1
  '(
    (t :foreground "firebrick" :weight bold))
  "a face."
  :group 'xah-text-mode )

(defface xah-text-fc2
  '(
    (t :weight bold))
  "a face."
  :group 'xah-text-mode )

;; temp for debugging
(face-spec-set
 'xah-text-fc1
 '(
   (t :foreground "firebrick" :weight bold))
 'face-defface-spec
 )

(face-spec-set
 'xah-text-fc2
 '(
   (t :weight bold))
 'face-defface-spec
 )



(defvar xah-text-kword nil "List of words to highlight")

(setq xah-text-kword '( ))



(defun xah-text-display-page-break-as-line ()
  "Display the formfeed ^L char as line.
Version 2017-01-27"
  (interactive)
  ;; 2016-10-11 thanks to Steve Purcell's page-break-lines.el
  (progn
    (when (not buffer-display-table)
      (setq buffer-display-table (make-display-table)))
    (aset buffer-display-table ?\^L
          (vconcat (make-list 70 (make-glyph-code ?─ 'font-lock-comment-face))))
    (redraw-frame)))


;; syntax table
(defvar xah-text-mode-syntax-table nil "Syntax table for `xah-text-mode'.")
(setq xah-text-mode-syntax-table
      (let ((synTable (make-syntax-table)))

        (modify-syntax-entry ?\« "(»" synTable)
        (modify-syntax-entry ?\» ")«" synTable)
        (modify-syntax-entry ?\‹ "(›" synTable)
        (modify-syntax-entry ?\› ")‹" synTable)

        ;; (modify-syntax-entry ?\“ "(”" synTable)
        ;; (modify-syntax-entry ?\” ")“" synTable)

        (modify-syntax-entry ?\“ "<" synTable)
        (modify-syntax-entry ?\” ">" synTable)

        synTable))


;; syntax coloring related

(setq xah-text-font-lock-keywords
      (let (($kw (regexp-opt xah-text-kword 'words)))
        `(

          (,$kw . font-lock-function-name-face)

          )))


;; indent/reformat related


;; keybinding

(defvar xah-text-mode-map nil "Keybinding for `xah-text-mode'")



;;;###autoload
(define-derived-mode xah-text-mode fundamental-mode "∑text"
  "A major mode for plain text, some xah enhancement stuff
Version 2017-06-23
\\{xah-text-mode-map}"
  (setq font-lock-defaults '((xah-text-font-lock-keywords)))

  (setq-local comment-start "“")
  (setq-local comment-start-skip "“")
  (setq-local comment-end "”")
  (setq-local comment-end-skip "”")

  (xah-text-display-page-break-as-line)
  :group 'xah-text-mode
  )

(add-to-list 'auto-mode-alist '("\\.txt\\'" . xah-text-mode))

(provide 'xah-text-mode)

;;; xah-text-mode.el ends here

