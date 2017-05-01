;;; xah-text-mode.el --- Major mode for plain text. -*- coding: utf-8; lexical-binding: t; -*-

;; Copyright © 2017 by Xah Lee

;; Author: Xah Lee ( http://xahlee.info/ )
;; Version: 0.1.0
;; Created: 29 April 2017
;; Package-Requires: ((emacs "25.1"))
;; Keywords: convenience

;; This file is not part of GNU Emacs.

;;; License:

;; You can redistribute this program and/or modify it under the terms of the GNU General Public License version 2.

;;; Commentary:

;; Major mode for editing text code. same enhancement for xah


;;; Code:

(defvar xah-text-mode-hook nil "Standard hook for `xah-text-mode'")



(defface xah-text-fc1
  '(
    (t :foreground "firebrick" :weight bold))
  "face for CSS ID selector “#…”."
  :group 'xah-text-mode )

(defface xah-text-fc2
  '(
    (t :weight bold))
  "face for CSS class name selector “.…”."
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

 

(defvar xah-text-kword nil "List of HTML5 tag names.") 

(setq xah-text-kword '( "abbr" "address" "applet" "area" "article" "aside" "audio" ))



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
      (let ((-kw (regexp-opt xah-text-kword 'words)))
        `(
          ("#[-_a-zA-Z]+[-_a-zA-Z0-9]*" . 'xah-text-fc1)
          ("\\.[a-zA-Z]+[-_a-zA-Z0-9]*" . 'xah-text-fc2)
            
          (,-kw . font-lock-function-name-face)
          
          )))


;; indent/reformat related


;; keybinding

(defvar xah-text-mode-map nil "Keybinding for `xah-text-mode'")



;;;###autoload
(define-derived-mode xah-text-mode fundamental-mode "ξxt"
  "A major mode for plain text, some xah enhancement stuff
v 2017-04-29
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

