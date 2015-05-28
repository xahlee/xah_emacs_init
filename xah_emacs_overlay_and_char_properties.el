;; -*- coding: utf-8 -*-
;; 2014-05-29

;; (put-text-property 1 20 'face (list :background 'yellow))
;; (put-text-property 1 20 'face 'highlight)


;; (defface page-break-lines
;;   '((t :inherit font-lock-comment-face :bold nil :italic nil))
;;   "Face used to colorize page break lines.
;; If using :bold or :italic, please ensure `page-break-lines-char'
;; is available in that variant of your font, otherwise it may be
;; displayed as a junk character."
;;   :group 'page-break-lines)

;; how to test if something is overlay?
;; just copy it, paste it in new buffer. if the highlighting etc disappears, than it's overlay

;; mark with transient on also seems to set a overlay

;; (defun xah-get-char-property-and-overlay ()
;;   "DOCSTRING"
;;   (interactive)
;;   (let ()
;;     (get-char-property-and-overlay (point) PROP &optional)))

;; (defun xah-syntax-color-hex ()
;;   "Syntax color text of the form 「#ff1100」 in current buffer."
;;   (interactive)
;;   (font-lock-add-keywords
;;    nil
;;    '(("#[abcdef[:digit:]]\\{6\\}"
;;       (0 (put-text-property
;;           (match-beginning 0)
;;           (match-end 0)
;;           'face (list :background (match-string-no-properties 0)))))))
;;   (font-lock-fontify-buffer))

(defun xah-make-overlay-bold-region (φbegin φend)
  "make the region bold, using overlay.
Calls `make-overlay' and `overlay-put'. This:
 (overlay-put (make-overlay φbegin φend) 'face 'bold)"
  (interactive "r")
  (progn
    (overlay-put (make-overlay φbegin φend) 'face 'bold)
    (setq mark-active nil )))

(defun xah-remove-overlays-region (φbegin φend)
  "Call `remove-overlays' interactively.
Call (remove-overlays φbegin φend)"
  (interactive "r")
  (remove-overlays φbegin φend))

(defun xah-show-overlay-at-point ()
  "Show the overlay at cursor position (if any).
Move cursor to begining of first overlay, mark at the overlay's end. And print the overlay object to message buffer."
  (interactive)
  (let ((ξoverlays (overlays-at (point))))
    (if ξoverlays
        (progn
          (goto-char (overlay-start (nth 0 ξoverlays)))
          (push-mark (overlay-end (nth 0 ξoverlays)))
          (print (overlay-properties (nth 0 ξoverlays))))
      (progn (message "No overlay found.")))))

(defun xah-goto-next-overlay ()
  "Goto next overlay.
call `next-overlay-change'.
move cursor there.
note: it seems `linum-mode' sets a overlay on every line, i think."
  (interactive)
  (let* ((ξol-pos (next-overlay-change (point))))
    (goto-char ξol-pos )))

(defun xah-goto-previous-overlay ()
  "Goto next overlay.
call `next-overlay-change'."
  (interactive)
  (let* ((ξol-pos (previous-overlay-change (point))))
    (goto-char ξol-pos)))

(defun xah-show-all-overlays ()
  "Call `overlay-in'.
 (overlays-in (point-min) (point-max))
print the list result."
  (interactive)
  (let* ((ξols (overlays-in (point-min) (point-max))))
    (print ξols)))

(defun xah-invisible-region (φbegin φend hide-p)
  "Hide or show region ΦBEGIN to ΦEND, according to HIDE-P."
  (remove-overlays φbegin φend 'invisible 'xah)
  (when hide-p
    (let ((ξovly (make-overlay φbegin φend nil 'front-advance)))
      (overlay-put ξovly 'evaporate t)
      (overlay-put ξovly 'invisible 'xah)
      (overlay-put ξovly 'isearch-open-invisible 'xah-isearch-open-invisible))))

;; add-to-invisibility-spec
;; remove-from-invisibility-spec
;; buffer-invisibility-spec

;; (add-to-invisibility-spec '(outline . t))

(defun xhide (φbegin φend)
  "DOCSTRING"
  (interactive "r")
  (progn
    (xah-invisible-region 50 100 t)))

(defun xshow (φbegin φend)
  "DOCSTRING"
  (interactive "r")
  (progn
    (xah-invisible-region 50 100 nil)))

;; Move cursor to the next begin/end position of a overlay, make a text selection of its region.

;; (overlay-put (make-overlay 4 14) 'invisible t)

;; ;; If you want to display an ellipsis:
;; (add-to-invisibility-spec '(my-symbol . t))
;; ;; If you don't want ellipsis:
;; (add-to-invisibility-spec 'my-symbol)

;; ;; When done with the overlays:
;; (remove-from-invisibility-spec '(my-symbol . t))
;; ;; Or respectively:
;; (remove-from-invisibility-spec 'my-symbol)
