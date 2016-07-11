;; -*- coding: utf-8 -*-
;; 2014-05-29

;; (put-text-property 1 20 'face 'highlight) ; works
;; (put-text-property 1 20 'face (list :background "#ff4433")) ; works
;; (put-text-property 1 20 'face (list :background "yellow")) ; works

;; (add-text-properties 1 20 '(mouse-face highlight help-echo "mouse-2: open this file in other window")) ; works

;; (add-text-properties 1 20 '(face highlight)) ; works
;; (add-text-properties 1 20 '(face highlight fontified t )) ; works
;; (add-text-properties 1 20 '(font-lock-face highlight fontified t)) ; works

;; (set-text-properties 1 20 nil)
;; (get-text-property 2 'face)
;; (propertize "foo" 'face 'italic 'mouse-face 'bold-italic)

(defun xah-show-text-properties ()
  "print properties of the char after `point'"
  (interactive)
  (message "%s" (text-properties-at (point))))

;; (defface xah-xx
;;   '((t :inherit font-lock-comment-face :bold nil :italic nil))
;;   "Face my face."
;;   :group 'xah-xx)

;; how to test if something is overlay?
;; just copy it, paste it in new buffer. if the highlighting etc disappears, than it's overlay

;; mark with transient on also seems to set a overlay

;; (defun xah-get-char-property-and-overlay ()
;;   "DOCSTRING"
;;   (interactive)
;;   (let ()
;;     (get-char-property-and-overlay (point) PROP &optional)))

(defun xah-make-overlay-bold-region (*begin *end)
  "make the region bold, using overlay.
Calls `make-overlay' and `overlay-put'. This:
 (overlay-put (make-overlay *begin *end) 'face 'bold)"
  (interactive "r")
  (progn
    (overlay-put (make-overlay *begin *end) 'face 'bold)
    (setq mark-active nil )))

(defun xah-remove-overlays-region (*begin *end)
  "Call `remove-overlays' interactively.
Call (remove-overlays *begin *end)"
  (interactive "r")
  (remove-overlays *begin *end))

(defun xah-show-overlay-at-point ()
  "Show the overlay at cursor position (if any).
Move cursor to begining of first overlay, mark at the overlay's end. And print the overlay object to message buffer."
  (interactive)
  (let ((-overlays (overlays-at (point))))
    (if -overlays
        (progn
          (goto-char (overlay-start (nth 0 -overlays)))
          (push-mark (overlay-end (nth 0 -overlays)))
          (print (overlay-properties (nth 0 -overlays))))
      (progn (message "No overlay found.")))))

(defun xah-goto-next-overlay ()
  "Goto next overlay.
call `next-overlay-change'.
move cursor there.
note: it seems `linum-mode' sets a overlay on every line, i think."
  (interactive)
  (let* ((-ol-pos (next-overlay-change (point))))
    (goto-char -ol-pos )))

(defun xah-goto-previous-overlay ()
  "Goto next overlay.
call `next-overlay-change'."
  (interactive)
  (let* ((-ol-pos (previous-overlay-change (point))))
    (goto-char -ol-pos)))

(defun xah-show-all-overlays ()
  "Call `overlay-in'.
 (overlays-in (point-min) (point-max))
print the list result."
  (interactive)
  (let* ((-ols (overlays-in (point-min) (point-max))))
    (print -ols)))

(defun xah-invisible-region (*begin *end hide-p)
  "Hide or show region ΦBEGIN to ΦEND, according to HIDE-P."
  (remove-overlays *begin *end 'invisible 'xah)
  (when hide-p
    (let ((-ovly (make-overlay *begin *end nil 'front-advance)))
      (overlay-put -ovly 'evaporate t)
      (overlay-put -ovly 'invisible 'xah)
      (overlay-put -ovly 'isearch-open-invisible 'xah-isearch-open-invisible))))

;; add-to-invisibility-spec
;; remove-from-invisibility-spec
;; buffer-invisibility-spec

;; (add-to-invisibility-spec '(outline . t))

(defun xhide (*begin *end)
  "DOCSTRING"
  (interactive "r")
  (progn
    (xah-invisible-region 50 100 t)))

(defun xshow (*begin *end)
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
