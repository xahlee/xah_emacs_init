;; -*- coding: utf-8 -*-
;; 2014-05-29

;; how to test if something is overlay?
;; just copy it, paste it in new buffer. if the highlighting etc disappears, than it's overlay

;; mark with transient on also seems to set a overlay

(defun xah-make-overlay-bold-region (φp1 φp2)
  "make the region bold, using overlay.
Calls `make-overlay' and `overlay-put'. This:
 (overlay-put (make-overlay φp1 φp2) 'face 'bold)
"
  (interactive "r")
  (let ()
    (overlay-put (make-overlay φp1 φp2) 'face 'bold)
    (setq mark-active nil )
    ))

(defun xah-remove-overlays-region (φp1 φp2)
  "Call `remove-overlays' interactively.
Call (remove-overlays φp1 φp2)"
  (interactive "r")
  (let ()
    (remove-overlays φp1 φp2)
  ))

(defun xah-show-overlay-at-point ()
  "Show the overlay at cursor position (if any).
Move cursor to begining of first overlay, mark at the overlay's end. And print the overlay object to message buffer."
  (interactive)
  (let ((ξoverlays (overlays-at (point))))
    (if ξoverlays
        (progn 
          (goto-char (overlay-start (nth 0 ξoverlays)) )
          (push-mark (overlay-end (nth 0 ξoverlays)) )
          (print (overlay-properties (nth 0 ξoverlays)) )
          )
      (progn (message "No overlay found.") )
      )
    ))

(defun xah-goto-next-overlay ()
  "Goto next overlay.
call `next-overlay-change'.
move cursor there.
note: it seems `linum-mode' sets a overlay on every line, i think.
"
  (interactive)
  (let* ((ξol-pos (next-overlay-change (point)))
        )
    (goto-char ξol-pos )
    ))

(defun xah-goto-previous-overlay ()
  "Goto next overlay.
call `next-overlay-change'.
"
  (interactive)
  (let* ((ξol-pos (previous-overlay-change (point)))
        )
    (goto-char ξol-pos)
    ))

(defun xah-show-all-overlays ()
  "Call `overlay-in'.
 (overlays-in (point-min) (point-max))
print the list result.
"
  (interactive)
  (let* ((ξols (overlays-in (point-min) (point-max) ) )
         )
    (print ξols)
    ))

;; Move cursor to the next begin/end position of a overlay, make a text selection of its region.

;; — Function: overlays-in beg end

;;     This function returns a list of the overlays that overlap the region beg through end. “Overlap” means that at least one character is contained within the overlay and also contained within the specified region; however, empty overlays are included in the result if they are located at beg, strictly between beg and end, or at end when end denotes the position at the end of the buffer. 

;; — Function: next-overlay-change pos

;;     This function returns the buffer position of the next beginning or end of an overlay, after pos. If there is none, it returns (point-max). 

;; — Function: previous-overlay-change pos

;;     This function returns the buffer position of the previous beginning or end of an overlay, before pos. If there is none, it returns (point-min). 

;; As an example, here's a simplified (and inefficient) version of the primitive function next-single-char-property-change (see Property Search). It searches forward from position pos for the next position where the value of a given property prop, as obtained from either overlays or text properties, changes.

;;      (defun next-single-char-property-change (position prop)
;;        (save-excursion
;;          (goto-char position)
;;          (let ((propval (get-char-property (point) prop)))
;;            (while (and (not (eobp))
;;                        (eq (get-char-property (point) prop) propval))
;;              (goto-char (min (next-overlay-change (point))
;;                              (next-single-property-change (point) prop)))))
;;          (point)))


;; (put-text-property
;;  (line-beginning-position)
;;  (line-end-position)
;;  'face 'bold)

;; (put-text-property
;;  (line-beginning-position)
;;  (line-end-position)
;;  'font-lock-face 'bold)

;; (overlay-put (make-overlay 4 14) 'invisible t)

;; ;; If you want to display an ellipsis:
;; (add-to-invisibility-spec '(my-symbol . t))
;; ;; If you don't want ellipsis:
;; (add-to-invisibility-spec 'my-symbol)

;; ;; When done with the overlays:
;; (remove-from-invisibility-spec '(my-symbol . t))
;; ;; Or respectively:
;; (remove-from-invisibility-spec 'my-symbol)
