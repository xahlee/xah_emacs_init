;; -*- coding: utf-8 -*-
;; some font related emacs commands

;; 2011-03-08
;;   Xah Lee
;; ∑ http://xahlee.org/

;; correct syntax for some fonts (tested on Windows Vista)
;; "-*-Courier New-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
;; "-outline-Lucida Sans Unicode-normal-normal-normal-sans-13-*-*-*-p-*-iso8859-1"
;; "-outline-Code2000-normal-normal-normal-*-15-*-*-*-p-*-iso8859-1"
;; "-raster-Fixedsys-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
;; "-outline-FixedsysTTF-semi-bold-normal-normal-mono-16-*-*-*-c-*-iso8859-1"

;; fixed-width "Courier New" "Unifont"  "FixedsysTTF" "Miriam Fixed" "Lucida Console" "Lucida Sans Typewriter" "DejaVu Sans Mono-10" "Lucida Console-10"
;; variable-width "Arial Unicode MS-10" "Code2000" "STIXGeneral" "Lucida Console-10"

(defun xah-set-font-to-monospace ()
  "Change font in current frame to a monospaced one."
  (interactive)
  (set-frame-parameter nil 'font "DejaVu Sans Mono")
  )

(defun set-font-to-variable-width ()
  "Change font in current frame to a monospaced one."
  (interactive)
  (set-frame-parameter nil 'font "DejaVu Sans")
  )

(defun xah-cycle-font-2 (ξ-n)
  "Change font in current frame between 2 fonts."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (ξ-font-list fontToUse stateBefore stateAfter )
    (setq ξ-font-list (list "DejaVu Sans Mono-10" "DejaVu Sans-10" ))

    (setq stateBefore (if (get 'xah-cycle-font-2 'state) (get 'xah-cycle-font-2 'state) 0))
    (setq stateAfter (% (+ stateBefore (length ξ-font-list) ξ-n) (length ξ-font-list)))
    (put 'xah-cycle-font-2 'state stateAfter)

    (setq fontToUse (nth stateAfter ξ-font-list))
    (set-frame-parameter nil 'font fontToUse)
    (redraw-frame (selected-frame))
    (message "Current font is: %s" fontToUse )
    )
  )

(defcustom ξ-font-list nil "A list of fonts for `xah-cycle-font' to cycle from." :group 'font)

(set-default 'ξ-font-list 
(cond
 ((string-equal system-type "windows-nt") ; Windows
'(
                         "Courier New-10"
                         "DejaVu Sans Mono-9"
                         "Lucida Console-10"
                         "Segoe UI Symbol-12"

                         "DejaVu Sans-10"
                         "Lucida Sans Unicode-10"
                         "Arial Unicode MS-10"
                         )
  )
 ((string-equal system-type "gnu/linux")
 '(
                         "DejaVu Sans Mono-9"
                         "DejaVu Sans-9"
                         "Symbola-13"
                         )
 )
 ((string-equal system-type "darwin") ; Mac
  '(
                         "DejaVu Sans Mono-9"
                         "DejaVu Sans-9"
                         "Symbola-13"
                         ) ) )
)

(defun xah-cycle-font (ξ-n)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined list of fonts in the variable `ξ-font-list' .
If ξ-n is 1, cycle forward.
If ξ-n is -1, cycle backward.
see also `xah-cycle-font-forward', `xah-cycle-font-backward'
"
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontToUse stateBefore stateAfter )
    (setq stateBefore (if (get 'xah-cycle-font 'state) (get 'xah-cycle-font 'state) 0))
    (setq stateAfter (% (+ stateBefore (length ξ-font-list) ξ-n) (length ξ-font-list)))

    (setq fontToUse (nth stateAfter ξ-font-list))
    (set-frame-parameter nil 'font fontToUse)
    (redraw-frame (selected-frame))
    (message "Current font is: %s" fontToUse )

    (put 'xah-cycle-font 'state stateAfter) ) )

(defun xah-cycle-font-forward ()
  "Switch to the next font, in the current frame.
See `xah-cycle-font'."
  (interactive)
  (xah-cycle-font 1)
  )

(defun xah-cycle-font-backward ()
  "Switch to the previous font, in the current frame.
See `xah-cycle-font'."
  (interactive)
  (xah-cycle-font -1)
  )

;; (defun set-font-by-mode ()
;;   "Change font in current frame according to the major mode."
;;   (interactive)
;; (message (symbol-name major-mode)  )
;;   (cond
;;    ((equal major-mode 'dired-mode) (set-frame-parameter nil 'font "Courier New-10") (message "thth"))
;;    (t (set-frame-parameter nil 'font "Lucida Sans Unicode-10") (message "bahhh"))
;;    )
;;   )

(defun xah-unfontify-selection-or-block ()
  "Unfontify text selection or current block of text.
See also: `font-lock-fontify-block', `font-lock-fontify-buffer'."
  (interactive)
  (let (bds p1 p2 )
    (setq bds (get-selection-or-unit 'block))
    (setq p1 (elt bds 1) p2 (elt bds 2)  )
    (font-lock-unfontify-region p1 p2)
    )
  )

(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (eq line-spacing nil)
      (setq-default line-spacing 0.5) ; add 0.5 height between lines
    (setq-default line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-display))

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (eq (cdr (window-margins)) nil)
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )
