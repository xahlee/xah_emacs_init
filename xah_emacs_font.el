;; -*- coding: utf-8 -*-
;; some font related emacs commands

;; 2011-03-08
;;   Xah Lee
;; ∑ http://xahlee.org/

;; Emacs: How to List ＆ Set Font
;; http://ergoemacs.org/emacs/emacs_list_and_set_font.html

;; Emacs: How to Quickly Switch Fonts
;; http://ergoemacs.org/emacs/emacs_switching_fonts.html



;; use variable-width font for some modes
(defun xah-font-change ()
  "Set current buffer to use variable-width font."
  (variable-pitch-mode 1)
  ;; (text-scale-increase 0.5 )
  )

;; (add-hook 'xah-html-mode-hook 'xah-font-change)



(defun xah-cycle-font-2 (φn)
  "Change font in current window between 2 fonts."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (
        (ξ-font-list (list "DejaVu Sans Mono-10" "DejaVu Sans-10" ))
        fontToUse
        stateBefore
        stateAfter )

    (setq stateBefore (if (get 'xah-cycle-font-2 'state) (get 'xah-cycle-font-2 'state) 0))
    (setq stateAfter (% (+ stateBefore (length ξ-font-list) φn) (length ξ-font-list)))
    (put 'xah-cycle-font-2 'state stateAfter)

    (setq fontToUse (nth stateAfter ξ-font-list))
    ;; (set-frame-font fontToUse t)
    (set-frame-parameter nil 'font fontToUse)
    (message "Current font is: %s" fontToUse )))



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
                 ))
              ((string-equal system-type "gnu/linux")
               '(
                 "DejaVu Sans Mono-9"
                 "DejaVu Sans-9"
                 "Symbola-13"
                 ))
              ((string-equal system-type "darwin") ; Mac
               '(
                 "DejaVu Sans Mono-9"
                 "DejaVu Sans-9"
                 "Symbola-13"
                 ))))

(defun xah-cycle-font (φn)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined list of fonts in the variable `ξ-font-list' .
If φn is 1, cycle forward.
If φn is -1, cycle backward.
See also `xah-cycle-font-next', `xah-cycle-font-previous'."
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let (fontToUse stateBefore stateAfter )
    (setq stateBefore (if (get 'xah-cycle-font 'state) (get 'xah-cycle-font 'state) 0))
    (setq stateAfter (% (+ stateBefore (length ξ-font-list) φn) (length ξ-font-list)))

    (setq fontToUse (nth stateAfter ξ-font-list))
    (set-frame-font fontToUse t)
    ;; (set-frame-parameter nil 'font fontToUse)
    (message "Current font is: %s" fontToUse )
    (put 'xah-cycle-font 'state stateAfter)))

(defun xah-cycle-font-next ()
  "Switch to the next font, in current window.
See `xah-cycle-font'."
  (interactive)
  (xah-cycle-font 1))

(defun xah-cycle-font-previous ()
  "Switch to the previous font, in current window.
See `xah-cycle-font'."
  (interactive)
  (xah-cycle-font -1))



(defun xah-toggle-line-spacing ()
  "Toggle line spacing between no extra space to extra half line height."
  (interactive)
  (if (null line-spacing)
      (setq line-spacing 0.5) ; add 0.5 height between lines
    (setq line-spacing nil)   ; no extra heigh between lines
    )
  (redraw-frame (selected-frame)))

(defun xah-toggle-margin-right ()
  "Toggle the right margin between `fill-column' or window width.
This command is convenient when reading novel, documentation."
  (interactive)
  (if (null (cdr (window-margins)))
      (set-window-margins nil 0 (- (window-body-width) fill-column))
    (set-window-margins nil 0 0) ) )

(defun xah-toggle-read-novel-mode ()
  "Setup current window to be suitable for reading long novel/article text.

• Line wrap at word boundaries. 
• Set a right margin.
• line spacing is increased.
• variable width font is used.

Call again to toggle back."
  (interactive)
  (if (null (get this-command 'state-on-p))
      (progn
        (set-window-margins nil 0 
                            (if (> fill-column (window-body-width))
                                0
                              (progn
                                (- (window-body-width) fill-column))))
        (variable-pitch-mode 1)
        (setq line-spacing 0.4)
        (setq word-wrap t)
        (put this-command 'state-on-p t))
    (progn
      (set-window-margins nil 0 0)
      (variable-pitch-mode 0)
      (setq line-spacing nil)
      (setq word-wrap nil)
      (put this-command 'state-on-p nil)))
  (redraw-frame (selected-frame)))

;; correct syntax for some fonts (tested on Windows Vista)
;; "-*-Courier New-normal-r-*-*-14-112-96-96-c-*-iso8859-1"
;; "-outline-Lucida Sans Unicode-normal-normal-normal-sans-13-*-*-*-p-*-iso8859-1"
;; "-outline-Code2000-normal-normal-normal-*-15-*-*-*-p-*-iso8859-1"
;; "-raster-Fixedsys-normal-normal-normal-mono-15-*-*-*-c-*-iso8859-1"
;; "-outline-FixedsysTTF-semi-bold-normal-normal-mono-16-*-*-*-c-*-iso8859-1"

;; fixed-width "Courier New" "Unifont"  "FixedsysTTF" "Miriam Fixed" "Lucida Console" "Lucida Sans Typewriter" "DejaVu Sans Mono-10" "Lucida Console-10"
;; variable-width "Arial Unicode MS-10" "Code2000" "STIXGeneral" "Lucida Console-10"
