;; -*- coding: utf-8; lexical-binding: t; -*-
;; some font related emacs commands

;; 2011-03-08
;;   Xah Lee
;; ∑ http://xahlee.info/



(progn
  ;; set a default font
  (cond
   ((string-equal system-type "gnu/linux")
    (when (member "DejaVu Sans Mono" (font-family-list))
      (set-face-attribute 'default nil :font "DejaVu Sans Mono"))
    ;; specify font for chinese characters using default chinese font on linux
    (when (member "WenQuanYi Micro Hei" (font-family-list))
      (set-fontset-font t '(#x4e00 . #x9fff) "WenQuanYi Micro Hei" ))
    ;;
    )
   ((string-equal system-type "darwin") ; Mac
    (when (member "Courier" (font-family-list)) (set-face-attribute 'default nil :font "Courier-14"))
    (when (member "Menlo" (font-family-list)) (set-face-attribute 'default nil :font "Menlo-14"))
    ;;
    )
   ((string-equal system-type "windows-nt") ; Windows
    (progn
      nil)))

  ;; specify font for all unicode characters
  (when (member "Symbola" (font-family-list))
    (set-fontset-font t 'unicode "Symbola" nil 'prepend))

  ;; ;; specify font for all unicode characters
  ;; (when (member "Apple Color Emoji" (font-family-list))
  ;;   (set-fontset-font t 'unicode "Apple Color Emoji" nil 'prepend))

  ;;
  )


(defun xah-cycle-font-2 (@n)
  "Change font in current window between 2 fonts.

URL `http://ergoemacs.org/emacs/emacs_switching_fonts.html'
Version 2015-09-21"
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are 0 to length of $fontList
  (let (
        ($fontList '("Courier-14" "Menlo-14"))
        $fontToUse
        $stateBefore
        $stateAfter)

    (setq $stateBefore (if (get 'xah-cycle-font-2 'state) (get 'xah-cycle-font-2 'state) 0))
    (setq $stateAfter (% (+ $stateBefore (length $fontList) @n) (length $fontList)))
    (put 'xah-cycle-font-2 'state $stateAfter)

    (setq $fontToUse (nth $stateAfter $fontList))
    (set-frame-parameter nil 'font $fontToUse)
    (message "Font set to: %s" $fontToUse)))



(defvar xah-font-list nil "A list of fonts for `xah-cycle-font' to cycle from.")

(setq xah-font-list
      (cond
       ((string-equal system-type "windows-nt")
        '(
          "Courier-10"
          "Lucida Console-10"
          "Segoe UI Symbol-12"
          "Lucida Sans Unicode-10"
          ))
       ((string-equal system-type "gnu/linux")
        '(
          "DejaVu Sans Mono-10"
          "DejaVu Sans-10"
          "Symbola-13"
          ))
       ((string-equal system-type "darwin") ; Mac
        '("Courier-14"
          "Menlo-14"))))

(defun xah-cycle-font (@n)
  "Change font in current frame.
Each time this is called, font cycles thru a predefined list of fonts in the variable `xah-font-list' .
If @n is 1, cycle forward.
If @n is -1, cycle backward.
See also `xah-cycle-font-next', `xah-cycle-font-previous'.

URL `http://ergoemacs.org/emacs/emacs_switching_fonts.html'
Version 2015-09-21"
  (interactive "p")
  ;; this function sets a property “state”. It is a integer. Possible values are any index to the fontList.
  (let ($fontToUse $stateBefore $stateAfter )
    (setq $stateBefore (if (get 'xah-cycle-font 'state) (get 'xah-cycle-font 'state) 0))
    (setq $stateAfter (% (+ $stateBefore (length xah-font-list) @n) (length xah-font-list)))
    (setq $fontToUse (nth $stateAfter xah-font-list))
    (set-frame-font $fontToUse t)
    ;; (set-frame-parameter nil 'font $fontToUse)
    (message "Current font is: %s" $fontToUse )
    (put 'xah-cycle-font 'state $stateAfter)))

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


