;-*- coding: utf-8 -*-
; Mac OS X Emacs related emacs customization

; 2007-06, 2009-08-04
;   Xah Lee
; ∑ http://xahlee.org/


;; § ----------------------------------------

;; start emacs with GUI from shell
;; /Applications/Emacs.app/Contents/MacOS/Emacs 

;; start emacsclient from shell  
;; Applications/Emacs.app/Contents/MacOS/bin/emacsclient 

(defun open-with-textwrangler ()
  "Open the current file in Mac's TextWrangler."
  (interactive)
      (if (eq major-mode 'dired-mode)
          (shell-command "open .")
        (shell-command
         (concat "open -a /Applications/TextWrangler.app " "\"" (buffer-file-name) "\""))))

(defun open-with-xcode ()
  "Open the current file in Apple's Xcode."
  (interactive)
  (shell-command
   (concat "open -a /Developer/Applications/Xcode.app " "\"" (buffer-file-name) "\"")))

(defun open-zir ()
 "Launch the “Compass And Ruler” Java program with file in current buffer."
 (interactive)
 (let (prog-path arg-str cmd-str)
   (setq prog-path "/usr/bin/java")
   (setq arg-str
         (concat " -jar /Applications/vrici/cmaci/CaR/doc_en/zirkel.jar "
                 buffer-file-name))
   (setq cmd-str  (concat prog-path " " arg-str " &"))
;   (start-process "myprocesss" "myp-buff" prog-path arg-str)
;   (start-process-shell-command "myprocesss" nil prog-path arg-str)
   (call-process-shell-command cmd-str nil 0 nil)
   ;(shell-command cmd-str)
   )
 )


;; § ----------------------------------------
; fonts

; make available extra CJK-font for carbon emacs in the menu
(if (eq window-system 'mac) (require 'carbon-font))

; about carbon emacs fonts, see /Applications/Emacs.app/Contents/Resources/site-lisp/mac/carbon-font.el
(if (featurep 'carbon-emacs-package) (fixed-width-set-default-fontset "-*-*-medium-r-normal--14-*-*-*-*-*-fontset-hirakaku_w6"))



;; § ----------------------------------------

; Carbon Emacs doesn't inherit shell's env.  A workaround is to shart it from shell like this: “/Applications/Emacs.app/Contents/MacOS/Emacs &” however, exiting the shell by Ctrl+D also kills your emacs. So a workaround is to use the nohub: “nohup /Applications/Emacs.app/Contents/MacOS/Emacs &”.  However, Apple's Term has a feature such that closing the window (by clicking on the Red Light or Cmd+w) kills all processes started from it.  So, the solution is to never close window by clicking or Cmd+w, instead, always use Ctrl+d. However, when ssh to remote server and running “tail -f”, sometimes broken connection causes the session to hang. So you are left with a frozen ssh session where the remote server has disconnected. So, here you can't use Ctrl-d to close the window. If you close the window by clicking, then you'll kill any emacs you might have started from that window. Hot Damn.  So, the optimal solution seems to get Carbon Emacs to start using the Mac's GUI way of inheriting env vars by the file “~/.MacOSX/environment.plist”. Though, that mean you gonna maintain two sets of env vars. Shit. Therefore at the end, maybe just dup it in elisp and forget about all quality solutions. Software Industry is quite fucked anyway. This is peanuts. All this is due to the motherfucking unix fucking env var system fuck. Fuck unix and fuck the unixer's mothers.


(setenv "PATH" "/Users/xah/bin:/usr/local/bin:/opt/local/bin:/Developer/Tools:/sw/bin:/sw/sbin:/bin:/sbin:/usr/bin:/usr/sbin:/usr/X11R6/bin:/usr/X11R6/bin")

(setenv "MANPATH" "/usr/local/man/:/sw/share/man:/usr/share/man:/usr/X11R6/man:/sw/lib/perl5/5.8.6/man")


; make carbon emacs open drag'n'dropped file ; this is in Carbon emacs as of 2006-06
; (load "/Users/xah/Documents/emacs/mac-drag-N-drop" t)

; if carbon emacs is started from the terminal, it will use the info at /usr/share/info/ that shipped with OS X, which don't have elisp doc. This is fixed sometimes in 2007 or before.
; (setq Info-default-directory-list '("/Applications/Emacs_carbon2.app/Contents/Resources/info/"))
