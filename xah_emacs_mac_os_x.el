;-*- coding: utf-8 -*-
; Mac OS X Emacs related emacs customization

; 2007-06, 2009-08-04
;   Xah Lee
; ∑ http://xahlee.org/



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



; Carbon Emacs doesn't inherit shell's env.  A workaround is to shart it from shell like this: “/Applications/Emacs.app/Contents/MacOS/Emacs &” however, exiting the shell by Ctrl+D also kills your emacs. So a workaround is to use the nohub: “nohup /Applications/Emacs.app/Contents/MacOS/Emacs &”.  However, Apple's Term has a feature such that closing the window (by clicking on the Red Light or Cmd+w) kills all processes started from it.  So, the solution is to never close window by clicking or Cmd+w, instead, always use Ctrl+d. However, when ssh to remote server and running “tail -f”, sometimes broken connection causes the session to hang. So you are left with a frozen ssh session where the remote server has disconnected. So, here you can't use Ctrl-d to close the window. If you close the window by clicking, then you'll kill any emacs you might have started from that window. Hot Damn.  So, the optimal solution seems to get Carbon Emacs to start using the Mac's GUI way of inheriting env vars by the file “~/.MacOSX/environment.plist”. Though, that mean you gonna maintain two sets of env vars. Shit. Therefore at the end, maybe just dup it in elisp and forget about all quality solutions. Software Industry is quite fucked anyway. This is peanuts. All this is due to the motherfucking unix fucking env var system fuck. Fuck unix and fuck the unixer's mothers.

