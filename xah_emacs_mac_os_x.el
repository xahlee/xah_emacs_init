;-*- coding: utf-8 -*-
; Mac OS X Emacs related emacs customization

; 2007-06, 2014-02-05
;   Xah Lee
; ∑ http://xahlee.org/



;; start emacs with GUI from shell
;; /Applications/Emacs.app/Contents/MacOS/Emacs

;; start emacsclient from shell
;; Applications/Emacs.app/Contents/MacOS/bin/emacsclient

; setting Super ＆ Hyper keys for the Mac keyboard, for emacs running in OS X

(setq mac-command-modifier 'meta)
(setq mac-option-modifier 'meta)
(setq ns-function-modifier 'control) ; set Mac's Fn key to type Hyper

;; (cond
;;  ((string-equal system-type "windows-nt") ; Microsoft Windows
;;   (progn
;;     nil )
;;   )
;;  ((string-equal system-type "darwin")   ; Mac OS X
;;   (progn

;; (global-set-key (kbd "s-\"") 'xah-compact-uncompact-Block)
;; (global-set-key (kbd "s-2") 'delete-window)
;; (global-set-key (kbd "s-9") 'xah-select-text-in-quote)
;; (global-set-key (kbd "s-s") 'xah-toggle-letter-case)

;; (global-set-key (kbd "s-SPC") 'set-mark-command)

;; (global-set-key (kbd "s-g") 'backward-word)
;; (global-set-key (kbd "s-r") 'forward-word)
;; (global-set-key (kbd "s-h") 'backward-char)
;; (global-set-key (kbd "s-n") 'forward-char)
;; (global-set-key (kbd "s-t") 'next-line)
;; (global-set-key (kbd "s-c") 'previous-line)
;; (global-set-key (kbd "s-e") 'delete-backward-char)
;; (global-set-key (kbd "s-u") 'delete-char)
;; (global-set-key (kbd "s-.") 'backward-kill-word)
;; (global-set-key (kbd "s-o") 'other-window)
;; (global-set-key (kbd "s-p") 'kill-word)
;; (global-set-key (kbd "s-i") 'kill-line)
;; (global-set-key (kbd "s-d") 'xah-beginning-of-line-or-block)
;; (global-set-key (kbd "s-q") 'xah-cut-line-or-region)
;; (global-set-key (kbd "s-j") 'xah-copy-line-or-region)
;; (global-set-key (kbd "s-k") 'yank)

;; (global-set-key (kbd "s-,") 'xah-shrink-whitespaces) ;5852    0.36%  xah-shrink-whitespaces
;; (global-set-key (kbd "s-'") 'xah-compact-uncompact-block) ;1037    0.06%  xah-compact-uncompact-block

;; (global-set-key (kbd "s-6") 'xah-select-current-block) ; 3107    0.19%  xah-select-current-block
;; (global-set-key (kbd "s-7") 'xah-select-current-line) ; 2526    0.16%  xah-select-current-line
;; (global-set-key (kbd "s-8") 'xah-extend-selection) ; 3332    0.21%  xah-extend-selection
;; (global-set-key (kbd "s-9") 'xah-select-text-in-quote) ; 4603    0.28%  xah-select-text-in-quote

;; (global-set-key (kbd "s-f") 'isearch-forward)

;; (global-set-key (kbd "s-m") 'hippie-expand)

;; (global-set-key (kbd "s-w") nil)        ; 2014-02-04 todo. this is a hack

;;     )
;;   )
;;  ((string-equal system-type "gnu/linux") ; linux
;;   (progn
;;      )
;;   )
;;  )

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

