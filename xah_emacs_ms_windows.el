;-*- coding: utf-8 -*-
; Microsoft Windows related emacs customization

; 2013-05-29
;   Xah Lee
; ∑ http://xahlee.org/



(when (string-equal system-type "windows-nt")
  (let (
        (myPathList
         [
"c:/cygwin/usr/local/bin"
"C:/cygwin/bin"
"C:/Program Files/Common Files/Microsoft Shared/Windows Live"
"C:/Program Files (x86)/Common Files/Microsoft Shared/Windows Live"
"C:/Windows/system32"
"C:/Windows"
"C:/Windows/System32/Wbem"
"C:/Windows/System32/WindowsPowerShell/v1.0"
"C:/Program Files (x86)/ATI Technologies/ATI.ACE/Core-Static"
"C:/Program Files (x86)/Common Files/Roxio Shared/DLLShared"
"C:/Program Files (x86)/Common Files/Roxio Shared/12.0/DLLShared"
"C:/Program Files (x86)/Windows Live/Shared"
"c:/Users/xah/apps/emacs-24.3/bin"
"c:/Users/xah/apps/emacs-24.3/lib-src/oo-spd/i386"
"c:/Users/xah/apps/emacs-24.3/lib-src/oo/i386"
           ] )
        )

    (setenv "PATH" (mapconcat 'identity myPathList ";") )

    (setq exec-path (append myPathList (list "." exec-directory)) )
    ) )

(when (string-equal system-type "windows-nt")
  ;; let emacs know Firefox's path
  (add-to-list 'exec-path "c:/Program Files (x86)/Mozilla Firefox/")
  )


;; (setenv "SHELL" "C:/Windows/system32/cmd.exe" )
;; (setenv "SHELL" "C:/Windows/system32/cmdproxy.exe" )

;; Warning! shell-command-switch is "-c".
;; You should set this to "/c" when using a system shell.

;; Warning! w32-quote-process-args is t.
;; You should set this to nil when using a system shell.
