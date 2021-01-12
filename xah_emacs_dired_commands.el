;; -*- coding: utf-8; lexical-binding: t; -*-

(defun xah-dired-to-zip ()
  "Zip the current file in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip command line tool.

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2015-07-30"
  (interactive)
  (require 'dired)
  (let ( ($fName (elt (dired-get-marked-files) 0)))
    (shell-command
     (format
      "zip -r '%s.zip' '%s'"
      (file-relative-name $fName)
      (file-relative-name $fName)))))

(defun xah-process-image (@fileList @argsStr @newNameSuffix @newFileExt )
  "Wrapper to ImageMagick's “convert” shell command.
@fileList is a list of image file paths.
@argsStr is argument string passed to ImageMagick's “convert” command.
@newNameSuffix is the string appended to file. e.g. “_2” may result “_2.jpg”
@newFileExt is the new file's file extension. e.g. “.png”

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2020-11-13"
  (mapc
   (lambda ($f)
     (let ( $newName $cmdStr )
       (setq $newName
             (concat
              (file-name-sans-extension $f)
              @newNameSuffix
              @newFileExt))
       (while (file-exists-p $newName)
         (setq $newName
               (concat
                (file-name-sans-extension $newName)
                @newNameSuffix
                (file-name-extension $newName t))))
       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (setq $cmdStr
             (format
              "convert %s '%s' '%s'"
              @argsStr
              (file-relative-name $f)
              (file-relative-name $newName)))
       (shell-command $cmdStr)
       (message "ran 「%s」" $cmdStr)))
   @fileList ))

(defun xah-dired-scale-image (@fileList @scalePercent @quality @sharpen-p)
  "Create a scaled version of marked image files in dired.
New file names have “-s” appended before the file name extension.

If `universal-argument' is ask for png/jpg and sharpen options.

When called in lisp code,
 @fileList is a file fullpath list.
 @scalePercent is a integer.
 @quality is a integer, from 1 to 100.
 @sharpen-p is true or false.

Requires ImageMagick unix shell command.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2019-12-30"
  (interactive
   (list (cond
          ((string-equal major-mode "dired-mode") (dired-get-marked-files))
          ((string-equal major-mode "image-mode") (list (buffer-file-name)))
          (t (list (read-from-minibuffer "file name:"))))
         (read-from-minibuffer "Scale %:")
         (if current-prefix-arg (string-to-number (read-string "quality:" "90")) 90)
         (if current-prefix-arg (y-or-n-p "Sharpen") t)))
  (let ( ($nameExt
          (if current-prefix-arg (if (y-or-n-p "to png?") ".png" ".jpg" ) ".jpg" )))
    (xah-process-image
     @fileList
     (format "-scale %s%% -quality %s%% %s " @scalePercent @quality (if @sharpen-p "-sharpen 1" "" ))
     "-s" $nameExt )))

(defun xah-image-autocrop ()
  "Create a new auto-cropped version of image.
If current buffer is jpg or png file, crop it.
If current buffer is dired, do the file under cursor or marked files.

The created file has “_crop.” in the name, in the same dir. The image format is same as the original.
Automatically call command 「optipng」 if available on the cropped png file.

Requires ImageMagick shell command “convert”

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2020-12-06"
  (interactive)
  (let (
        ($buffFileName (buffer-file-name))
        $newName
        $cmdStr
        )
    (if (string-equal major-mode "dired-mode")
        (progn
          (let (($flist (dired-get-marked-files)))
            (mapc
             (lambda ($f)
               (setq $newName (concat (file-name-sans-extension $f) "_crop." (file-name-extension $f)))
               (setq $cmdStr (format "convert -trim '%s' '%s'" (file-relative-name $f) (file-relative-name $newName)))
               (shell-command $cmdStr))
             $flist ))
          (revert-buffer))
      (progn
        (if $buffFileName
            (let (($ext (file-name-extension $buffFileName)))
              (if (and (not (string-equal $ext "jpg"))
                       (not (string-equal $ext "png")))
                  (user-error "not png or jpg at %s" $buffFileName)
                (progn
                  (setq $newName (concat (file-name-sans-extension $f) "_crop." (file-name-extension $f)))
                  (setq $cmdStr
                        (format
                         "convert -trim '%s' '%s'"
                         $buffFileName
                         $newName))
                  (message "running 「%s」" $cmdStr)
                  (shell-command  $cmdStr )
                  (when (string-equal $ext "png")
                    (when (eq (shell-command "which optipng") 0)
                      (message "optimizing with optipng")
                      (shell-command (concat "optipng " $newName " &")))))))
          (user-error "not img file or dired at %s" $buffFileName))))))

(defun xah-image-remove-transparency ()
  "Create a new version of image without alpha channel.
Works on png images only.
If current buffer is png file, crop it.
If current buffer is dired, do the file under cursor or marked files.

The created file has the name, in the same dir.

Requires ImageMagick shell command “convert”

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2018-04-23"
  (interactive)
  (let (
        ($bfName (buffer-file-name))
        $cmdStr
        )
    (if (string-equal major-mode "dired-mode")
        (progn
          (let (($flist (dired-get-marked-files)))
            (mapc
             (lambda ($f)
               (if (not (string-equal (file-name-extension $f) "png"))
                   (message "skipping %s" $f)
                 (progn
                   (setq $cmdStr (format "convert -flatten '%s' '%s'" (file-relative-name $f) (file-relative-name $f)))                (shell-command $cmdStr))))
             $flist ))
          (revert-buffer))
      (progn
        (if $bfName
            (if (not (string-equal (file-name-extension $bfName) "png"))
                (message "skipping %s" $newName)
              (progn
                (setq $cmdStr
                      (format
                       "convert -flatten '%s' '%s'"
                       $bfName
                       $bfName))
                (shell-command  $cmdStr )))
          (user-error "not img file or dired at %s" $bfName))))))

(defun xah-dired-2png (@fileList)
  "Create a png version of images of marked files in dired.
Requires ImageMagick shell command.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2016-07-19"
  (interactive
   (let (
         ($fileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list $fileList)))
  (xah-process-image @fileList "" "-2" ".png" ))

(defun xah-dired-2drawing (@fileList @grayscale-p @max-colors-count)
  "Create a png version of (drawing type) images of marked files in dired.
Basically, make it grayscale, and reduce colors to any of {2, 4, 16, 256}.
Requires ImageMagick shell command.

Version 2017-02-02"
  (interactive
   (let (
         ($fileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list $fileList
           (yes-or-no-p "Grayscale?")
           (ido-completing-read "Max number of colors:" '( "2" "4" "16" "256" )))))
  (xah-process-image @fileList
                     (format "+dither %s -depth %s"
                             (if @grayscale-p "-type grayscale" "")
                             ;; image magick “-colors” must be at least 8
                             ;; (if (< (string-to-number @max-colors-count) 3)
                             ;;     8
                             ;;     (expt 2 (string-to-number @max-colors-count)))
                             (cond
                              ((equal @max-colors-count "256") 8)
                              ((equal @max-colors-count "16") 4)
                              ((equal @max-colors-count "4") 2)
                              ((equal @max-colors-count "2") 1)
                              (t (error "logic error 0444533051: impossible condition on @max-colors-count: %s" @max-colors-count))))  "-2" ".png" ))

(defun xah-dired-2jpg (@fileList)
  "Create a JPG version of images of marked files in dired.
If `universal-argument' is called first, ask for jpeg quality. (default is 90)

Requires ImageMagick shell command.
URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2018-11-28"
  (interactive
   (let (
         ($fileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:"))))))
     (list $fileList)))
  (let ((quality
         (if current-prefix-arg
             (progn (string-to-number (read-string "quality:" "85")))
           (progn 90))))
    (xah-process-image @fileList (format "-quality %s%%" quality ) "-2" ".jpg" )))

(defun xah-dired-show-metadata (@fileList)
  "Display metatata of buffer image file or marked files in dired.
 (typically image files)
URL `http://xahlee.info/img/metadata_in_image_files.html'
Requires exiftool shell command.

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2019-12-04"
  (interactive
   (list
    (cond
     ((string-equal major-mode "dired-mode") (dired-get-marked-files))
     ((string-equal major-mode "image-mode") (list (buffer-file-name)))
     (t (list (read-from-minibuffer "file name:"))))))
  (let ( (outputBuf (get-buffer-create "*xah metadata output*")))
    (switch-to-buffer outputBuf)
    (erase-buffer)
    (mapc (lambda (f)
            (call-process
             "exiftool"
             nil outputBuf nil
             (file-relative-name f))
            (insert "\nhh========================================\n"))
          @fileList)
    (goto-char (point-min))))

(defun xah-dired-remove-all-metadata (@fileList)
  "Remove all metatata of buffer image file or marked files in dired.
 (typically image files)
URL `http://xahlee.info/img/metadata_in_image_files.html'
Requires exiftool shell command.

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2016-07-19"
  (interactive
   (list
    (cond
     ((string-equal major-mode "dired-mode") (dired-get-marked-files))
     ((string-equal major-mode "image-mode") (list (buffer-file-name)))
     (t (list (read-from-minibuffer "file name:"))))))
  (if (y-or-n-p "Sure to remove all metadata?")
      (mapc
       (lambda ($f)
         (let ($cmdStr)
           (setq $cmdStr
                 (format "exiftool -all= -overwrite_original '%s'" (file-relative-name $f))) ; relative paths used to get around Windows/Cygwin path remapping problem
           (shell-command $cmdStr)))
       @fileList )
    nil
    ))

(defun xah-dired-sort ()
  "Sort dired dir listing in different ways.
Prompt for a choice.
URL `http://ergoemacs.org/emacs/dired_sort.html'
Version 2018-12-23"
  (interactive)
  (let ($sort-by $arg)
    (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" )))
    (cond
     ((equal $sort-by "name") (setq $arg "-Al "))
     ((equal $sort-by "date") (setq $arg "-Al -t"))
     ((equal $sort-by "size") (setq $arg "-Al -S"))
     ((equal $sort-by "dir") (setq $arg "-Al --group-directories-first"))
     (t (error "logic error 09535" )))
    (dired-sort-other $arg )))

;; (defun xah-dired-sort-old-2018-12-23 ()
;;   "Sort dired dir listing in different ways.
;; Prompt for a choice.
;; URL `http://ergoemacs.org/emacs/dired_sort.html'
;; Version 2015-07-30"
;;   (interactive)
;;   (let ($sort-by $arg)
;;     (setq $sort-by (ido-completing-read "Sort by:" '( "date" "size" "name" "dir")))
;;     (cond
;;      ((equal $sort-by "name") (setq $arg "-Al --si --time-style long-iso "))
;;      ((equal $sort-by "date") (setq $arg "-Al --si --time-style long-iso -t"))
;;      ((equal $sort-by "size") (setq $arg "-Al --si --time-style long-iso -S"))
;;      ((equal $sort-by "dir") (setq $arg "-Al --si --time-style long-iso --group-directories-first"))
;;      (t (error "logic error 09535" )))
;;     (dired-sort-other $arg )))

(defun xah-open-dired-marked ()
  "Open marked files in dired.
Version 2019-10-22"
  (interactive)
  (mapc 'find-file (dired-get-marked-files)))

