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

(defun xah-process-image (@file-list @args-str @new-name-suffix @new-file-ext )
  "Wrapper to ImageMagick's “convert” shell command.
@file-list is a list of image file paths.
@args-str is argument string passed to ImageMagick's “convert” command.
@new-name-suffix is the string appended to file. e.g. “_new” gets you “…_new.jpg”
@new-file-ext is the new file's file extension. e.g. “.png”

URL `http://ergoemacs.org/emacs/emacs_dired_convert_images.html'
Version 2020-11-13"
  (require 'dired)
  (mapc
   (lambda ($f)
     (let ( $newName $cmdStr )
       (setq $newName
             (concat
              (file-name-sans-extension $f)
              @new-name-suffix
              @new-file-ext))
       (while (file-exists-p $newName)
         (setq $newName
               (concat
                (file-name-sans-extension $newName)
                @new-name-suffix
                (file-name-extension $newName t))))
       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (setq $cmdStr
             (format
              "convert %s '%s' '%s'"
              @args-str
              (file-relative-name $f)
              (file-relative-name $newName)))
       (shell-command $cmdStr)
       (message "ran 「%s」" $cmdStr)))
   @file-list )
  (revert-buffer))

(defun xah-dired-scale-image (@file-list @scale-percentage @quality @sharpen-p)
  "Create a scaled version of marked image files in dired.
New file names have “-s” appended before the file name extension.

If `universal-argument' is ask for png/jpg and sharpen options.

When called in lisp code,
 @file-list is a file fullpath list.
 @scale-percentage is a integer.
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
     @file-list
     (format "-scale %s%% -quality %s%% %s " @scale-percentage @quality (if @sharpen-p "-sharpen 1" "" ))
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

(defun xah-dired-2png (@file-list)
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
  (xah-process-image @file-list "" "-2" ".png" ))

(defun xah-dired-2drawing (@file-list @grayscale-p @max-colors-count)
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
  (xah-process-image @file-list
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

(defun xah-dired-2jpg (@file-list)
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
    (xah-process-image @file-list (format "-quality %s%%" quality ) "-2" ".jpg" )))

(defun xah-dired-show-metadata (@file-list)
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
          @file-list)
    (goto-char (point-min))))

(defun xah-dired-remove-all-metadata (@file-list)
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
       @file-list )
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

(defun xah-create-thumbnail-img ()
  "Create a thumbnail version of image path under cursor.

usage: in a html file, put cursor on a image file path, call the command,
a thumbnail will be created, with file name prefix tn_‹width›x‹height› in the same dir,
and relative path will be inserted before the img tag.

If `universal-argument' is called first, ask for jpeg quality. (default is 90)

Version 2020-11-13"
  (interactive)
  (let* (
         (bounds (bounds-of-thing-at-point 'filename))
         (p1 (car bounds))
         (p2 (cdr bounds))
         (inputPath (buffer-substring-no-properties p1 p2))
         fPath
         directory
         filename
         corename
         fnameExt
         sideLength
         thumbnailSizeArea
         size
         width
         height
         scale
         quality
         newWidth
         newHeight
         filenameNew
         fPathNew
         new-rel-path
         $cmdStr
         )
    (when (not (file-exists-p inputPath))
      (user-error "File not exist: %s" inputPath))
    (setq fPath (expand-file-name inputPath ))
    (setq directory (file-name-directory fPath))
    (setq filename (file-name-nondirectory fPath))
    (setq corename (file-name-sans-extension filename))
    (setq fnameExt (file-name-extension filename))
    (setq sideLength (string-to-number (read-from-minibuffer "~width:" "250" )))
    (setq thumbnailSizeArea (* sideLength sideLength))
    (setq size (xah-html--get-image-dimensions fPath))
    (setq width (aref size 0))
    (setq height (aref size 1))
    (setq scale (sqrt (/ (float thumbnailSizeArea) (float (* width height)))))
    (setq quality
          (if current-prefix-arg
              (progn (string-to-number (read-string "quality:" "85")))
            (progn 90)))
    (setq newWidth (round (* scale (float width))))
    (setq newHeight (round (* scale (float height))))
    ;; (filenameNew (format "tn_%dx%d_%s" newWidth newHeight filename))
    ;; (setq filenameNew (format "%s-s%dx%d.%s" corename newWidth newHeight fnameExt ))
    (setq filenameNew (format "%s-s%d.%s" corename sideLength fnameExt ))
    (setq fPathNew (concat directory filenameNew))
    (setq new-rel-path (file-relative-name fPathNew))
    (setq $cmdStr
          (format
           "convert %s '%s' '%s'"
           (format " -scale %s%% -quality %s%% %s "
                   (* scale 100)
                   quality
                   " -sharpen 1 "
                   )
           fPath
           fPathNew))
    (if (file-exists-p fPathNew)
        (if (y-or-n-p (format "File exist 「%s」. Replace it?" fPathNew))
            (shell-command $cmdStr)
          (progn
            (message "path copied to kill-ring")
            (kill-new fPathNew)))
      (shell-command $cmdStr))
    (message "ran 「%s」" $cmdStr)
    (search-backward "<" )
    (insert fPathNew "\n")
    (backward-word )))

(defun xah-html-convert-png-to-jpg ()
  "Convert the image file under cursor in a html file, from jpg to png, then, linkify it in html.

If `universal-argument' is called first, ask to delete png.

Version 2019-12-17"
  (interactive)
  (let (
        inputPath
        cmdStr
        fileCoreName
        )
    (setq inputPath (thing-at-point 'filename))
    (setq fileCoreName (file-name-sans-extension inputPath))
    (setq cmdStr (format "convert %s.png %s.jpg" fileCoreName fileCoreName ))
    (shell-command cmdStr )
    (when current-prefix-arg
      (when (yes-or-no-p "Delete the png file?")
        (delete-file inputPath)))
    (search-backward "<" )
    (insert fileCoreName ".jpg")
    (insert "\n")
    (backward-char 2)
    (xah-html-image-linkify)
    ;;
    ))

(defun xah-open-dired-marked ()
  "Open marked files in dired.
Version 2019-10-22"
  (interactive)
  (mapc 'find-file (dired-get-marked-files)))

