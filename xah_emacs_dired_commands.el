;; -*- coding: utf-8 -*-

;; 2014-02-13
;;   Xah Lee
;; ∑ http://xahlee.org/


(defun xah-dired-2zip ()
  "Zip the current file in `dired'.
If multiple files are marked, only zip the first one.
Require unix zip command line tool."
  (interactive)
  (require 'dired)
  (let ( (fileName (elt (dired-get-marked-files) 0))  )
    (shell-command (format "zip -r '%s.zip' '%s'" (file-relative-name fileName) (file-relative-name fileName)))
    ))

(defun xah-process-image (fileList argsString newNameSuffix newNameFileSuffix )
  "Create a new image.
fileList is a list of image file paths.
argsString is argument string passed to ImageMagick's “convert” command.
newNameSuffix is the string appended to file. e.g. “_new” gets you “…_new.jpg”
newNameFileSuffix is the new file's file extension. e.g. “.png”
Requires ImageMagick shell tool."
  (require 'dired)
  (mapc
   (lambda (ξf)
     (let ( newName cmdStr )
       (setq newName (concat (file-name-sans-extension ξf) newNameSuffix newNameFileSuffix) )
       (while (file-exists-p newName)
         (setq newName (concat (file-name-sans-extension newName) newNameSuffix (file-name-extension newName t))) )

       ;; relative paths used to get around Windows/Cygwin path remapping problem
       (setq cmdStr
             (format "convert %s '%s' '%s'" argsString (file-relative-name ξf) (file-relative-name newName)) )
       (shell-command cmdStr)
       ))
   fileList ))

(defun xah-dired-scale-image (fileList scalePercentage sharpen-p)
  "Create a scaled version of images of marked files in dired.
The new names have “-s” appended before the file name extension.

If `universal-argument' is given, output is PNG format. Else, JPG.

When called in lisp code,
 fileList is a list.
 scalePercentage is a integer.
 sharpen-p is true or false.

Requires ImageMagick unix shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList
           (read-from-minibuffer "Scale %:")
           (y-or-n-p "Sharpen")
           ) )
   )
  (let ((sharpenOrNo (if sharpen-p "-sharpen 1" "" ))
        (outputSuffix (if current-prefix-arg ".png" ".jpg" ) )
        )
    (xah-process-image fileList
                   (format "-scale %s%% -quality 85%% %s " scalePercentage sharpenOrNo)
                   "-s" outputSuffix )
    )
  )

(defun xah-image-autocrop (fileList)
  "Create a new auto-cropped JPG version of images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList) )
   )
  (xah-process-image fileList "-trim" "-c" ".jpg" )
  )

(defun xah-dired-2png (fileList)
  "Create a png version of images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList) )
   )
  (xah-process-image fileList "" "-2" ".png" )
  )

(defun xah-dired-2drawing (fileList grayscale-p bitsPerPixel)
  "Create a png version of (drawing type) images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList
           (setq grayscale-p (yes-or-no-p "Grayscale?"))
           (read-string "Bits per pixel (1 2 4 8):" "4")) ) )
  (xah-process-image fileList
                 (format "+dither %s -depth %s"
                         (if grayscale-p "-type grayscale" "")
                         ;; image magick “-colors” must be at least 8
                         ;; (if (< (string-to-number bitsPerPixel) 3)
                         ;;     8
                         ;;     (expt 2 (string-to-number bitsPerPixel)))
                         bitsPerPixel)  "-2" ".png" )
  )

(defun xah-dired-2jpg (fileList)
  "Create a JPG version of images of marked files in dired.
Requires ImageMagick shell tool."
  (interactive
   (let (
         (myFileList
          (cond
           ((string-equal major-mode "dired-mode") (dired-get-marked-files))
           ((string-equal major-mode "image-mode") (list (buffer-file-name)))
           (t (list (read-from-minibuffer "file name:") )) ) ) )
     (list myFileList) )
   )
  (xah-process-image fileList "" "-2" ".jpg" ))
