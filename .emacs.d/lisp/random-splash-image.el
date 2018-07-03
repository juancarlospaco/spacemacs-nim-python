;;; random-splash-image.el --- Randomly sets splash image to *GNU Emacs* buffer on startup.

;; Copyright (C) 2015  kakakaya

;; Author: kakakaya <kakakaya AT gmail.com>
;; Keywords: games
;; Version: 1.0.0
;; URL: https://github.com/kakakaya/random-splash-image

;; Licence:
;; The MIT License (MIT)

;; Copyright (c) 2015 kakakaya

;; Permission is hereby granted, free of charge, to any person obtaining a copy
;; of this software and associated documentation files (the "Software"), to deal
;; in the Software without restriction, including without limitation the rights
;; to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
;; copies of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:

;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.

;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
;; FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
;; AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
;; LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE
;; SOFTWARE.


;;; Code:

(defun random-splash-image-elt (choices)
  (elt choices (random (length choices))))

(defun random-splash-image-choose-image (img-dir)
  (random-splash-image-elt (directory-files img-dir t "^\\([^.]\\|\\.[^.]\\|\\.\\..\\)")))

(defun random-splash-image-set ()
  (setq fancy-splash-image (random-splash-image-choose-image "~/.emacs.d/assets/"))))

(defun random-splash-image-reopen-screen ()
  (interactive)
  (random-splash-image-set)
  (display-startup-screen))

(provide 'random-splash-image)
;;; random-splash-image.el ends here
