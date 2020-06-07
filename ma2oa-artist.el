;;; ma2oa-artist.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C)  7 June 2020
;;

;; Author: Sébastien Le Maguer <lemagues@tcd.ie>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; ma2oa-artist is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ma2oa-artist is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ma2oa-artist.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'org)
(require 'org-element)



(defvar artist-map-filename (format "%s/artist-map.cache" user-emacs-directory))

(defun ma2oa-load-artists-map ()
  "Loading the artist priority map stored in FiLENAME."
  (with-current-buffer (find-file artist-map-filename)
    (setq ma2oa-favorite-artists (read (buffer-string)))

    ;; Close the buffer
    (kill-this-buffer)))

(defun ma2oa-save-artists-map ()
  "Storing the artist priority map in FiLENAME."
  (with-current-buffer (find-file artist-map-filename)
    (erase-buffer)

    (insert (prin1-to-string ma2oa-favorite-artists))

    ;; Save the buffer
    (save-buffer)

    ;; Close the buffer
    (kill-this-buffer)))



(defun ma2oa-fill-artists-priority-map (filename)
  "2019-01-14"
  (interactive)

  (with-current-buffer (find-file filename)
    (org-map-entries
     (lambda ()
       (let ((cur-artist (replace-regexp-in-string " (.*" ""
                                                   (nth 4 (org-heading-components)))))
         (unless (ht-get ma2oa-favorite-artists cur-artist)
           (ht-set ma2oa-favorite-artists cur-artist 'normal)))))
    (kill-this-buffer)))

(ma2oa-fill-artists-priority-map "~/shared/Dropbox/music/cd.org")



;; (defun ma2oa-manage-favorite (entry)
;;   (ma2oa-favorite-alert entry))

;; (setq ma2oa-favorite-handle 'ma2oa-manage-favorite)
;; ;; ma2oa~format-entry


;; (with-current-buffer (find-file "~/shared/pCloudDrive/org/todo/todo.org")
;;   (org-element-parse-buffer))



(provide 'ma2oa-artist)
;;; ma2oa-artist.el ends here
