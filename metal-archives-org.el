;;; metal-archives-org.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 10 June 2020
;;

;; Author: Sébastien Le Maguer <sebastien.lemaguer@adaptcentre.ie>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; metal-archives-org is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; metal-archives-org is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with metal-archives-org.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'cl-lib)
(require 'metal-archives)



(defcustom metal-archives-org-template "* %s - %s :%s:\nSCHEDULED: <%s>\n:PROPERTIES:\n:GENRE: %s\n:CATEGORY: Release\n:END:\n"
  "Org entry template. The formatting assume the quadriplet ARTIST, ALBUM, TYPE, DATE and GENRE."
  :type 'string
  :group 'metal-archives)

(defcustom metal-archives-org-target-file (format "%s/ma-archive.org" user-emacs-directory)
  "The release org formatted file"
  :type 'file
  :group 'metal-archives)




(defun metal-archives-org~format-entry (entry)
  "Format an ENTRY in org format to be added to the org agenda file."
  (let* ((org-entry (format metal-archives-org-template
                            (metal-archives-entry-artist entry)
                            (metal-archives-entry-album entry)
                            (metal-archives-entry-type entry)
                            (org-read-date nil nil (metal-archives-entry-date entry) nil)
                            (metal-archives-entry-genre entry))))
    (insert org-entry)))

(defun metal-archives-org-generate-org-from-db ()
  "Generate the org-agenda buffer and update the global agenda using the entry database."
  (interactive)
  (let* ((coding-system-for-write 'utf-8))
    (with-current-buffer (find-file metal-archives-org-target-file)

      ;; Clean
      (erase-buffer)

      ;; Synchronize db and the file
      (mapc 'metal-archives-org~format-entry metal-archives-entry-database)

      ;; Save the buffer
      (save-buffer)

      ;; Move back to previous buffer
      (switch-to-buffer (other-buffer (current-buffer) 1)))))



(provide 'metal-archives-org)
;;; metal-archives-org.el ends here
