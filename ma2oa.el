;;; ma2oa.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C)  8 January 2019
;;

;; Author: Sébastien Le Maguer <lemagues@tcd.ie>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

;; ma2oa is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; ma2oa is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with ma2oa.  If not, see http://www.gnu.org/licenses.

;;; Commentary:


;;; Code:

(require 'parse-time)
(require 'cl-lib)
(require 'request)
(require 'json)

(defvar ma2oa-max-vue 10
  "The maximum number of retrieved entries")

(defvar ma2oa-org-template "* %s - %s :%s:\n:PROPERTIES:\n:GENRE: %s\n:CATEGORY: Release\n:END:\nSCHEDULED: <%s>\n"
  "Org entry template. The formatting assume the quadriplet ARTIST, ALBUM, TYPE, GENRE and DATE everything string formatted.")

(defvar ma2oa-input-date-regexp "\\([a-zA-Z]*\\) \\([0-9]\\{1,2\\}\\)[a-z]\\{2\\}, \\([0-9]\\{4\\}\\)"
  "Regexp to parse the date coming from metal-archives.com")

(defvar ma2oa-output-date-format "\\1 \\2, \\3"
  "Substitution regexp generated based on the groups captured by the input regexp.")

(defvar ma2oa-entry-database (make-hash-table)
  "The release entry database.")

(defvar ma2oa-target-file "~/.emacs.d/ma-releases.org"
  "The release org formatted file")

(cl-defstruct ma2oa-entry artist album type genre date)

(defun ma2oa~add-entry-to-db (vector-entry)
  "Parse an VECTOR_ENTRY coming from the metal-archives.com
website. A ma2oa-entry is then created and added to
ma2oa-entry-database."
  (let* ((artist (decode-coding-string (replace-regexp-in-string "<a[^>]*>\\([^<]*\\)<.*" "\\1" (aref vector-entry 0)) 'utf-8)) ;; NOTE: MA gives a string encoded in Latin-1
         (album (decode-coding-string (replace-regexp-in-string "<a[^>]*>\\([^<]*\\)<.*" "\\1" (aref vector-entry 1)) 'utf-8))
         (type (decode-coding-string (replace-regexp-in-string "[ -]" "_" (aref vector-entry 2)) 'utf-8))
         (genre (decode-coding-string (aref vector-entry 3) 'utf-8))
         (date (org-read-date nil nil (replace-regexp-in-string ma2oa-input-date-regexp
                                                                ma2oa-output-date-format
                                                                (aref vector-entry 4))))
         (entry (make-ma2oa-entry :artist artist :album album :type type :genre genre :date date)))
    (puthash entry t ma2oa-entry-database)))

(defun ma2oa~update-db (data)
  "Update the entry database from the DATA give in parameter."
  (let* ((entries (assoc-default 'aaData data))
         (formatted-entries))
    ;; (message "nb_items = %d" (assoc-default 'iTotalRecords data))
    ;; (message "nb_displayed_items = %d" (assoc-default 'iTotalDisplayRecords data))
    (mapc 'ma2oa~add-entry-to-db entries)))


(defun ma2oa~format-entry (entry)
  "Format an ENTRY in org format to be added to the org agenda
file."
  (let* ((org-entry (format ma2oa-org-template
                            (ma2oa-entry-artist entry)
                            (ma2oa-entry-album entry)
                            (ma2oa-entry-type entry)
                            (ma2oa-entry-genre entry)
                            (ma2oa-entry-date entry))))
    (insert org-entry)))

(defun ma2oa~db-to-agenda ()
  "Generate the org-agenda buffer and update the global agenda using the entry database."
  (let* ((coding-system-for-write 'utf-8))
    (with-current-buffer (find-file ma2oa-target-file)

      ;; Clean
      (erase-buffer)

      ;; Synchronize db and the file
      (mapc 'ma2oa~format-entry (hash-table-keys ma2oa-entry-database))

      ;; Save the buffer
      (save-buffer)

      ;; Close the buffer
      (kill-this-buffer)
      )))

(defun ma2oa-retrieve-next-releases ()
  (interactive)
  (request
   "https://www.metal-archives.com/release/ajax-upcoming/json/1"
   :params '(("sEcho" . 1)) ;; FIXME: actually not used ! ("iDisplayLength" . ma2oa-max-vue))
   :parser 'json-read

   :success (cl-function
             (lambda (&key data &allow-other-keys)
               (clrhash ma2oa-entry-database)
               (ma2oa~update-db data)
               (ma2oa~db-to-agenda)))

   :error
   (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                  (message "Got error: %S" error-thrown)))))

(provide 'ma2oa)

;;; ma2oa.el ends here
