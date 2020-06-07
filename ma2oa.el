;;; ma2oa.el --- Package to list future releases using Metal-Archive API  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C)  8 January 2019
;;

;; Author: Sébastien Le Maguer <lemagues@tcd.ie>
;; URL: https://github.com/seblemaguer/ma2oa.el
;; Package-Requires: ((emacs "25.2"))
;; Keywords: lisp, calendar
;; Version: 0.1
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
;;
;; Package which list future releases using the Metal-Archive (metal-archives.com) API.
;;
;; The package supports synchronization with org-agenda as well as Alert notification for the favorite artists


;;; Code:

(require 'parse-time)
(require 'cl-lib)
(require 'request)
(require 'json)
(require 'alert)
(require 'ht)


(defcustom ma2oa-max-vue 10
  "The maximum number of retrieved entries"
  :type 'int
  :group 'metal-archive-to-org-agenda)

(defcustom ma2oa-org-template "* %s - %s :%s:\n:PROPERTIES:\n:GENRE: %s\n:CATEGORY: Release\n:END:\nSCHEDULED: <%s>\n"
  "Org entry template. The formatting assume the quadriplet ARTIST, ALBUM, TYPE, GENRE and DATE everything string formatted."
  :type 'string
  :group 'metal-archive-to-org-agenda)

(defcustom ma2oa-input-date-regexp "\\([a-zA-Z]*\\) \\([0-9]\\{1,2\\}\\)[a-z]\\{2\\}, \\([0-9]\\{4\\}\\)"
  "Regexp to parse the date coming from metal-archives.com"
  :type 'regexp
  :group 'metal-archive-to-org-agenda)

(defcustom ma2oa-output-date-format "\\1 \\2, \\3"
  "Substitution regexp generated based on the groups captured by the input regexp."
  :type 'string
  :group 'metal-archive-to-org-agenda)

(defcustom ma2oa-target-file (format "%s/ma-archive.org" user-emacs-directory)
  "The release org formatted file"
  :type 'file
  :group 'metal-archive-to-org-agenda)



(defvar ma2oa-favorite-artists (ht-create)
  "Hash-table of favorite artist. The key correspond to the artist name, the value to the priority value.")

(defvar ma2oa-favorite-handle 'ma2oa-favorite-alert
  "The handle of a release of an artist which is wanted.")

(defvar ma2oa-entry-database '()
  "The release entry database set.")



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

    (when (member (ma2oa-entry-artist entry) (ht-keys ma2oa-favorite-artists))
      (funcall ma2oa-favorite-handle entry))

    (unless (member entry ma2oa-entry-database)
      (push entry ma2oa-entry-database))))

(defun ma2oa~format-entry (entry)
  "Format an ENTRY in org format to be added to the org agenda file."
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
      (mapc 'ma2oa~format-entry ma2oa-entry-database)

      ;; Save the buffer
      (save-buffer)

      ;; Close the buffer
      (kill-this-buffer))))



(defun ma2oa-favorite-alert (entry)
  "Documentation."
  (alert (format "%s from %s is going to be released the following date %s"
                 (ma2oa-entry-album entry)
                 (ma2oa-entry-artist entry)
                 (ma2oa-entry-date entry))
         :category 'release
         :severity (ht-get ma2oa-favorite-artists (ma2oa-entry-artist entry))))



(defun ma2oa-retrieve-next-releases ()
  "Retrieve the next releases for metal from metal-archives.com."
  (interactive)
  (let*
      ((thisrequest (request "https://www.metal-archives.com/release/ajax-upcoming/json/1"
                             :params '(("sEcho" . 1))
                             :parser 'json-read
                             :sync t
                             :error
                             (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                            (error "Got error: %S" error-thrown)))))

       (data (request-response-data thisrequest))
       (nb-elements (assoc-default 'iTotalRecords data))
       (entries (assoc-default 'aaData data))
       (i (length entries)))

    ;; Initialisation (by adding current entries as we are forced to get them)
    (setq ma2oa-entry-database '())
    (mapc 'ma2oa~add-entry-to-db entries)

    ;; Add the following up entries to the data base
    (while (< i nb-elements)
      (progn
        (setq thisrequest  (request "https://www.metal-archives.com/release/ajax-upcoming/json/1"
                                    :params `(("sEcho" . 1) ("iDisplayStart" . ,i)) ;; FIXME: actually not used ! ("iDisplayLength" . ma2oa-max-vue))
                                    :parser 'json-read
                                    :sync t

                                    :error
                                    (cl-function (lambda (&rest args &key error-thrown &allow-other-keys)
                                                   (error "Got error: %S" error-thrown)))))
        (setq data (request-response-data thisrequest))
        (setq entries (assoc-default 'aaData data))
        (setq i (+ i (length entries)))
        (mapc 'ma2oa~add-entry-to-db entries)))

    ;; Generate the agenda from the db
    (ma2oa~db-to-agenda)))

(provide 'ma2oa)

;;; ma2oa.el ends here
