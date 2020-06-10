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

(require 'ma2oa)
(require 'org)
(require 'org-element)
(require 'om)



(defvar artist-map-filename (format "%s/artist-map.cache" user-emacs-directory)
  "The cache containing the hashtable which key are the favorite
  artists and the corresponding value is the priority.")

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



(defcustom ma2oa-to-order-target-file "~/test.org"
  "File containing the order list in an org-mode format.")

(defcustom ma2oa-to-order-root-node "VVV"
  "Headline value whose children are the CDs to order.")

(defvar ma2oa-cd-to-flush '())

(defun ma2oa~generate-node (level cd)
  (om-build-headline! :title-text (format "%s - %s" (ma2oa-entry-artist cd) (ma2oa-entry-album cd))
                      :level level
                      :tags (list (ma2oa-entry-type cd))
                      :todo-keyword "RELEASE"
                      :section-children (list
                                         ;; (om-build-planning! :scheduled '(2018 1 1))
                                         (om-build-property-drawer! (list (intern "GENRE")
                                                                          (intern (ma2oa-entry-genre cd)))
                                                                    '(CATEGORY RELEASE))
                                         )))

(defun ma2oa~children-headline-set (children)
  (let* ((headlines))
    (dolist (elt children)
      (when (eq (org-element-type elt) 'headline)
        (add-to-list 'headlines (org-element-property :raw-value elt))))
      (delete-dups headlines)))

(defun ma2oa~add-release (cur-node)
  "Add release as a children node to CUR-NODE."
  (let* ((level (+ 1 (org-element-property :level cur-node)))
         (children (om-get-children cur-node))
         (headline-set (ma2oa~children-headline-set children)))
    (dolist (cd ma2oa-cd-to-flush)
      (unless (member (format "%s - %s" (ma2oa-entry-artist cd) (ma2oa-entry-album cd)) headline-set)
        (setq children (append children (list (ma2oa~generate-node level cd))))))
    (om-set-children children cur-node)))

(defun ma2oa~update-cd (cur-node)
  "Recursive browsing and updating of the tree which root is given by CUR-NODE in order to add the RELEASE."
  (if (string= (org-element-property :raw-value cur-node) ma2oa-to-order-root-node)
      (setq cur-node (ma2oa~add-release cur-node))
    (om-map-children* (--map (ma2oa~update-cd it) it) cur-node))
  cur-node)

(defun ma2oa-update-order-list ()
  "Update the CD order list."
  (interactive)
  (with-current-buffer (find-file ma2oa-to-order-target-file)
    (let* ((todo-tree (org-element-parse-buffer)))
      ;; We don't need the content, everything will be replaced!
      (erase-buffer)

      ;; Remove the first useless nodes
      (pop todo-tree)
      (pop todo-tree)

      ;; Update the todo list
      (dolist (elt todo-tree)
        (when (eq (org-element-type elt) 'headline)
          (setq elt (om-map-children* (--map (ma2oa~update-cd it) it) elt)))
        (insert (om-to-string elt)))

      ;; Flush
      (save-buffer)
      (setq ma2oa-cd-to-flush '())

      ;; Switch back to the previous buffer
      (switch-to-buffer (other-buffer (current-buffer) 1))
      )))

(defun ma2oa-add-cd-and-alert (entry)
  "Handler to add the ENTRY to the database of CD to order and emit an alert."
  (ma2oa-favorite-alert entry)
  (add-to-list 'ma2oa-cd-to-flush entry))



(provide 'ma2oa-artist)
;;; ma2oa-artist.el ends here
