;;; metal-archives-shopping-list.el --- Add shopping list generation support to metal-archives   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C) 2020 - 2023
;;

;; Author: Sébastien Le Maguer <lemagues@tcd.ie>

;; Package-Requires: ((emacs "26.3") (org-ml "5.8.7") (alert "1.2") (ht "2.3") (metal-archives "0.3"))
;; Keywords: org, calendar
;; Version: 0.3
;; Homepage: https://github.com/seblemaguer/metal-archives.el

;; metal-archives-shopping-list is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; metal-archives-shopping-list is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with metal-archives-shopping-list.  If not, see http://www.gnu.org/licenses.

;;; Commentary:

;; This package provide a support to manage a shopping list of future releases using the metal
;; archives database.

;;; Code:

(require 'metal-archives)
(require 'org)
(require 'org-element)
(require 'org-ml)
(require 'alert)
(require 'ht)



(defcustom metal-archives-shopping-list-target-file (format "%s/shopping_list.org"
                                                            user-emacs-directory)
  "File containing the order list in an `org-mode' format."
  :type 'file
  :group 'metal-archives)

(defcustom metal-archives-shopping-list-root-node "CD"
  "Headline value whose children are the CDs to order."
  :type 'string
  :group 'metal-archives)

(defcustom metal-archives-shopping-list-release-threshold 'normal
  "Priority threshold level to add a release to the shopping list."
  :type 'symbol
  :group 'metal-archives)

(defcustom metal-archives-shopping-list-release-todo-keyword "TODO"
  "The TODO keyword associated to the release entry."
  :type 'string
  :group 'metal-archives)

(defcustom metal-archives-shopping-list-release-category "RELEASE"
  "The category associated to the release entry."
  :type 'string
  :group 'metal-archives)

(defvar metal-archives-shopping-list-release-to-flush '())


(defun metal-archives-shopping-list~generate-node (level release)
  "Generate an entry at a specific LEVEL using the RELEASE information."
  (org-ml-build-headline! :title-text (format "%s - %s"
                                              (metal-archives-entry-artist release)
                                              (metal-archives-entry-album release))
                          :level level
                          :tags (list (metal-archives-entry-type release))
                          :todo-keyword metal-archives-shopping-list-release-todo-keyword
                          :section-children (list
                                             (org-ml-build-planning! :scheduled
                                                                     (reverse (seq-subseq (parse-time-string (metal-archives-entry-date release)) 3 6)))
                                             (org-ml-build-property-drawer! (list "GENRE"
                                                                                  (metal-archives-entry-genre release))
                                                                            `("CATEGORY" ,metal-archives-shopping-list-release-category)))))


(defun metal-archives-shopping-list~children-headline-set (children)
  "Get the set of albums already in the shopping list from the given CHILDREN."
  (let* ((headlines))
    (dolist (elt children)
      (when (eq (org-element-type elt) 'headline)
        (push (org-element-property :raw-value elt) headlines)))
    (delete-dups headlines)))

(defun metal-archives-shopping-list~add-release (cur-node)
  "Add release as a children node to CUR-NODE."
  (let* ((level (+ 1 (org-element-property :level cur-node)))
         (children (org-ml-get-children cur-node))
         (headline-set (metal-archives-shopping-list~children-headline-set children)))
    (dolist (release metal-archives-shopping-list-release-to-flush)
      (unless (member (format "%s - %s" (metal-archives-entry-artist release)
                              (metal-archives-entry-album release)) headline-set)
        (setq children (append children (list (metal-archives-shopping-list~generate-node level release))))))
    (org-ml-set-children children cur-node)))

(defun metal-archives-shopping-list~update-release (cur-node)
  "Recursive browsing and updating of the tree which root is given by CUR-NODE in order to add the RELEASE."
  (if (string= (org-element-property :raw-value cur-node) metal-archives-shopping-list-root-node)
      (setq cur-node (metal-archives-shopping-list~add-release cur-node))
    (org-ml-map-children* (--map (metal-archives-shopping-list~update-release it) it) cur-node))
  cur-node)

(defun metal-archives-shopping-list-update (&optional retrieve)
  "Update the shopping list.
if RETRIEVE is non nil, also ran `metal-archives-retrieve-next-releases'"
  (interactive "P")

  ;; Retrieve the new releases if required
  (when retrieve
    (metal-archives-retrieve-next-releases))

  ;; Update the shopping list
  (with-current-buffer (find-file metal-archives-shopping-list-target-file)
    (let* ((todo-tree (org-element-parse-buffer)))
      ;; We don't need the content, everything will be replaced!
      (erase-buffer)

      ;; Remove the first useless nodes
      (pop todo-tree)
      (pop todo-tree)

      ;; Update the todo list
      (dolist (elt todo-tree)
        (when (eq (org-element-type elt) 'headline)
          (setq elt (metal-archives-shopping-list~update-release elt)))
        (insert (org-ml-to-string elt)))

      ;; Flush
      (save-buffer)
      (setq metal-archives-shopping-list-release-to-flush '())

      ;; Switch back to the previous buffer
      (switch-to-buffer (other-buffer (current-buffer) 1)))))

(defun metal-archives-shopping-list-add-release-and-alert (entry)
  "Handler to add the ENTRY to the database of release to order and emit an alert."
  (metal-archives-favorite-alert entry)
  (when (>
         (cdr (assq (ht-get metal-archives-favorite-artists (metal-archives-entry-artist entry))
                    alert-growl-priorities))
         (cdr (assq metal-archives-shopping-list-release-threshold alert-growl-priorities)))
    (add-to-list 'metal-archives-shopping-list-release-to-flush entry)))



(defun metal-archives-shopping-list-add-entry (entry)
  "Add the given ENTRY to the current shopping list cache (metal-archives-shopping-list-update needs to be called after this!)."
  (add-to-list 'metal-archives-shopping-list-release-to-flush entry))

(defun metal-archives-add-to-shopping-list ()
  "Execute the handler function on the current entry."
  (interactive)
  (let ((entry (metal-archives-get-current-entry)))
    (when entry
      (metal-archives-shopping-list-add-entry entry))))



(provide 'metal-archives-shopping-list)
;;; metal-archives-shopping-list.el ends here
