;;; metal-archives-shopping-list.el ---   -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C)  7 June 2020
;;

;; Author: Sébastien Le Maguer <lemagues@tcd.ie>

;; Package-Requires: ((emacs "25.2"))
;; Keywords:
;; Homepage:

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


;;; Code:

(require 'metal-archives)
(require 'org)
(require 'org-element)
(require 'om)



(defcustom metal-archives-shopping-list-target-file "~/test.org"
  "File containing the order list in an org-mode format.")

(defcustom metal-archives-shopping-list-root-node "VVV"
  "Headline value whose children are the CDs to order.")

(defvar metal-archives-shopping-list-cd-to-flush '())

(defun metal-archives-shopping-list~generate-node (level cd)
  (om-build-headline! :title-text (format "%s - %s" (metal-archives-entry-artist cd) (metal-archives-entry-album cd))
                      :level level
                      :tags (list (metal-archives-entry-type cd))
                      :todo-keyword "RELEASE"
                      :section-children (list
                                         ;; (om-build-planning! :scheduled '(2018 1 1))
                                         (om-build-property-drawer! (list (intern "GENRE")
                                                                          (intern (metal-archives-entry-genre cd)))
                                                                    '(CATEGORY RELEASE))
                                         )))

(defun metal-archives-shopping-list~children-headline-set (children)
  (let* ((headlines))
    (dolist (elt children)
      (when (eq (org-element-type elt) 'headline)
        (add-to-list 'headlines (org-element-property :raw-value elt))))
      (delete-dups headlines)))

(defun metal-archives-shopping-list~add-release (cur-node)
  "Add release as a children node to CUR-NODE."
  (let* ((level (+ 1 (org-element-property :level cur-node)))
         (children (om-get-children cur-node))
         (headline-set (metal-archives-shopping-list~children-headline-set children)))
    (dolist (cd metal-archives-shopping-list-cd-to-flush)
      (unless (member (format "%s - %s" (metal-archives-entry-artist cd) (metal-archives-entry-album cd)) headline-set)
        (setq children (append children (list (metal-archives-shopping-list~generate-node level cd))))))
    (om-set-children children cur-node)))

(defun metal-archives-shopping-list~update-cd (cur-node)
  "Recursive browsing and updating of the tree which root is given by CUR-NODE in order to add the RELEASE."
  (if (string= (org-element-property :raw-value cur-node) metal-archives-shopping-list-root-node)
      (setq cur-node (metal-archives-shopping-list~add-release cur-node))
    (om-map-children* (--map (metal-archives-shopping-list~update-cd it) it) cur-node))
  cur-node)

(defun metal-archives-shopping-list-update ()
  "Update the CD order list."
  (interactive)
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
          (setq elt (om-map-children* (--map (metal-archives-shopping-list~update-cd it) it) elt)))
        (insert (om-to-string elt)))

      ;; Flush
      (save-buffer)
      (setq metal-archives-cd-to-flush '())

      ;; Switch back to the previous buffer
      (switch-to-buffer (other-buffer (current-buffer) 1))
      )))

(defun metal-archives-shopping-list-add-cd-and-alert (entry)
  "Handler to add the ENTRY to the database of CD to order and emit an alert."
  (metal-archives-favorite-alert entry)
  (add-to-list 'metal-archives-shopping-list-cd-to-flush entry))



(provide 'metal-archives-shopping-list)
;;; metal-archives-shopping-list.el ends here
