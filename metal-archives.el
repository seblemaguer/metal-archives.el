;;; metal-archives.el --- Package to list future releases using Metal-Archive API  -*- lexical-binding: t; coding: utf-8 -*-

;; Copyright (C)  8 January 2019
;;

;; Author: Sébastien Le Maguer <lemagues@tcd.ie>
;; URL: https://github.com/seblemaguer/metal-archives.el
;; Package-Requires: ((emacs "26.3"))
;; Keywords: lisp, calendar
;; Version: 0.1
;; Homepage:

;; metal-archives is free software; you can redistribute it and/or modify it
;; under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3, or (at your option)
;; any later version.
;;
;; metal-archives is distributed in the hope that it will be useful, but WITHOUT
;; ANY WARRANTY; without even the implied warranty of MERCHANTABILITY
;; or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public
;; License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with metal-archives.  If not, see http://www.gnu.org/licenses.

;;; Commentary:
;;
;; Package which list future releases using the Metal-Archive (metal-archives.com) API.
;;


;;; Code:

(require 'metal-archives-org)
(require 'metal-archives-shopping-list)


(provide 'metal-archives)
;;; metal-archives.el ends here
