;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
;;
;; This program is free software: you can redistribute it and/or
;; modify it under the terms of the GNU Lesser General Public License
;; as published by the Free Software Foundation, either version 3 of
;; the License, or (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful, but
;; WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
;; Lesser General Public License for more details.
;;
;; You should have received a copy of the GNU Lesser General Public
;; License along with this program.  If not, see
;; <http://www.gnu.org/licenses/>.


(in-package #:cl-git)

(defbitfield (git-checkout-strategy)
  :safe
  :safe-create
  :force
  :allow-conflicts
  :remove-untracked
  :remove-ignored
  :update-only
  :dont-update-index
  :no-refresh
  :skip-unmerged
  :use-ours
  :use-theirs
  :disable-pathspec-match
  (:skip-locked-directories 262144)
  :dont-overwrite-ignored
  :conflict-style-merge
  :conflict-style-diff3)

(defcstruct (git-checkout-options)
  (version :uint)
  (checkout-strategy git-checkout-strategy)
  
)
