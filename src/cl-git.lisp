;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git an Common Lisp interface to git repositories.
;; Copyright (C) 2011-2012 Russell Sim <russell.sim@gmail.com>
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

(define-foreign-library libgit2
  (:linux "libgit2.so.0")
  (:windows "libgit2.dll")
  (:darwin "libgit2.0.dylib")
  (:default "libgit2"))

(use-foreign-library libgit2)

(defmethod translate-name-to-foreign ((lisp-name symbol)
                                      (package (eql *package*))
                                      &optional varp)
  (let ((name (translate-underscore-separated-name lisp-name)))
    (if varp
        (string-trim '(#\* #\%) name)
        (string-trim '(#\%) name))))
