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

(define-foreign-type git-structure-pointer ()
  ()
  (:actual-type :pointer))

(define-foreign-type git-remote-callbacks (git-structure-pointer)
  ((version
	:initarg :version
	:initform +git-remote-callbacks-version+
	:reader git-remote-callbacks-version)
   (credentials
	:initarg :credentials
	:initform (callback %acquire-credentials-ssh-key-from-agent)
	:reader git-remote-callbacks-credentials))
  (:actual-type :pointer)
  (:simple-parser git-remote-callbacks))

(define-foreign-type git-checkout-opts (git-structure-pointer)
  ((version
	:initarg :version
	:initform +git-checkout-opts-version+
	:reader git-checkout-opts-version)
   (checkout-strategy
	:initarg :checkout-strategy
	:initform :git-checkout-safe-create
	:reader git-checkout-opts-checkout-strategy))
  (:actual-type :pointer)
  (:simple-parser git-checkout-opts))

(define-foreign-type git-clone-options (git-structure-pointer)
  ((version
	:initarg :version
	:initform +git-clone-options-version+
	:reader git-clone-options-version)
   (remote-callbacks
	:initarg :remote-callbacks
	:initform (make-instance 'git-remote-callbacks)
	:reader git-clone-options-remote-callbacks)
   (checkout-opts
	:initarg :checkout-opts
	:initform (make-instance 'git-checkout-opts)
	:reader git-clone-options-checkout-opts))
  (:actual-type :pointer)
  (:simple-parser git-clone-options))

(defmethod translate-to-foreign (value (type git-structure-pointer))
  ;; allocate a chunk of foreign memory for the structure and return
  ;; it.
  (if (and (pointerp value) (null-pointer-p value))
	  value
	  (fill-structure value)))

(defmethod free-translated-object (value (type git-structure-pointer) param)
  (declare (ignore param))
  (unless (null-pointer-p value)
	(foreign-free value)))

(defmethod fill-structure ((value git-remote-callbacks) &optional ptr)
  "If given a pointer to a preallocated chunk of memory of the right
size, fill it with the information from VALUE. If PTR is not given,
allocate it. Returns the pointer."
  ;; If ptr is not given, allocate memory.
  (unless ptr
	(setf ptr (foreign-alloc '(:struct %git-clone-options))))
  ;; Zero out all the memory.
  (dotimes (i size-of-%git-remote-callbacks)
	(setf (mem-aref ptr :char i) 0))
  (with-foreign-slots ((version credentials)
					   ptr (:struct %git-remote-callbacks))
	(setf version (git-remote-callbacks-version value))
	(setf credentials (git-remote-callbacks-credentials value)))
  ptr)

(defmethod fill-structure ((value git-checkout-opts) &optional ptr)
  ;; If ptr is not given, allocate memory.
  (unless ptr
	(setf ptr (foreign-alloc '(:struct %git-checkout-opts))))
  ;; Zero out all the memory.
  (dotimes (i size-of-%git-checkout-opts)
	(setf (mem-aref ptr :char i) 0))
  ;; Now fill out the default version and checkout strategy.
  (with-foreign-slots ((version checkout-strategy) ptr (:struct %git-checkout-opts))
	(setf version (git-checkout-opts-version value)
		  checkout-strategy (git-checkout-opts-checkout-strategy value)))
  ptr)

(defmethod fill-structure ((value git-clone-options) &optional ptr)
  ;; If ptr is not given, allocate memory.
  (unless ptr
	(setf ptr (foreign-alloc '(:struct %git-clone-options))))
  ;; Zero out all the memory.
  (dotimes (i size-of-%git-clone-options)
	(setf (mem-aref ptr :char i) 0))
  ;; Now fill in the default version
  (with-foreign-slots ((version
						(:pointer checkout-options)
						(:pointer remote-callbacks))
					   ptr (:struct %git-clone-options))
	(setf version (git-clone-options-version value))
	(fill-structure (git-clone-options-checkout-opts value) checkout-options)
	(fill-structure (git-clone-options-remote-callbacks value) remote-callbacks))
  ptr)
