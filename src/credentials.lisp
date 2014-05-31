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

(defun default-git-clone-options (&optional ptr)
  ;; If ptr is not given, allocate memory.
  (unless ptr
	(setf ptr (foreign-alloc '(:struct git-clone-options))))
  ;; Zero out all the memory.
  (dotimes (i size-of-git-clone-options)
	(setf (mem-aref ptr :char i) 0))
  ;; Now fill in the default version
  (with-foreign-slots ((version
						(:pointer checkout-options)
						(:pointer remote-callbacks))
					   ptr (:struct git-clone-options))
	(setf version +git-clone-options-version+)
	(default-git-checkout-opts checkout-options)
	(default-git-remote-callbacks remote-callbacks))
  ptr)

(defun default-git-remote-callbacks (&optional ptr)
  ;; If ptr is not given, allocate memory.
  (unless ptr
	(setf ptr (foreign-alloc '(:struct git-clone-options))))
  ;; Zero out all the memory.
  (dotimes (i size-of-git-remote-callbacks)
	(setf (mem-aref ptr :char i) 0))
  (with-foreign-slots ((version) ptr (:struct git-remote-callbacks))
	(setf version +git-remote-callbacks-version+))
  ptr)

(defun default-git-checkout-opts (&optional ptr)
  ;; If ptr is not given, allocate memory.
  (unless ptr
	(setf ptr (foreign-alloc '(:struct git-checkout-opts))))
  ;; Zero out all the memory.
  (dotimes (i size-of-git-checkout-opts)
	(setf (mem-aref ptr :char i) 0))
  ;; Now fill out the default version and checkout strategy.
  (setf (foreign-slot-value ptr '(:struct git-checkout-opts) 'version)
		+git-checkout-opts-version+)
  (setf (foreign-slot-value ptr '(:struct git-checkout-opts) 'checkout-strategy)
		:git-checkout-safe-create)
  ptr)

(defclass git-cred ()
  ())

(defclass git-cred-ssh-key-from-file (git-cred)
  ())

