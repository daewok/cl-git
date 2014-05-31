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

(defclass git-cred ()
  ((username
	:initarg :username
	:documentation "If provided, this username is used instead of
using the username inferred from the repository URL.")))

(defcallback %acquire-credentials-no-credentials git-error-code
	((git-cred :pointer)
	 (url :string)
	 (username-from-url :string)
	 (allowed-types :uint)
	 (payload :pointer))
  (declare (ignore git-cred url username-from-url allowed-types payload))
  :git-passthrough)

(defclass no-credentials (git-cred)
  ())

;; Static functions can't be called by cffi, so we have to provide our
;; own function to free the ssh key.
(defcallback %ssh-key-free :void
	((struct :pointer))
  struct)

(defcallback %acquire-credentials-ssh-key-from-agent git-error-code
	((git-cred :pointer)
	 (url :string)
	 (username-from-url :string)
	 (allowed-types :uint)
	 (payload :pointer))
  ;;(declare (ignorable git-cred url username-from-url allowed-types payload))
  (let ((ptr (foreign-alloc '(:struct %git-cred-ssh-key))))
	;; init all to zero
	(dotimes (i size-of-%git-cred-ssh-key)
	  (setf (mem-ref ptr :char i) 0))
	(with-foreign-slots (((:pointer parent)
						  username
						  privatekey
						  publickey)
						 ptr (:struct %git-cred-ssh-key))
	  (setf (foreign-slot-value parent '(:struct %git-cred) 'credtype) :git-credtype-ssh-key)
	  (setf (foreign-slot-value parent '(:struct %git-cred) 'free) (callback %ssh-key-free))
	  (setf username (foreign-string-alloc username-from-url))
	  (setf privatekey (foreign-string-alloc "/home/etimmons/.ssh/id_rsa_no_passphrase"))
	  ;;(setf publickey (foreign-string-alloc "/home/etimmons/.ssh/id_rsa_no_passphrase.puasdfb"))
	  )
	(break)
	(setf (mem-ref git-cred :pointer) ptr)

)
  ;;(break)
;;  (format t "url: ~a~%username-from-url: ~a~%" url username-from-url)
  :git-ok)

(defclass ssh-key-from-file (git-cred)
  ((private-key-path
	:initarg :private-key-path
	:documentation "path to the private key file.")
   (public-key-path
	:initarg :public-key-path
	:initform nil
	:documentation "path to the public key file. Required if the
public key is not the same as the private key with \".pub\"
appended.")))

