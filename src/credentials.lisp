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

(defvar *available-credentials* nil)

(defbitfield git-credtype-t
  (:userpass-plaintext)
  (:ssh-key)
  (:ssh-custom)
  (:default)
  (:ssh-interactive))

(defcallback %acquire-credentials git-error-code
	((git-cred :pointer)
	 (url :string)
	 (username-from-url :string)
	 (allowed-types git-credtype-t)
	 (payload :pointer))
  (let ((allowed-types (foreign-bitfield-symbols 'git-credtype-t allowed-types)))
	(dolist (cred *available-credentials* :git-passthrough)
	  (when (cred-allowed-p cred allowed-types)
		(return (acquire-credentials git-cred cred url username-from-url payload))))))

(defgeneric acquire-credentials (git-cred cred url username-from-url payload))

(defcfun ("git_cred_ssh_key_new" %git-cred-ssh-key-new) git-error-code
  (git-cred :pointer)
  (username :string)
  (public-key-file :string)
  (private-key-file :string)
  (passphrase :string))


(defcfun ("git_cred_ssh_key_from_agent" %git-cred-ssh-key-from-agent) git-error-code
  (git-cred :pointer)
  (username :string))

(defgeneric cred-allowed-p (git-cred allowed-types)
  (:documentation "Returns T iff the credential instance is allowed by
the list of symbols in ALLOWED-TYPES."))

(defclass git-cred ()
  ((username
	:initarg :username
	:initform nil
	:reader git-cred-username)))

(defclass ssh-key (git-cred)
  ())

(defmethod cred-allowed-p ((git-cred ssh-key) allowed-types)
  (member :ssh-key allowed-types))

(defclass ssh-key-from-file (ssh-key)
  ((private-key-path
	:initarg :private-key-path
	:initform (error "You must provide a path to the private key.")
	:reader ssh-key-private-key-path
	:documentation "path to the private key file.")
   (public-key-path
	:initarg :public-key-path
	:initform nil
	:reader ssh-key-public-key-path
	:documentation "path to the public key file. Required if the
public key is not the same as the private key with \".pub\"
appended.")
   (passphrase
	:initarg :passphrase
	:initform nil
	:reader ssh-key-passphrase)))

(defmethod acquire-credentials (git-cred (cred ssh-key-from-file) url username-from-url payload)
  (%git-cred-ssh-key-new git-cred
						 (or (git-cred-username cred)
							 username-from-url)
						 (cond
						   ((stringp (ssh-key-public-key-path cred))
							(ssh-key-public-key-path cred))
						   ((pathnamep (ssh-key-public-key-path cred))
							(namestring (ssh-key-public-key-path cred)))
						   (:default
							(null-pointer)))
						 (namestring (ssh-key-private-key-path cred))
						 (or (ssh-key-passphrase cred)
							 (null-pointer))))

;; (defcallback %acquire-credentials-ssh-key-from-agent git-error-code
;; 	((git-cred :pointer)
;; 	 (url :string)
;; 	 (username-from-url :string)
;; 	 (allowed-types :uint)
;; 	 (payload :pointer))
;;   ;;(declare (ignorable git-cred url username-from-url allowed-types payload))
;;   (let ((ptr (foreign-alloc '(:struct %git-cred-ssh-key))))
;; 	;; init all to zero
;; 	(dotimes (i size-of-%git-cred-ssh-key)
;; 	  (setf (mem-ref ptr :char i) 0))
;; 	(with-foreign-slots (((:pointer parent)
;; 						  username
;; 						  privatekey
;; 						  publickey)
;; 						 ptr (:struct %git-cred-ssh-key))
;; 	  (setf (foreign-slot-value parent '(:struct %git-cred) 'credtype) :git-credtype-ssh-key)
;; 	  (setf username (foreign-string-alloc username-from-url))
;; 	  (setf privatekey (foreign-string-alloc "/home/etimmons/.ssh/id_rsa_no_passphrase"))
;; 	  ;;(setf publickey (foreign-string-alloc "/home/etimmons/.ssh/id_rsa_no_passphrase.puasdfb"))
;; 	  )
;; 	(break)
;; 	(setf (mem-ref git-cred :pointer) ptr)

;; )
;;   ;;(break)
;; ;;  (format t "url: ~a~%username-from-url: ~a~%" url username-from-url)
;;   :git-ok)

