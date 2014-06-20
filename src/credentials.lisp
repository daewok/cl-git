;;; -*- Mode: Lisp; Syntax: COMMON-LISP; Base: 10 -*-

;; cl-git is a Common Lisp interface to git repositories.
;; Copyright (C) 2011-2014 Russell Sim <russell.sim@gmail.com>
;; Copyright (C) 2014 Eric Timmons <etimmons@alum.mit.edu>
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

(defvar *available-credentials* nil "A list of the credentials
available for interacting with remotes.")

(defbitfield git-credtype
  (:userpass-plaintext)
  (:ssh-key)
  (:ssh-custom)
  (:default)
  (:ssh-interactive))

(defcfun %git-cred-ssh-key-from-agent
    :int
  (git-cred :pointer)
  (username :string))

(defcfun %git-cred-ssh-key-new
    :int
  (git-cred :pointer)
  (username :string)
  ;; this is a pointer so we can pass null to it.
  (publickey :pointer)
  (privatekey :string)
  ;; This is a pointer so we can pass null to it.
  (passphrase :pointer))

(defcfun %git-cred-userpass-plaintext-new
    :int
  (git-cred :pointer)
  (username :string)
  (password :string))

(defcallback git-cred-acquire-cb
    :int
    ((git-cred :pointer)
     (url :string)
     (username-from-url :string)
     (allowed-types git-credtype)
     (payload :pointer))
  ;; If no credentials have been provided, return a positive integer.
  (if *available-credentials*
      (acquire-credentials *available-credentials* git-cred url username-from-url allowed-types payload)
      1))

(defmethod acquire-credentials ((credential-type (eql 'ssh-key-from-agent)) git-cred url username-from-url allowed-types payload)
  (declare (ignore payload url allowed-types))
  (%git-cred-ssh-key-from-agent git-cred username-from-url))

(defmethod acquire-credentials ((credential-type (eql 'ssh-key)) git-cred url username-from-url allowed-types payload)
  (declare (ignore payload url allowed-types))
  ;; Tries the default values for an ssh key: private key at
  ;; ~/.ssh/id_rsa, public key at ~/.ssh/id_rsa.pub, no passphrase.
  (%git-cred-ssh-key-new git-cred username-from-url
                         (null-pointer)
                         (namestring (merge-pathnames ".ssh/id_rsa" (user-homedir-pathname)))
                         (null-pointer)))

(defmethod acquire-credentials (credential-type git-cred url username-from-url allowed-types payload)
  (declare (ignore payload url allowed-types credential-type username-from-url git-cred))
  2)

(defclass credentials ()
  ())

(defclass ssh-key (credentials)
  ((public-key
    :initform nil
    :initarg :public-key
    :accessor public-key)
   (private-key
    :initform (namestring (merge-pathnames ".ssh/id_rsa" (user-homedir-pathname)))
    :initarg :private-key
    :accessor private-key)
   (passphrase
    :initform nil
    :initarg :passphrase
    :accessor passphrase))
  (:documentation "An SSH-key credential, possibly with a nonstandard
  path."))

(defmethod acquire-credentials ((credentials ssh-key) git-cred url username-from-url allowed-types payload)
  (declare (ignore payload url allowed-types))
  ;; Tries the default values for an ssh key: private key at
  ;; ~/.ssh/id_rsa, public key at ~/.ssh/id_rsa.pub, no passphrase.
  (%git-cred-ssh-key-new git-cred username-from-url
                         (if (public-key credentials)
                             (public-key credentials)
                             (null-pointer))
                         (private-key credentials)
                         (if (passphrase credentials)
                             (passphrase credentials)
                             (null-pointer))))

(defclass username-password (credentials)
  ((username
    :initform nil
    :initarg :username
    :accessor username)
   (password
    :initform (error "A password must be provided.")
    :initarg :password
    :accessor password)))


(defmethod acquire-credentials ((credentials username-password) git-cred url username-from-url allowed-types payload)
  (declare (ignore payload url allowed-types))
  (%git-cred-userpass-plaintext-new git-cred
                                    (or (username credentials)
                                        username-from-url)
                                    (password credentials)))

