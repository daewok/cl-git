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

;; General sizes.
(include "stddef.h")
(include "git2/errors.h")

(ctype size-t "size_t")

;; Errors

(cenum git-error-code
	   ((:git-passthrough "GIT_PASSTHROUGH"))
	   ((:git-ok "GIT_OK")))

;; credentials

(include "git2/transport.h")

(cstruct %git-cred "git_cred"
		 (credtype "credtype" :type git-credtype-t)
		 (free "free" :type :pointer))

(cstruct %git-cred-ssh-key "git_cred_ssh_key"
		 (parent "parent" :type (:struct %git-cred))
		 (username "username" :type :string)
		 (publickey "publickey" :type :string)
		 (privatekey "privatekey" :type :string)
		 (passphrase "passphrase" :type :string))

;; Option structures and constants.

(include "git2/clone.h") ; clone.h pulls in all of these

(constant (+git-clone-options-version+ "GIT_CLONE_OPTIONS_VERSION")
		  :type integer
		  :documentation "The constant representing the version of the
options structure.")

(constant (+git-checkout-opts-version+ "GIT_CHECKOUT_OPTS_VERSION")
		  :type integer
		  :documentation "The constant representing the version of the
options structure.")

(constant (+git-remote-callbacks-version+ "GIT_REMOTE_CALLBACKS_VERSION")
		  :type integer
		  :documentation "The constant representing the version of the
remote callbacks structure.")

;; (cenum git-checkout-strategy
;; 	   ((:git-checkout-safe-create "GIT_CHECKOUT_SAFE_CREATE")))

;; (bitfield git-checkout-strategy-t
;; 		  ((:safe "git_checkout_strategy_t.GIT_CHECKOUT_SAFE_CREATE")))

(cstruct %git-checkout-opts "git_checkout_opts"
		 (version "version" :type :uint)
		 (checkout-strategy "checkout_strategy" :type git-checkout-strategy))

(cstruct %git-remote-callbacks "git_remote_callbacks"
		 (version "version" :type :uint)
		 (credentials "credentials" :type :pointer))

(cstruct %git-clone-options "git_clone_options"
		 (version "version" :type :uint)
		 (checkout-options "checkout_opts" :type (:struct %git-checkout-opts))
		 (remote-callbacks "remote_callbacks" :type (:struct %git-remote-callbacks)))

