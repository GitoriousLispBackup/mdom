;;;; -*- Mode: Lisp; Syntax: ANSI-Common-Lisp; Base: 10 -*-
(defpackage #:mdom-asd
  (:use :cl :asdf))

(in-package :mdom-asd)

(defsystem mdom
  :name "eu.tentacleriot.mdom"
  :version "0.0"
  :maintainer "Michael Bauer <mihi@lo-res.org>"
  :author "Michael Bauer <mihi@lo-res.org>"
  :licence "BSD (see LICENSE for details)"
  :description "A simple HTML/XML DOM tree parser"
  :long-description "A simple HTML/XML DOM tree parser"
  :serial T
  :components ((:file "package")
	       (:file "mdom" :depends-on "package")))