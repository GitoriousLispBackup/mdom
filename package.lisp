;;;; MDOM - (C) 2012 by Michael Bauer <mihi@lo-res.org> 
;;;; Released with BSD License (see LICENSE for details)
;;;; Package declarations

(defpackage :eu.tentacleriot.mdom 
  (:documentation "A simple DOM implementation for HTML/XML") 
  (:use :common-lisp)
  (:export :parse
	   :to-xml
	   :add-child
	   :get-children
	   :get-attributes
	   :get-attribute
	   :get-attribute-values
	   :get-elements-bt-tag-name
	   :attribute
	   :tag
	   :textnode
	   ))