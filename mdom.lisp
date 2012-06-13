;;;; MDOM - A simple DOM XML/HTML Parser (C) 2012 by Michael Bauer <mihi@lo-res.org>
;;;; Released with BSD License, see LICENSE for details

(in-package :eu.tentacleriot.mdom )

(defclass node ()
  ()
  )

(defclass tag (node)
  ((tag :initarg :tag)
   (attributes :initarg :attributes)
   (children :initform ())))

(defclass textnode (node)
  ((text :initarg :text)))

(defclass attribute (node)
  ((name :initarg :name)
   (value :initarg :value)))

(defgeneric add-child (n c))

(defmethod add-child ((n tag)(c node))
  (with-slots (children) n (setf children (append children (list c)))))

(defgeneric get-children (node))

(defmethod get-children ((node tag))
  (slot-value node 'children))

(defmethod get-children ((node textnode))
  nil)

(defgeneric get-attributes (node))

(defmethod get-attributes ((node tag))
  (slot-value node 'attributes))

(defgeneric get-attribute (node name))

(defmethod get-attribute ((node tag) name) 
       (delete-if-not (lambda (x) (string= (slot-value x 'name) name)) 
		      (get-attributes node)))

(defgeneric get-attribute-values (node name))

(defmethod get-attribute-values ((node tag) name)
  (loop for x in (get-attribute node name) collecting (slot-value x 'value)))

(defmethod get-attribute-values ((node textnode) name) nil)

(defgeneric get-tag (node))

(defmethod get-tag ((node tag))
  (slot-value node 'tag))

(defmethod get-tag ((node textnode))
  Nil)

(defgeneric to-xml (node))

(defmethod to-xml ((node tag))
  (with-output-to-string (stream)
    (with-slots (tag children attributes) node 
      (format stream "<~a" tag)
      (if attributes (format stream "~{ ~a~}" (loop for i in attributes collecting (to-xml i))))
      (if children (format stream ">~{~a~}</~a>" (loop for i in children collecting (to-xml i)) tag)
	  (format stream "/>")))))

(defmethod to-xml ((node textnode))
  (slot-value node 'text))

(defmethod to-xml ((node attribute))
  (with-output-to-string (stream) 
    (with-slots (name value) node
      (format stream "~a=\"~a\"" name value))))

(defmethod print-object ((node tag) stream)
  (print-unreadable-object (node stream :type t)
    (with-slots (tag) node 
      (format stream " ~a" tag))))

(defmethod print-object ((node textnode) stream)
  (print-unreadable-object (node stream :type t)
    (with-slots (text) node 
      (let ((otext (if (> (length text) 10) (concatenate 'string (subseq text 0 7) "...") text)))
	(format stream " ~a" otext)))))

(defmethod print-object ((node attribute) stream)
  (print-unreadable-object (node stream :type t)
    (with-slots (name value) node
      (format stream "~a=\"~a\"" name value))))
  
(defun collect-string-until (stop stream &key (peek T))
  (coerce 
   (loop for char = (if peek (peek-char nil stream nil)
			(read-char stream nil))
      while (and char (not (char= char stop))) 
      collecting (if peek (read-char stream)
		     char)) 
   'string))

(defun read-text (stream) 
  (collect-string-until #\< stream))

(defun process-text (stream)
  (make-instance 'textnode :text (read-text stream)))

(defun read-tag (stream)
  (subseq (collect-string-until #\> stream :peek Nil) 1))

(defun skip-spaces (stream)
  (loop while (and (peek-char nil stream nil) (char= (peek-char nil stream) #\Space)) do (read-char stream)))

(defun remove-tail-spaces (str)
  (labels ((tsp (str) 
	     (let ((l (length str))) 
	       (if (string= (subseq str (- l 1) l) " ") 
		   (tsp (subseq str 0 (- l 1))) 
		   str))))
    (tsp str)))

(defun closing-tag-p (str)
  (let ((ps (position #\/ str)))
    (and ps (= 0 ps))))

(defun closes-itself-p (str)
  (position #\/ str))

(defun process-tag (stream)
  (let ((tag (read-tag stream)))
    (let ((tag-name (extract-tag-name tag))(attributes (extract-attribute tag)))
      (if (closing-tag-p tag-name) nil
	  (let ((node (make-instance 'tag :tag tag-name :attributes attributes)))
	    (if (not (closes-itself-p tag))
	    (loop for next-node = (read-next stream) while next-node do
		 (add-child node next-node)))
	    node)))))
 
(defun extract-tag-name (str)
  (let ((tn (with-input-from-string (stream str)
	      (skip-spaces stream)
	      (collect-string-until #\Space stream))))
    (let ((l (length tn))) (if (string= (subseq tn (- l 1) l) "/") 
			       (subseq tn 0 (- l 1)) 
			       tn))))

(defun extract-attribute-name (stream)
  (skip-spaces stream)
  (remove-tail-spaces (collect-string-until #\= stream :peek Nil)))

(defun extract-attribute-value (stream)
  (skip-spaces stream)
  (let ((char (peek-char nil stream nil))) (if (and char (or (char= char #\') (char= char #\"))) (read-char stream))
  (collect-string-until (case char (#\' #\') (#\" #\") (otherwise #\space)) stream :peek Nil)))

(defun extract-attribute (str)
  (delete-if (lambda (x) (string= (slot-value x 'name) "/"))
  (with-input-from-string (stream (remove-tail-spaces str))
    (skip-spaces stream)
    (collect-string-until #\space stream :peek Nil)
    (loop while (peek-char nil stream nil) 
       collecting (make-instance 'attribute :name (extract-attribute-name stream) :value (extract-attribute-value stream))))))
  
(defun read-next (stream)
 (let ((char (peek-char nil stream nil)))
   (if char (if (char= char #\<) 
		(process-tag stream)
		(process-text stream))
       nil)))
	       

(defun read-stream (stream)
  (loop for char = (peek-char nil stream nil) while char 
     collecting (read-next stream)))

(defun parse (html)
  (with-input-from-string (stream html) (read-stream stream)))

(defun flatten (tree)
  (labels ((flat (node) (append (list node) (mapcan #'flat (get-children node))))) (flat tree)))

(defun get-elements-by-tag-name (tree tag)
  (delete-if-not (lambda (x) (and (slot-exists-p x 'tag) (string= (slot-value x 'tag) tag))) (flatten tree)))

(defun get-element-by-id (tree id)
  (car 
   (delete-if-not (lambda (x) (string= (car (get-attribute-values x "id")) id)) (flatten tree))))