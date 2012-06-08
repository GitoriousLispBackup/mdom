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

(defgeneric add-child (n c))

(defmethod add-child ((n tag)(c node))
  (with-slots (children) n (setf children (append children (list c)))))

(defgeneric to-xml (node))

(defmethod to-xml ((node tag))
  (with-slots (tag children attributes) node 
    (if children
	(let ((ret 
	       (concatenate 'string "<" tag 
			    (if (> (length attributes) 0) (concatenate 'string " " attributes) "") 
			    ">"))) 
	  (loop for child in children do (setf ret (concatenate 'string ret (to-xml child))))
	  (concatenate 'string ret "</" tag ">"))
	(concatenate 'string "<" tag 
		     (if (> (length attributes) 0) (concatenate 'string " " attributes) "") 
		     "/>"))))
  

(defmethod to-xml ((node textnode))
  (slot-value node 'text))

  
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
  (let ((spc (position #\space str))) 
    (if spc (subseq str 0 spc) str)))

(defun extract-attribute (str)
  (let ((spc (position #\space str))) 
    (if spc (subseq str (+ 1 spc)) "")))
  
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