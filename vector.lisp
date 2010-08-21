#.(require :vector)
(in-package :vector)
(declaim (optimize (speed 2) (safety 3) (debug 3)))
#+nil(declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype vec-float ()
  `single-float)

(defconstant +zero+ #.(coerce 0 'vec-float))
(defconstant +one+ #.(coerce 1 'vec-float))

(deftype vec ()
  `(simple-array vec-float (3)))

(defun make-vec (&optional (x +zero+) (y +zero+) (z +zero+))
  (declare (vec-float x y z)
	   (values vec &optional))
  (make-array 3 
	      :element-type 'vec-float
	      :initial-contents (list x y z)))

(defmacro v (&rest args)
  `(make-vec ,@(mapcar #'(lambda (x)
			   (coerce x 'vec-float))
		       args)))

#+nil
(v 34d0)

#+nil
(v 1 2)


(defun v. (a b)
  (declare (vec a b)
	   (values vec-float &optional))
  (let ((sum +zero+))
    (declare (vec-float sum))
    (with-arrays (a b)
      (dotimes (i 3)
	(incf sum (* (a i) (b i)))))
    sum))

#+nil
(v. (v 1 2 3) (v 2 3 4))


(defmacro def-v-op (op)
  `(defun ,(alexandria:format-symbol nil "V~a" op) (a b)
     (declare (vec a b)
	      (values vec &optional))
     (let ((result (v)))
       (with-arrays (result a b)
	(dotimes (i 3)
	  (setf (result i) (,op (a i) (b i)))))
       result)))

(def-v-op +)
(def-v-op -)

#+nil
(v- (v) (v 1 2 3))

(defun v* (scalar v)
  (declare (vec-float scalar)
	   (vec v)
	   (values vec &optional))
  (let ((result v))
    (with-arrays (result v)
      (dotimes (i 3)
	(setf (result i) (* scalar (v i)))))
    result))

#+nil
(v* 2.0 (v 1 2 3))