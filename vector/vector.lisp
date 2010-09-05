(in-package :vector)

(declaim (optimize (speed 2) (safety 3) (debug 3)))

#+nil(declaim (optimize (speed 3) (safety 0) (debug 0)))

(deftype vec-float-helper ()
  `double-float)

(deftype vec-float (&optional (low '*) (high '*))
  `(double-float ,(if (eq low '*)
		      '*
		      (coerce low 'vec-float-helper)) 
		 ,(if (eq high '*)
		      '*
		      (coerce high 'vec-float-helper))))

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
  `(defun ,(alexandria:format-symbol :vector "V~a" op) (a b)
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
#+nil
(v+ (v 2 3 4) (v 1 2 3))

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

(defun vx (a b)
  "Cross product"
  (declare (vec a b)
	   (values vec &optional))
  (with-arrays (a b)
    (make-vec (- (* (a 1) (b 2))
		 (* (a 2) (b 1)))
	      (- (* (a 2) (b 0))
		 (* (a 0) (b 2)))
	      (- (* (a 0) (b 1))
		 (* (a 1) (b 0))))))
#+nil
(vx (v 1)
    (v 0 1))

(defun norm (v)
  (declare (vec v)
	   (values (vec-float 0) &optional))
  (let ((l2 (v. v v)))
    (declare ((vec-float 0) l2))
    (sqrt l2)))

(defun normalize (v)
  (declare (vec v)
	   (values vec &optional))
  (let ((l (norm v)))
    (if (zerop l)
	(v 0 0 1)
	(v* (/ l) v))))


(deftype mat ()
  `(simple-array vec-float (3 3)))

(defun m (a b c d e f g h i)
  (make-array '(3 3)
              :element-type 'vec-float
              :initial-contents (list (list a b c)
				      (list d e f)
				      (list g h i))))

(defun rotation-matrix (angle vec)
  "Create matrix that rotates by ANGLE radians around the direction
 VECT. VECT must be normalized."
  (declare ((real 0 #.(* 2 pi)) angle)
	   (vec vec)
	   (values mat &optional))
  (with-arrays (vec)
    (let* ((u (vec 0)) (v (vec 1)) (w (vec 2))
	   (c (cos (coerce angle 'vec-float)))
	   (s (sqrt (- 1 (* c c))))
	   (1-c (- 1 c))
	   (su (* s u)) (sv (* s v)) (sw (* s w)))
      (m (+ c (* 1-c u u))   (+ (* 1-c u v) sw)   (- (* 1-c u w) sv)
	 (- (* 1-c u v) sw)  (+ c (* 1-c v v))    (+ (* 1-c v w) su)
	 (+ (* 1-c u w) sv)  (- (* 1-c v w) su)   (+ c (* 1-c w w))))))
#+nil
(rotation-matrix (/ pi 2) (v 0 0 1))

(defun m* (m v)
  "Multiply matrix M with vector V."
  (declare (mat m)
	   (vec v)
	   (values vec &optional))
  (let ((res (v)))
    (with-arrays (res v m)
      (dotimes (i 3)
	(dotimes (j 3)
	  (incf (res i) (* (m i j) (v j))))))
    res))
#+nil
(m* (rotation-matrix (/ pi 2) (v 0d0 0d0 1d0)) (v 1d0))
