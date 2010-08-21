(in-package :cl-user)

(defpackage :vector
  (:use :cl)
  (:export #:with-arrays
	   #:vec
	   #:vec-float
	   #:make-vec
	   #:v
	   #:v.
	   #:v-
	   #:v+
	   #:v*
	   #:vx
	   #:norm
	   #:normalize
	   #:mat
	   #:rotation-matrix
	   #:m*))
