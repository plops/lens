(in-package :cl-user)

(defpackage :vector
  (:use :cl)
  (:export #:with-arrays
	   #:vec
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
