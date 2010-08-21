(in-package :cl-user)

(defpackage :optics
  (:use :cl :vector)
  (:export #:plane
	   #:center
	   #:normal
	   #:circular-mirror
	   #:radius
	   #:lens
	   #:lens-radius
	   #:focal-length
	   #:objective
	   #:immersion-index
	   #:numerical-aperture
	   #:bfp-radius
	   #:ray
	   #:start
	   #:direction
	   #:ray-lost
	   #:intersect
	   #:refract))
