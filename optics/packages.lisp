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
	   #:refract
	   #:reflect
	   #:lambert-random
	   #:lambert-direction
	   #:back-focal-plane-radius
	   #:focal-length-from-magnification
	   #:grad->rad
	   #:rad->grad
	   #:etendue
	   #:oil-objective-etendue
	   #:magnification-from-angles
	   #:make-objective))
