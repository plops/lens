(in-package :optics)

(defclass plane ()
  ((center :accessor center :initarg :center 
	   :initform (alexandria:required-argument)
	   :type vec)
   (normal :accessor normal :initarg :normal
	   :initform (alexandria:required-argument)
	   :type vec)))

(defclass mirror (plane)
  ((radius :accessor radius :initarg :radius
	   :initform (alexandria:required-argument)
	   :type vec-float)))

(defclass lens (plane)
  ((lens-radius :accessor lens-radius :initarg :lens-radius
		:initform (alexandria:required-argument)
		:type vec-float)
   (focal-length :accessor focal-length :initarg :focal-length
		 :initform (alexandria:required-argument)
		 :type vec-float)))

(defclass objective (lens)
  ((immersion-index :accessor immersion-index :initarg :immersion-index
		    :initform (alexandria:required-argument)
		    :type vec-float)
   (numerical-aperture :accessor numerical-aperture :initarg :numerical-aperture
		    :initform (alexandria:required-argument)
		    :type vec-float)
   (bfp-radius :accessor bfp-radius :initarg :bfp-radius
		:initform (alexandria:required-argument)
		:type vec-float)))

(defclass ray ()
  ((start :accessor start :initarg :start
	  :initform (alexandria:required-argument)
	  :type vec)
   (direction :accessor direction :initarg :direction
	  :initform (alexandria:required-argument)
	  :type vec)))