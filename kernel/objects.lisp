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
  ;; set the lens-radius to something bigger (maybe twice bfp-radius)
  ;; to prevent spurious RAY-LOST when dispatching to lens refract
  ((immersion-index :accessor immersion-index :initarg :immersion-index
		    :initform (alexandria:required-argument)
		    :type vec-float)
   (numerical-aperture :accessor numerical-aperture :initarg :numerical-aperture
		    :initform (alexandria:required-argument)
		    :type vec-float)
   (bfp-radius :accessor bfp-radius :initarg :bfp-radius
		:initform (alexandria:required-argument)
		:type vec-float)))

(defmethod print-object ((objective objective) stream)
  (with-slots (immersion-index numerical-aperture focal-length) objective
   (format stream "#<objective f: ~2,2f na: ~f index: ~f>" focal-length
	   numerical-aperture immersion-index)))

(defclass ray ()
  ((start :accessor start :initarg :start
	  :initform (alexandria:required-argument)
	  :type vec)
   (direction :accessor direction :initarg :direction
	  :initform (alexandria:required-argument)
	  :type vec)))

(defmethod print-object ((ray ray) stream)
  (with-slots (start direction) ray
   (format stream "#<ray start: ~a dir: ~a>" start direction)))

#+nil
(make-instance 'ray :start (v) :direction (v))