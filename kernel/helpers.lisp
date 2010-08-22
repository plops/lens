(in-package :optics)

;; 		        |
;; --------		|
;;   SS    \---		|
;;             \--      |
;;                X-----+-------------------------------
;;              /-  \   |
;;            /-     \  |
;;    nf=h  /-        \ | D/2=R
;;        /-           \|
;;      /-              |
;;    /- alpha	        |
;; ---------------------+----------------------------------------
;;             nf       |
;; 
;; sin(alpha) = R/nf
(defmethod back-focal-plane-radius ((objective objective))
  (declare (values vec-float &optional))
  (with-slots (focal-length numerical-aperture) objective
    (* focal-length numerical-aperture)))

#+nil
(back-focal-plane-radius 2.61 1.38)

(defun focal-length-from-magnification (mag)
  (declare (vec-float mag)
	   (values vec-float &optional))
  (/ 164.5 mag))
#+nil
(focal-length-from-magnification 63.0)

(defun make-objective (&key (magnification #.(* +one+ 63))
		       (numerical-aperture #.(* +one+ 1.38))
		       (immersion-index #.(* +one+ 1.515))
		       (center (v))
		       (normal (v 0 0 1)))
  (let* ((o (make-instance 'objective 
			   :bfp-radius +one+ ;; these need to be generated
			   :focal-length +one+
			   :lens-radius +one+
			   :numerical-aperture numerical-aperture
			   :immersion-index immersion-index
			   :center center
			   :normal normal))
	 (bfp-radius (back-focal-plane-radius o))
	 (f (focal-length-from-magnification magnification)))
    (make-instance 'objective 
		   :bfp-radius bfp-radius
		   :focal-length f
		   :lens-radius (* 10 bfp-radius)
		   :numerical-aperture numerical-aperture
		   :immersion-index immersion-index
		   :center center
		   :normal normal)))
#+nil
(make-objective)

(defun grad->rad (grad)
  (declare (real grad)
	   (values vec-float &optional))
  (* #.(coerce (/ pi 180) 'vec-float)
     (coerce grad 'vec-float)))

(defun rad->grad (rad)
  (declare (real rad)
	   (values vec-float &optional))
  (* #.(coerce (/ 180 pi) 'vec-float)
     (coerce rad 'vec-float)))

(defun etendue (chief-height marginal-angle
		&optional
		(refractive-index +one+)
		(marginal-height +zero+) (chief-angle +zero+))
  (declare (vec-float chief-height marginal-angle refractive-index
		      marginal-height chief-angle)
	   (values vec-float &optional))
  (let ((q (- (* chief-height refractive-index marginal-angle)
	      (* marginal-height refractive-index chief-angle))))
    (* q q)))
#+nil
(etendue .07 (grad->rad 67) 1.515)

(defmethod oil-objective-etendue ((objective objective) field-radius)
  (declare (vec-float field-radius)
	   (values vec-float &optional))
  (with-slots (numerical-aperture immersion-index) objective
   (let ((rat (/ numerical-aperture immersion-index)))
     (unless (<= (abs rat) 1)
       (error "impossible angle, check numerical aperture and refractive index."))
     (let* ((marginal-angle (asin rat)))
       (etendue field-radius marginal-angle immersion-index)))))

#+nil
(oil-objective-etendue (make-objective) .07)

(defun magnification-from-angles (u uu &optional (n +one+) (nn +one+))
  "u is the objects marginal ray angle and uu for the image."
  (declare ((vec-float 0 #.(* 2 pi)) u uu)
	   (vec-float n nn)
	   (values vec-float &optional))
  (/ (* n (sin u))
     (* nn (sin uu))))

