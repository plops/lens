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
(defun back-focal-plane-radius (focal-length numerical-aperture)
  (declare (vec-float focal-length numerical-aperture)
	   (values vec-float &optional))
  (* focal-length numerical-aperture))

#+nil
(back-focal-plane-radius 2.61 1.38)

(defun focal-length-from-magnification (mag)
  (declare (vec-float mag)
	   (values vec-float &optional))
  (/ 164.5 mag))
#+nil
(focal-length-from-magnification 63.0)

(defun grad->rad (grad)
  (declare (real grad)
	   (values vec-float &optional))
  (* #.(coerce (/ pi 180) 'vec-float)
     (coerce grad 'vec-float)))

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

(defun oil-objective-etendue (field-radius &optional 
			      (numerical-aperture #.(* +one+ 1.38))
			      (refractive-index #.(* +one+ 1.515)))
  (declare (vec-float field-radius numerical-aperture refractive-index)
	   (values vec-float &optional))
  (let ((rat (/ numerical-aperture refractive-index)))
    (unless (<= (abs rat) 1)
      (error "impossible angle, check numerical aperture and refractive index."))
    (let* ((marginal-angle (asin rat)))
      (etendue field-radius marginal-angle refractive-index))))

#+nil
(oil-objective-etendue .07)

(defun magnification-from-angles (u uu &optional (n +one+) (nn +one+))
  "u is the objects marginal ray angle and uu for the image."
  (declare ((vec-float 0 #.(* 2 pi)) u uu)
	   (vec-float n nn)
	   (values vec-float &optional))
  (/ (* n (sin u))
     (* nn (sin uu))))

