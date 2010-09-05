(in-package :optics)

(defgeneric intersect (ray object))

(defmethod intersect ((ray ray) (plane plane))
  "Find the point where a ray intersects a plane."
  (declare (values vec &optional))
  (with-slots (center normal) plane
    (with-slots (start direction) ray
      (let* ((hess-dist (v. center normal)) 
	     ;; distance of plane to origin
	     (eta (/ (- hess-dist (v. normal start))
		   ;; scaling along ray to hit exactly on plane
		   (v. normal direction))))
	(v+ start (v* eta direction))))))

#+nil
(intersect 
 (make-instance 'ray :start (v 0 1 -1) :direction (v 0 0 1))
 (make-instance 'plane :normal (v 0 0 1) :center (v)))


(define-condition ray-lost () ())

(defgeneric refract (ray object))

;;
;;		   --\
;;		  |   -----\   intersection
;;		  | 	    --+
;;	   |	   \	   ---|--\
;;	   |	   | -----/   |   ----\
;;	   |   ----+/  	r'    |	       ----\
;;       f |--/    c	      |		    ----\      dir
;;         |  	r	      |		         ---\
;;	   |  		      |rho		     ----\
;;	   |   		      |				  ----\
;;	   |   		      |				       ----\
;;	   |   		      |				  phi	    ----\
;;  -------+----------<-------+-------------------------------------------
;;	   |             n    |	center
;;	   |     	      |
;;	   |     	      |
;;	   |    	      |
;;	      		      |
;;	     	      f	      |

(defmethod refract ((ray ray) (lens lens))
  "Return new ray after refraction on thin lens. In general you will
have to normalize its direction. The refraction on an objective needs
the non-normalized result. When the ray doesn't hit the lens the
condition RAY-LOST is signalled."
  (declare (values ray &optional))
  (with-slots (start direction) ray
    (with-slots (center normal focal-length lens-radius) lens
      (assert (< (abs (- 1 (norm direction))) 1e-12))
      (assert (< (abs (- 1 (norm normal))) 1e-12))
      (let* ((intersection (intersect ray lens))
	     (rho (v- intersection center)))
	(format t "~a~%" (norm rho))
	(when (< lens-radius (norm rho))
	  (error 'ray-lost))
	(let* ((cosphi (v. normal direction)))
	  (make-instance 'ray
			 :start intersection
			 :direction (v- (v* (/ focal-length cosphi) direction)
					rho)))))))
#+nil
(handler-case 
    (refract (make-instance 'ray :start (v 0 .1 -10)
			    :direction (v 0 0 1))
	     (make-instance 'lens 
			    :focal-length 10.0
			    :center (v)
			    :normal (v 0 0 1)
			    :lens-radius .2))
  (ray-lost () nil))

;;
;;				   |
;;				   |
;;  ---------------	s	   |
;;-/  		   X----	   |
;;                /     \-- ----   |
;;             ro/         \--  \--+----
;;               |          ----/  |    \-------
;;    	        /     -----/    \  |            \------
;;             / ----/ ru        \ | 	               \-------
;;            /-/                 \|rho		               \-------
;;     	                           |			               \---
;;                     	           |
;;   ------------------------------+-----------------------------------------
;;                     	           |
;;     	       |        nf         /
;;             +--------------------
;;             |                 /

;; 2008 Hwang Simulation of an oil immersion objective lens...
(defmethod refract ((ray ray) (objective objective))
  "Returns the refracted ray with the starting point on the principle
sphere of the objective. If the cap of the principal sphere (given by
NA) is missed then the condition LOST-RAY is signalled."
  (declare (values ray &optional))
  (with-slots (start (dir direction)) ray
    (with-slots ((c center)
		 (n normal)
		 (f focal-length) 
		 (rad lens-radius) 
		 (bfprad bfp-radius)
		 (ri immersion-index)
		 (na numerical-aperture)) objective
      (assert (< (abs (- 1 (norm normal))) 1e-12))
      (assert (< (abs (- 1 (norm direction))) 1e-12))
      ;; call refract for lens and refine the result
      (let* ((lens-ray (call-next-method ray objective))
	     (r (direction lens-ray))
	     (i (start lens-ray))
	     (a (v* (* f (- ri 1)) n))
	     (ru (v+ r a))
	     (rho (v- i c))
	     (rho2 (v. rho rho))
	     (nf (* ri f))
	     (nf2 (* nf nf))
	     (rat (- nf2 rho2)))
	(when (<= 0d0 rat) ;; ray doesn't hit principal sphere
	  (error 'ray-lost))
	(let* ((s (v* (- nf (sqrt rat)) dir))
	       (ro (v- ru s))
	       (nro (normalize ro))
	       (cosu (v. nro normal))
	       (sinu2 (- 1 (* cosu cosu)))
	       (sinu-max (/ na ri)))
	  (when (<= (* sinu-max sinu-max) sinu2) ;; angle to steep
	    (error 'ray-lost))
	  (make-instance 'ray
			 :direction nro 
			 :start (v+ s i)))))))
#+nil
(handler-case 
    (refract (make-instance 'ray 
			    :direction (v 0 0 -1)
			    :start (v 0 .0 10))
	     (make-instance 'objective 
			    :bfp-radius 4.6
			    :numerical-aperture 1.38
			    :lens-radius 8.0
			    :immersion-index 1.515
			    :focal-length 2.3
			    :center (v)
			    :normal (v 0 0 -1)))
#+nil  (ray-lost () nil))

;;   sketch of the mirror for incoming parallel light
;;   --------------------+-----------------------
;; 		      /|\
;; 		     / | \
;; 		    / n|  \	       N=n*(- (p.n))
;; 		q  /   |   \  p	       p+N=r
;; 	          /    v    \	       q=N+r
;; 	         /           \
;; 	        /      |      \	       q=p-2(p.n)*n
;; 	       /       |       \
;; 	      /       N|        \
;; 	     /         |         \
;; 	    /          |     r    \
;; 	   /   	       v<----------\
;; p .. ray-direction
;; N .. mirror-normal

(defgeneric reflect (ray object))

(defmethod reflect ((ray ray) (mirror mirror))
  "Return reflected ray. If the ray isn't inside of the radius return
signal RAY-LOST."
  (declare (values ray &optional))
  (with-slots (start direction) ray
   (with-slots (center normal radius) mirror
     (assert (< (abs (- 1 (norm normal))) 1e-12))
     (assert (< (abs (- 1 (norm direction))) 1e-12))
     (let ((intersection (intersect ray mirror)))
       (when (< (norm (v- intersection center)) radius)
	 (signal 'ray-lost))
       (let ((dir (v+ direction (v* (* +one+ -2 (v. direction normal))
					normal))))
	 (make-instance 'ray 
			:start intersection 
			:direction (normalize dir)))))))

#+nil
(reflect 
 (make-instance 'ray :start (v 0 0 10)
		:direction (v 0 0 -1))
 (make-instance 'mirror :radius 1.0
		:center (v)
		:normal (v 0 0 1)))