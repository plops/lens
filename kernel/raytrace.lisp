#.(require :optics)
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
have to normalize the result. The refraction on an objective needs the
non-normalized result. When the ray doesn't hit the lens the condition
RAY-LOST is signalled."
  (declare (values ray &optional))
  (with-slots (start direction) ray
    (with-slots (center normal focal-length lens-radius) lens
      (assert (< (abs (- 1 (norm direction))) 1e-12))
      (assert (< (abs (- 1 (norm normal))) 1e-12))
      (let* ((intersection (intersect ray lens))
	     (rho (v- intersection center)))
	(format t "~a~%" (norm rho))
	(when (< lens-radius (norm rho))
	  (signal 'ray-lost))
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

(declaim (ftype (function (thin-objective 
			    vec vec)
			  (values (or null vec) vec &optional))
		thin-objective-ray))
;; 2008 Hwang Simulation of an oil immersion objective lens...
(defmethod refract ((ray ray) (objective objective))
  "Returns direction of outgoing ray. Call INTERSECT to find the starting point of the outgoing ray  If the principle sphere isn't
hit inside the maximum angle (given by NA) or the back focal plane
isn't hit inside its radius, then the condition LOST-RAY is
singalled."
  (with-slots (center normal focal-length 
		      radius ;; bfp-radius
		      immersion-index numerical-aperture)
      objective
    (unless (< (abs (- 1 (norm normal))) 1d-12)
      (error "lens normal doesn't have unit length"))
   (unless (< (abs (- 1 (norm ray-direction))) 1d-12)
     (error "ray-direction doesn't have unit length"))
 
   (multiple-value-bind (r intersection)
       (lens-ray objective 
		 ray-start ray-direction :normalize nil)
     (unless r 
       (return-from thin-objective-ray (values nil intersection)))
     (let* ((a (v* normal (* focal-length (- immersion-index 1))))
	    (ru (v+ r a))
	    (rho (v- intersection center))
	    (rho2 (v. rho rho))
	    (nf (* immersion-index focal-length))
	    (nf2 (* nf nf))
	    (rat (- nf2 rho2)))
       (unless (<= 0d0 rat)
	 (return-from thin-objective-ray (values nil intersection))
	 #+nil	(error "ray can't pass through objective"))
       (let* ((s (v* ray-direction (- nf (sqrt rat))))
	      (ro (v- ru s))
	      (cosu (v. ro normal))
	      (sinu2 (- 1 (* cosu cosu)))
	      (sinu-max (/ numerical-aperture immersion-index)))
	 (unless (<= sinu2 (* sinu-max sinu-max))
	   (return-from thin-objective-ray (values nil intersection)))
	 (values ro (v+ s intersection)))))))

#+nil
(thin-objective-ray (v 0d0 0d0 0d0)
		    (v 0d0 0d0 -1d0)
		    2.3d0 1.515d0
		    (v 0d0 1d0 10d0)
		    (normalize (v 0d0 0d0 -1d0)))
