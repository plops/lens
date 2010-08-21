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
  "Return new ray-direction after refraction on thin lens. In general
you will have to normalize the result. The refraction on an objective
needs the non-normalized result. When the ray doesn't hit the lens the
condition RAY-LOST is signalled."
  (declare (values vec &optional))
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
	  (v- (v* (/ focal-length cosphi) direction)
	      rho))))))
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
