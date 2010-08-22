#.(require :specific)
(in-package :specific)

(defun led-half-angle-to-achieve-etendue (led-radius target-etendue)
  (declare (vec-float led-radius target-etendue)
	   (values vec-float &optional))
  (sqrt (/ target-etendue (* led-radius led-radius))))

#+nil
(rad->grad (led-half-angle-to-achieve-etendue
	    .5 (oil-objective-etendue .07)))
;; we need 13.9 degree half-angle of the led to illuminate a 140 um
;; diameter field in the sample


(defun led-magnification-to-fit-in-galvo-interval
    (led-half-angle
     &optional (number-of-leds 10) 
     (galvo-half-angle #.(grad->rad 20)))
  (declare (vec-float led-half-angle galvo-half-angle)
	   (fixnum number-of-leds)
	   (values vec-float vec-float &optional))
  ;; all leds should fit into full galvo range because. the mirror has
  ;; a deflection range of +/- 20 degrees, so the leds can be
  ;; distributed over 80 degree
  (let ((uu (/ (* 4 galvo-half-angle) number-of-leds))) 
    (values (magnification-from-angles led-half-angle
				       uu)
	    uu)))

#+nil
(led-magnification-to-fit-in-galvo-interval
 (led-half-angle-to-achieve-etendue
  .5 (oil-objective-etendue (make-objective) .07)))

;; 		 |
;; 	         +--\
;; 	         |   ------\
;; 	         |          -------\
;; 		 |                  ------\
;; 	    D/2	 |                         ------\
;; 	         |                                -------\	 ^
;; yy...         |                             u          ------\| y
;;     ----------+-----------------------------------------------+-----
;; 	b	 |                     g
;;
;; 1/f=1/b+1/g
;; M=yy/y, yy/b=y/g -> M=b/g

(defun magnifier-focal-length-from-its-radius (u M &optional
					       (radius #.(* +one+ 12.5)))
  "M is the magnification of the lens behind the LED. u is the half
angle of LED flux that is meant to be captured by the lens."
  (declare  ((vec-float 0 #.(* 2 pi)) u)
	    (vec-float M radius)
	   (values vec-float vec-float vec-float &optional))
  (let* ((g (/ radius (tan u)))
	 (b (* M g))
	 (f (/ (+ (/ g) (/ b)))))
    (values f g b)))

#+nil
(let* ((u (led-half-angle-to-achieve-etendue
	   .5 (oil-objective-etendue (make-objective) .07)))
       (M (led-magnification-to-fit-in-galvo-interval u)))
  (format nil "~a" (list u M
			 (multiple-value-list
			  (magnifier-focal-length-from-its-radius
			   u M 11.655)))))
