(in-package :optics)

(defun lambert-random ()
  "Generate a random variable with a lambert-cos distribution
in 0..pi/2. The biggest probability is for an angle 45 degree (pi/4).
Its zero for 0 and pi/2."
  (declare (values vec-float &optional))
  ;; note that this was difficult to get right. You have to consider
  ;; that a uniform phi will add a higher density of points for theta=0 and
  ;; theta=pi/2, i.e. the poles of the lambert sphere using theta0 would give
  ;; uniform spread of the random points on a unit sphere, centered on
  ;; the origin. The transformation below should make a uniform sphere
  ;; ontop of the origin, resembling the lambert cosine law.
  (let ((theta0 (asin (- (random (* +one+ 2)) 1))))
    (* +one+ .5 (+ #.(coerce (* .5 pi) 'vec-float) theta0))))

#+nil
(time
 (let ((hist (make-array 360 :element-type 'fixnum)))
   ;; check that the distribution of the random variable is as expected
   ;; due to the rounding the bin is only half filled for theta=90 grad
   ;; the distribution will be right anyway
   (dotimes (i (expt 10 7))
     (incf (aref hist (floor (lambert-random) (/ pi 180)))))
   (with-open-file (s "/dev/shm/o.dat" :direction :output
		      :if-exists :supersede)
     (dotimes (i 180)
       (format s "~d ~d~%" i (aref hist i))))
   (with-open-file (s "/dev/shm/p1.gp" :direction :output
		      :if-exists :supersede)
     (format s "plot \"/dev/shm/o.dat\" u 1:2 w l
pause -1"))))

(defun lambert-direction ()
  "Create a normalized direction vector with lambert-cos distributed theta
and uniform phi distribution. Theta=0 is the z-axis. The z-component
will be positive."
  (declare (values vec &optional))
  (let* ((theta (lambert-random))
	 (st (sin theta))
	 (phi (random #.(coerce (* 2 pi) 'vec-float))))
    (declare ((vec-float 0 #.(* .5 pi)) theta) ;; ranges were a bit complicated
	     ((vec-float 0 #.(* 2 pi)) phi))
    (make-vec (* st (cos phi)) ;; convert spherical coordinates into cartesian
	      (* st (sin phi))
	      (* (cos theta)))))

#+nil
(lambert-direction)
