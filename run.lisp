#.(require :optics)
#.(require :gui)
(defpackage :run
  (:use :cl :vector :gui))
(in-package :run)

(defun run ()
  (with-gui
    (gl:clear-color 1 0 0 1)
    (gl:clear :color-buffer-bit)))

#+nil
(run)
