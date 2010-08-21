(asdf:defsystem vector
  :depends-on (:alexandria)
  :components
  ((:module "math"
	    :serial t
	    :components ((:file "packages")
			 (:file "macros")
			 (:file "vector")))))
