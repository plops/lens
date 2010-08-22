(asdf:defsystem optics
  :depends-on (:alexandria :vector)
  :components
  ((:module "kernel"
	    :serial t
	    :components ((:file "packages")
			 (:file "objects")
			 (:file "raytrace")
			 (:file "helpers")))))
