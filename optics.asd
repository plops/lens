(asdf:defsystem optics
  :depends-on (:alexandria :vector)
  :components
  ((:module "optics"
	    :serial t
	    :components ((:file "packages")
			 (:file "objects")
			 (:file "raytrace")
			 (:file "helpers")))))
