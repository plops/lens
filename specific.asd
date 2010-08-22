(asdf:defsystem specific
  :depends-on (:alexandria :vector :optics)
  :components
  ((:module "model"
	    :serial t
	    :components ((:file "packages")))))
