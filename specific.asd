(asdf:defsystem specific
  :depends-on (:alexandria :vector :optics)
  :components
  ((:module "specific"
	    :serial t
	    :components ((:file "packages")))))
