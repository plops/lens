(asdf:defsystem vector
  :depends-on (:alexandria)
  :components
  ((:module "vector"
	    :serial t
	    :components ((:file "packages")
			 (:file "macros")
			 (:file "vector")))))
