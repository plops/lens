(asdf:defsystem vector
  :depends-on (:alexandria)
  :serial t
  :components ((:file "packages")
	       (:file "macros")))
