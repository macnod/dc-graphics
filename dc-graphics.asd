(asdf:defsystem :dc-graphics
  :description "Graphics functions that Donnie uses all the time."
  :author "Donnie Cameron <macnod@gmail.com>"
  :license "MIT License"
  :depends-on (:vecto :dc-utilities)
  :serial t
  :components ((:file "dc-graphics-package")
               (:file "dc-graphics")))
