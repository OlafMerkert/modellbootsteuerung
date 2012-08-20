(defsystem hello-katja-sender
  :depends-on (ol-utils
               com.gigamonkeys.binary-data
               lispbuilder-sdl)
  :serial t
  :components ((:file "datenprotokoll")
               (:file "joystick-eingabe")
               (:file "fernsteuerung")))
