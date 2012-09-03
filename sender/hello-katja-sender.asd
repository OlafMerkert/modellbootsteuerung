(defsystem hello-katja-sender
  :depends-on (ol-utils
               com.gigamonkeys.binary-data
               lispbuilder-sdl
               mcclim)
  :serial t
  :components ((:file "datenprotokoll")
               (:file "joystick-eingabe")
               (:file "fernsteuerung")
               (:file "fernsteuerung-gui")))
