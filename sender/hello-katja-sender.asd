(defsystem :hello-katja-sender
  :depends-on (ol-utils
               com.gigamonkeys.binary-data)
    :serial t
    :components ((:file "datenprotokoll")))
