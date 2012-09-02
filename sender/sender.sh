#!/bin/sh

# start sender with XBox controller in use

sbcl <<EOF
(ql:quickload 'hello-katja-sender)
(in-package :fernsteuerung)
(steuerung 'flugzeug 'Hotas-X)
EOF
