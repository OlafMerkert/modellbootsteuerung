#!/bin/zsh

# start sender with XBox controller in use

sbcl <<EOF
(ql:quickload 'hello-katja-sender)
(in-package :joystick)
(steuerung-main xbox-spec)
EOF
