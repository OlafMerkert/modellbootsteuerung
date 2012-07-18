#!/bin/sh

SERIALPATH=/dev/ttyUSB0
BAUDRATE=115200

setserial $SERIALPATH skip_test auto_irq # autoconfig
stty $BAUDRATE cs8 -parenb -crtscts -echo -F $SERIALPATH
