#!/bin/bash -e

dup xlunit -Ufiles.b9.com -D/home/ftp/xlunit -C"(umask 022; /home/kevin/bin/remove-old-versions xlunit latest)" -su $*
