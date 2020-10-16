#!/usr/bin/env perl

use strict ;
BEGIN { push (@INC, "..") }
use Version ;

our $destdir = shift @ARGV ;

print <<"EOF";
# Weak-tea Event loop
version = "$Version::version"
description = "event"
requires = "unix,fmt"
archive(byte) = "event.cmo iBuffer.cmo justBuffer.cmo directJustBuffer.cmo"
archive(native) = "event.cmx iBuffer.cmx justBuffer.cmx directJustBuffer.cmx"

EOF
