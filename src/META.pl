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
archive(byte) = "event.cmo iBuffer.cmo"
archive(native) = "event.cmx iBuffer.cmx"

EOF
