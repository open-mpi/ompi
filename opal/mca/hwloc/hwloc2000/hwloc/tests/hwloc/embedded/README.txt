This test is meant to be run manually; it is not part of "make check".

Someday I may figure out how to make this part of "make check", but
today is not that day.  :-)

You can run these tests in one of two ways:

1. PREFERRED METHOD: From this directory, invoke
   "./run-embedded-tests.sh <path-to-hwloc-tarball>".  This will run a
   battery of tests against that tarball to verify that embedding is
   working properly from that tarball.

2. MANUAL METHOD: Expand a distribution hwloc tarball in this
   directory and rename the top-level directory from hwloc-<version>/
   to hwloc-tree/.  Then run ./autogen.sh, ./configure, and make.  And
   whatever other tests you'd like to run.

Things to test (most of which are done in the run-embedded-tests.sh
script):

 - autogen.sh runs properly and to completion
 - configure runs properly and to completion (normal, absolute VPATH,
   and relative VPATH)
 - make runs properly and to completion
 - you can run the resulting "./main" executable and it properly shows
   the hwloc depth of the current machine
 - make test works

If you look at configure.ac, you see that it uses the HWLOC m4 macros
to build the hwloc located at hwloc-tree/, and renames all the symbols
from "hwloc_<foo>" to "mytest_<foo>".  The main.c source calls several
hwloc functions via the "mytest_<foo>" symbols.

Bottom line: if the "main" executable runs and prints the current
depth (or you can run "make check" successfully), the embedding should
be working properly.
