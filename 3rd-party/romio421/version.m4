[#] start of __file__
#
# Copyright (C) by Argonne National Laboratory
#     See COPYRIGHT in top-level directory
#
# This file contains versioning information for MPICH's configure process.
# This was previously (as "maint/Version") a bit of shell code that was sourced
# by configure, but shell variables are not permitted in the modern form of
# AC_INIT.  See "Rebuilding Makefiles" in the automake-1.11.1 manual.
#
# !!! NOTE !!! absolutely no shell code from this file will end up in the
# configure script, including these shell comments.  Any shell code must live in
# the configure script and/or use m4 values defined here.  We could consider
# changing this by playing with diversions, but then we would probably be
# playing with autotools-fire.

m4_define([MPICH_VERSION_m4],[4.2.1])dnl
m4_define([MPICH_RELEASE_DATE_m4],[Wed Apr 17 15:30:02 CDT 2024])dnl

# For libtool ABI versioning rules see:
# http://www.gnu.org/software/libtool/manual/libtool.html#Updating-version-info
#
#     1. If the library source code has changed at all since the last
#     update, then increment revision (`c:r:a' becomes `c:r+1:a').
#
#     2. If any interfaces have been added, removed, or changed since
#     the last update, increment current, and set revision to 0.
#
#     3. If any interfaces have been added since the last public
#     release, then increment age.
#
#     4. If any interfaces have been removed or changed since the last
#     public release, then set age to 0.

# libmpi so version only includes functionality defined in the MPI
# standard, and does not include MPIX_ functions and C++ bindings.

# Use [0:0:0] for unstable (e.g. alpha and beta) releases.
# last version: 4.2.1 - 16:1:4

m4_define([libmpi_so_version_m4],[16:1:4])dnl

[#] end of __file__
