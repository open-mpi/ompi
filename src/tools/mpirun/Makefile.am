#
# Copyright (c) 2004-2005 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
#                         All rights reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

EXTRA_DIST = $(pkgdata_DATA)
pkgdata_DATA = help-mpirun.txt

libs = $(top_builddir)/src/libmpi.la

bin_PROGRAMS = mpirun

mpirun_SOURCES = \
        mpirun.c 

mpirun_LDADD = $(libs) 
mpirun_DEPENDENCIES = $(libs)
