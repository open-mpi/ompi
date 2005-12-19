#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AM_CPPFLAGS = \
	$(OPAL_LTDL_CPPFLAGS) \
        -DBUILDDIR=\"$(OMPI_TOP_BUILDDIR)\" \
        -DSRCDIR=\"$(OMPI_TOP_SRCDIR)\"

#
# This should be libsupport.a, not libsupport.la.  Automake doesn't
# support check_LTLIBRARIES, as technically you have to install a
# shared library before you can use it.
#
check_LIBRARIES = libsupport.a

libsupport_a_SOURCES = \
        components.h \
        components.c \
        support.c \
        support.h
