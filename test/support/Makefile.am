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

#
# This should be libsupport.a, not libsupport.la.  Automake doesn't
# support check_LTLIBRARIES, as technically you have to install a
# shared library before you can use it.
#
check_LIBRARIES = libsupport.a

libsupport_a_SOURCES = \
        support.c \
        support.h
