#
# Copyright (c) 2004-2010 The Trustees of Indiana University.
#                         All rights reserved.
# Copyright (c) 2014-2015 Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

sources = \
        compress_bzip.h \
        compress_bzip_component.c \
        compress_bzip_module.c

# Make the output library in this directory, and name it either
# mca_<type>_<name>.la (for DSO builds) or libmca_<type>_<name>.la
# (for static builds).

if MCA_BUILD_opal_compress_bzip_DSO
component_noinst =
component_install = mca_compress_bzip.la
else
component_noinst = libmca_compress_bzip.la
component_install =
endif

mcacomponentdir = $(opallibdir)
mcacomponent_LTLIBRARIES = $(component_install)
mca_compress_bzip_la_SOURCES = $(sources)
mca_compress_bzip_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(component_noinst)
libmca_compress_bzip_la_SOURCES = $(sources)
libmca_compress_bzip_la_LDFLAGS = -module -avoid-version
