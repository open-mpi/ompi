# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

# According to the MCA spec, we have to make the output library here
# in the top-level directory, and it has to be named
# libompi_ssi_coll_ompi_basic.la

if OMPI_BUILD_allocator_bucket_LOADABLE_MODULE
module_noinst =
module_install = mca_allocator_bucket.la
else
module_noinst = libmca_allocator_bucket.la
module_install =
endif

mcamoduledir = $(libdir)/ompi
mcamodule_LTLIBRARIES = $(module_install)
mca_allocator_bucket_la_SOURCES =
mca_allocator_bucket_la_LIBADD = \
        src/libmca_allocator_bucket.la \
        $(LIBOMPI_LA)
mca_allocator_bucket_la_LDFLAGS = -module -avoid-version

libmca_allocator_bucket_la_LDFLAGS = -module -avoid-version

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

noinst_LTLIBRARIES = libmca_allocator_bucket.la
libmca_allocator_bucket_la_SOURCES = \
	allocator_bucket.c \
	allocator_bucket_alloc.h \
        allocator_bucket_alloc.c
