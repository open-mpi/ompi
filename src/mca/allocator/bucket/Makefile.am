# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

AM_CPPFLAGS = \
        -I$(top_ompi_builddir)/src/include \
        -I$(top_ompi_srcdir)/src \
        -I$(top_ompi_srcdir)/src/include

if OMPI_BUILD_allocator_bucket_LOADABLE_MODULE
module_noinst =
module_install = mca_allocator_bucket.la
else
module_noinst = libmca_allocator_bucket.la
module_install =
endif
                                                                                                          
mcamoduledir = $(libdir)/openmpi
mcamodule_LTLIBRARIES = $(module_install)
mca_allocator_bucket_la_SOURCES = $(sources)
mca_allocator_bucket_la_LIBADD = $(LIBOMPI_LA)
mca_allocator_bucket_la_LDFLAGS = -module -avoid-version

noinst_LTLIBRARIES = $(module_noinst)
libmca_allocator_bucket_la_SOURCES = $(sources)
libmca_allocator_bucket_la_LDFLAGS = -module -avoid-version

