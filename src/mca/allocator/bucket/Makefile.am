# -*- makefile -*-
#
# $HEADER$
#

include $(top_ompi_srcdir)/config/Makefile.options

sources = \
        allocator_bucket.c \
        allocator_bucket_alloc.c \
        allocator_bucket_alloc.h 

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

