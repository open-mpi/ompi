#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_allocator_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_allocator_ALL_SUBDIRS)

# Source code files

headers = allocator.h

noinst_LTLIBRARIES = libmca_allocator.la
libmca_allocator_la_SOURCES =
libmca_allocator_la_LIBADD = \
        base/libmca_allocator_base.la \
        $(MCA_allocator_STATIC_LTLIBS)
libmca_allocator_la_DEPENDENCIES = $(libmca_allocator_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/allocator
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
