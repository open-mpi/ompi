#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_mpool_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_mpool_ALL_SUBDIRS)

# Source code files

headers = mpool.h

noinst_LTLIBRARIES = libmca_mpool.la
libmca_mpool_la_SOURCES =
libmca_mpool_la_LIBADD = \
        base/libmca_mpool_base.la \
        $(MCA_mpool_STATIC_LTLIBS)
libmca_mpool_la_DEPENDENCIES = $(libmca_mpool_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mca/mpool
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
