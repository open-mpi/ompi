#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_ptl_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_ptl_ALL_SUBDIRS)

# Source code files

headers = ptl.h

noinst_LTLIBRARIES = libmca_ptl.la
libmca_ptl_la_SOURCES =
libmca_ptl_la_LIBADD = \
        base/libmca_ptl_base.la \
        $(MCA_ptl_STATIC_LTLIBS)
libmca_ptl_la_DEPENDENCIES = $(libmca_ptl_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mca/ptl
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
