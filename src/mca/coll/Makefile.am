#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_coll_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_coll_ALL_SUBDIRS)

# Source code files

headers = coll.h

noinst_LTLIBRARIES = libmca_coll.la
libmca_coll_la_SOURCES =
libmca_coll_la_LIBADD = \
        base/libmca_coll_base.la \
        $(MCA_coll_STATIC_LTLIBS)
libmca_coll_la_DEPENDENCIES = $(libmca_coll_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mca/coll
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
