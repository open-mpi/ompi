#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_ns_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_ns_ALL_SUBDIRS)

# Source code files

headers = ns.h

noinst_LTLIBRARIES = libmca_ns.la
libmca_ns_la_SOURCES =
libmca_ns_la_LIBADD = \
        base/libmca_ns_base.la \
        $(MCA_ns_STATIC_LTLIBS)
libmca_ns_la_DEPENDENCIES = $(libmca_ns_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/ns
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
