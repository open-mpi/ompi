#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_mem_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_mem_ALL_SUBDIRS)

# Source code files

headers = mem.h

noinst_LTLIBRARIES = libmca_mem.la
libmca_mem_la_SOURCES =
libmca_mem_la_LIBADD = \
        base/libmca_mem_base.la \
        $(MCA_mem_STATIC_LTLIBS)
libmca_mem_la_DEPENDENCIES = $(libmca_mem_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mca/mem
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
