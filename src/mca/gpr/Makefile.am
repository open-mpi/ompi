#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_gpr_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_gpr_ALL_SUBDIRS)

# Source code files

headers = gpr.h

noinst_LTLIBRARIES = libmca_gpr.la
libmca_gpr_la_SOURCES =
libmca_gpr_la_LIBADD = \
        base/libmca_gpr_base.la \
        $(MCA_gpr_STATIC_LTLIBS)
libmca_gpr_la_DEPENDENCIES = $(libmca_gpr_la_LIBADD)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/gpr
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
