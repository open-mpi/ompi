#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_gpr_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_gpr_ALL_SUBDIRS)

# Source code files

headers = gpr.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/gpr
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
