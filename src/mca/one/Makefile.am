#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_one_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_one_ALL_SUBDIRS)

# Source code files

headers = one.h

# Conditonenally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/one
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
