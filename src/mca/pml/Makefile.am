#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = base $(MCA_pml_STATIC_SUBDIRS)
DIST_SUBDIRS = base $(MCA_pml_ALL_SUBDIRS)

# Source code files

headers = pml.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/mca/pml
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
