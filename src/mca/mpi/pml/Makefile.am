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
lamdir = $(includedir)/lam/mca/mpi/pml
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
