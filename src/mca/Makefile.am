#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS =  lam mpi

# Source code files

headers = mca.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif



