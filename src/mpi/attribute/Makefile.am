# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# Source code files

headers = \
        attribute.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/attribute
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
