# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# Source code files

headers = \
        proc.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mpi/proc
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
