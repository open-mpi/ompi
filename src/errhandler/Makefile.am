# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# Source code files

headers = \
        errhandler.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/errhandler
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
