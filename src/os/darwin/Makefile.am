# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = ppc_32 ppc_64

headers = ulm_os.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/os/darwin
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
