# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = alpha i686 ia64 x86_64

headers = ulm_os.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/os/linux
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
