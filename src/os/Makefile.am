# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = cygwin darwin irix linux tru64

headers = \
	atomic.h \
	lam_system.h \
	numa.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/os
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
