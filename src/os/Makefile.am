# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = cygwin darwin irix linux tru64

headers = \
	atomic.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/os
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
