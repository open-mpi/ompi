# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

headers = \
	atomic.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/os/darwin/ppc_64
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
