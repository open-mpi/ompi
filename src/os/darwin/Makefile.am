# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = ppc_32 ppc_64

headers = ulm_os.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/os/darwin
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
