# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = alpha i686 ia64 x86_64

headers = ulm_os.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/os/linux
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
