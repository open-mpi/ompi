# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = cygwin darwin irix linux tru64

headers = \
	atomic.h \
	ompi_system.h \
	numa.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/os
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
