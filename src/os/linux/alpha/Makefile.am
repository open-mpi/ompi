# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

headers = \
	atomic.h \
	smp_dev_params.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/os/linux/alpha
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
