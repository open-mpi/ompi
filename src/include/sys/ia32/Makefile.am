#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

headers = atomic.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/include/sys/ia32
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
