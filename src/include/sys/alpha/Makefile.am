#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_HEADERS = atomic.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/include/sys/alpha
ompi_HEADERS = $(noinst_HEADERS)
else
ompidir = $(includedir)
endif
