#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS = sys

noinst_HEADERS = \
        atomic.h \
        constants.h \
        ompi.h \
        totalview.h \
        types.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/include
ompi_HEADERS = $(noinst_HEADERS)
else
ompidir = $(includedir)
endif
