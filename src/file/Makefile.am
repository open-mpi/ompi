# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

# JMS Remove this when we actually start building a library in here
# and $(headers) gets included in a SOURCES somewhere
EXTRA_DIST = $(headers)

# Source code files

headers = \
        file.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/file
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
