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
lamdir = $(includedir)/lam/file
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
