# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

SUBDIRS	= ctnetwork lfc mem os runtime threads util

noinst_LTLIBRARIES = liblam_convenience.la

headers = \
        atomic.h \
        constants.h \
        lam.h \
        stdint.h \
        totalview.h \
        types.h

# List the input convenience libraries here

sources = \
        ctnetwork/libctnetwork.la \
        lfc/liblfc.la \
        mem/libmem.la \
        runtime/libruntime.la \
        threads/libthreads.la \
        util/libutil.la 

liblam_convenience_la_SOURCES =
liblam_convenience_la_LIBADD = $(sources)

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
