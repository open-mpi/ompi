#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_pcm_base.la

# Source code files

headers = \
        base.h

# Library

libmca_pcm_base_la_SOURCES = \
        $(headers) \
        pcm_base_close.c \
        pcm_base_open.c \
        pcm_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/lam/pcm/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
