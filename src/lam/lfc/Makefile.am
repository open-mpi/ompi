# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = liblfc.la

# Source code files

headers = \
	array.h \
	hash_table.h \
	list.h \
	object.h

liblfc_la_SOURCES = \
	$(headers) \
	array.c \
	hash_table.c \
	list.c \
	object.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/lfc
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
