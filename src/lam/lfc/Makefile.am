# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = liblfc.la

# Source code files

headers = \
	hash_table.h \
    lam_pointer_array.h \
	list.h \
	object.h \
	varray.h 

liblfc_la_SOURCES = \
	$(headers) \
	hash_table.c \
	lam_pointer_array.c \
	lam_value_array.c \
	list.c \
	object.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/lfc
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
