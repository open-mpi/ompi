# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = liblfc.la

# Source code files

headers = \
	lam_hash_table.h \
	lam_list.h \
	lam_object.h \
	lam_pointer_array.h \
	lam_value_array.h 

liblfc_la_SOURCES = \
	$(headers) \
	array.c \
	lam_hash_table.c \
	lam_list.c \
	lam_object.c \
	lam_pointer_array.c \
	lam_value_array.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/lam/lfc
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif
