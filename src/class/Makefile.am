# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = liblfc.la

# Source code files

headers = \
	ompi_bitmap.h \
	ompi_hash_table.h \
	ompi_list.h \
	ompi_object.h \
	ompi_pointer_array.h \
	ompi_value_array.h 

liblfc_la_SOURCES = \
	$(headers) \
	ompi_hash_table.c \
	ompi_list.c \
	ompi_object.c \
	ompi_pointer_array.c \
	ompi_bitmap.c \
	ompi_value_array.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/class
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
