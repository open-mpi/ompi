# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmem.la

# Source code files

headers = \
	allocator.h \
	free_list.h \
	free_lists.h \
        malloc.h \
	mem_globals.h \
	mem_pool.h \
	seg_list.h \
	sharedmem_util.h 

libmem_la_SOURCES = \
	$(headers) \
	allocator.c \
	free_list.c \
	free_lists.c \
        malloc.c \
	mem_globals.c \
	mem_pool.c \
	seg_list.c \
	sharedmem_util.c 

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/mem
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
