#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_pml_base.la

# For VPATH builds, have to specify where static-modules.h will be found

AM_CPPFLAGS = -I$(top_builddir)/src

# Source code files

headers = \
        base.h \
	pml_base_request.h

libmca_pml_base_la_SOURCES = \
        $(headers) \
	pml_base_close.c \
	pml_base_request.c \
	pml_base_open.c \
	pml_base_select.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/pml/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

