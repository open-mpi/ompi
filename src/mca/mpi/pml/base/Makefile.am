#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpi_pml_base.la

# Source code files

headers = \
        base.h \
	pml_base_fragment.h \
	pml_base_header.h \
	pml_base_recvfrag.h \
	pml_base_request.h \
	pml_base_sendfrag.h \
	pml_base_sendreq.h 

libmca_mpi_pml_base_la_SOURCES = \
        $(headers) \
        pml_base_fragment.c \
        pml_base_recvfrag.c \
        pml_base_recvreq.c \
        pml_base_request.c \
        pml_base_sendfrag.c \
        pml_base_sendreq.c

# Conditionally install the header files

if WANT_INSTALL_HEADERS
lamdir = $(includedir)/lam/mca/mpi/pml/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

