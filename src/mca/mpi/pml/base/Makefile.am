#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libmca_mpi_pml_base.la

# Source code files

headers = \
        base.h \
	fragment.h \
	header.h \
	recvfrag.h \
	request.h \
	sendfrag.h \
	sendreq.h 

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
lamdir = $(includedir)/mca/mpi/pml/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

