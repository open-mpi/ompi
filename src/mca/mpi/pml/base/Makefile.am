#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options
                                                                                                    
noinst_LTLIBRARIES = libpmlb.la
                                                                                                                  
# Source code files
                                                                                                                  
headers = \
	fragment.h \
	header.h \
	recvfrag.h \
	request.h \
	sendfrag.h \
	sendreq.h 

libpmlb_la_SOURCES = \
        $(headers) \
        fragment.c \
        recvfrag.c \
        request.c \
        sendfrag.c
                                                                                                                  
# Conditionally install the header files
                                                                                                                  
if WANT_INSTALL_HEADERS
lamdir = $(includedir)/mfc/mpi/pml/base
lam_HEADERS = $(headers)
else
lamdir = $(includedir)
endif

