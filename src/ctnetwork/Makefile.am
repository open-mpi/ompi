# -*- makefile -*-
#
# $HEADER$
#

include $(top_srcdir)/config/Makefile.options

noinst_LTLIBRARIES = libctnetwork.la

# Source code files

headers = \
	ctpeer.h \
	ctchannel.h \
	ctcontroller.h \
	ctmessage.h \
	ctnode.h

libctnetwork_la_SOURCES = \
	$(headers) \
	ctpeer.c

bogus = \
        ctchannel.c \
	ctchannel.h \
	ctcontroller.c \
	ctcontroller.h \
	ctmessage.c \
	ctmessage.h \
	ctnode.c \
	ctnode.h

# Conditionally install the header files

if WANT_INSTALL_HEADERS
ompidir = $(includedir)/ompi/ctnetwork
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif
