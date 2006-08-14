#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#


# OMPI Get the dirs where ompi_config.h and ompi_config_bottom.h live.
# Also specify where we can find this package's config.h because some
# of the header files in this directory get included elsewhere in OMPI.
# #@%@#$...
AM_CPPFLAGS = \
        -I$(top_srcdir)/opal/event/compat

# OMPI: No need to build the sample or test
#SUBDIRS = . sample test
SUBDIRS = compat

EXTRA_DIST = event.h event-internal.h evsignal.h event.3 \
	kqueue.c epoll_sub.c epoll.c select.c rtsig.c poll.c signal.c \
	devpoll.c log.h\
	WIN32-Code/config.h WIN32-Code/misc.c \
	WIN32-Code/misc.h  WIN32-Code/win32.c

# OMPI: This causes problems
junk = \
	sample/Makefile.am sample/Makefile.in sample/event-test.c \
	sample/signal-test.c sample/time-test.c \
	test/Makefile.am test/Makefile.in test/bench.c test/regress.c \
	test/test-eof.c test/test-weof.c test/test-time.c \
	test/test-init.c test/test.sh \
	compat/sys/queue.h compat/sys/tree.h compat/sys/_time.h \
	WIN32-Code WIN32-Code/config.h WIN32-Code/misc.c \
	WIN32-Code/win32.c WIN32-Code/misc.h \
	WIN32-Prj/event_test/event_test.dsp \
	WIN32-Prj/event_test/test.txt WIN32-Prj/libevent.dsp \
	WIN32-Prj/libevent.dsw WIN32-Prj/signal_test/signal_test.dsp \
	WIN32-Prj/time_test WIN32-Prj/time_test/time_test.dsp

# OMPI: Changed to noinst and libevent.la
noinst_LTLIBRARIES = libevent.la

# OMPI: Automake sillyness so that we can have variable sources
objects = $(OMPI_LIBEVENT_SOURCES:.c=.lo)

# OMPI: Changed to libevent_la_*
headers = event.h event_rename.h

libevent_la_SOURCES = event.c buffer.c evbuffer.c log.c $(headers)
libevent_la_LIBADD = $(objects)
libevent_la_DEPENDENCIES = $(objects)


if WANT_INSTALL_HEADERS
ompidir = $(includedir)/openmpi/opal/event
ompi_HEADERS = $(headers)
else
ompidir = $(includedir)
endif

verify: libevent.la
	cd $(srcdir)/test && make verify	
