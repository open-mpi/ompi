AUTOMAKE_OPTIONS = foreign no-dependencies

CFLAGS = -Wall @CFLAGS@
# LAM: No need to build the sample or test
#SUBDIRS = . sample test

EXTRA_DIST = acconfig.h err.c event.h evsignal.h event.3 kqueue.c \
	epoll_sub.c epoll.c select.c rtsig.c poll.c signal.c \
	sample/Makefile.am sample/Makefile.in sample/event-test.c \
	sample/signal-test.c sample/time-test.c \
	test/Makefile.am test/Makefile.in test/bench.c test/regress.c \
	test/test-eof.c test/test-weof.c test/test-time.c \
	test/test-init.c test/test.sh \
	compat/err.h compat/sys/queue.h compat/sys/tree.h compat/sys/_time.h \
	WIN32-Code WIN32-Code/config.h WIN32-Code/misc.c \
	WIN32-Code/win32.c WIN32-Code/misc.h \
	WIN32-Prj WIN32-Prj/event_test WIN32-Prj/event_test/event_test.dsp \
	WIN32-Prj/event_test/test.txt WIN32-Prj/libevent.dsp \
	WIN32-Prj/libevent.dsw WIN32-Prj/signal_test \
	WIN32-Prj/signal_test/signal_test.dsp WIN32-Prj/time_test \
	WIN32-Prj/time_test/time_test.dsp

# LAM: Changed to libevent.la
lib_LTLIBRARIES = libevent.la

# LAM: Changed to libevent_la_*
libevent_la_SOURCES = event.c
libevent_la_LIBADD = @LIBOBJS@

include_HEADERS = event.h

# LAM: Make it work in a VPATH environment
INCLUDES = -I$(top_srcdir)/compat

man_MANS = event.3

DISTCLEANFILES = *~
