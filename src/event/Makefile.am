include $(top_srcdir)/config/Makefile.options

# LAM Get the dirs where lam_config.h and lam_config_bottom.h live.
# Also specify where we can find this package's config.h because some
# of the header files in this directory get included elsewhere in LAM.
# #@%@#$...
AM_CPPFLAGS = \
        -I$(top_srcdir)/src/event/compat \
        -I$(top_srcdir)/../include \
        -I$(top_builddir)/../include \
        -I$(top_builddir)/..

CFLAGS = @CFLAGS@
# LAM: No need to build the sample or test
#SUBDIRS = . sample test
SUBDIRS = compat

EXTRA_DIST = err.c event.h evsignal.h event.3 kqueue.c \
	epoll_sub.c epoll.c select.c rtsig.c poll.c signal.c

# LAM: This causes problems
junk =
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

# LAM: Changed to noinst and libevent.la
noinst_LTLIBRARIES = libevent.la

# LAM: Automake sillyness so that we can have variable sources
objects = $(LAM_LIBEVENT_SOURCES:.c=.lo)

# LAM: Changed to libevent_la_*
libevent_la_SOURCES = event.c event.h
libevent_la_LIBADD = $(objects)
libevent_la_DEPENDENCIES = $(objects)
