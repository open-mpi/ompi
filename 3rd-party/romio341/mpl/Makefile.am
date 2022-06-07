##
## Copyright (C) by Argonne National Laboratory
##     See COPYRIGHT in top-level directory
##

ACLOCAL_AMFLAGS = -I confdb
AM_CPPFLAGS = -I$(top_srcdir)/include -I$(top_builddir)/include

lib@MPLLIBNAME@_la_SOURCES =
lib@MPLLIBNAME@_la_LDFLAGS = ${lib@MPLLIBNAME@_so_versionflags}

noinst_HEADERS =
include src/Makefile.mk

MPL_TESTS = strsep
TESTS = $(MPL_TESTS)
check_PROGRAMS = $(MPL_TESTS)
strsep_SOURCES = test/strsep.c
strsep_LDADD = lib@MPLLIBNAME@.la

mpl_headers =               \
    include/mpl.h           \
    include/mpl_arg_serial.h\
    include/mpl_argstr.h    \
    include/mpl_atomic.h    \
    include/mpl_atomic_by_lock.h\
    include/mpl_atomic_c11.h\
    include/mpl_atomic_gcc_atomic.h\
    include/mpl_atomic_gcc_sync.h\
    include/mpl_atomic_none.h\
    include/mpl_atomic_nt_intrinsics.h\
    include/mpl_base.h      \
    include/mpl_dbg.h       \
    include/mpl_gpu.h       \
    include/mpl_gpu_cuda.h  \
    include/mpl_gpu_fallback.h\
    include/mpl_gpu_ze.h    \
    include/mpl_math.h      \
    include/utlist.h    \
    include/mpl_valgrind.h  \
    include/mpl_env.h       \
    include/mpl_err.h       \
    include/mpl_gavl.h      \
    include/mpl_str.h       \
    include/mpl_trmem.h     \
    include/mpl_msg.h       \
    include/mpl_posix_mutex_native.h\
    include/mpl_proc_mutex.h\
    include/mpl_proc_mutex_posix.h\
    include/mpl_shm_mmap.h  \
    include/mpl_shm_sysv.h  \
    include/mpl_shm_win.h   \
    include/mpl_sock.h      \
    include/mpl_sockaddr.h  \
    include/mpl_iov.h       \
    include/mpl_bt.h        \
    include/mpl_shm.h       \
    include/mpl_thread_argobots.h\
    include/mpl_thread.h    \
    include/mpl_thread_posix.h\
    include/mpl_thread_priv.h\
    include/mpl_thread_solaris.h\
    include/mpl_thread_win.h\
    include/mpl_timer_clock_gettime.h\
    include/mpl_timer_gcc_ia64_cycle.h\
    include/mpl_timer_gethrtime.h\
    include/mpl_timer_gettimeofday.h\
    include/mpl_timer_linux86_cycle.h\
    include/mpl_timer_mach_absolute_time.h\
    include/mpl_timer_ppc64_cycle.h\
    include/mpl_yield.h\
    include/utarray.h\
    include/uthash.h

if MPL_EMBEDDED_MODE
noinst_HEADERS += $(mpl_headers)
nodist_noinst_HEADERS = include/mplconfig.h
noinst_LTLIBRARIES = lib@MPLLIBNAME@.la
else !MPL_EMBEDDED_MODE
include_HEADERS = $(mpl_headers)
nodist_include_HEADERS = include/mplconfig.h
lib_LTLIBRARIES = lib@MPLLIBNAME@.la
endif !MPL_EMBEDDED_MODE

# --------------------------------------------------------------------------
.PHONY: coverage
gcov_sources = $(libmpl_la_SOURCES)
# assumes that these sources were compiled appropriately ("-fprofile-arcs"
# and "-ftest-coverage")
coverage:
	@for file in $(gcov_sources) ; do \
	    dir=`dirname $$file` ; \
	    bname=`basename $$file` ; \
	    aux=`echo $$bname | sed -e 's,\.*$$,,'` ; \
	    echo "( $(GCOV) -b -f -o $$file $$file && mv $${bname}.gcov $$dir )" ; \
	    ( $(GCOV) -b -f -o $$file $$file && mv $${bname}.gcov $$dir ) ; \
	    rm -f *.gcov ; \
	done
	for subdir in $(SUBDIRS) - ; do \
	    if test $$subdir = "-" ; then break ; fi ; \
	    ( cd $$subdir && $(MAKE) $(AM_MAKEFLAGS) coverage ) ; \
	done
# --------------------------------------------------------------------------

# files that automake doesn't know about by default that should be included in
# the distribution
EXTRA_DIST = VERSION confdb/cmd_prefix_config_h.pl

DISTCLEANFILES = _configs.sed include/mplconfig.h
