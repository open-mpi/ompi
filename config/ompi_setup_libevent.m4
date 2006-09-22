dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_SETUP_LIBEVENT],[

dnl OMPI: don't use AC_LIBOBJ
sources=

dnl Check for optional stuff
AC_ARG_WITH(event-rtsig,
  AC_HELP_STRING([--with-event-rtsig],
	         [compile with support for real time signals (experimental)]),
  [usertsig=yes], [usertsig=no])

dnl Checks for libraries.
dnl AC_CHECK_LIB(socket, socket)

dnl Checks for header files.
AC_HEADER_STDC
AC_CHECK_HEADERS(fcntl.h stdarg.h inttypes.h stdint.h poll.h signal.h unistd.h sys/epoll.h sys/time.h sys/queue.h sys/event.h sys/ioctl.h sys/devpoll.h)
if test "x$ac_cv_header_sys_queue_h" = "xyes"; then
	AC_MSG_CHECKING(for TAILQ_FOREACH in sys/queue.h)
	AC_EGREP_CPP(yes,
[
#include <sys/queue.h>
#ifdef TAILQ_FOREACH
 yes
#endif
],	[AC_MSG_RESULT(yes)
	 AC_DEFINE(HAVE_TAILQFOREACH, 1,
		[Define if TAILQ_FOREACH is defined in <sys/queue.h>])],
	AC_MSG_RESULT(no)
	)
fi

if test "x$ac_cv_header_sys_time_h" = "xyes"; then
	AC_MSG_CHECKING(for timeradd in sys/time.h)
	AC_EGREP_CPP(yes,
[
#include <sys/time.h>
#ifdef timeradd
 yes
#endif
],	[ AC_DEFINE(HAVE_TIMERADD, 1,
		[Define if timeradd is defined in <sys/time.h>])
	  AC_MSG_RESULT(yes)] ,AC_MSG_RESULT(no)
)
fi

dnl Checks for typedefs, structures, and compiler characteristics.
AC_HEADER_TIME

dnl Checks for library functions.
AC_CHECK_FUNCS(gettimeofday vasprintf fcntl)

AC_MSG_CHECKING(for F_SETFD in fcntl.h)
AC_EGREP_CPP(yes,
[
#define _GNU_SOURCE
#include <fcntl.h>
#ifdef F_SETFD
yes
#endif
],      [ AC_DEFINE(HAVE_SETFD, 1,
              [Define if F_SETFD is defined in <fcntl.h>])
          AC_MSG_RESULT(yes) ], AC_MSG_RESULT(no))

needsignal=no
haveselect=no
AC_CHECK_FUNCS(select, [haveselect=yes], )
if test "x$haveselect" = "xyes" ; then
	# OMPI: Don't use AC_LIBOBJ
	sources="select.c $sources"
	needsignal=yes
fi

havepoll=no
havertsig=no
AC_CHECK_FUNCS(poll, [havepoll=yes], )
case "$host" in
    *apple-darwin*)
        haveworkingpoll=0
    ;;
    *)
        if test "$havepoll" = "yes" ; then
	    haveworkingpoll=1
	else
	    haveworkingpoll=0
	fi
    ;;
esac
AC_DEFINE_UNQUOTED([HAVE_WORKING_POLL], [$haveworkingpoll], 
                   [Whether poll works for file descriptors and devices])
if test "x$havepoll" = "xyes" -a "$haveworkingpoll" = "1" ; then
	# OMPI: Don't use AC_LIBOBJ
	sources="poll.c $sources"
	needsignal=yes

	if test "x$usertsig" = "xyes" ; then
		AC_CHECK_FUNCS(sigtimedwait, [havertsig=yes], )
	fi
fi
if test "x$havertsig" = "xyes" ; then
	AC_MSG_CHECKING(for F_SETSIG in fcntl.h)
	AC_EGREP_CPP(yes,
[
#define _GNU_SOURCE
#include <fcntl.h>
#ifdef F_SETSIG
yes
#endif
],	[ AC_MSG_RESULT(yes) ], [ AC_MSG_RESULT(no); havertsig=no])
fi
if test "x$havertsig" = "xyes" ; then
	AC_DEFINE(HAVE_RTSIG, 1, [Define if your system supports POSIX realtime signals])
	# OMPI: Don't use AC_LIBOBJ
	sources="rtsig.c $sources"
	AC_MSG_CHECKING(for working rtsig on pipes)
	AC_TRY_RUN(
[
#define _GNU_SOURCE
#include <fcntl.h>
#include <signal.h>
#include <unistd.h>
#include <stdlib.h>

int sigio()
{
	exit(0);
}

int main()
{
	int fd[2];

	pipe(fd);
	signal(SIGIO, sigio);
	fcntl(fd[0], F_SETOWN, getpid());
	fcntl(fd[0], F_SETSIG, SIGIO);
	fcntl(fd[0], F_SETFL, fcntl(fd[0], F_GETFL) | O_ASYNC);
	write(fd[1], "", 1);
	return 1;
}
],	[ AC_MSG_RESULT(yes)
	  AC_DEFINE(HAVE_WORKING_RTSIG, 1, [Define if realtime signals work on pipes])],
	AC_MSG_RESULT(no))
fi

haveepoll=no
AC_CHECK_FUNCS(epoll_ctl, [haveepoll=yes], )
if test "x$haveepoll" = "xyes" -a "$cross_compiling" != "yes" ; then

	# OMPI: Unfortunately, this test is not sufficient on some
	# Linux distros (e.g., RH 9), where the function is defined
	# and you can link against it, but it's hardwired to return
	# ENOSYS -- and /usr/include/gnu/stubs.h fails to define
	# __stub_epoll_ctl (the usual mechanism in glibc to indicate
	# that a function is a stub and isn't really implemented).
	# Hence, the test succeeds because it thinks it can use
	# epoll_ctl (and friends).  So we have to do a better test
	# after we determine that epoll_ctl is linkable.  Grumble.
        # If we are cross compiling, just trust AC_CHECK_FUNCS

	AC_MSG_CHECKING([for epoll_ctl on broken Linux distros])
        rm -f conftest.out
        AC_RUN_IFELSE(AC_LANG_PROGRAM([[
AC_INCLUDES_DEFAULT
#include <sys/epoll.h>]], 
[[int i = epoll_create(2);
FILE *fp = fopen("conftest.out", "w");
fprintf(fp, "%d", i);
fclose(fp);]]))
	haveepoll=no
        if test -f conftest.out -a "`cat conftest.out`" = "0"; then
            haveepoll=yes
        fi
        rm -f conftest.out
	AC_MSG_RESULT([$haveepoll])
fi

if test "x$haveepoll" = "xyes" ; then
	AC_DEFINE(HAVE_EPOLL, 1,
		[Define if your system supports the epoll system calls])
	# OMPI: Don't use AC_LIBOBJ
	sources="epoll.c $sources"
	needsignal=yes
fi

havedevpoll=no
if test "x$ac_cv_header_sys_devpoll_h" = "xyes"; then
        AC_DEFINE(HAVE_DEVPOLL, 1,
                    [Define if /dev/poll is available])
        # OMPI: Don't use AC_LIBOBJ(devpoll)
        sources="devpoll.c $sources"
        needsignal=yes
fi

havekqueue=no
if test "x$ac_cv_header_sys_event_h" = "xyes"; then
	AC_CHECK_FUNCS(kqueue, [havekqueue=yes], )
	if test "x$havekqueue" = "xyes" ; then
		AC_MSG_CHECKING(for working kqueue)
		AC_TRY_RUN(
#include <sys/types.h>
#include <sys/time.h>
#include <sys/event.h>
#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdlib.h>

int
main()
{
	int kq;
	int n;
	int fd[[2]];
	struct kevent ev;
	struct timespec ts;
	char buf[[8000]];

	if (pipe(fd) == -1)
		exit(1);
	if (fcntl(fd[[1]], F_SETFL, O_NONBLOCK) == -1)
		exit(1);

	while ((n = write(fd[[1]], buf, sizeof(buf))) == sizeof(buf))
		;

        if ((kq = kqueue()) == -1)
		exit(1);

	ev.ident = fd[[1]];
	ev.filter = EVFILT_WRITE;
	ev.flags = EV_ADD | EV_ENABLE;
	n = kevent(kq, &ev, 1, NULL, 0, NULL);
	if (n == -1)
		exit(1);
	
	read(fd[[0]], buf, sizeof(buf));

	ts.tv_sec = 0;
	ts.tv_nsec = 0;
	n = kevent(kq, NULL, 0, &ev, 1, &ts);
	if (n == -1 || n == 0)
		exit(1);

	exit(0);
}, [AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_WORKING_KQUEUE, 1,
		[Define if kqueue works correctly with pipes])
    sources="kqueue.c $sources"], AC_MSG_RESULT(no), AC_MSG_RESULT(no))
    # OMPI: don't use AC_LIBOBJ
	fi
fi

haveepollsyscall=no
if test "x$ac_cv_header_sys_epoll_h" = "xyes"; then
	if test "x$haveepoll" = "xno" ; then
		AC_MSG_CHECKING(for epoll system call)
		AC_TRY_RUN(
#include <stdint.h>
#include <sys/param.h>
#include <sys/types.h>
#include <sys/syscall.h>
#include <sys/epoll.h>
#include <unistd.h>
#include <stdlib.h>

int
epoll_create(int size)
{
	return (syscall(__NR_epoll_create, size));
}

int
main()
{
	int epfd;

	epfd = epoll_create(256);
	exit (epfd == -1 ? 1 : 0);
}, [AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_EPOLL, 1,
	[Define if your system supports the epoll system calls])
    needsignal=yes
    # OMPI: don't use AC_LIBOBJ
    sources="epoll_sub.c epoll.c $sources"
    ], AC_MSG_RESULT(no), AC_MSG_RESULT(no))
	fi
fi

if test "x$needsignal" = "xyes" ; then
    # OMPI: don't use AC_LIBOBJ
    sources="signal.c $sources"
fi

# OMPI: AC_REPLACE_FUNCS doesn't have much meaning here because it
# uses AC_LIBOBJ; use our own test
#AC_REPLACE_FUNCS(err)

AC_TYPE_PID_T
AC_TYPE_SIZE_T
AC_CHECK_TYPE(u_int64_t, unsigned long long)
AC_CHECK_TYPE(u_int32_t, unsigned int)
AC_CHECK_TYPE(u_int16_t, unsigned short)
AC_CHECK_TYPE(u_int8_t, unsigned char)


# OMPI: Save the libobj sources
OMPI_LIBEVENT_SOURCES="$sources"
AC_SUBST(OMPI_LIBEVENT_SOURCES)

# OMPI: All done
unset sources])dnl
