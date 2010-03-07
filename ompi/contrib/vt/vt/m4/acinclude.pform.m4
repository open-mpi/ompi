AC_DEFUN([ACVT_PLATFORM],
[
	PLATFORM=

	pform_timer=

	AC_MSG_CHECKING([for platform])

	AC_ARG_WITH(platform,
		AC_HELP_STRING([--with-platform], [configure for given platform (altix,ibm,linux,macos,sun,origin,crayxt,generic), default: automatically by configure]),
	[
		PLATFORM=$withval
		AC_MSG_RESULT([skipped (--with-platform=$withval)])
	],
	[
		case $host_os in
			linux*)
				AS_IF([test "$host_cpu" = "ia64" -a -f /etc/sgi-release],
				[PLATFORM=altix],
				[AS_IF([test "$host_cpu" = "powerpc64" -a -d /bgl/BlueLight],
				 [PLATFORM=bgl],
				 [AS_IF([test "$host_cpu" = "x86_64" -a -d /opt/xt-boot],
				  [PLATFORM=crayxt],
				  [PLATFORM=linux])])])
				;;
			sunos* | solaris*)
				PLATFORM=sun
				;;
			darwin*)
				PLATFORM=macos
				;;
			irix*)
				AS_IF([test "$host_cpu" = "mips"], [PLATFORM="origin"])
				;;
			aix*)
				PLATFORM=ibm
				;;
			unicosmp*)
				PLATFORM=crayx1
				;;
			superux*)
				PLATFORM=necsx
				;;
		esac

		AS_IF([test x"$PLATFORM" = x],
		[
			AC_MSG_WARN([unknown platform '$host'! using generic configuration])
			PLATFORM=generic
		],
		[
			AC_MSG_RESULT([$PLATFORM])
		])
	])

	AC_DEFINE([DISABLE_CLOCK_SYNC], [0], [Define to 1 to disable clock synchronization])

	case $PLATFORM in
	linux)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [2], [Use `gettimeofday' function])
		pform_timer=TIMER_GETTIMEOFDAY

		case $host_cpu in
			i*86 | x86* | powerpc*)
				AC_DEFINE([TIMER_CYCLE_COUNTER], [3], [Cycle counter (e.g. TSC)])
				pform_timer=TIMER_CYCLE_COUNTER
				;;
			ia64)
				AC_CHECK_HEADERS([asm/intrinsics.h],
				[
					AC_CHECK_DECL([_IA64_REG_AR_ITC],
					[
						AC_DEFINE([TIMER_CYCLE_COUNTER], [3], [Cycle counter (e.g. ITC)])
						pform_timer=TIMER_CYCLE_COUNTER
					], [], [#include <asm/intrinsics.h>])
				])
				;;
		esac
		;;
	macos)
		AC_DEFINE([TIMER_CYCLE_COUNTER], [1], [Cycle counter (e.g. TSC)])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [2], [Use `gettimeofday' function])
		pform_timer=TIMER_CYCLE_COUNTER
		;;
	altix)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		pform_timer=TIMER_CLOCK_GETTIME

		mmtimer_h_found=no
		AC_CHECK_HEADERS([linux/mmtimer.h], [mmtimer_h_found=yes],
		[AC_CHECK_HEADERS([sn/mmtimer.h], [mmtimer_h_found=yes],
		[AC_CHECK_HEADERS([mmtimer.h], [mmtimer_h_found=yes])])])
		AS_IF([test x"$mmtimer_h_found" = "xyes"],
		[
			AC_CHECK_FILE([/dev/mmtimer],
			[
				AC_DEFINE([TIMER_MMTIMER], [2], [Intel Multimedia Timer])
				pform_timer=TIMER_MMTIMER
			])
		])
		;;
	bgl)
		AC_DEFINE([TIMER_RTS_GET_TIMEBASE], [1], [Read PowerPC 440 time base registers])
		pform_timer=TIMER_RTS_GET_TIMEBASE
		;;
	ibm)
		AC_DEFINE([TIMER_POWER_REALTIME], [1], [IBM Power family Real-Time-Clock])
		AC_DEFINE([TIMER_SWITCH_CLOCK], [2], [Hardware Switch-Clock (it's necessary to link your application with '-lswclock')])
		pform_timer=TIMER_POWER_REALTIME
		;;
	sun)
		AC_DEFINE([TIMER_GETHRTIME], [1], [gethrtime])
		pform_timer=TIMER_GETHRTIME
		;;
	necsx)
		AC_DEFINE([TIMER_SYSSX_HGTIME], [1], [NEC SX HGTIME])
		pform_timer=TIMER_SYSSX_HGTIME
		;;
	crayt3e)
		AC_DEFINE([TIMER_CRAY_RTCLOCK],[1], [CRAY Real-Time-Clock])
		pform_timer=TIMER_CRAY_RTCLOCK
		;;
	crayx1)
		AC_DEFINE([TIMER_GETTIMEOFDAY], [1], [Use `gettimeofday' function])
		AC_DEFINE([TIMER_RTC], [2], [RTC (DOES NOT WORK YET WITH FORTRAN CODES)])
		pform_timer=TIMER_GETTIMEOFDAY
		;;
	crayxt)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		AC_DEFINE([TIMER_CYCLE_COUNTER], [2], [Cycle counter (e.g. TSC)])
		AC_DEFINE([TIMER_GETTIMEOFDAY], [3], [Use `gettimeofday' function])
		pform_timer=TIMER_CYCLE_COUNTER

		AC_TRY_COMPILE([],
[
#ifndef __LIBCATAMOUNT__
#  error "__LIBCATAMOUNT__ not defined"
#endif
],
		[AC_CHECK_HEADERS([catamount/dclock.h],
		[AC_CHECK_HEADERS([catamount/data.h],
		[
			AC_DEFINE([TIMER_DCLOCK], [4], [Use `dclock' function])
			pform_timer=TIMER_DCLOCK
		])])])
		;;
	origin)
		AC_DEFINE([TIMER_CLOCK_GETTIME], [1], [Use `clock_gettime' function])
		pform_timer=TIMER_CLOCK_GETTIME
		;;
	generic)
		AC_DEFINE([TIMER_GETTIMEOFDAY], [1], [Use `gettimeofday' function])
		pform_timer=TIMER_GETTIMEOFDAY
		;;
	esac

	AC_DEFINE_UNQUOTED([TIMER], [$pform_timer], [Use timer (see below)])
	AC_MSG_NOTICE([selected timer: $pform_timer])

	AC_SUBST(PLATFORM)
])

