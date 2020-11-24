AC_PREREQ(2.63)
#
# (C) 2006 by Argonne National Laboratory.
#     See COPYRIGHT in top-level directory.
#

AC_INIT([MPL], [0.1])

# sanity check that --srcdir was specified correctly
AC_CONFIG_SRCDIR([src/str/mpl_str.c])

AC_CONFIG_AUX_DIR(confdb)
AC_CONFIG_MACRO_DIR(confdb)
AM_INIT_AUTOMAKE([subdir-objects] [-Wall -Werror foreign 1.12.3])

dnl must come before LT_INIT, which AC_REQUIREs AC_PROG_CC
PAC_PROG_CC
AM_PROG_CC_C_O

AC_USE_SYSTEM_EXTENSIONS

AM_PROG_AR

LT_PREREQ([2.2.6])

# Bug in libtool adds -O2 and -g by default
PAC_PUSH_FLAG([CFLAGS])
LT_INIT()
PAC_POP_FLAG([CFLAGS])

# ----------------------------------------------------------------------------
# Set default library names if names haven't already been provided
AC_ARG_VAR([MPLLIBNAME],[can be used to override the name of the MPL library (default: "mpl")])
MPLLIBNAME=${MPLLIBNAME:-"mpl"}
AC_SUBST(MPLLIBNAME)
export MPLLIBNAME

if test -s "$srcdir/VERSION" ; then
   . $srcdir/VERSION
   AC_SUBST(libmpl_so_version)
else
   AC_MSG_ERROR([Version information not found. Configuration aborted.])
fi

AC_CONFIG_HEADER([include/config.h])
AC_CONFIG_COMMANDS([prefix-config],[perl $srcdir/confdb/cmd_prefix_config_h.pl MPL include/config.h include/mplconfig.h])

# Non-verbose make
m4_ifdef([AM_SILENT_RULES], [AM_SILENT_RULES([yes])])

AC_C_CONST
AC_C_RESTRICT
AC_C_INLINE

PAC_C_MACRO_VA_ARGS
PAC_C_BUILTIN_EXPECT

AC_ARG_ENABLE(embedded,
    AC_HELP_STRING([--enable-embedded], [Build MPL in embedded mode (default is no)]),
    [embedded=yes],
    [embedded=no])
AM_CONDITIONAL([MPL_EMBEDDED_MODE],[test "x${embedded}" = "xyes"])

AC_ARG_ENABLE(g,
    AC_HELP_STRING([--enable-g=option],
	[
		Control the level of debugging support in MPL.
		"option" is a list of comma separated names.  Default
		is "all".

	    none|no   - No debugging
	    log       - Enable debug event logging
	    mem       - Enable memory tracing
	    yes|all   - All of the above choices (except "none", obviously)
	]),,[enable_g=none])

# enable-g
# strip off multiple options, separated by commas
PAC_PUSH_FLAG(IFS)
IFS=","
for option in $enable_g ; do
    case "$option" in
	 log)
	 enable_g_log=yes
	 ;;

	 mem)
	 enable_g_mem=yes
	 ;;

	 all|yes)
	 enable_g_log=yes
	 enable_g_mem=yes
	 ;;

	 no|none)
	 ;;

	 *)
	 AC_MSG_WARN([Unknown value $option for enable-g])
	 ;;
    esac
done
PAC_POP_FLAG(IFS)

if test "$enable_g_log" = "yes" ; then
   AC_DEFINE([USE_DBG_LOGGING],[1],[Define to enable logging macros])
fi

if test "$enable_g_mem" = "yes" ; then
   AC_DEFINE([USE_MEMORY_TRACING],[1],[Define to enable memory tracing])
fi


# support gcov test coverage information
PAC_ENABLE_COVERAGE

# check for compiler support for the __typeof() extension
AC_CACHE_CHECK([whether the compiler supports __typeof(variable)],
               [pac_cv_have___typeof],
[AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[]],[[double foo = 0.0; __typeof(foo) bar = 1.0;]])],
                  [pac_cv_have___typeof=yes],
                  [pac_cv_have___typeof=no])]
)
if test "$pac_cv_have___typeof" = "yes" ; then
    AC_DEFINE([HAVE___TYPEOF],[1],[defined if the C compiler supports __typeof(variable)])
fi

dnl Check if the necessary headers are available
AC_CHECK_HEADERS(stdio.h stdlib.h string.h stdarg.h ctype.h sys/types.h sys/uio.h execinfo.h unistd.h errno.h windows.h sys/mman.h)

# A C99 compliant compiler should have inttypes.h for fixed-size int types
AC_CHECK_HEADERS(inttypes.h stdint.h)
AC_HEADER_STDBOOL

#######################################################################
# valgrind support
AC_ARG_WITH([valgrind],
[AS_HELP_STRING([--without-valgrind],[to disable valgrind support (such as because of version issues)])]
[AS_HELP_STRING([--with-valgrind=PATH],[use valgrind headers installed in PATH (default is "yes", use no special path)])],
[],[with_valgrind=yes])
if test "$with_valgrind" != "no" ; then
    savedCPPFLAGS="$CPPFLAGS"
    if test "$with_valgrind" != "yes" ; then
        # Clients of MPL will either need to respect the localdefs file (as in
        # MPICH) or add this entry to their own CPPFLAGS-equivalent.
        # (TODO: a pkg-config file would help with this)
        PAC_APPEND_FLAG([-I${with_valgrind}], [CPPFLAGS])
    fi
    # headers for valgrind client requests
    AC_CHECK_HEADERS([valgrind.h memcheck.h valgrind/valgrind.h valgrind/memcheck.h])
    # headers for valgrind-based thread checking tools
    # TODO: incorporate ThreadSanitizer as well (include dynamic_annotations.h,
    # link with dynamic_annotations.c)
    AC_CHECK_HEADERS([helgrind.h valgrind/helgrind.h])
    AC_CHECK_HEADERS([drd.h valgrind/drd.h])

    # ensure that we have a new enough valgrind with all the client macros
    # a preproc test would probably be sufficient, but the LINK_IFELSE helps us
    # double-check that we aren't accidentally grabbing the headers for some
    # other platform
    AC_CACHE_CHECK([whether the valgrind headers are broken or too old],
                   [pac_cv_have_broken_valgrind],
                   [AC_LINK_IFELSE(
                       [AC_LANG_PROGRAM([
#if defined(HAVE_VALGRIND_H) && defined(HAVE_MEMCHECK_H)
#  include <valgrind.h>
#  include <memcheck.h>
#elif defined(HAVE_VALGRIND_VALGRIND_H) && defined(HAVE_VALGRIND_MEMCHECK_H)
#  include <valgrind/valgrind.h>
#  include <valgrind/memcheck.h>
#else
#  error unexpected valgrind header error
#endif
int foo = 10;
char mempool_obj;
],[
#if defined(VALGRIND_MAKE_MEM_DEFINED)
    VALGRIND_MAKE_MEM_NOACCESS(&foo,sizeof(foo));
    VALGRIND_MAKE_MEM_UNDEFINED(&foo,sizeof(foo));
    VALGRIND_MAKE_MEM_DEFINED(&foo,sizeof(foo));
    VALGRIND_CHECK_MEM_IS_DEFINED(&foo,sizeof(foo));
    VALGRIND_CHECK_MEM_IS_ADDRESSABLE(&foo,sizeof(foo));
#elif defined(VALGRIND_MAKE_READABLE)
/* older (pre-3.2.0), but still supported style */
    VALGRIND_MAKE_READABLE(&foo,sizeof(foo));
    VALGRIND_MAKE_NOACCESS(&foo,sizeof(foo));
    VALGRIND_MAKE_UNDEFINED(&foo,sizeof(foo));
    VALGRIND_CHECK_READABLE(&foo,sizeof(foo));
    VALGRIND_CHECK_WRITEABLE(&foo,sizeof(foo));
#else
#error missing essential valgrind client macros
#endif
    VALGRIND_CREATE_BLOCK(&foo,sizeof(foo),"description");
    if (RUNNING_ON_VALGRIND) ++foo;
    VALGRIND_PRINTF_BACKTRACE("testing: %s","valgrind support");
    VALGRIND_CREATE_MEMPOOL(&mempool_obj,0,0);
    VALGRIND_MEMPOOL_ALLOC(&mempool_obj,&foo,sizeof(foo));
    VALGRIND_MEMPOOL_FREE(&mempool_obj,&foo);
    VALGRIND_DESTROY_MEMPOOL(&mempool_obj);
]) dnl end PROGRAM
                       ],
                       [pac_cv_have_broken_valgrind=no], dnl end if-true
                       [pac_cv_have_broken_valgrind=yes] dnl end if-false
                   )] dnl end IFELSE
                   ) dnl end CACHE_CHECK

    if test "$pac_cv_have_broken_valgrind" = "yes" ; then
        AC_DEFINE([HAVE_BROKEN_VALGRIND],[1],[define if valgrind is old and/or broken compared to what we are expecting])
        CPPFLAGS="$savedCPPFLAGS"
    fi
fi


#######################################################################
## TIMER CODE
#######################################################################

# ----------------------------------------------------------------------------
# Support for timers.  The following code processes the 
#  --enable-timer-type=name argument and selects the timer based on 
# both that field and what configure is able to determine is available.
# The file src/include/mpl_timer.h is also created.
# ----------------------------------------------------------------------------

# clock_gettime is the POSIX gettimeofday
# gethrtime is the Solaris high-resolution timer
dnl
dnl Specific checks that a function works correctly
dnl
dnl Now that we know what the options are, choose the timer to use
dnl
dnl The default preference is
dnl    Solaris gethrtime
dnl    Posix   clock_gettime
dnl    Unix    gettimeofday (one of two versions)
dnl
dnl Also available are various hardware time stamps
dnl    Linux-x86 cycle counter
dnl    Powerpc-64bit timebase cycle counter
dnl
dnl We also allow --enable-timer-type=name to select a timer type
AC_ARG_ENABLE(timer-type,
[  --enable-timer-type=name - Select the timer to use for MPI_Wtime and
                             internal timestamps.
        ppc64_cycle        - Powerpc-64bit; returns cycle counts using timebase register
        gethrtime          - Solaris timer (Solaris systems only)
        clock_gettime      - Posix timer (where available)
        gettimeofday       - Most Unix systems
        linux86_cycle      - Linux x86; returns cycle counts, not time in seconds*
        gcc_ia64_cycle     - IPF ar.itc timer*
	mach_absolute_time - Mach absolute time (alternative to clock_gettime
                             for OSX)
        device             - The timer is provided by the device

        *Note that the cycle timers are intended to be used by
        developers for internal low-level timing.  Normal users should
        not use these as they are not guaranteed to be accurate in
        certain situations.
],timer_type=$enable_timer_type)

## The user did not specify a timer type.  Try to find a sane option
## that is supported by the platform.
if test -z "$timer_type" ; then
    # Try to pick a timer based on what is available
    AC_CHECK_FUNCS(clock_gettime clock_getres gethrtime mach_absolute_time gettimeofday)
    if test "$ac_cv_func_gethrtime" = "yes" ; then
        # Sigh.  The Solaris include files do not define hrtime_t
	# Before we accept this choice, make sure that we can 
	# do arithmetic with hrtime_t .
        AC_CACHE_CHECK([that hrtime_t is properly defined for gethrtime],
	pac_cv_hrtime_works,[
	AC_TRY_COMPILE([
#include <sys/time.h>
],[hrtime_t t1, t2; t1 = 1; t2 = 2; t1 = t1 + t2;],
pac_cv_hrtime_works=yes,pac_cv_hrtime_works=no)])
	# A more ambitious test would look to see if casting an 
	# hrtime_t to int64_t works, and even more ambitious
	# would check whether long or long long was 64 bits (or even
	# better, the sizeof hrtime_t).  

        # AC_CHECK_FUNCS has false positive when checking whether gethrtime is
        # available on Solaris with strict configuration. We must use
        # AC_CHECK_DECL to confirm it.
        AC_CHECK_DECL(gethrtime)
    fi
    if test "$ac_cv_func_gethrtime" = "yes" -a \
            "$ac_cv_has_decl_gethrtime" = "yes" -a \
            "$pac_cv_hrtime_works" = "yes" ; then
        timer_type=gethrtime
    elif test "$ac_cv_func_clock_gettime" = "yes" -a \
              "$ac_cv_func_clock_getres" = "yes" ; then
	 # Test on both because some systems (e.g., cygwin) provide
	 # clock_gettime but not clock_getres
        timer_type=clock_gettime
    elif test "$ac_cv_func_mach_absolute_time" = "yes" ; then 
        timer_type=mach_absolute_time
    elif test "$ac_cv_func_gettimeofday" = "yes" ; then
        timer_type=gettimeofday
    fi
fi
if test -z "$timer_type" ; then
    AC_MSG_ERROR([No timer found])
fi

# Check for valid timer and select datatypes for the time stamp
case "$timer_type" in

    gethrtime)
    MPL_TIMER_TYPE=hrtime_t
    AC_CHECK_FUNC(gethrtime,,[
         AC_MSG_ERROR([Requested timer gethrtime is not available])
])
    ;;

    clock_gettime)
    missing_function=no
    AC_SEARCH_LIBS([clock_gettime],[rt],,AC_MSG_ERROR([clock_gettime is not available]))
    AC_SEARCH_LIBS([clock_getres],[rt],,AC_MSG_ERROR([clock_getres is not available]))
    MPL_TIMER_TYPE="struct timespec"
    # AIX does not always define struct timespec (!)
    # Make sure that we can use struct timespec
    AC_CACHE_CHECK([whether struct timespec is defined in time.h],
                    pac_cv_struct_timespec_defined,[
    AC_TRY_COMPILE([
#include <time.h>],[
    struct timespec t;],pac_cv_struct_timespec_defined=yes,
    pac_cv_struct_timespec_defined=no)
])
    if test "$pac_cv_struct_timespec_defined" != "yes" ; then
        # Try again, but with -D_XOPEN_SOURCE=500 (works for AIX)
        AC_CACHE_CHECK([whether struct timespec is defined in time.h with _XOPEN_SOURCE=500],
                    pac_cv_struct_timespec_defined_with_xopen500,[
        AC_TRY_COMPILE([
#define _XOPEN_SOURCE 500
#include <time.h>],[
    struct timespec t;],pac_cv_struct_timespec_defined_with_xopen500=yes,
    pac_cv_struct_timespec_defined_with_xopen500=no)
])
	if test "$pac_cv_struct_timespec_defined_with_xopen500" = "yes" ; then
	    # We need to define _XOPEN_SOURCE=500, but we need to ensure that
	    # this is done before any include files are loaded.  At
	    # this point it is really too late to add this definition,
	    # since it may make other tests incompatible.
	    AC_MSG_ERROR([The available timer requires _XOPEN_SOURCE=500.  Add -D_XOPEN_SOURCE=500 to CFLAGS and rerun configure])
        fi
    fi
    # 
    # FreeBSD 4.3 incorrectly puts the header into sys/time.h; 
    # time.h is required (see pages 45 and 46 in the POSIX standard).
    # See if we can compile
    AC_CACHE_CHECK([for CLOCK_REALTIME defined in time.h],pac_cv_posix_clock_realtime,[
    AC_TRY_COMPILE([
#include <time.h>],[
    clockid_t cid = CLOCK_REALTIME;],pac_cv_posix_clock_realtime=yes,
pac_cv_posix_clock_realtime=no)])
    if test "$pac_cv_posix_clock_realtime" = "no" ; then
         AC_MSG_WARN([POSIX timer requires definitions in time.h])
	 # Check for the definition in sys/time.h, which is where
	 # OpenBSD and FreeBSD have put it by mistake
         AC_TRY_COMPILE([
#include <time.h>
#include <sys/time.h>],[
    clockid_t cid = CLOCK_REALTIME;],pac_cv_posix_clock_realtime=yes,
pac_cv_posix_clock_realtime=no)
	if test "$pac_cv_posix_clock_realtime" = yes ; then
	    AC_MSG_WARN([sys/time.h required for POSIX timer])
	    AC_DEFINE(NEEDS_SYS_TIME_H,1,[Define if sys/time.h is required to get timer definitions])
	else
	    AC_MSG_ERROR([Cannot find the definition of CLOCK_REALTIME for the POSIX timer])
	fi
    fi
    ;;

    gettimeofday)
    MPL_TIMER_TYPE="struct timeval"
    # We may have already tested for gettimeofday.  If we got a "yes",
    # we're good to go
    if test "$ac_cv_func_gettimeofday" != "yes" ; then
        AC_CHECK_FUNC(gettimeofday,,[
         AC_MSG_ERROR([Requested timer gettimeofday is not available])
])
    fi
    ;;

    linux86_cycle|linux86_cycle_2)
    
# The following AC_TRY_RUN statements are needed because x86_64 compilers
# usually know about rdtscp but the cpu may or may not actually implement the
# feature.  This is not cross-compile safe, unfortunately.  In the long run we
# should allow the user to override this with a configure flag.
    AC_CACHE_CHECK([that linux86 cycle counter is available],
        pac_cv_linux86_cycle,
            AC_TRY_RUN([
int main()
{
    /* rdtscp */
    long long var, *var_ptr=&var;
    __asm__ __volatile__("rdtscp; shl \$32, %%rdx; or %%rdx, %%rax" : "=a" (*var_ptr) : : "ecx", "rdx");
    return 0;
}
            ],pac_cv_linux86_cycle=rdtscp,
                AC_TRY_RUN([[
int main()
{
    /* cpuid 64 */
    long long var, *var_ptr=&var;
    __asm__ __volatile__("push %%rbx ; cpuid ; rdtsc ; pop %%rbx ; shl $32, %%rdx; or %%rdx, %%rax" : "=a" (*var_ptr) : : "ecx", "rdx");
    return 0;
}
                ]],pac_cv_linux86_cycle=cpuid_rdtsc64,
                    AC_TRY_RUN([[[
int main()
{
    /* cpuid 32 */
    long long var, *var_ptr=&var;
    __asm__ __volatile__("push %%ebx ; cpuid ; rdtsc ; pop %%ebx" : "=A" (*var_ptr) : : "ecx");
    return 0;
}
                    ]]],pac_cv_linux86_cycle=cpuid_rdtsc32,
                        AC_TRY_RUN([[[[
int main()
{
    /* simple */
    long long var, *var_ptr=&var;
    __asm__ __volatile__("rdtsc" : "=A" (*var_ptr));
    return 0;
}
                        ]]]],pac_cv_linux86_cycle=rdtsc,
                        pac_cv_linux86_cycle=no)
                    )
                ),
dnl The if-cross-compiling clause from the first AC_TRY_RUN.  Hope that if the
dnl compiler knows about the instruction then it's supported by the target
dnl platform.
                AC_TRY_COMPILE(,[[
                    long long var, *var_ptr=&var;
                    __asm__ __volatile__("rdtscp; shl \$32, %%rdx; or %%rdx, %%rax" : "=a" (*var_ptr) : : "ecx", "rdx");
                ]],pac_cv_linux86_cycle=rdtscp,
                    AC_TRY_COMPILE(,[[[
                        long long var, *var_ptr=&var;
                        __asm__ __volatile__("push %%rbx ; cpuid ; rdtsc ; pop %%rbx ; shl $32, %%rdx; or %%rdx, %%rax" : "=a" (*var_ptr) : : "ecx", "rdx");
                    ]]],pac_cv_linux86_cycle=cpuid_rdtsc64,
                        AC_TRY_COMPILE(,[[[[
                            long long var, *var_ptr=&var;
                            __asm__ __volatile__("push %%ebx ; cpuid ; rdtsc ; pop %%ebx" : "=A" (*var_ptr) : : "ecx");
                        ]]]],pac_cv_linux86_cycle=cpuid_rdtsc32,
                            AC_TRY_COMPILE(,[[[[[
                                long long var, *var_ptr=&var;
                                __asm__ __volatile__("rdtsc" : "=A" (*var_ptr));
                            ]]]]],pac_cv_linux86_cycle=rdtsc,
                            pac_cv_linux86_cycle=no)
                        )
                    )
                )
            )
    )

    case "$pac_cv_linux86_cycle" in
        "rdtscp")
            AC_DEFINE(LINUX86_CYCLE_RDTSCP,1,[Define which x86 cycle counter to use])
	    ;;
        "cpuid_rdtsc64")
            AC_DEFINE(LINUX86_CYCLE_CPUID_RDTSC64,1,[Define which x86 cycle counter to use])
            ;;
        "cpuid_rdtsc32")
            AC_DEFINE(LINUX86_CYCLE_CPUID_RDTSC32,1,[Define which x86 cycle counter to use])
	    ;;
        "rdtsc")
            AC_DEFINE(LINUX86_CYCLE_RDTSC,1,[Define which x86 cycle counter to use])
	    ;;
        *)
            cpu_gcc_x86_cycle=no
	    ;;
    esac

    if test "$cpu_gcc_x86_cycle" = "no" ; then
        AC_MSG_ERROR([Linux86 cycle counter is not available on this system and or with the $CC compiler])
    fi
    MPL_TIMER_TYPE="long long"
    ;;

    gcc_ia64_cycle)
    AC_CACHE_CHECK([that IPF timer is available],
pac_cv_ia64_cycle,[
    AC_TRY_COMPILE(,[
    long var, *var_ptr=&var;
#ifdef __INTEL_COMPILER
#include "ia64regs.h"
    var=__getReg(_IA64_REG_AR_ITC);
#else
    __asm__ __volatile__("mov %0=ar.itc" : "=r" (var_ptr));
#endif
],pac_cv_gcc_ia64_cycle=yes,pac_cv_gcc_ia64_cycle=no)])
    if test "$pac_cv_gcc_ia64_cycle" != "yes" ; then
        AC_MSG_ERROR([IPF cycle counter is not available on this system and or with the $CC compiler])
     fi
     MPL_TIMER_TYPE="long"
     ;;

    mach_absolute_time)
    AC_CHECK_FUNC(mach_absolute_time,,[AC_MSG_ERROR([mach_absolute_time is not available])])
    AC_CHECK_FUNC(mach_timebase_info,,[AC_MSG_ERROR([mach_timebase_info is not available])])
    MPL_TIMER_TYPE="uint64_t"
    ;;

    device)
    # The device selected should export the datatype for the timer
    MPL_TIMER_TYPE="void *"
    ;;

    ppc64_cycle)
    AC_CACHE_CHECK([that ppc64 timebase cycle counter is available],
                pac_cv_ppc64_cycle,[
    AC_TRY_COMPILE([
    ],[
        unsigned temp;
        asm volatile ("mfspr %0,%1" : "=r" (temp)        : "i" (0x10D));
    ],pac_cv_ppc64_cycle=yes,
    pac_cv_ppc64_cycle=no)
    ])
    if test "$pac_cv_ppc64_cycle" != "yes" ; then
        AC_MSG_ERROR([The PPC64 cycle counter is not available on this system and or with the $CC compiler])
    fi
    MPL_TIMER_TYPE="uint64_t"
    ;;
    *)
    AC_MSG_ERROR([Invalid timer type $timer_type])
    ;;
esac
# Convert timer type to upper case
timer_type=`echo $timer_type | \
    tr 'abcdefghijklmnopqrstuvwxyz' 'ABCDEFGHIJKLMNOPQRSTUVWXYZ'`
MPL_TIMER_KIND=MPL_TIMER_KIND__$timer_type
AC_SUBST(MPL_TIMER_KIND)
AC_SUBST(MPL_TIMER_TYPE)
AC_MSG_NOTICE([Timer type selected is $timer_type])

#######################################################################
## END OF TIMER CODE
#######################################################################


#######################################################################
## START OF PROCESSOR YIELD CODE
#######################################################################
# If the user specified a method to use, we check if it's available.
# If a method was not specified, we make a guess based on the OS.  The
# default is to use sched_yield() or yield() if one is available,
# otherwise, default to nothing.  After that we define the appropriate
# macro.

AC_CHECK_HEADERS(sched.h)
AC_CHECK_HEADERS(unistd.h)
AC_CHECK_HEADERS(sys/select.h)
AC_CHECK_FUNCS(sched_yield yield usleep sleep select)

if test "$ac_cv_func_usleep" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <unistd.h>],usleep)
fi

AC_ARG_ENABLE([yield],
    [AS_HELP_STRING([--enable-yield], [choose a method to yield the processor in busy loops.  Available methods are: sched_yield, yield, select, usleep, sleep, nothing])],
    [AS_CASE([$enableval],
        [sched_yield], [AS_IF([test "x$ac_cv_func_sched_yield" != "xyes"], [enable_yield=unavail])],
        [yield],       [AS_IF([test "x$ac_cv_func_yield"       != "xyes"], [enable_yield=unavail])],
        [select],      [AS_IF([test "x$ac_cv_func_select"      != "xyes"], [enable_yield=unavail])],
        [usleep],      [AS_IF([test "x$ac_cv_func_usleep"      != "xyes"], [enable_yield=unavail])],
        [sleep],       [AS_IF([test "x$ac_cv_func_sleep"       != "xyes"], [enable_yield=unavail])],
        [nothing|no|none], [],
        [AC_MSG_ERROR([Invalid option $enableval for --enable-yield])])

     AS_IF([test "x$enable_yield" = "xunavail"],
              [AC_MSG_ERROR([Yield method $enableval is not available on this platform.])])
    ],
    [# none specified by user; make a guess based on os
     AS_CASE([$host],
        [*-*-darwin*],
            [# In Lion, sched_yield worked but none of the other options had any effect
             AS_IF([test "x$ac_cv_func_sched_yield" = "xyes"], [enable_yield=sched_yield],
                   [enable_yield=nothing])],
        [*-*-linux*],
            [# sched_yield() has been broken in linux since 2.6.23, and no good alternative exists.
             enable_yield=nothing],
        [# default: just use sched_yield() or yield()
         AS_IF([test "x$ac_cv_func_sched_yield" = "xyes"], [enable_yield=sched_yield],
               [test "x$ac_cv_func_yield" = "xyes"], [enable_yield=yield],
               [enable_yield=nothing])])
    ]
)

# set the appropriate macro
AS_CASE([$enable_yield],
    [sched_yield],
        [AC_DEFINE(USE_SCHED_YIELD_FOR_YIELD,1,[Define to use sched_yield to yield processor])],
    [yield],
        [AC_DEFINE(USE_YIELD_FOR_YIELD,1,[Define to use yield to yield processor])],
    [select],
        [AC_DEFINE(USE_SELECT_FOR_YIELD,1,[Define to use select to yield processor])],
    [usleep],
        [AC_DEFINE(USE_USLEEP_FOR_YIELD,1,[Define to use usleep to yield processor])],
    [sleep],
        [AC_DEFINE(USE_SLEEP_FOR_YIELD,1,[Define to use sleep to yield processor])],
    [nothing|no|none],
        [AC_DEFINE(USE_NOTHING_FOR_YIELD,1,[Define to use nothing to yield processor])],
    [AC_MSG_ERROR([Invalid value $enable_yield for enable_yield.])]
)

#######################################################################
## END OF PROCESSOR YIELD CODE
#######################################################################


#######################################################################
## START OF THREADS CODE
#######################################################################
PAC_SET_HEADER_LIB_PATH(uti)

AC_ARG_WITH([thread-package],
[  --with-thread-package=package     Thread package to use. Supported thread packages include:
        posix or pthreads - POSIX threads (default, if required)
        solaris - Solaris threads (Solaris OS only)
        abt or argobots - Argobots threads
        win - windows threads
        uti - POSIX threads plus Utility Thread Offloading library
        none - no threads
],,with_thread_package=posix)

if test "${thread_pkg_required}" = "no" -o "${with_thread_package}" = "no" ; then
   with_thread_package=none
fi

if test "${with_thread_package}" = "yes" ; then
   with_thread_package=posix
fi

if test "${thread_pkg_required}" = "yes" -a "${with_thread_package}" = "none" ; then
   AC_ERROR([if no thread package is available, use --enable-threads=single or funneled])
fi

THREAD_PACKAGE_NAME=MPL_THREAD_PACKAGE_INVALID
case $with_thread_package in
    posix|pthreads|uti)
	if test "${with_thread_package}" = "pthreads" ; then
	    with_thread_package=posix
	fi
	AC_CHECK_HEADERS(pthread.h)

        # If pthreads library is found, just include it on the link line. We don't try
        # to test if the C compiler needs it or not, since the C++ or Fortran
        # compilers might need it even if the C compiler doesn't
        # (nvcc with gfortran, for example).
        #
        # OSF1 has __pthread_create but not pthread_create (because of
        # inconsistencies in the pthread spec).  Thus, we look for pthread_key_create
        AC_CHECK_LIB([pthread],[pthread_key_create],have_pthreads=yes)
	if test "$have_pthreads" = "yes" ; then
	   PAC_PREPEND_FLAG([-lpthread],[LIBS])
	fi

        AC_CHECK_FUNCS(pthread_yield)

        # this check should come after the AC_CHECK_LIB for -lpthread
        AC_CHECK_FUNC([pthread_key_create],[],[AC_MSG_ERROR([unable to find pthreads library])])

	# Check for a routine that specify a routine to call on
	# thread exit.  We can use this to release memory that may
	# be allocated by the MPL library in the thread.
	# A complication: pthread_cleanup_push may be a macro; in that
	# case, check_funcs will fail to find it.
	# Under OSX, pthread_cleanup_push and pop are macros that must
	# appear together in the same lexical scope, and hence are
	# really useless in libraries that may allocate data within
	# a user-managed thread.
	AC_CHECK_FUNCS(pthread_cleanup_push)
	if test "$ac_cv_func_pthread_cleanup_push" = "no" ; then
            AC_CACHE_CHECK([whether pthread_cleanup_push is available (may be a macro in pthread.h)],pac_cv_func_pthread_cleanup_push,[
	    AC_TRY_LINK([
#include <pthread.h>
void f1(void *a) { return; }],
[pthread_cleanup_push( f1, (void *)0 );],
            pac_cv_func_pthread_cleanup_push=yes,
            pac_cv_func_pthread_cleanup_push=no)])
            if test "$pac_cv_func_pthread_cleanup_push" = yes ; then
	        AC_DEFINE(HAVE_PTHREAD_CLEANUP_PUSH_MACRO,1,[Define if pthread_cleanup_push is available, even as a macro])
            fi

        fi

	if test "${with_thread_package}" = "uti" ; then
	    PAC_CHECK_HEADER_LIB(uti.h,uti,uti_attr_init,have_uti=yes,have_uti=no)
	    if test "${have_uti}" = "yes" ; then
		AC_MSG_NOTICE([Using POSIX and Utility Thread Offloading library for thread package])
	    else
		AC_MSG_ERROR([Unable to find Utility Thread Offloading library])
	    fi
	    THREAD_PACKAGE_NAME=MPL_THREAD_PACKAGE_UTI
	else
	    AC_MSG_NOTICE([POSIX will be used for thread package.])
	    THREAD_PACKAGE_NAME=MPL_THREAD_PACKAGE_POSIX
	fi
	;;
    solaris)
	AC_CHECK_HEADERS(thread.h)
	AC_CHECK_FUNCS(thr_yield)
	AC_SEARCH_LIBS(thr_create,thread,found=yes,found=no)
	if test "$found" != "yes" ; then
	   AC_MSG_ERROR([unable to find Solaris threads library])
	fi
	# FIXME: need to add -mt if using solaris compilers
        THREAD_PACKAGE_NAME=MPL_THREAD_PACKAGE_SOLARIS
	;;
    win|windows)
        with_thread_package=win
        THREAD_PACKAGE_NAME=MPL_THREAD_PACKAGE_WIN
        AC_MSG_ERROR([The 'win' thread package is not supported via autoconf builds at this time.])
        ;;
    abt|argobots)
        with_thread_package=argobots
        AC_CHECK_HEADER([abt.h],[],AC_MSG_ERROR([unable to find Argobots header file]))
        AC_CHECK_LIB([abt],[ABT_key_create],[],AC_MSG_ERROR([unable to find Argobots library]))
        PAC_PREPEND_FLAG([-labt],[LIBS])
        THREAD_PACKAGE_NAME=MPL_THREAD_PACKAGE_ARGOBOTS
        ;;
    no|none)
	with_thread_package=none
        THREAD_PACKAGE_NAME=MPL_THREAD_PACKAGE_NONE
	;;
    *)
	AC_MSG_ERROR([The specified thread package, $with_thread_package, is not supported.])
	;;
esac

# Define and export the selected thread library so that other packages
# know what's used in MPL
AC_DEFINE_UNQUOTED([THREAD_PACKAGE_NAME],[$THREAD_PACKAGE_NAME],[set to the name of the thread package])

# check for compiler-support for thread-local storage (MPL_TLS)
if test "${with_thread_package}" != "argobots" ; then
    PAC_CC_CHECK_TLS
fi

#######################################################################
## END OF THREADS CODE
#######################################################################

#######################################################################
## START OF PROCESS MUTEX CODE
#######################################################################

AC_ARG_WITH([proc-mutex-package],
[  --with-proc-mutex-package=package     Interprocess mutex package to use. Supported packages include:
        posix or pthreads - POSIX threads (default, if required)
        none - no interprocess mutex support
],,with_proc_mutex_package=posix)

if test "${with_proc_mutex_package}" = "no" ; then
   with_proc_mutex_package=none
fi
if test "${with_proc_mutex_package}" = "yes" ; then
   with_proc_mutex_package=posix
fi

PROC_MUTEX_PACKAGE_NAME=MPL_PROC_MUTEX_PACKAGE_INVALID
case $with_proc_mutex_package in
  posix|pthreads)
    if test "${with_proc_mutex_package}" = "pthreads" ; then
        with_proc_mutex_package=posix
    fi

    # Do not prepend -lpthread again if already checked by thread package
    if test "${with_thread_package}" != "posix" ; then
        AC_CHECK_HEADERS(pthread.h)

        # If pthreads library is found, just include it on the link line. We don't try
        # to test if the C compiler needs it or not, since the C++ or Fortran
        # compilers might need it even if the C compiler doesn't
        # (nvcc with gfortran, for example).
        AC_CHECK_LIB([pthread],[pthread_mutex_init],have_pthreads=yes)
        if test "$have_pthreads" = "yes" ; then
           PAC_PREPEND_FLAG([-lpthread],[LIBS])
        fi
    fi

    # this check should come after the AC_CHECK_LIB for -lpthread
    AC_CHECK_FUNC([pthread_mutex_init],[],[AC_MSG_ERROR([unable to find pthreads library])])
    # pthread_mutexattr_setpshared is first released in Issue 5
    AC_CHECK_FUNCS(pthread_mutexattr_setpshared)

    AC_MSG_NOTICE([POSIX will be used for interprocess mutex package.])
    PROC_MUTEX_PACKAGE_NAME=MPL_PROC_MUTEX_PACKAGE_POSIX
  ;;
  no|none)
    with_proc_mutex_package=none
    PROC_MUTEX_PACKAGE_NAME=MPL_PROC_MUTEX_PACKAGE_NONE
  ;;
  *)
    AC_MSG_ERROR([The specified interprocess mutex package, $with_proc_mutex_package, is not supported.])
  ;;
esac

# Define and export the selected interprocess mutex package so that other packages
# know what's used in MPL
AC_DEFINE_UNQUOTED([PROC_MUTEX_PACKAGE_NAME],[$PROC_MUTEX_PACKAGE_NAME],[set to the name of the interprocess mutex package])
#######################################################################
## END OF PROCESS MUTEX CODE
#######################################################################

#######################################################################
## START OF PTHREAD MUTEX COMMON CHECK
#######################################################################
if test "${with_thread_package}" = "pthreads" -o "${with_proc_mutex_package}" = "pthreads"; then
  # Check for PTHREAD_MUTEX_ERRORCHECK_NP and PTHREAD_MUTEX_ERRORCHECK
  AC_CACHE_CHECK([whether pthread.h defines PTHREAD_MUTEX_ERRORCHECK_NP],
  pac_cv_has_pthread_mutex_errorcheck_np,[
  AC_TRY_COMPILE([#include <pthread.h>],
                 [int a=PTHREAD_MUTEX_ERRORCHECK_NP;],
                 pac_cv_has_pthread_mutex_errorcheck_np=yes,
                 pac_cv_has_pthread_mutex_errorcheck_np=no)])
  AC_CACHE_CHECK([whether pthread.h defines PTHREAD_MUTEX_ERRORCHECK],
  pac_cv_has_pthread_mutex_errorcheck,[
  AC_TRY_COMPILE([#include <pthread.h>],
                 [int a=PTHREAD_MUTEX_ERRORCHECK;],
                 pac_cv_has_pthread_mutex_errorcheck=yes,
                 pac_cv_has_pthread_mutex_errorcheck=no)])

  if test "$pac_cv_has_pthread_mutex_errorcheck" = yes ; then
      AC_DEFINE(PTHREAD_MUTEX_ERRORCHECK_VALUE,PTHREAD_MUTEX_ERRORCHECK,
                [Define to an expression that will result in an error checking mutex type.])
  elif test "$pac_cv_has_pthread_mutex_errorcheck_np" = yes ; then
      AC_DEFINE(PTHREAD_MUTEX_ERRORCHECK_VALUE,PTHREAD_MUTEX_ERRORCHECK_NP,
                [Define to an expression that will result in an error checking mutex type.])
  fi

  PAC_FUNC_NEEDS_DECL([#include <pthread.h>],pthread_mutexattr_settype)
fi
#######################################################################
## END OF PTHREAD MUTEX COMMON CHECK
#######################################################################

#######################################################################
## START OF DBG CODE
#######################################################################

# mkstemp() is a better replacement for mktemp()
AC_HAVE_FUNCS(mkstemp)
if test "$ac_cv_func_mkstemp" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <stdlib.h>],mkstemp)
fi
# fdopen() converts from an fd to a FILE*
AC_HAVE_FUNCS(fdopen)
if test "$ac_cv_func_fdopen" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <stdio.h>],fdopen)
fi

AC_CHECK_FUNCS(getpid)

#######################################################################
## END OF DBG CODE
#######################################################################

#######################################################################
## START OF SHM CODE
#######################################################################

dnl Check for shm
PAC_ARG_SHARED_MEMORY

case $with_shared_memory in
    sysv)
        AC_DEFINE(MPL_USE_SYSV_SHM,1,[Define if use SYSV shared memory])
        ;;
    mmap)
        AC_DEFINE(MPL_USE_MMAP_SHM,1,[Define if use MMAP shared memory])
        ;;
    nt)
        AC_DEFINE(MPL_USE_NT_SHM,1,[Define if use Windows shared memory])
        ;;
    *)
        AC_MSG_ERROR([cannot support shared memory:  need either sysv shared memory functions or mmap in order to support shared memory])
esac


#######################################################################
## END OF SHM CODE
#######################################################################

## Enable creation of libtool-style versioning or no versioning
AC_ARG_ENABLE(versioning,
        [AC_HELP_STRING([--enable-versioning],[Enable library versioning])],,
        [enable_versioning=yes])

if test "$enable_versioning" = "yes" ; then
   libmpl_so_versionflags="-version-info \$(libmpl_so_version)"
else
   libmpl_so_versionflags="-avoid-version"
fi
export libmpl_so_versionflags
AC_SUBST(libmpl_so_versionflags)

#######################################################################


# Check for strdup
AC_CHECK_FUNCS(strdup)
if test "$ac_cv_func_strdup" = "yes" ; then
   PAC_FUNC_NEEDS_DECL([#include <string.h>],strdup)
fi

# Check for snprintf
AC_CHECK_FUNCS(snprintf)
if test "$ac_cv_func_snprintf" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <stdio.h>],snprintf)
fi

# Check for strncmp
AC_CHECK_FUNCS(strncmp)
if test "$ac_cv_func_strncmp" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <string.h>],strncmp)
fi

# Check for putenv
AC_CHECK_FUNCS(putenv)
if test "$ac_cv_func_putenv" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <stdlib.h>],putenv)
fi

# Check for strerror
AC_CHECK_FUNCS(strerror)
if test "$ac_cv_func_strerror" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <string.h>],strerror)
fi

# Check for usleep
AC_CHECK_FUNCS(usleep)
if test "$ac_cv_func_usleep" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <unistd.h>],usleep)
fi

# Check if we can implement MPL_aligned_alloc
AC_CHECK_FUNCS(posix_memalign)
AC_CHECK_FUNCS(aligned_alloc)
if test "$ac_cv_func_aligned_alloc" = "yes" ; then
    PAC_FUNC_NEEDS_DECL([#include <stdlib.h>],aligned_alloc)
fi
if test "$ac_cv_func_posix_memalign" = "yes" -o "$ac_cv_func_aligned_alloc" = "yes"; then
   AC_DEFINE([DEFINE_ALIGNED_ALLOC],[1],[Define to 1 if MPL enables MPL_aligned_alloc.])
fi

AC_CHECK_FUNCS(backtrace_symbols)

# Check for libbacktrace support
PAC_PUSH_FLAG([LIBS])
PAC_PREPEND_FLAG([-lbacktrace],[LIBS])
AC_LINK_IFELSE([
    AC_LANG_PROGRAM([
#include <backtrace.h>
],[
backtrace_create_state(0, 0, 0, 0);
backtrace_print(0, 0, 0);
])],
    [ac_cv_lib_backtrace=yes],
    [ac_cv_lib_backtrace=no]
)
PAC_POP_FLAG([LIBS])

if test "$ac_cv_lib_backtrace" = "yes" ; then
    AC_DEFINE([HAVE_LIBBACKTRACE],[1],[Define to 1 if you have the backtrace header (backtrace.h) and library (-lbacktrace)])
    PAC_PREPEND_FLAG([-lbacktrace],[LIBS])
fi
AC_CHECK_DECLS([backtrace_create_state, backtrace_print])

# Check for libunwind support
PAC_PUSH_FLAG([LIBS])
PAC_PREPEND_FLAG([-lunwind],[LIBS])
AC_LINK_IFELSE([
    AC_LANG_PROGRAM([
#include<libunwind.h>
],[
unw_backtrace(0, 0);
])],
    [ac_cv_lib_libunwind=yes],
    [ac_cv_lib_libunwind=no]
)
PAC_POP_FLAG([LIBS])

if test "$ac_cv_lib_libunwind" = "yes" ; then
    AC_DEFINE([HAVE_LIBUNWIND],[1],[Define to 1 if you have the libunwind header (libunwind.h) and library (-lunwind)])
    PAC_PREPEND_FLAG([-lunwind],[LIBS])
fi

AX_LIB_SOCKET_NSL
AC_CHECK_HEADERS(ifaddrs.h arpa/inet.h)
AC_CHECK_FUNCS(inet_ntop getifaddrs)

dnl Check for ATTRIBUTE
PAC_C_GNU_ATTRIBUTE
AX_GCC_VAR_ATTRIBUTE(aligned)
AX_GCC_VAR_ATTRIBUTE(used)

dnl Check for fallthrough attribute
PAC_PUSH_ALL_FLAGS
dnl This check requires removing -Wall and -Wextra first or it will fail. Just
dnl clear them all.
CFLAGS=""
AX_GCC_FUNC_ATTRIBUTE(fallthrough)
PAC_POP_ALL_FLAGS

dnl Final output
AC_CONFIG_FILES([Makefile localdefs include/mpl_timer.h])
AC_OUTPUT
