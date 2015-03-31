dnl -*- Autoconf -*-
dnl
dnl Copyright © 2009-2014 Inria.  All rights reserved.
dnl Copyright (c) 2009-2012 Université Bordeaux 1
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2012 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2008 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright © 2006-2013 Cisco Systems, Inc.  All rights reserved.
dnl Copyright © 2012  Blue Brain Project, BBP/EPFL. All rights reserved.
dnl Copyright © 2012       Oracle and/or its affiliates.  All rights reserved.
dnl See COPYING in top-level directory.

# Main hwloc m4 macro, to be invoked by the user
#
# Expects two or three paramters:
# 1. Configuration prefix
# 2. What to do upon success
# 3. What to do upon failure
# 4. If non-empty, print the announcement banner
#
AC_DEFUN([HWLOC_SETUP_CORE],[
    AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])
    AC_REQUIRE([AC_CANONICAL_TARGET])
    AC_REQUIRE([AC_PROG_CC])

    AS_IF([test "x$4" != "x"],
          [cat <<EOF

###
### Configuring hwloc core
###
EOF])

    # If no prefix was defined, set a good value
    m4_ifval([$1], 
             [m4_define([hwloc_config_prefix],[$1/])],
             [m4_define([hwloc_config_prefix], [])])

    # Unless previously set to "standalone" mode, default to embedded
    # mode
    AS_IF([test "$hwloc_mode" = ""], [hwloc_mode=embedded])
    AC_MSG_CHECKING([hwloc building mode])
    AC_MSG_RESULT([$hwloc_mode])

    # Get hwloc's absolute top builddir (which may not be the same as
    # the real $top_builddir, because we may be building in embedded
    # mode).
    HWLOC_startdir=`pwd`
    if test x"hwloc_config_prefix" != "x" -a ! -d "hwloc_config_prefix"; then
        mkdir -p "hwloc_config_prefix"
    fi
    if test x"hwloc_config_prefix" != "x"; then
        cd "hwloc_config_prefix"
    fi
    HWLOC_top_builddir=`pwd`
    AC_SUBST(HWLOC_top_builddir)

    # Get hwloc's absolute top srcdir (which may not be the same as
    # the real $top_srcdir, because we may be building in embedded
    # mode).  First, go back to the startdir incase the $srcdir is
    # relative.

    cd "$HWLOC_startdir"
    cd "$srcdir"/hwloc_config_prefix
    HWLOC_top_srcdir="`pwd`"
    AC_SUBST(HWLOC_top_srcdir)

    # Go back to where we started
    cd "$HWLOC_startdir"

    AC_MSG_NOTICE([hwloc builddir: $HWLOC_top_builddir])
    AC_MSG_NOTICE([hwloc srcdir: $HWLOC_top_srcdir])
    if test "$HWLOC_top_builddir" != "$HWLOC_top_srcdir"; then
        AC_MSG_NOTICE([Detected VPATH build])
    fi

    # Debug mode?
    AC_MSG_CHECKING([if want hwloc maintainer support])
    hwloc_debug=

    # Unconditionally disable debug mode in embedded mode; if someone
    # asks, we can add a configure-time option for it.  Disable it
    # now, however, because --enable-debug is not even added as an
    # option when configuring in embedded mode, and we wouldn't want
    # to hijack the enclosing application's --enable-debug configure
    # switch.
    AS_IF([test "$hwloc_mode" = "embedded"],
          [hwloc_debug=0
           hwloc_debug_msg="disabled (embedded mode)"])
    AS_IF([test "$hwloc_debug" = "" -a "$enable_debug" = "yes"],
          [hwloc_debug=1
           hwloc_debug_msg="enabled"])
    AS_IF([test "$hwloc_debug" = ""],
          [hwloc_debug=0
           hwloc_debug_msg="disabled"])
    # Grr; we use #ifndef for HWLOC_DEBUG!  :-(
    AH_TEMPLATE(HWLOC_DEBUG, [Whether we are in debugging mode or not])
    AS_IF([test "$hwloc_debug" = "1"], [AC_DEFINE([HWLOC_DEBUG])])
    AC_MSG_RESULT([$hwloc_debug_msg])

    # We need to set a path for header, etc files depending on whether
    # we're standalone or embedded. this is taken care of by HWLOC_EMBEDDED.

    AC_MSG_CHECKING([for hwloc directory prefix])
    AC_MSG_RESULT(m4_ifval([$1], hwloc_config_prefix, [(none)]))

    # Note that private/config.h *MUST* be listed first so that it
    # becomes the "main" config header file.  Any AC-CONFIG-HEADERS
    # after that (hwloc/config.h) will only have selective #defines
    # replaced, not the entire file.
    AC_CONFIG_HEADERS(hwloc_config_prefix[include/private/autogen/config.h])
    AC_CONFIG_HEADERS(hwloc_config_prefix[include/hwloc/autogen/config.h])

    # What prefix are we using?
    AC_MSG_CHECKING([for hwloc symbol prefix])
    AS_IF([test "$hwloc_symbol_prefix_value" = ""],
          [AS_IF([test "$with_hwloc_symbol_prefix" = ""],
                 [hwloc_symbol_prefix_value=hwloc_],
                 [hwloc_symbol_prefix_value=$with_hwloc_symbol_prefix])])
    AC_DEFINE_UNQUOTED(HWLOC_SYM_PREFIX, [$hwloc_symbol_prefix_value],
                       [The hwloc symbol prefix])
    # Ensure to [] escape the whole next line so that we can get the
    # proper tr tokens
    [hwloc_symbol_prefix_value_caps="`echo $hwloc_symbol_prefix_value | tr '[:lower:]' '[:upper:]'`"]
    AC_DEFINE_UNQUOTED(HWLOC_SYM_PREFIX_CAPS, [$hwloc_symbol_prefix_value_caps],
                       [The hwloc symbol prefix in all caps])
    AC_MSG_RESULT([$hwloc_symbol_prefix_value])

    # Give an easy #define to know if we need to transform all the
    # hwloc names
    AH_TEMPLATE([HWLOC_SYM_TRANSFORM], [Whether we need to re-define all the hwloc public symbols or not])
    AS_IF([test "$hwloc_symbol_prefix_value" = "hwloc_"],
          [AC_DEFINE([HWLOC_SYM_TRANSFORM], [0])],
          [AC_DEFINE([HWLOC_SYM_TRANSFORM], [1])])

    # GCC specifics.
    if test "x$GCC" = "xyes"; then
        HWLOC_GCC_CFLAGS="-Wall -Wmissing-prototypes -Wundef"
        HWLOC_GCC_CFLAGS="$HWLOC_GCC_CFLAGS -Wpointer-arith -Wcast-align"
    fi

    # Enample system extensions for O_DIRECTORY, fdopen, fssl, etc.
    AH_VERBATIM([USE_HPUX_SYSTEM_EXTENSIONS],
[/* Enable extensions on HP-UX. */
#ifndef _HPUX_SOURCE
# undef _HPUX_SOURCE
#endif
])
    AC_DEFINE([_HPUX_SOURCE], [1], [Are we building for HP-UX?])
    
    AC_LANG_PUSH([C])
    
    # Check to see if we're producing a 32 or 64 bit executable by
    # checking the sizeof void*.  Note that AC CHECK_SIZEOF even works
    # when cross compiling (!), according to the AC 2.64 docs.  This
    # check is needed because on some systems, you can instruct the
    # compiler to specifically build 32 or 64 bit executables -- even
    # though the $target may indicate something different.
    AC_CHECK_SIZEOF([void *])

    #
    # List of components to be built, either statically or dynamically.
    # To be enlarged below.
    #
    hwloc_components="noos xml synthetic custom xml_nolibxml"

    #
    # Check OS support
    #
    AC_MSG_CHECKING([which OS support to include])
    case ${target} in
      powerpc64-bgq-linux*) # must be before Linux
	AC_DEFINE(HWLOC_BGQ_SYS, 1, [Define to 1 on BlueGene/Q])
	hwloc_bgq=yes
	AC_MSG_RESULT([bgq])
	hwloc_components="$hwloc_components bgq"
	;;
      *-*-linux*)
        AC_DEFINE(HWLOC_LINUX_SYS, 1, [Define to 1 on Linux])
        hwloc_linux=yes
        AC_MSG_RESULT([Linux])
        hwloc_components="$hwloc_components linux"
	if test x$enable_pci != xno; then
	  hwloc_components="$hwloc_components linuxpci"
	  AC_DEFINE(HWLOC_HAVE_LINUXPCI, 1, [Define to 1 if building the Linux PCI component])
	  hwloc_linuxpci_happy=yes
	fi				    
        ;;
      *-*-irix*)
        AC_DEFINE(HWLOC_IRIX_SYS, 1, [Define to 1 on Irix])
        hwloc_irix=yes
        AC_MSG_RESULT([IRIX])
        # no irix component yet
        ;;
      *-*-darwin*)
        AC_DEFINE(HWLOC_DARWIN_SYS, 1, [Define to 1 on Darwin])
        hwloc_darwin=yes
        AC_MSG_RESULT([Darwin])
        hwloc_components="$hwloc_components darwin"
        ;;
      *-*-solaris*)
        AC_DEFINE(HWLOC_SOLARIS_SYS, 1, [Define to 1 on Solaris])
        hwloc_solaris=yes
        AC_MSG_RESULT([Solaris])
        hwloc_components="$hwloc_components solaris"
        ;;
      *-*-aix*)
        AC_DEFINE(HWLOC_AIX_SYS, 1, [Define to 1 on AIX])
        hwloc_aix=yes
        AC_MSG_RESULT([AIX])
        hwloc_components="$hwloc_components aix"
        ;;
      *-*-osf*)
        AC_DEFINE(HWLOC_OSF_SYS, 1, [Define to 1 on OSF])
        hwloc_osf=yes
        AC_MSG_RESULT([OSF])
        hwloc_components="$hwloc_components osf"
        ;;
      *-*-hpux*)
        AC_DEFINE(HWLOC_HPUX_SYS, 1, [Define to 1 on HP-UX])
        hwloc_hpux=yes
        AC_MSG_RESULT([HP-UX])
        hwloc_components="$hwloc_components hpux"
        ;;
      *-*-mingw*|*-*-cygwin*)
        AC_DEFINE(HWLOC_WIN_SYS, 1, [Define to 1 on WINDOWS])
        hwloc_windows=yes
        AC_MSG_RESULT([Windows])
        hwloc_components="$hwloc_components windows"
        ;;
      *-*-*freebsd*)
        AC_DEFINE(HWLOC_FREEBSD_SYS, 1, [Define to 1 on *FREEBSD])
        hwloc_freebsd=yes
        AC_MSG_RESULT([FreeBSD])
        hwloc_components="$hwloc_components freebsd"
        ;;
      *-*-*netbsd*)
        AC_DEFINE(HWLOC_NETBSD_SYS, 1, [Define to 1 on *NETBSD])
        hwloc_netbsd=yes
        AC_MSG_RESULT([NetBSD])
        hwloc_components="$hwloc_components netbsd"
        ;;
      *)
        AC_MSG_RESULT([Unsupported! ($target)])
        AC_DEFINE(HWLOC_UNSUPPORTED_SYS, 1, [Define to 1 on unsupported systems])
        AC_MSG_WARN([***********************************************************])
        AC_MSG_WARN([*** hwloc does not support this system.])
        AC_MSG_WARN([*** hwloc will *attempt* to build (but it may not work).])
        AC_MSG_WARN([*** hwloc run-time results may be reduced to showing just one processor.])
        AC_MSG_WARN([*** You have been warned.])
        AC_MSG_WARN([*** Pausing to give you time to read this message...])
        AC_MSG_WARN([***********************************************************])
        sleep 10
        ;;
    esac

    #
    # Check CPU support
    #
    AC_MSG_CHECKING([which CPU support to include])
    case ${target} in
      i*86-*-*|x86_64-*-*|amd64-*-*)
        case ${ac_cv_sizeof_void_p} in
          4)
            AC_DEFINE(HWLOC_X86_32_ARCH, 1, [Define to 1 on x86_32])
            hwloc_x86_32=yes
	    HWLOC_MS_LIB_ARCH=X86
            AC_MSG_RESULT([x86_32])
            ;;
          8)
            AC_DEFINE(HWLOC_X86_64_ARCH, 1, [Define to 1 on x86_64])
            hwloc_x86_64=yes
	    HWLOC_MS_LIB_ARCH=X64
            AC_MSG_RESULT([x86_64])
            ;;
          *)
            AC_DEFINE(HWLOC_X86_64_ARCH, 1, [Define to 1 on x86_64])
            hwloc_x86_64=yes
	    HWLOC_MS_LIB_ARCH=X64
            AC_MSG_RESULT([unknown -- assuming x86_64])
            ;;
        esac
        ;;
      *)
        AC_MSG_RESULT([unknown])
        ;;
    esac
    AC_SUBST(HWLOC_MS_LIB_ARCH)
    
    AC_CHECK_SIZEOF([unsigned long])
    AC_DEFINE_UNQUOTED([HWLOC_SIZEOF_UNSIGNED_LONG], $ac_cv_sizeof_unsigned_long, [The size of `unsigned long', as computed by sizeof])
    AC_CHECK_SIZEOF([unsigned int])
    AC_DEFINE_UNQUOTED([HWLOC_SIZEOF_UNSIGNED_INT], $ac_cv_sizeof_unsigned_int, [The size of `unsigned int', as computed by sizeof])

    #
    # Check for compiler attributes and visibility
    #
    _HWLOC_C_COMPILER_VENDOR([hwloc_c_vendor])
    _HWLOC_CHECK_ATTRIBUTES
    _HWLOC_CHECK_VISIBILITY
    HWLOC_CFLAGS="$HWLOC_FLAGS $HWLOC_VISIBILITY_CFLAGS"
    AS_IF([test "$HWLOC_VISIBILITY_CFLAGS" != ""],
          [AC_MSG_WARN(["$HWLOC_VISIBILITY_CFLAGS" has been added to the hwloc CFLAGS])])

    # Make sure the compiler returns an error code when function arg
    # count is wrong, otherwise sched_setaffinity checks may fail.
    HWLOC_STRICT_ARGS_CFLAGS=
    hwloc_args_check=0
    AC_MSG_CHECKING([whether the C compiler rejects function calls with too many arguments])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        extern int one_arg(int x);
        int foo(void) { return one_arg(1, 2); }
      ]])],
      [AC_MSG_RESULT([no])],
      [hwloc_args_check=1
       AC_MSG_RESULT([yes])])
    AC_MSG_CHECKING([whether the C compiler rejects function calls with too few arguments])
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
        extern int two_arg(int x, int y);
        int foo(void) { return two_arg(3); }
      ]])],
      [AC_MSG_RESULT([no])],
      [hwloc_args_check=`expr $hwloc_args_check + 1`
       AC_MSG_RESULT([yes])])
    AS_IF([test "$hwloc_args_check" != "2"],[
         AC_MSG_WARN([Your C compiler does not consider incorrect argument counts to be a fatal error.])
        case "$hwloc_c_vendor" in
        ibm)
            HWLOC_STRICT_ARGS_CFLAGS="-qhalt=e"
            ;;
        intel)
            HWLOC_STRICT_ARGS_CFLAGS="-we140"
            ;;
        *)
            HWLOC_STRICT_ARGS_CFLAGS=FAIL
            AC_MSG_WARN([Please report this warning and configure using a different C compiler if possible.])
            ;;
        esac
        AS_IF([test "$HWLOC_STRICT_ARGS_CFLAGS" != "FAIL"],[
            AC_MSG_WARN([Configure will append '$HWLOC_STRICT_ARGS_CFLAGS' to the value of CFLAGS when needed.])
             AC_MSG_WARN([Alternatively you may configure with a different compiler.])
        ])
    ])

    #
    # Now detect support
    #

    AC_CHECK_HEADERS([unistd.h])
    AC_CHECK_HEADERS([dirent.h])
    AC_CHECK_HEADERS([strings.h])
    AC_CHECK_HEADERS([ctype.h])
    
    AC_CHECK_FUNCS([strncasecmp], [
      _HWLOC_CHECK_DECL([strncasecmp], [
	AC_DEFINE([HWLOC_HAVE_DECL_STRNCASECMP], [1], [Define to 1 if function `strncasecmp' is declared by system headers])
      ])
    ])

    AC_CHECK_FUNCS([strftime])
    AC_CHECK_FUNCS([setlocale])
    
    AC_CHECK_HEADER([stdint.h], [
      AC_DEFINE([HWLOC_HAVE_STDINT_H], [1], [Define to 1 if you have the <stdint.h> header file.])
    ])
    AC_CHECK_HEADERS([sys/mman.h])
    
    old_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -D_WIN32_WINNT=0x0601"
    AC_CHECK_TYPES([KAFFINITY,
                    PROCESSOR_CACHE_TYPE,
                    CACHE_DESCRIPTOR,
                    LOGICAL_PROCESSOR_RELATIONSHIP,
                    RelationProcessorPackage,
                    SYSTEM_LOGICAL_PROCESSOR_INFORMATION,
                    GROUP_AFFINITY,
                    PROCESSOR_RELATIONSHIP,
                    NUMA_NODE_RELATIONSHIP,
                    CACHE_RELATIONSHIP,
                    PROCESSOR_GROUP_INFO,
                    GROUP_RELATIONSHIP,
                    SYSTEM_LOGICAL_PROCESSOR_INFORMATION_EX,
		    PSAPI_WORKING_SET_EX_BLOCK,
		    PSAPI_WORKING_SET_EX_INFORMATION],
                    [],[],[[#include <windows.h>]])
    CPPFLAGS="$old_CPPFLAGS"
    AC_CHECK_LIB([gdi32], [main],
                 [HWLOC_LIBS="-lgdi32 $HWLOC_LIBS"
                  AC_DEFINE([HAVE_LIBGDI32], 1, [Define to 1 if we have -lgdi32])])
    
    AC_CHECK_HEADER([windows.h], [
      AC_DEFINE([HWLOC_HAVE_WINDOWS_H], [1], [Define to 1 if you have the `windows.h' header.])
    ])
    
    AC_CHECK_HEADERS([sys/lgrp_user.h], [
      AC_CHECK_LIB([lgrp], [lgrp_latency_cookie],
                   [HWLOC_LIBS="-llgrp $HWLOC_LIBS"
                    AC_DEFINE([HAVE_LIBLGRP], 1, [Define to 1 if we have -llgrp])])
    ])
    AC_CHECK_HEADERS([kstat.h], [
      AC_CHECK_LIB([kstat], [main], 
                   [HWLOC_LIBS="-lkstat $HWLOC_LIBS"
                    AC_DEFINE([HAVE_LIBKSTAT], 1, [Define to 1 if we have -lkstat])])
    ])

    AC_CHECK_DECLS([fabsf], [
      AC_CHECK_LIB([m], [fabsf],
                   [HWLOC_LIBS="-lm $HWLOC_LIBS"])
    ], [], [[#include <math.h>]])

    AC_CHECK_HEADERS([picl.h], [
      AC_CHECK_LIB([picl], [picl_initialize],
                   [HWLOC_LIBS="-lpicl $HWLOC_LIBS"])])

    AC_CHECK_DECLS([_SC_NPROCESSORS_ONLN,
    		_SC_NPROCESSORS_CONF,
    		_SC_NPROC_ONLN,
    		_SC_NPROC_CONF,
    		_SC_PAGESIZE,
    		_SC_PAGE_SIZE,
    		_SC_LARGE_PAGESIZE],,[:],[[#include <unistd.h>]])
    
    AC_HAVE_HEADERS([mach/mach_host.h])
    AC_HAVE_HEADERS([mach/mach_init.h], [
      AC_CHECK_FUNCS([host_info])
    ])

    AC_CHECK_HEADERS([sys/param.h])
    AC_CHECK_HEADERS([sys/sysctl.h], [
      AC_CHECK_DECLS([CTL_HW, HW_NCPU],,,[[
      #if HAVE_SYS_PARAM_H
      #include <sys/param.h>
      #endif
      #include <sys/sysctl.h>
      ]])
    ],,[
      AC_INCLUDES_DEFAULT
      #if HAVE_SYS_PARAM_H
      #include <sys/param.h>
      #endif
    ])

    AC_CHECK_DECLS([strtoull], [], [], [AC_INCLUDES_DEFAULT])

    # Do a full link test instead of just using AC_CHECK_FUNCS, which
    # just checks to see if the symbol exists or not.  For example,
    # the prototype of sysctl uses u_int, which on some platforms
    # (such as FreeBSD) is only defined under __BSD_VISIBLE, __USE_BSD
    # or other similar definitions.  So while the symbols "sysctl" and
    # "sysctlbyname" might still be available in libc (which autoconf
    # checks for), they might not be actually usable.
    AC_TRY_LINK([
               #include <stdio.h>
               #include <sys/types.h>
               #include <sys/sysctl.h>
               ],
                [return sysctl(NULL,0,NULL,NULL,NULL,0);],
                AC_DEFINE([HAVE_SYSCTL],[1],[Define to '1' if sysctl is present and usable]))
    AC_TRY_LINK([
               #include <stdio.h>
               #include <sys/types.h>
               #include <sys/sysctl.h>
               ],
                [return sysctlbyname(NULL,NULL,NULL,NULL,0);],
                AC_DEFINE([HAVE_SYSCTLBYNAME],[1],[Define to '1' if sysctlbyname is present and usable]))

    case ${target} in
      *-*-mingw*|*-*-cygwin*)
        hwloc_pid_t=HANDLE
        hwloc_thread_t=HANDLE
        ;;
      *)
        hwloc_pid_t=pid_t
        AC_CHECK_TYPES([pthread_t], [hwloc_thread_t=pthread_t], [:], [[#include <pthread.h>]])
        ;;
    esac
    AC_DEFINE_UNQUOTED(hwloc_pid_t, $hwloc_pid_t, [Define this to the process ID type])
    if test "x$hwloc_thread_t" != "x" ; then
      AC_DEFINE_UNQUOTED(hwloc_thread_t, $hwloc_thread_t, [Define this to the thread ID type])
    fi
    
    _HWLOC_CHECK_DECL([sched_setaffinity], [
      AC_DEFINE([HWLOC_HAVE_SCHED_SETAFFINITY], [1], [Define to 1 if glibc provides a prototype of sched_setaffinity()])
      AS_IF([test "$HWLOC_STRICT_ARGS_CFLAGS" = "FAIL"],[
        AC_MSG_WARN([Support for sched_setaffinity() requires a C compiler which])
        AC_MSG_WARN([considers incorrect argument counts to be a fatal error.])
        AC_MSG_ERROR([Cannot continue.])
      ])
      AC_MSG_CHECKING([for old prototype of sched_setaffinity])
      hwloc_save_CFLAGS=$CFLAGS
      CFLAGS="$CFLAGS $HWLOC_STRICT_ARGS_CFLAGS"
      AC_COMPILE_IFELSE([
          AC_LANG_PROGRAM([[
              #define _GNU_SOURCE
              #include <sched.h>
              static unsigned long mask;
              ]], [[ sched_setaffinity(0, (void*) &mask); ]])],
          [AC_DEFINE([HWLOC_HAVE_OLD_SCHED_SETAFFINITY], [1], [Define to 1 if glibc provides the old prototype (without length) of sched_setaffinity()])
           AC_MSG_RESULT([yes])],
          [AC_MSG_RESULT([no])])
      CFLAGS=$hwloc_save_CFLAGS
    ], , [[
#define _GNU_SOURCE
#include <sched.h>
]])
    
    AC_MSG_CHECKING([for working CPU_SET])
    AC_LINK_IFELSE([
      AC_LANG_PROGRAM([[
        #include <sched.h>
        cpu_set_t set;
        ]], [[ CPU_ZERO(&set); CPU_SET(0, &set);]])],
	[AC_DEFINE([HWLOC_HAVE_CPU_SET], [1], [Define to 1 if the CPU_SET macro works])
         AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])])
    
    AC_MSG_CHECKING([for working CPU_SET_S])
    AC_LINK_IFELSE([
      AC_LANG_PROGRAM([[
          #include <sched.h>
          cpu_set_t *set;
        ]], [[
          set = CPU_ALLOC(1024);
          CPU_ZERO_S(CPU_ALLOC_SIZE(1024), set);
          CPU_SET_S(CPU_ALLOC_SIZE(1024), 0, set);
          CPU_FREE(set);
        ]])],
        [AC_DEFINE([HWLOC_HAVE_CPU_SET_S], [1], [Define to 1 if the CPU_SET_S macro works])
         AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])])

    AC_MSG_CHECKING([for working _syscall3])
    AC_LINK_IFELSE([
      AC_LANG_PROGRAM([[
          #include <linux/unistd.h>
          #include <errno.h>
          #define __NR_hwloc_test 123
          _syscall3(int, hwloc_test, int, param1, int, param2, int, param3);
        ]], [[ hwloc_test(1, 2, 3); ]])],
        [AC_DEFINE([HWLOC_HAVE__SYSCALL3], [1], [Define to 1 if the _syscall3 macro works])
         AC_MSG_RESULT([yes])],
        [AC_MSG_RESULT([no])])

    # Check for kerrighed, but don't abort if not found.  It's illegal
    # to pass in an empty 3rd argument, but we trust the output of
    # pkg-config, so just give it a value that will always work:
    # printf.
    HWLOC_PKG_CHECK_MODULES([KERRIGHED], [kerrighed >= 2.0], [printf], [], [:])

    AC_PATH_PROGS([HWLOC_MS_LIB], [lib])
    AC_ARG_VAR([HWLOC_MS_LIB], [Path to Microsoft's Visual Studio `lib' tool])

    AC_PATH_PROG([BASH], [bash])
    
    AC_CHECK_FUNCS([ffs], [
      _HWLOC_CHECK_DECL([ffs],[
        AC_DEFINE([HWLOC_HAVE_DECL_FFS], [1], [Define to 1 if function `ffs' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FFS], [1], [Define to 1 if you have the `ffs' function.])
      if ( $CC --version | grep gccfss ) >/dev/null 2>&1 ; then
        dnl May be broken due to
        dnl    https://forums.oracle.com/forums/thread.jspa?threadID=1997328
        dnl TODO: a more selective test, since bug may be version dependent.
        dnl We can't use AC_TRY_LINK because the failure does not appear until
        dnl run/load time and there is currently no precedent for AC_TRY_RUN
        dnl use in hwloc.  --PHH
        dnl For now, we're going with "all gccfss compilers are broken". 
        dnl Better to be safe and correct; it's not like this is
        dnl performance-critical code, after all.
        AC_DEFINE([HWLOC_HAVE_BROKEN_FFS], [1], 
                  [Define to 1 if your `ffs' function is known to be broken.])
      fi
    ])
    AC_CHECK_FUNCS([ffsl], [
      _HWLOC_CHECK_DECL([ffsl],[
        AC_DEFINE([HWLOC_HAVE_DECL_FFSL], [1], [Define to 1 if function `ffsl' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FFSL], [1], [Define to 1 if you have the `ffsl' function.])
    ])
    
    AC_CHECK_FUNCS([fls], [
      _HWLOC_CHECK_DECL([fls],[
        AC_DEFINE([HWLOC_HAVE_DECL_FLS], [1], [Define to 1 if function `fls' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FLS], [1], [Define to 1 if you have the `fls' function.])
    ])
    AC_CHECK_FUNCS([flsl], [
      _HWLOC_CHECK_DECL([flsl],[
        AC_DEFINE([HWLOC_HAVE_DECL_FLSL], [1], [Define to 1 if function `flsl' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_FLSL], [1], [Define to 1 if you have the `flsl' function.])
    ])
    
    AC_CHECK_FUNCS([clz], [
      _HWLOC_CHECK_DECL([clz],[
        AC_DEFINE([HWLOC_HAVE_DECL_CLZ], [1], [Define to 1 if function `clz' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_CLZ], [1], [Define to 1 if you have the `clz' function.])
    ])
    AC_CHECK_FUNCS([clzl], [
      _HWLOC_CHECK_DECL([clzl],[
        AC_DEFINE([HWLOC_HAVE_DECL_CLZL], [1], [Define to 1 if function `clzl' is declared by system headers])
      ])
      AC_DEFINE([HWLOC_HAVE_CLZL], [1], [Define to 1 if you have the `clzl' function.])
    ])
    
    AC_CHECK_FUNCS([openat], [hwloc_have_openat=yes])

    AC_CHECK_HEADERS([malloc.h])
    AC_CHECK_FUNCS([getpagesize memalign posix_memalign])

    AC_CHECK_HEADERS([sys/utsname.h])
    AC_CHECK_FUNCS([uname])

    AC_CHECK_HEADERS([pthread_np.h])
    AC_CHECK_DECLS([pthread_setaffinity_np],,[:],[[
      #include <pthread.h>
      #ifdef HAVE_PTHREAD_NP_H
      #  include <pthread_np.h>
      #endif
    ]])
    AC_CHECK_DECLS([pthread_getaffinity_np],,[:],[[
      #include <pthread.h>
      #ifdef HAVE_PTHREAD_NP_H
      #  include <pthread_np.h>
      #endif
    ]])
    AC_CHECK_FUNC([sched_setaffinity], [hwloc_have_sched_setaffinity=yes])
    AC_CHECK_HEADERS([sys/cpuset.h],,,[[#include <sys/param.h>]])
    AC_CHECK_FUNCS([cpuset_setaffinity])
    AC_SEARCH_LIBS([pthread_getthrds_np], [pthread],
      AC_DEFINE([HWLOC_HAVE_PTHREAD_GETTHRDS_NP], 1, `Define to 1 if you have pthread_getthrds_np')
    )
    AC_CHECK_FUNCS([cpuset_setid])

    # Linux libnuma support
    hwloc_linux_libnuma_happy=no
    if test "x$enable_libnuma" != "xno"; then
        hwloc_linux_libnuma_happy=yes
        AC_CHECK_HEADERS([numaif.h], [
            AC_CHECK_LIB([numa], [numa_available], [HWLOC_LINUX_LIBNUMA_LIBS="-lnuma"], [hwloc_linux_libnuma_happy=no])
        ], [hwloc_linux_libnuma_happy=no])
    fi
    AC_SUBST(HWLOC_LINUX_LIBNUMA_LIBS)
    # If we asked for Linux libnuma support but couldn't deliver, fail
    HWLOC_LIBS="$HWLOC_LIBS $HWLOC_LINUX_LIBNUMA_LIBS"
    AS_IF([test "$enable_libnuma" = "yes" -a "$hwloc_linux_libnuma_happy" = "no"],
          [AC_MSG_WARN([Specified --enable-libnuma switch, but could not])
           AC_MSG_WARN([find appropriate support])
           AC_MSG_ERROR([Cannot continue])])
    if test "x$hwloc_linux_libnuma_happy" = "xyes"; then
      tmp_save_LIBS="$LIBS"
      LIBS="$LIBS $HWLOC_LINUX_LIBNUMA_LIBS"

      AC_CHECK_LIB([numa], [set_mempolicy], [
	enable_set_mempolicy=yes
	AC_DEFINE([HWLOC_HAVE_SET_MEMPOLICY], [1], [Define to 1 if set_mempolicy is available.])
      ])
      AC_CHECK_LIB([numa], [mbind], [
	enable_mbind=yes
	AC_DEFINE([HWLOC_HAVE_MBIND], [1], [Define to 1 if mbind is available.])
      ])
      AC_CHECK_LIB([numa], [migrate_pages], [
	enable_migrate_pages=yes
	AC_DEFINE([HWLOC_HAVE_MIGRATE_PAGES], [1], [Define to 1 if migrate_pages is available.])
      ])

      LIBS="$tmp_save_LIBS"
    fi

    # PCI support
    hwloc_pci_happy=no
    if test "x$enable_pci" != xno -a "x$enable_libpci" != "xyes"; then
      hwloc_pci_happy=yes
      HWLOC_PKG_CHECK_MODULES([PCIACCESS], [pciaccess], [pci_slot_match_iterator_create], [:], [hwloc_pci_happy=no])
      if test x$hwloc_pci_happy = xyes; then hwloc_pci_lib=pciaccess; fi
    fi
    # PCI support with pciutils instead of pciaccess
    if test "x$enable_pci" != "xno" -a "x$hwloc_pci_lib" != "xpciaccess"; then
        hwloc_pci_happy=yes
        HWLOC_PKG_CHECK_MODULES([PCIUTILS], [libpci], [pci_cleanup], [:], [
          # manually check pciutils in case a old one without .pc is installed
          AC_CHECK_HEADERS([pci/pci.h], [
	    # try first without -lz, it's not always needed (RHEL5, Debian Etch)
	    AC_CHECK_LIB([pci], [pci_init], [
	      HWLOC_PCIUTILS_LIBS="-lpci"
	      ], [
              # try again with -lz because it's needed sometimes (FC7).
              # don't use AC_CHECK_LIB again because the cache would
              # return "no" without actually rechecking
              AC_MSG_CHECKING([for pci_init in -lpci with -lz])
              tmp_save_LIBS=$LIBS
              LIBS="-lpci -lz $LIBS"
              AC_LINK_IFELSE([AC_LANG_CALL([], [pci_init])],
                             [HWLOC_PCIUTILS_LIBS="-lpci -lz"
                              HWLOC_PCIUTILS_ADDITIONAL_LIBS="-lz"
                              AC_MSG_RESULT(yes)],
                             [hwloc_pci_happy=no
                              AC_MSG_RESULT(no)])
              LIBS=$tmp_save_LIBS])
            # Also check with pci_lookup_name, because that sometimes
            # requires -lresolv (RHEL5.6). don't use AC_CHECK_LIB twice
            # because the cache would return "no" without actually rechecking
	    AC_CHECK_LIB([pci], [pci_lookup_name], [],
                [AC_CHECK_LIB([resolv], [inet_ntoa], 
                    [AC_MSG_CHECKING([for pci_lookup_name in -lpci with -lresolv])
                     tmp_save_LIBS=$LIBS
                     LIBS="-lpci -lresolv $LIBS $HWLOC_PCIUTILS_ADDITIONAL_LIBS"
                     AC_LINK_IFELSE([AC_LANG_CALL([], [pci_lookup_name])],
                                    [HWLOC_PCIUTILS_LIBS="$HWLOC_PCIUTILS_LIBS -lresolv"
                                     HWLOC_PCIUTILS_ADDITIONAL_LIBS="$HWLOC_PCIUTILS_ADDITIONAL_LIBS -lresolv"
                                     AC_MSG_RESULT(yes)],
                                    [hwloc_pci_happy=no
                                     AC_MSG_RESULT(no)])
                     LIBS=$tmp_save_LIBS],
                    [hwloc_pci_happy=no])])
            ], [hwloc_pci_happy=no])
        ])
        if test x$hwloc_pci_happy = xyes; then
	  # pciutils could be used, but we don't want to force use it since it may GPL-taint hwloc
	  if test x$enable_libpci = xyes; then
	    hwloc_pci_lib=pciutils
	  else
	    # user didn't explicit request pciutils, disable PCI and warn the user
	    hwloc_pci_happy=no
	    hwloc_warn_may_use_libpci=yes
	  fi
	else
	  # pciutils not found, error out if it was requested
	  if test x$enable_libpci = xyes; then
	    AC_MSG_WARN([Specified --enable-libpci switch, but could not])
	    AC_MSG_WARN([find appropriate support])
	    AC_MSG_ERROR([Cannot continue])
	  fi
	fi
    fi
    AC_SUBST(HWLOC_PCIUTILS_LIBS)
    # If we asked for pci support but couldn't deliver, fail
    AS_IF([test "$enable_pci" = "yes" -a "$hwloc_pci_happy" = "no" -a "$hwloc_warn_may_use_libpci" != "yes"],
          [AC_MSG_WARN([Specified --enable-pci switch, but could not])
           AC_MSG_WARN([find appropriate support])
           AC_MSG_ERROR([Cannot continue])])
    # pciaccess specific enabling
    if test "x$hwloc_pci_lib" = "xpciaccess"; then
      HWLOC_PCIACCESS_REQUIRES=pciaccess
      AC_DEFINE([HWLOC_HAVE_LIBPCIACCESS], [1], [Define to 1 if you have the `libpciaccess' library.])
    fi
    # pciutils specific checks and enabling
    if test "x$hwloc_pci_lib" = "xpciutils"; then
      tmp_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS $HWLOC_PCIUTILS_CFLAGS"
      tmp_save_LIBS="$LIBS"
      LIBS="$LIBS $HWLOC_PCIUTILS_LIBS"

      AC_CHECK_DECLS([PCI_LOOKUP_NO_NUMBERS],,[:],[[#include <pci/pci.h>]])
      AC_CHECK_DECLS([PCI_LOOKUP_NO_NUMBERS],,[:],[[#include <pci/pci.h>]])
      AC_CHECK_LIB([pci], [pci_find_cap], [enable_pci_caps=yes], [enable_pci_caps=no], [$HWLOC_PCIUTILS_ADDITIONAL_LIBS])
      if test "x$enable_pci_caps" = "xyes"; then
        AC_DEFINE([HWLOC_HAVE_PCI_FIND_CAP], [1], [Define to 1 if `libpci' has the `pci_find_cap' function.])
      fi

      AC_MSG_CHECKING(whether struct pci_dev has a device_class field)
      AC_TRY_COMPILE([#include <pci/pci.h>],
	[int f(struct pci_dev *dev) { return dev->device_class; }],
        [pcidev_device_class=yes], [pcidev_device_class=no])
      AC_MSG_RESULT([$pcidev_device_class])
      if test x$pcidev_device_class = xyes; then
        AC_DEFINE([HWLOC_HAVE_PCIDEV_DEVICE_CLASS], [1], [Define to 1 if `libpci' struct pci_dev has a `device_class' field.])
      fi

      AC_MSG_CHECKING(whether struct pci_dev has a domain field)
      AC_TRY_COMPILE([#include <pci/pci.h>],
	[int f(struct pci_dev *dev) { return dev->domain; }],
        [pcidev_domain=yes], [pcidev_domain=no])
      AC_MSG_RESULT([$pcidev_domain])
      if test x$pcidev_domain = xyes; then
        AC_DEFINE([HWLOC_HAVE_PCIDEV_DOMAIN], [1], [Define to 1 if `libpci' struct pci_dev has a `domain' field.])
      fi

      CFLAGS="$tmp_save_CFLAGS"
      LIBS="$tmp_save_LIBS"

      HWLOC_PCIUTILS_REQUIRES=libpci
      AC_DEFINE([HWLOC_HAVE_PCIUTILS], [1], [Define to 1 if you have the pciutils `libpci' library.])
    fi
    # final common PCI enabling
    if test "x$hwloc_pci_happy" = "xyes"; then
      hwloc_components="$hwloc_components pci"
      hwloc_pci_component_maybeplugin=1
    fi
    # don't add LIBS/CFLAGS/REQUIRES yet, depends on plugins

    # OpenCL support
    hwloc_opencl_happy=no
    if test "x$enable_opencl" != "xno"; then
        hwloc_opencl_happy=yes
        AC_CHECK_HEADERS([CL/cl_ext.h], [
	  AC_CHECK_LIB([OpenCL], [clGetDeviceIDs], [HWLOC_OPENCL_LIBS="-lOpenCL"], [hwloc_opencl_happy=no])
        ], [hwloc_opencl_happy=no])
    fi
    AC_SUBST(HWLOC_OPENCL_LIBS)
    # Check if required extensions are available
    if test "x$hwloc_opencl_happy" = "xyes"; then
      tmp_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS $HWLOC_OPENCL_CFLAGS"
      tmp_save_LIBS="$LIBS"
      LIBS="$LIBS $HWLOC_OPENCL_LIBS"
      AC_CHECK_DECLS([CL_DEVICE_TOPOLOGY_AMD],[hwloc_opencl_amd_happy=yes],[:],[[#include <CL/cl_ext.h>]])
      CFLAGS="$tmp_save_CFLAGS"
      LIBS="$tmp_save_LIBS"
      # We can't do anything without CL_DEVICE_TOPOLOGY_AMD so far, so disable OpenCL entirely if not found
      test "x$hwloc_opencl_amd_happy" != "xyes" && hwloc_opencl_happy=no
    fi
    # If we asked for opencl support but couldn't deliver, fail
    AS_IF([test "$enable_opencl" = "yes" -a "$hwloc_opencl_happy" = "no"],
          [AC_MSG_WARN([Specified --enable-opencl switch, but could not])
           AC_MSG_WARN([find appropriate support])
           AC_MSG_ERROR([Cannot continue])])
    if test "x$hwloc_opencl_happy" = "xyes"; then
      AC_DEFINE([HWLOC_HAVE_OPENCL], [1], [Define to 1 if you have the `OpenCL' library.])
      AC_SUBST([HWLOC_HAVE_OPENCL], [1])
      hwloc_components="$hwloc_components opencl"
      hwloc_opencl_component_maybeplugin=1
    else
      AC_SUBST([HWLOC_HAVE_OPENCL], [0])
    fi
    # don't add LIBS/CFLAGS/REQUIRES yet, depends on plugins

    # CUDA support
    hwloc_have_cuda=no
    hwloc_have_cudart=no
    if test "x$enable_cuda" != "xno"; then
      AC_CHECK_HEADERS([cuda.h], [
        AC_MSG_CHECKING(if CUDA_VERSION >= 3020)
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <cuda.h>
#ifndef CUDA_VERSION
#error CUDA_VERSION undefined
#elif CUDA_VERSION < 3020
#error CUDA_VERSION too old
#endif]], [[int i = 3;]])],
         [AC_MSG_RESULT(yes)
          AC_CHECK_LIB([cuda], [cuInit],
                       [AC_DEFINE([HAVE_CUDA], 1, [Define to 1 if we have -lcuda])
                        hwloc_have_cuda=yes])],
         [AC_MSG_RESULT(no)])])

      AC_CHECK_HEADERS([cuda_runtime_api.h], [
        AC_MSG_CHECKING(if CUDART_VERSION >= 3020)
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([[
#include <cuda_runtime_api.h>
#ifndef CUDART_VERSION
#error CUDART_VERSION undefined
#elif CUDART_VERSION < 3020
#error CUDART_VERSION too old
#endif]], [[int i = 3;]])],
         [AC_MSG_RESULT(yes)
          AC_CHECK_LIB([cudart], [cudaGetDeviceProperties], [
            HWLOC_CUDA_LIBS="-lcudart"
            AC_SUBST(HWLOC_CUDA_LIBS)
            hwloc_have_cudart=yes
            AC_DEFINE([HWLOC_HAVE_CUDART], [1], [Define to 1 if you have the `cudart' SDK.])
          ])
        ])
      ])

      AS_IF([test "$enable_cuda" = "yes" -a "$hwloc_have_cudart" = "no"],
            [AC_MSG_WARN([Specified --enable-cuda switch, but could not])
             AC_MSG_WARN([find appropriate support])
             AC_MSG_ERROR([Cannot continue])])

      if test "x$hwloc_have_cudart" = "xyes"; then
	hwloc_components="$hwloc_components cuda"
        hwloc_cuda_component_maybeplugin=1
      fi
    fi
    # don't add LIBS/CFLAGS yet, depends on plugins

    # NVML support
    hwloc_nvml_happy=no
    if test "x$enable_nvml" != "xno"; then
	hwloc_nvml_happy=yes
	AC_CHECK_HEADERS([nvml.h], [
	  AC_CHECK_LIB([nvidia-ml], [nvmlInit], [HWLOC_NVML_LIBS="-lnvidia-ml"], [hwloc_nvml_happy=no])
        ], [hwloc_nvml_happy=no])
    fi
    if test "x$hwloc_nvml_happy" = "xyes"; then
      tmp_save_CFLAGS="$CFLAGS"
      CFLAGS="$CFLAGS $HWLOC_NVML_CFLAGS"
      tmp_save_LIBS="$LIBS"
      LIBS="$LIBS $HWLOC_NVML_LIBS"
      AC_CHECK_DECLS([nvmlDeviceGetMaxPcieLinkGeneration],,[:],[[#include <nvml.h>]])
      CFLAGS="$tmp_save_CFLAGS"
      LIBS="$tmp_save_LIBS"
    fi
    AC_SUBST(HWLOC_NVML_LIBS)
    # If we asked for nvml support but couldn't deliver, fail
    AS_IF([test "$enable_nvml" = "yes" -a "$hwloc_nvml_happy" = "no"],
	  [AC_MSG_WARN([Specified --enable-nvml switch, but could not])
	   AC_MSG_WARN([find appropriate support])
	   AC_MSG_ERROR([Cannot continue])])
    if test "x$hwloc_nvml_happy" = "xyes"; then
      AC_DEFINE([HWLOC_HAVE_NVML], [1], [Define to 1 if you have the `NVML' library.])
      AC_SUBST([HWLOC_HAVE_NVML], [1])
      hwloc_components="$hwloc_components nvml"
      hwloc_nvml_component_maybeplugin=1
    else
      AC_SUBST([HWLOC_HAVE_NVML], [0])
    fi
    # don't add LIBS/CFLAGS/REQUIRES yet, depends on plugins

    # X11 support
    AC_PATH_XTRA

    CPPFLAGS_save=$CPPFLAGS
    LIBS_save=$LIBS

    CPPFLAGS="$CPPFLAGS $X_CFLAGS"
    LIBS="$LIBS $X_PRE_LIBS $X_LIBS $X_EXTRA_LIBS"
    AC_CHECK_HEADERS([X11/Xlib.h],
        [AC_CHECK_LIB([X11], [XOpenDisplay],
            [
             # the GL backend just needs XOpenDisplay
             hwloc_enable_X11=yes
             # lstopo needs more
             AC_CHECK_HEADERS([X11/Xutil.h],
                [AC_CHECK_HEADERS([X11/keysym.h],
                    [AC_DEFINE([HWLOC_HAVE_X11_KEYSYM], [1], [Define to 1 if X11 headers including Xutil.h and keysym.h are available.])])
                     AC_SUBST([HWLOC_X11_LIBS], ["-lX11"])
                ], [], [#include <X11/Xlib.h>])
            ])
         ])
    CPPFLAGS=$CPPFLAGS_save
    LIBS=$LIBS_save

    # GL Support 
    hwloc_gl_happy=no
    if test "x$enable_gl" != "xno"; then
	hwloc_gl_happy=yes

	AS_IF([test "$hwloc_enable_X11" != "yes"],
              [AC_MSG_WARN([X11 not found; GL disabled])
               hwloc_gl_happy=no])

        AC_CHECK_HEADERS([NVCtrl/NVCtrl.h], [
          AC_CHECK_LIB([XNVCtrl], [XNVCTRLQueryTargetAttribute], [:], [hwloc_gl_happy=no], [-lXext])
        ], [hwloc_gl_happy=no])

        if test "x$hwloc_gl_happy" = "xyes"; then
            AC_DEFINE([HWLOC_HAVE_GL], [1], [Define to 1 if you have the GL module components.])
	    HWLOC_GL_LIBS="-lXNVCtrl -lXext -lX11"
	    AC_SUBST(HWLOC_GL_LIBS)
	    HWLOC_GL_REQUIRES="xext x11"
            hwloc_have_gl=yes
	    hwloc_components="$hwloc_components gl"
	    hwloc_gl_component_maybeplugin=1
	else
            AS_IF([test "$enable_gl" = "yes"], [
                AC_MSG_WARN([Specified --enable-gl switch, but could not])
                AC_MSG_WARN([find appropriate support])
                AC_MSG_ERROR([Cannot continue])
            ])
 	fi      
    fi
    # don't add LIBS/CFLAGS yet, depends on plugins
    
    # libxml2 support
    hwloc_libxml2_happy=
    if test "x$enable_libxml2" != "xno"; then
        HWLOC_PKG_CHECK_MODULES([LIBXML2], [libxml-2.0], [xmlNewDoc], 
                                [hwloc_libxml2_happy=yes], 
                                [hwloc_libxml2_happy=no])
    fi
    if test "x$hwloc_libxml2_happy" = "xyes"; then
        HWLOC_LIBXML2_REQUIRES="libxml-2.0"
        AC_DEFINE([HWLOC_HAVE_LIBXML2], [1], [Define to 1 if you have the `libxml2' library.])
        AC_SUBST([HWLOC_HAVE_LIBXML2], [1])

        hwloc_components="$hwloc_components xml_libxml"
        hwloc_xml_libxml_component_maybeplugin=1
    else
        AC_SUBST([HWLOC_HAVE_LIBXML2], [0])
	AS_IF([test "$enable_libxml2" = "yes"],
              [AC_MSG_WARN([--enable-libxml2 requested, but libxml2 was not found])
               AC_MSG_ERROR([Cannot continue])])
    fi
    # don't add LIBS/CFLAGS/REQUIRES yet, depends on plugins

    # Try to compile the x86 cpuid inlines
    AC_MSG_CHECKING([for x86 cpuid])
    old_CPPFLAGS="$CPPFLAGS"
    CPPFLAGS="$CPPFLAGS -I$HWLOC_top_srcdir/include"
    # We need hwloc_uint64_t but we can't use hwloc/autogen/config.h before configure ends.
    # So pass #include/#define manually here for now.
    CPUID_CHECK_HEADERS=
    CPUID_CHECK_DEFINE=
    if test "x$hwloc_windows" = xyes; then
      X86_CPUID_CHECK_HEADERS="#include <windows.h>"
      X86_CPUID_CHECK_DEFINE="#define hwloc_uint64_t DWORDLONG"
    else
      X86_CPUID_CHECK_DEFINE="#define hwloc_uint64_t uint64_t"
      if test "x$ac_cv_header_stdint_h" = xyes; then
        X86_CPUID_CHECK_HEADERS="#include <stdint.h>"
      fi
    fi
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        #include <stdio.h>
        $X86_CPUID_CHECK_HEADERS
        $X86_CPUID_CHECK_DEFINE
        #define __hwloc_inline
        #include <private/cpuid-x86.h>
      ]], [[
        if (hwloc_have_x86_cpuid()) {
          unsigned eax = 0, ebx, ecx = 0, edx;
          hwloc_x86_cpuid(&eax, &ebx, &ecx, &edx);
          printf("highest x86 cpuid %x\n", eax);
          return 0;
        }
      ]])],
      [AC_MSG_RESULT([yes])
       AC_DEFINE(HWLOC_HAVE_X86_CPUID, 1, [Define to 1 if you have x86 cpuid])
       hwloc_have_x86_cpuid=yes],
      [AC_MSG_RESULT([no])])
    if test "x$hwloc_have_x86_cpuid" = xyes; then
      hwloc_components="$hwloc_components x86"
    fi
    CPPFLAGS="$old_CPPFLAGS"

    # Components require pthread_mutex, see if it needs -lpthread
    hwloc_pthread_mutex_happy=no
    # Try without explicit -lpthread first
    AC_CHECK_FUNC([pthread_mutex_lock],
      [hwloc_pthread_mutex_happy=yes
       HWLOC_LIBS_PRIVATE="$HWLOC_LIBS_PRIVATE -lpthread"
      ],
      [AC_MSG_CHECKING([for pthread_mutex_lock with -lpthread])
       # Try again with explicit -lpthread, but don't use AC_CHECK_FUNC to avoid the cache
       tmp_save_LIBS=$LIBS
       LIBS="$LIBS -lpthread"
       AC_LINK_IFELSE([AC_LANG_CALL([], [pthread_mutex_lock])],
         [hwloc_pthread_mutex_happy=yes
          HWLOC_LIBS="$HWLOC_LIBS -lpthread"
         ])
       AC_MSG_RESULT([$hwloc_pthread_mutex_happy])
       LIBS="$tmp_save_LIBS"
      ])
    AS_IF([test "x$hwloc_pthread_mutex_happy" = "xyes"],
      [AC_DEFINE([HWLOC_HAVE_PTHREAD_MUTEX], 1, [Define to 1 if pthread mutexes are available])])

    AS_IF([test "x$hwloc_pthread_mutex_happy" != xyes -a "x$hwloc_windows" != xyes],
      [AC_MSG_WARN([pthread_mutex_lock not available, required for thread-safe initialization on non-Windows platforms.])
       AC_MSG_WARN([Please report this to the hwloc-devel mailing list.])
       AC_MSG_ERROR([Cannot continue])])

    #
    # Now enable registration of listed components
    #

    # Plugin support
    AC_MSG_CHECKING([if plugin support is enabled])
    # Plugins (even core support) are totally disabled by default
    AS_IF([test "x$enable_plugins" = "x"], [enable_plugins=no])
    AS_IF([test "x$enable_plugins" != "xno"], [hwloc_have_plugins=yes], [hwloc_have_plugins=no])
    AC_MSG_RESULT([$hwloc_have_plugins])
    AS_IF([test "x$hwloc_have_plugins" = "xyes"],
          [AC_DEFINE([HWLOC_HAVE_PLUGINS], 1, [Define to 1 if the hwloc library should support dynamically-loaded plugins])])

    # Some sanity checks about plugins
    # libltdl doesn't work on AIX as of 2.4.2
    AS_IF([test "x$enable_plugins" = "xyes" -a "x$hwloc_aix" = "xyes"],
      [AC_MSG_WARN([libltdl does not work on AIX, plugins support cannot be enabled.])
       AC_MSG_ERROR([Cannot continue])])
    # posix linkers don't work well with plugins and windows dll constraints
    AS_IF([test "x$enable_plugins" = "xyes" -a "x$hwloc_windows" = "xyes"],
      [AC_MSG_WARN([Plugins not supported on non-native Windows build, plugins support cannot be enabled.])
       AC_MSG_ERROR([Cannot continue])])

    # If we want plugins, look for ltdl.h and libltdl
    if test "x$hwloc_have_plugins" = xyes; then
      AC_CHECK_HEADER([ltdl.h], [],
	[AC_MSG_WARN([Plugin support requested, but could not find ltdl.h])
	 AC_MSG_ERROR([Cannot continue])])
      AC_CHECK_LIB([ltdl], [lt_dlopenext],
	[HWLOC_LIBS="$HWLOC_LIBS -lltdl"],
	[AC_MSG_WARN([Plugin support requested, but could not find libltdl])
	 AC_MSG_ERROR([Cannot continue])])
      # Add libltdl static-build dependencies to hwloc.pc
      HWLOC_CHECK_LTDL_DEPS
    fi

    AC_ARG_WITH([hwloc-plugins-path],
		AC_HELP_STRING([--with-hwloc-plugins-path=dir:...],
                               [Colon-separated list of plugin directories. Default: "$prefix/lib/hwloc". Plugins will be installed in the first directory. They will be loaded from all of them, in order.]),
		[HWLOC_PLUGINS_PATH="$with_hwloc_plugins_path"],
		[HWLOC_PLUGINS_PATH="\$(libdir)/hwloc"])
    AC_SUBST(HWLOC_PLUGINS_PATH)
    HWLOC_PLUGINS_DIR=`echo "$HWLOC_PLUGINS_PATH" | cut -d: -f1`
    AC_SUBST(HWLOC_PLUGINS_DIR)

    # Static components output file
    hwloc_static_components_dir=${HWLOC_top_builddir}/src
    mkdir -p ${hwloc_static_components_dir}
    hwloc_static_components_file=${hwloc_static_components_dir}/static-components.h
    rm -f ${hwloc_static_components_file}

    # Make $enable_plugins easier to use (it contains either "yes" (all) or a list of <name>)
    HWLOC_PREPARE_FILTER_COMPONENTS([$enable_plugins])
    # Now we have some hwloc_<name>_component_wantplugin=1

    # See which core components want plugin and support it
    HWLOC_FILTER_COMPONENTS
    # Now we have some hwloc_<name>_component=plugin/static
    # and hwloc_static/plugin_components
    AC_MSG_CHECKING([components to build statically])
    AC_MSG_RESULT($hwloc_static_components)
    HWLOC_LIST_STATIC_COMPONENTS([$hwloc_static_components_file], [$hwloc_static_components])
    AC_MSG_CHECKING([components to build as plugins])
    AC_MSG_RESULT([$hwloc_plugin_components])

    AS_IF([test "$hwloc_pci_component" = "static"],
          [HWLOC_LIBS="$HWLOC_LIBS $HWLOC_PCIUTILS_LIBS $HWLOC_PCIACCESS_LIBS"
           HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_PCIUTILS_CFLAGS $HWLOC_PCIACCESS_CFLAGS"
           HWLOC_REQUIRES="$HWLOC_PCIUTILS_REQUIRES $HWLOC_PCIACCESS_REQUIRES $HWLOC_REQUIRES"])
    AS_IF([test "$hwloc_opencl_component" = "static"],
          [HWLOC_LIBS="$HWLOC_LIBS $HWLOC_OPENCL_LIBS"
           HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_OPENCL_CFLAGS"
           HWLOC_REQUIRES="$HWLOC_OPENCL_REQUIRES $HWLOC_REQUIRES"])
    AS_IF([test "$hwloc_cuda_component" = "static"],
          [HWLOC_LIBS="$HWLOC_LIBS $HWLOC_CUDA_LIBS"
           HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_CUDA_CFLAGS"
           HWLOC_REQUIRES="$HWLOC_CUDA_REQUIRES $HWLOC_REQUIRES"])
    AS_IF([test "$hwloc_nvml_component" = "static"],
          [HWLOC_LIBS="$HWLOC_LIBS $HWLOC_NVML_LIBS"
           HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_NVML_CFLAGS"
           HWLOC_REQUIRES="$HWLOC_NVML_REQUIRES $HWLOC_REQUIRES"])
    AS_IF([test "$hwloc_gl_component" = "static"],
          [HWLOC_LIBS="$HWLOC_LIBS $HWLOC_GL_LIBS"
           HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_GL_CFLAGS"
           HWLOC_REQUIRES="$HWLOC_GL_REQUIRES $HWLOC_REQUIRES"])
    AS_IF([test "$hwloc_xml_libxml_component" = "static"],
          [HWLOC_LIBS="$HWLOC_LIBS $HWLOC_LIBXML2_LIBS"
           HWLOC_CFLAGS="$HWLOC_CFLAGS $HWLOC_LIBXML2_CFLAGS"
           HWLOC_REQUIRES="$HWLOC_LIBXML2_REQUIRES $HWLOC_REQUIRES"])

    #
    # Setup HWLOC's C, CPP, and LD flags, and LIBS
    #
    AC_SUBST(HWLOC_REQUIRES)
    AC_SUBST(HWLOC_CFLAGS)
    HWLOC_CPPFLAGS='-I$(HWLOC_top_builddir)/include -I$(HWLOC_top_srcdir)/include'
    AC_SUBST(HWLOC_CPPFLAGS)
    AC_SUBST(HWLOC_LDFLAGS)
    AC_SUBST(HWLOC_LIBS)
    AC_SUBST(HWLOC_LIBS_PRIVATE)

    # Set these values explicitly for embedded builds.  Exporting
    # these values through *_EMBEDDED_* values gives us the freedom to
    # do something different someday if we ever need to.  There's no
    # need to fill these values in unless we're in embedded mode.
    # Indeed, if we're building in embedded mode, we want HWLOC_LIBS
    # to be empty so that nothing is linked into libhwloc_embedded.la
    # itself -- only the upper-layer will link in anything required.

    AS_IF([test "$hwloc_mode" = "embedded"],
          [HWLOC_EMBEDDED_CFLAGS=$HWLOC_CFLAGS
           HWLOC_EMBEDDED_CPPFLAGS=$HWLOC_CPPFLAGS
           HWLOC_EMBEDDED_LDADD='$(HWLOC_top_builddir)/src/libhwloc_embedded.la'
           HWLOC_EMBEDDED_LIBS=$HWLOC_LIBS
           HWLOC_LIBS=])
    AC_SUBST(HWLOC_EMBEDDED_CFLAGS)
    AC_SUBST(HWLOC_EMBEDDED_CPPFLAGS)
    AC_SUBST(HWLOC_EMBEDDED_LDADD)
    AC_SUBST(HWLOC_EMBEDDED_LIBS)

    # Always generate these files
    AC_CONFIG_FILES(
        hwloc_config_prefix[Makefile]
        hwloc_config_prefix[include/Makefile]
        hwloc_config_prefix[src/Makefile ]
    )

    # Cleanup
    AC_LANG_POP

    # Success
    $2
])dnl

#-----------------------------------------------------------------------

# Specify the symbol prefix
AC_DEFUN([HWLOC_SET_SYMBOL_PREFIX],[
    hwloc_symbol_prefix_value=$1
])dnl

#-----------------------------------------------------------------------

# This must be a standalone routine so that it can be called both by
# HWLOC_INIT and an external caller (if HWLOC_INIT is not invoked).
AC_DEFUN([HWLOC_DO_AM_CONDITIONALS],[
    AS_IF([test "$hwloc_did_am_conditionals" != "yes"],[
        AM_CONDITIONAL([HWLOC_BUILD_STANDALONE], [test "$hwloc_mode" = "standalone"])

        AM_CONDITIONAL([HWLOC_HAVE_GCC], [test "x$GCC" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_MS_LIB], [test "x$HWLOC_MS_LIB" != "x"])
        AM_CONDITIONAL([HWLOC_HAVE_OPENAT], [test "x$hwloc_have_openat" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_LINUX_LIBNUMA],
                       [test "x$hwloc_have_linux_libnuma" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_SCHED_SETAFFINITY],
                       [test "x$hwloc_have_sched_setaffinity" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_PTHREAD],
                       [test "x$hwloc_have_pthread" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_LIBIBVERBS], 
                       [test "x$hwloc_have_libibverbs" = "xyes"])
	AM_CONDITIONAL([HWLOC_HAVE_CUDA],
		       [test "x$hwloc_have_cuda" = "xyes"])
	AM_CONDITIONAL([HWLOC_HAVE_GL],
		       [test "x$hwloc_have_gl" = "xyes"])
	AM_CONDITIONAL([HWLOC_HAVE_MYRIEXPRESS],
		       [test "x$hwloc_have_myriexpress" = "xyes"])
	AM_CONDITIONAL([HWLOC_HAVE_CUDART],
		       [test "x$hwloc_have_cudart" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_LIBXML2], [test "$hwloc_libxml2_happy" = "yes"])
        AM_CONDITIONAL([HWLOC_HAVE_CAIRO], [test "$hwloc_cairo_happy" = "yes"])
        AM_CONDITIONAL([HWLOC_HAVE_PCI], [test "$hwloc_pci_happy" = "yes"])
        AM_CONDITIONAL([HWLOC_HAVE_OPENCL], [test "$hwloc_opencl_happy" = "yes"])
        AM_CONDITIONAL([HWLOC_HAVE_NVML], [test "$hwloc_nvml_happy" = "yes"])
        AM_CONDITIONAL([HWLOC_HAVE_SET_MEMPOLICY], [test "x$enable_set_mempolicy" != "xno"])
        AM_CONDITIONAL([HWLOC_HAVE_MBIND], [test "x$enable_mbind" != "xno"])
        AM_CONDITIONAL([HWLOC_HAVE_BUNZIPP], [test "x$BUNZIPP" != "xfalse"])

        AM_CONDITIONAL([HWLOC_BUILD_DOXYGEN],
                       [test "x$hwloc_generate_doxs" = "xyes"])
        AM_CONDITIONAL([HWLOC_BUILD_README], 
                       [test "x$hwloc_generate_readme" = "xyes" -a \( "x$hwloc_install_doxs" = "xyes" -o "x$hwloc_generate_doxs" = "xyes" \) ])
        AM_CONDITIONAL([HWLOC_INSTALL_DOXYGEN], 
                       [test "x$hwloc_install_doxs" = "xyes"])

        AM_CONDITIONAL([HWLOC_HAVE_LINUX], [test "x$hwloc_linux" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_BGQ], [test "x$hwloc_bgq" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_IRIX], [test "x$hwloc_irix" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_DARWIN], [test "x$hwloc_darwin" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_FREEBSD], [test "x$hwloc_freebsd" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_NETBSD], [test "x$hwloc_netbsd" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_SOLARIS], [test "x$hwloc_solaris" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_AIX], [test "x$hwloc_aix" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_OSF], [test "x$hwloc_osf" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_HPUX], [test "x$hwloc_hpux" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_WINDOWS], [test "x$hwloc_windows" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_MINGW32], [test "x$target_os" = "xmingw32"])

        AM_CONDITIONAL([HWLOC_HAVE_X86_32], [test "x$hwloc_x86_32" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_X86_64], [test "x$hwloc_x86_64" = "xyes"])
        AM_CONDITIONAL([HWLOC_HAVE_X86_CPUID], [test "x$hwloc_have_x86_cpuid" = "xyes"])

        AM_CONDITIONAL([HWLOC_HAVE_PLUGINS], [test "x$hwloc_have_plugins" = "xyes"])
        AM_CONDITIONAL([HWLOC_PCI_BUILD_STATIC], [test "x$hwloc_pci_component" = "xstatic"])
        AM_CONDITIONAL([HWLOC_OPENCL_BUILD_STATIC], [test "x$hwloc_opencl_component" = "xstatic"])
        AM_CONDITIONAL([HWLOC_CUDA_BUILD_STATIC], [test "x$hwloc_cuda_component" = "xstatic"])
        AM_CONDITIONAL([HWLOC_NVML_BUILD_STATIC], [test "x$hwloc_nvml_component" = "xstatic"])
        AM_CONDITIONAL([HWLOC_GL_BUILD_STATIC], [test "x$hwloc_gl_component" = "xstatic"])
        AM_CONDITIONAL([HWLOC_XML_LIBXML_BUILD_STATIC], [test "x$hwloc_xml_libxml_component" = "xstatic"])

        AM_CONDITIONAL([HWLOC_HAVE_CXX], [test "x$hwloc_have_cxx" = "xyes"])
    ])
    hwloc_did_am_conditionals=yes
])dnl

#-----------------------------------------------------------------------

AC_DEFUN([_HWLOC_CHECK_DIFF_U], [
  AC_MSG_CHECKING([whether diff accepts -u])
  if diff -u /dev/null /dev/null 2> /dev/null
  then
    HWLOC_DIFF_U="-u"
  else
    HWLOC_DIFF_U=""
  fi
  AC_SUBST([HWLOC_DIFF_U])
  AC_MSG_RESULT([$HWLOC_DIFF_U])
])

AC_DEFUN([_HWLOC_CHECK_DIFF_W], [
  AC_MSG_CHECKING([whether diff accepts -w])
  if diff -w /dev/null /dev/null 2> /dev/null
  then
    HWLOC_DIFF_W="-w"
  else
    HWLOC_DIFF_W=""
  fi
  AC_SUBST([HWLOC_DIFF_W])
  AC_MSG_RESULT([$HWLOC_DIFF_W])
])

#-----------------------------------------------------------------------

dnl HWLOC_CHECK_DECL
dnl
dnl Check declaration of given function by trying to call it with an insane
dnl number of arguments (10). Success means the compiler couldn't really check.
AC_DEFUN([_HWLOC_CHECK_DECL], [
  AC_MSG_CHECKING([whether function $1 is declared])
  AC_REQUIRE([AC_PROG_CC])
  AC_COMPILE_IFELSE([AC_LANG_PROGRAM(
       [AC_INCLUDES_DEFAULT([$4])
       $1(int,long,int,long,int,long,int,long,int,long);],
       [$1(1,2,3,4,5,6,7,8,9,10);])],
    [AC_MSG_RESULT([no])
     $3],
    [AC_MSG_RESULT([yes])
     $2]
  )
])

#-----------------------------------------------------------------------

dnl HWLOC_CHECK_DECLS
dnl
dnl Same as HWLOCK_CHECK_DECL, but defines HAVE_DECL_foo to 1 or 0 depending on
dnl the result.
AC_DEFUN([_HWLOC_CHECK_DECLS], [
  HWLOC_CHECK_DECL([$1], [ac_have_decl=1], [ac_have_decl=0], [$4])
  AC_DEFINE_UNQUOTED(AS_TR_CPP([HAVE_DECL_$1]), [$ac_have_decl],
    [Define to 1 if you have the declaration of `$1', and to 0 if you don't])
])

#-----------------------------------------------------------------------

dnl HWLOC_CHECK_LTDL_DEPS
dnl
dnl Add ltdl dependencies to HWLOC_LIBS_PRIVATE
AC_DEFUN([HWLOC_CHECK_LTDL_DEPS], [
  # save variables that we'll modify below
  save_lt_cv_dlopen="$lt_cv_dlopen"
  save_lt_cv_dlopen_libs="$lt_cv_dlopen_libs"
  save_lt_cv_dlopen_self="$lt_cv_dlopen_self"
  ###########################################################
  # code stolen from LT_SYS_DLOPEN_SELF in libtool.m4
  case $host_os in
  beos*)
    lt_cv_dlopen="load_add_on"
    lt_cv_dlopen_libs=
    lt_cv_dlopen_self=yes
    ;;

  mingw* | pw32* | cegcc*)
    lt_cv_dlopen="LoadLibrary"
    lt_cv_dlopen_libs=
    ;;

  cygwin*)
    lt_cv_dlopen="dlopen"
    lt_cv_dlopen_libs=
    ;;

  darwin*)
  # if libdl is installed we need to link against it
    AC_CHECK_LIB([dl], [dlopen],
                [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-ldl"],[
    lt_cv_dlopen="dyld"
    lt_cv_dlopen_libs=
    lt_cv_dlopen_self=yes
    ])
    ;;

  *)
    AC_CHECK_FUNC([shl_load],
          [lt_cv_dlopen="shl_load"],
      [AC_CHECK_LIB([dld], [shl_load],
            [lt_cv_dlopen="shl_load" lt_cv_dlopen_libs="-ldld"],
        [AC_CHECK_FUNC([dlopen],
              [lt_cv_dlopen="dlopen"],
          [AC_CHECK_LIB([dl], [dlopen],
                [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-ldl"],
            [AC_CHECK_LIB([svld], [dlopen],
                  [lt_cv_dlopen="dlopen" lt_cv_dlopen_libs="-lsvld"],
              [AC_CHECK_LIB([dld], [dld_link],
                    [lt_cv_dlopen="dld_link" lt_cv_dlopen_libs="-ldld"])
              ])
            ])
          ])
        ])
      ])
    ;;
  esac
  # end of code stolen from LT_SYS_DLOPEN_SELF in libtool.m4
  ###########################################################

  HWLOC_LIBS_PRIVATE="$HWLOC_LIBS_PRIVATE $lt_cv_dlopen_libs"

  # restore modified variable in case the actual libtool code uses them
  lt_cv_dlopen="$save_lt_cv_dlopen"
  lt_cv_dlopen_libs="$save_lt_cv_dlopen_libs"
  lt_cv_dlopen_self="$save_lt_cv_dlopen_self"
])
