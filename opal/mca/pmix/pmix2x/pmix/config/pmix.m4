dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2010 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2011 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
dnl Copyright (c) 2013-2017 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      Mellanox Technologies, Inc.
dnl                         All rights reserved.
dnl
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([PMIX_SETUP_CORE],[

    AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])
    AC_REQUIRE([AC_CANONICAL_TARGET])
    AC_REQUIRE([AC_PROG_CC])

    # If no prefix was defined, set a good value
    m4_ifval([$1],
             [m4_define([pmix_config_prefix],[$1/])],
             [m4_define([pmix_config_prefix], [])])

    # Get pmix's absolute top builddir (which may not be the same as
    # the real $top_builddir)
    PMIX_startdir=`pwd`
    if test x"pmix_config_prefix" != "x" && test ! -d "pmix_config_prefix"; then
        mkdir -p "pmix_config_prefix"
    fi
    if test x"pmix_config_prefix" != "x"; then
        cd "pmix_config_prefix"
    fi
    PMIX_top_builddir=`pwd`
    AC_SUBST(PMIX_top_builddir)

    # Get pmix's absolute top srcdir (which may not be the same as the
    # real $top_srcdir.  First, go back to the startdir incase the
    # $srcdir is relative.

    cd "$PMIX_startdir"
    cd "$srcdir"/pmix_config_prefix
    PMIX_top_srcdir="`pwd`"
    AC_SUBST(PMIX_top_srcdir)

    # Go back to where we started
    cd "$PMIX_startdir"

    AC_MSG_NOTICE([pmix builddir: $PMIX_top_builddir])
    AC_MSG_NOTICE([pmix srcdir: $PMIX_top_srcdir])
    if test "$PMIX_top_builddir" != "$PMIX_top_srcdir"; then
        AC_MSG_NOTICE([Detected VPATH build])
    fi

    # Get the version of pmix that we are installing
    AC_MSG_CHECKING([for pmix version])
    PMIX_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_MSG_RESULT([$PMIX_VERSION])
    AC_SUBST(PMIX_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_VERSION], ["$PMIX_VERSION"],
                       [The library version is always available, contrary to VERSION])

    PMIX_RELEASE_DATE="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --release-date`"
    AC_SUBST(PMIX_RELEASE_DATE)

    # Save the breakdown the version information
    PMIX_MAJOR_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --major`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_SUBST(PMIX_MAJOR_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_MAJOR_VERSION], [$PMIX_MAJOR_VERSION],
                       [The library major version is always available, contrary to VERSION])

    PMIX_MINOR_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --minor`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_SUBST(PMIX_MINOR_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_MINOR_VERSION], [$PMIX_MINOR_VERSION],
                       [The library minor version is always available, contrary to VERSION])

    pmixmajor=${PMIX_MAJOR_VERSION}L
    pmixminor=${PMIX_MINOR_VERSION}L
    AC_SUBST(pmixmajor)
    AC_SUBST(pmixminor)
    AC_CONFIG_FILES(pmix_config_prefix[include/pmix_version.h])

    PMIX_RELEASE_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --release`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_SUBST(PMIX_RELEASE_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_RELEASE_VERSION], [$PMIX_RELEASE_VERSION],
                       [The library release version is always available, contrary to VERSION])

    # Debug mode?
    AC_MSG_CHECKING([if want pmix maintainer support])
    pmix_debug=
    AS_IF([test "$pmix_debug" = "" && test "$enable_debug" = "yes"],
          [pmix_debug=1
           pmix_debug_msg="enabled"])
    AS_IF([test "$pmix_debug" = ""],
          [pmix_debug=0
           pmix_debug_msg="disabled"])
    # Grr; we use #ifndef for PMIX_DEBUG!  :-(
    AH_TEMPLATE(PMIX_ENABLE_DEBUG, [Whether we are in debugging mode or not])
    AS_IF([test "$pmix_debug" = "1"], [AC_DEFINE([PMIX_ENABLE_DEBUG])])
    AC_MSG_RESULT([$pmix_debug_msg])

    AC_MSG_CHECKING([for pmix directory prefix])
    AC_MSG_RESULT(m4_ifval([$1], pmix_config_prefix, [(none)]))

    # Note that private/config.h *MUST* be listed first so that it
    # becomes the "main" config header file.  Any AC-CONFIG-HEADERS
    # after that (pmix/config.h) will only have selective #defines
    # replaced, not the entire file.
    AC_CONFIG_HEADERS(pmix_config_prefix[src/include/pmix_config.h])

    # Rename symbols?
    AC_ARG_WITH([pmix-symbol-rename],
                AC_HELP_STRING([--with-pmix-symbol-rename=PREFIX],
                               [Provide a prefix to rename PMIx symbols]))
    AC_MSG_CHECKING([for symbol rename])
    AS_IF([test ! -z "$with_pmix_symbol_rename" && test "$with_pmix_symbol_rename" != "yes"],
          [AC_MSG_RESULT([$with_pmix_symbol_rename])
           pmix_symbol_rename="$with_pmix_symbol_rename"
           PMIX_RENAME=$with_pmix_symbol_rename],
          [AC_MSG_RESULT([no])
           pmix_symbol_rename=""
           PMIX_RENAME=])
    AC_DEFINE_UNQUOTED(PMIX_SYMBOL_RENAME, [$pmix_symbol_rename],
                       [The pmix symbol rename include directive])
    AC_SUBST(PMIX_RENAME)
    AC_CONFIG_FILES(pmix_config_prefix[include/pmix_rename.h])

    # GCC specifics.
    if test "x$GCC" = "xyes"; then
        PMIX_GCC_CFLAGS="-Wall -Wmissing-prototypes -Wundef"
        PMIX_GCC_CFLAGS="$PMIX_GCC_CFLAGS -Wpointer-arith -Wcast-align"
    fi

    ############################################################################
    # Check for compilers and preprocessors
    ############################################################################
    pmix_show_title "Compiler and preprocessor tests"

    #
    # Check for some types
    #

    AC_CHECK_TYPES(int8_t)
    AC_CHECK_TYPES(uint8_t)
    AC_CHECK_TYPES(int16_t)
    AC_CHECK_TYPES(uint16_t)
    AC_CHECK_TYPES(int32_t)
    AC_CHECK_TYPES(uint32_t)
    AC_CHECK_TYPES(int64_t)
    AC_CHECK_TYPES(uint64_t)
    AC_CHECK_TYPES(long long)

    AC_CHECK_TYPES(intptr_t)
    AC_CHECK_TYPES(uintptr_t)
    AC_CHECK_TYPES(ptrdiff_t)

    #
    # Check for type sizes
    #

    AC_CHECK_SIZEOF(_Bool)
    AC_CHECK_SIZEOF(char)
    AC_CHECK_SIZEOF(short)
    AC_CHECK_SIZEOF(int)
    AC_CHECK_SIZEOF(long)
    if test "$ac_cv_type_long_long" = yes; then
        AC_CHECK_SIZEOF(long long)
    fi
    AC_CHECK_SIZEOF(float)
    AC_CHECK_SIZEOF(double)

    AC_CHECK_SIZEOF(void *)
    AC_CHECK_SIZEOF(size_t)
    if test "$ac_cv_type_ssize_t" = yes ; then
        AC_CHECK_SIZEOF(ssize_t)
    fi
    if test "$ac_cv_type_ptrdiff_t" = yes; then
        AC_CHECK_SIZEOF(ptrdiff_t)
    fi
    AC_CHECK_SIZEOF(wchar_t)

    AC_CHECK_SIZEOF(pid_t)

    #
    # Check for type alignments
    #

    PMIX_C_GET_ALIGNMENT(bool, PMIX_ALIGNMENT_BOOL)
    PMIX_C_GET_ALIGNMENT(int8_t, PMIX_ALIGNMENT_INT8)
    PMIX_C_GET_ALIGNMENT(int16_t, PMIX_ALIGNMENT_INT16)
    PMIX_C_GET_ALIGNMENT(int32_t, PMIX_ALIGNMENT_INT32)
    PMIX_C_GET_ALIGNMENT(int64_t, PMIX_ALIGNMENT_INT64)
    PMIX_C_GET_ALIGNMENT(char, PMIX_ALIGNMENT_CHAR)
    PMIX_C_GET_ALIGNMENT(short, PMIX_ALIGNMENT_SHORT)
    PMIX_C_GET_ALIGNMENT(wchar_t, PMIX_ALIGNMENT_WCHAR)
    PMIX_C_GET_ALIGNMENT(int, PMIX_ALIGNMENT_INT)
    PMIX_C_GET_ALIGNMENT(long, PMIX_ALIGNMENT_LONG)
    if test "$ac_cv_type_long_long" = yes; then
        PMIX_C_GET_ALIGNMENT(long long, PMIX_ALIGNMENT_LONG_LONG)
    fi
    PMIX_C_GET_ALIGNMENT(float, PMIX_ALIGNMENT_FLOAT)
    PMIX_C_GET_ALIGNMENT(double, PMIX_ALIGNMENT_DOUBLE)
    if test "$ac_cv_type_long_double" = yes; then
        PMIX_C_GET_ALIGNMENT(long double, PMIX_ALIGNMENT_LONG_DOUBLE)
    fi
    PMIX_C_GET_ALIGNMENT(void *, PMIX_ALIGNMENT_VOID_P)
    PMIX_C_GET_ALIGNMENT(size_t, PMIX_ALIGNMENT_SIZE_T)


    #
    # Does the C compiler native support "bool"? (i.e., without
    # <stdbool.h> or any other help)
    #

    PMIX_VAR_SCOPE_PUSH([MSG])
    AC_MSG_CHECKING(for C bool type)
    AC_COMPILE_IFELSE([AC_LANG_PROGRAM([
                                          AC_INCLUDES_DEFAULT],
                                       [[bool bar, foo = true; bar = foo;]])],
                      [PMIX_NEED_C_BOOL=0 MSG=yes],[PMIX_NEED_C_BOOL=1 MSG=no])
    AC_DEFINE_UNQUOTED(PMIX_NEED_C_BOOL, $PMIX_NEED_C_BOOL,
                       [Whether the C compiler supports "bool" without any other help (such as <stdbool.h>)])
    AC_MSG_RESULT([$MSG])
    AC_CHECK_SIZEOF(_Bool)
    PMIX_VAR_SCOPE_POP

    #
    # Check for other compiler characteristics
    #

    PMIX_VAR_SCOPE_PUSH([PMIX_CFLAGS_save])
    if test "$GCC" = "yes"; then

        # gcc 2.96 will emit oodles of warnings if you use "inline" with
        # -pedantic (which we do in developer builds).  However,
        # "__inline__" is ok.  So we have to force gcc to select the
        # right one.  If you use -pedantic, the AC_C_INLINE test will fail
        # (because it names a function foo() -- without the (void)).  So
        # we turn off all the picky flags, turn on -ansi mode (which is
        # implied by -pedantic), and set warnings to be errors.  Hence,
        # this does the following (for 2.96):
        #
        # - causes the check for "inline" to emit a warning, which then
        # fails
        # - checks for __inline__, which then emits no error, and works
        #
        # This also works nicely for gcc 3.x because "inline" will work on
        # the first check, and all is fine.  :-)

        PMIX_CFLAGS_save=$CFLAGS
        CFLAGS="$PMIX_CFLAGS_BEFORE_PICKY -Werror -ansi"
    fi
    AC_C_INLINE
    if test "$GCC" = "yes"; then
        CFLAGS=$PMIX_CFLAGS_save
    fi
    PMIX_VAR_SCOPE_POP

    if test "x$CC" = "xicc"; then
        PMIX_CHECK_ICC_VARARGS
    fi


    ##################################
    # Only after setting up
    # C do we check compiler attributes.
    ##################################

    pmix_show_subtitle "Compiler characteristics"

    PMIX_CHECK_ATTRIBUTES
    PMIX_CHECK_COMPILER_VERSION_ID

    ##################################
    # Header files
    ##################################

    pmix_show_title "Header file tests"

    AC_CHECK_HEADERS([arpa/inet.h \
                      fcntl.h ifaddrs.h inttypes.h libgen.h \
                      net/uio.h netinet/in.h \
                      stdint.h stddef.h \
                      stdlib.h string.h strings.h \
                      sys/ioctl.h sys/param.h \
                      sys/select.h sys/socket.h sys/sockio.h \
                      stdarg.h sys/stat.h sys/time.h \
                      sys/types.h sys/un.h sys/uio.h \
                      sys/wait.h syslog.h \
                      time.h unistd.h dirent.h \
                      crt_externs.h signal.h \
                      ioLib.h sockLib.h hostLib.h limits.h \
                      sys/statfs.h sys/statvfs.h \
                      netdb.h ucred.h zlib.h])

    AC_CHECK_HEADERS([sys/mount.h], [], [],
                     [AC_INCLUDES_DEFAULT
                      #if HAVE_SYS_PARAM_H
                      #include <sys/param.h>
                      #endif
                      ])

    AC_CHECK_HEADERS([sys/sysctl.h], [], [],
                     [AC_INCLUDES_DEFAULT
                      #if HAVE_SYS_PARAM_H
                      #include <sys/param.h>
                      #endif
                      ])

    # Needed to work around Darwin requiring sys/socket.h for
    # net/if.h
    AC_CHECK_HEADERS([net/if.h], [], [],
                     [#include <stdio.h>
                      #if STDC_HEADERS
                      # include <stdlib.h>
                      # include <stddef.h>
                      #else
                      # if HAVE_STDLIB_H
                      #  include <stdlib.h>
                      # endif
                      #endif
                      #if HAVE_SYS_SOCKET_H
                      # include <sys/socket.h>
                      #endif
                      ])

    # Note that sometimes we have <stdbool.h>, but it doesn't work (e.g.,
    # have both Portland and GNU installed; using pgcc will find GNU's
    # <stdbool.h>, which all it does -- by standard -- is define "bool" to
    # "_Bool" [see
    # http://pmixw.opengroup.org/onlinepubs/009695399/basedefs/stdbool.h.html],
    # and Portland has no idea what to do with _Bool).

    # So first figure out if we have <stdbool.h> (i.e., check the value of
    # the macro HAVE_STDBOOL_H from the result of AC_CHECK_HEADERS,
    # above).  If we do have it, then check to see if it actually works.
    # Define PMIX_USE_STDBOOL_H as approrpaite.
    AC_CHECK_HEADERS([stdbool.h], [have_stdbool_h=1], [have_stdbool_h=0])
    AC_MSG_CHECKING([if <stdbool.h> works])
    if test "$have_stdbool_h" = "1"; then
        AC_COMPILE_IFELSE([AC_LANG_PROGRAM([AC_INCLUDES_DEFAULT[
                                                   #if HAVE_STDBOOL_H
                                                   #include <stdbool.h>
                                                   #endif
                                               ]],
                                           [[bool bar, foo = true; bar = foo;]])],
                          [PMIX_USE_STDBOOL_H=1 MSG=yes],[PMIX_USE_STDBOOL_H=0 MSG=no])
    else
        PMIX_USE_STDBOOL_H=0
        MSG="no (don't have <stdbool.h>)"
    fi
    AC_DEFINE_UNQUOTED(PMIX_USE_STDBOOL_H, $PMIX_USE_STDBOOL_H,
                       [Whether to use <stdbool.h> or not])
    AC_MSG_RESULT([$MSG])

    # checkpoint results
    AC_CACHE_SAVE

    ##################################
    # Types
    ##################################

    pmix_show_title "Type tests"

    AC_CHECK_TYPES([socklen_t, struct sockaddr_in, struct sockaddr_un,
                    struct sockaddr_in6, struct sockaddr_storage],
                   [], [], [AC_INCLUDES_DEFAULT
                            #if HAVE_SYS_SOCKET_H
                            #include <sys/socket.h>
                            #endif
                            #if HAVE_SYS_UN_H
                            #include <sys/un.h>
                            #endif
                            #ifdef HAVE_NETINET_IN_H
                            #include <netinet/in.h>
                            #endif
                           ])

    AC_CHECK_DECLS([AF_UNSPEC, PF_UNSPEC, AF_INET6, PF_INET6],
                   [], [], [AC_INCLUDES_DEFAULT
                            #if HAVE_SYS_SOCKET_H
                            #include <sys/socket.h>
                            #endif
                            #ifdef HAVE_NETINET_IN_H
                            #include <netinet/in.h>
                            #endif
                           ])

    # SA_RESTART in signal.h
    PMIX_VAR_SCOPE_PUSH([MSG2])
    AC_MSG_CHECKING([if SA_RESTART defined in signal.h])
                        AC_EGREP_CPP(yes, [
                                            #include <signal.h>
                                            #ifdef SA_RESTART
                                            yes
                                            #endif
                                        ], [MSG2=yes VALUE=1], [MSG2=no VALUE=0])
    AC_DEFINE_UNQUOTED(PMIX_HAVE_SA_RESTART, $VALUE,
                       [Whether we have SA_RESTART in <signal.h> or not])
    AC_MSG_RESULT([$MSG2])
    PMIX_VAR_SCOPE_POP

    AC_CHECK_MEMBERS([struct sockaddr.sa_len], [], [], [
                         #include <sys/types.h>
                         #if HAVE_SYS_SOCKET_H
                         #include <sys/socket.h>
                         #endif
                     ])

    AC_CHECK_MEMBERS([struct dirent.d_type], [], [], [
                         #include <sys/types.h>
                         #include <dirent.h>])

    AC_CHECK_MEMBERS([siginfo_t.si_fd],,,[#include <signal.h>])
    AC_CHECK_MEMBERS([siginfo_t.si_band],,,[#include <signal.h>])

    #
    # Checks for struct member names in struct statfs
    #
    AC_CHECK_MEMBERS([struct statfs.f_type], [], [], [
                         AC_INCLUDES_DEFAULT
                         #ifdef HAVE_SYS_VFS_H
                         #include <sys/vfs.h>
                         #endif
                         #ifdef HAVE_SYS_STATFS_H
                         #include <sys/statfs.h>
                         #endif
                     ])

    AC_CHECK_MEMBERS([struct statfs.f_fstypename], [], [], [
                         AC_INCLUDES_DEFAULT
                         #ifdef HAVE_SYS_PARAM_H
                         #include <sys/param.h>
                         #endif
                         #ifdef HAVE_SYS_MOUNT_H
                         #include <sys/mount.h>
                         #endif
                         #ifdef HAVE_SYS_VFS_H
                         #include <sys/vfs.h>
                         #endif
                         #ifdef HAVE_SYS_STATFS_H
                         #include <sys/statfs.h>
                         #endif
                     ])

    #
    # Checks for struct member names in struct statvfs
    #
    AC_CHECK_MEMBERS([struct statvfs.f_basetype], [], [], [
                         AC_INCLUDES_DEFAULT
                         #ifdef HAVE_SYS_STATVFS_H
                         #include <sys/statvfs.h>
                         #endif
                     ])

    AC_CHECK_MEMBERS([struct statvfs.f_fstypename], [], [], [
                         AC_INCLUDES_DEFAULT
                         #ifdef HAVE_SYS_STATVFS_H
                         #include <sys/statvfs.h>
                         #endif
                     ])

    AC_CHECK_MEMBERS([struct ucred.uid, struct ucred.cr_uid, struct sockpeercred.uid],
                     [], [],
                     [#include <sys/types.h>
                      #include <sys/socket.h> ])

    #
    # Check for ptrdiff type.  Yes, there are platforms where
    # sizeof(void*) != sizeof(long) (64 bit Windows, apparently).
    #
    AC_MSG_CHECKING([for pointer diff type])
    if test $ac_cv_type_ptrdiff_t = yes ; then
        pmix_ptrdiff_t="ptrdiff_t"
        pmix_ptrdiff_size=$ac_cv_sizeof_ptrdiff_t
    elif test $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long ; then
        pmix_ptrdiff_t="long"
        pmix_ptrdiff_size=$ac_cv_sizeof_long
    elif test $ac_cv_type_long_long = yes && test $ac_cv_sizeof_void_p -eq $ac_cv_sizeof_long_long ; then
        pmix_ptrdiff_t="long long"
        pmix_ptrdiff_size=$ac_cv_sizeof_long_long
        #else
        #    AC_MSG_ERROR([Could not find datatype to emulate ptrdiff_t.  Cannot continue])
    fi
    AC_DEFINE_UNQUOTED([PMIX_PTRDIFF_TYPE], [$pmix_ptrdiff_t],
                       [type to use for ptrdiff_t])
    AC_MSG_RESULT([$pmix_ptrdiff_t (size: $pmix_ptrdiff_size)])

    ##################################
    # Linker characteristics
    ##################################

    AC_MSG_CHECKING([the linker for support for the -fini option])
    PMIX_VAR_SCOPE_PUSH([LDFLAGS_save])
    LDFLAGS_save=$LDFLAGS
    LDFLAGS="$LDFLAGS_save -Wl,-fini -Wl,finalize"
    AC_TRY_LINK([void finalize (void) {}], [], [AC_MSG_RESULT([yes])
            pmix_ld_have_fini=1], [AC_MSG_RESULT([no])
            pmix_ld_have_fini=0])
    LDFLAGS=$LDFLAGS_save
    PMIX_VAR_SCOPE_POP

    pmix_destructor_use_fini=0
    pmix_no_destructor=0
    if test x$pmix_cv___attribute__destructor = x0 ; then
        if test x$pmix_ld_have_fini = x1 ; then
            pmix_destructor_use_fini=1
        else
            pmix_no_destructor=1;
        fi
    fi

    AC_DEFINE_UNQUOTED(PMIX_NO_LIB_DESTRUCTOR, [$pmix_no_destructor],
        [Whether libraries can be configured with destructor functions])
    AM_CONDITIONAL(PMIX_DESTRUCTOR_USE_FINI, [test x$pmix_destructor_use_fini = x1])

    ##################################
    # Libraries
    ##################################

    pmix_show_title "Library and Function tests"

    PMIX_SEARCH_LIBS_CORE([socket], [socket])

    # IRIX and CentOS have dirname in -lgen, usually in libc
    PMIX_SEARCH_LIBS_CORE([dirname], [gen])

    # Darwin doesn't need -lm, as it's a symlink to libSystem.dylib
    PMIX_SEARCH_LIBS_CORE([ceil], [m])

    AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf strsignal socketpair strncpy_s usleep statfs statvfs getpeereid getpeerucred strnlen posix_fallocate])

    # On some hosts, htonl is a define, so the AC_CHECK_FUNC will get
    # confused.  On others, it's in the standard library, but stubbed with
    # the magic glibc foo as not implemented.  and on other systems, it's
    # just not there.  This covers all cases.
    AC_CACHE_CHECK([for htonl define],
                   [pmix_cv_htonl_define],
                   [AC_PREPROC_IFELSE([AC_LANG_PROGRAM([
                                                          #ifdef HAVE_SYS_TYPES_H
                                                          #include <sys/types.h>
                                                          #endif
                                                          #ifdef HAVE_NETINET_IN_H
                                                          #include <netinet/in.h>
                                                          #endif
                                                          #ifdef HAVE_ARPA_INET_H
                                                          #include <arpa/inet.h>
                                                          #endif],[
                                                          #ifndef ntohl
                                                          #error "ntohl not defined"
                                                          #endif
                                                      ])], [pmix_cv_htonl_define=yes], [pmix_cv_htonl_define=no])])
    AC_CHECK_FUNC([htonl], [pmix_have_htonl=yes], [pmix_have_htonl=no])
    AS_IF([test "$pmix_cv_htonl_define" = "yes" || test "$pmix_have_htonl" = "yes"],
          [AC_DEFINE_UNQUOTED([HAVE_UNIX_BYTESWAP], [1],
                              [whether unix byteswap routines -- htonl, htons, nothl, ntohs -- are available])])

    # check pandoc separately so we can setup an AM_CONDITIONAL off it
    AC_CHECK_PROG([pmix_have_pandoc], [pandoc], [yes], [no])
    AM_CONDITIONAL([PMIX_HAVE_PANDOC], [test "x$pmix_have_pandoc" = "xyes"])

    #
    # Make sure we can copy va_lists (need check declared, not linkable)
    #

    AC_CHECK_DECL(va_copy, PMIX_HAVE_VA_COPY=1, PMIX_HAVE_VA_COPY=0,
                  [#include <stdarg.h>])
    AC_DEFINE_UNQUOTED(PMIX_HAVE_VA_COPY, $PMIX_HAVE_VA_COPY,
                       [Whether we have va_copy or not])

    AC_CHECK_DECL(__va_copy, PMIX_HAVE_UNDERSCORE_VA_COPY=1,
                  PMIX_HAVE_UNDERSCORE_VA_COPY=0, [#include <stdarg.h>])
    AC_DEFINE_UNQUOTED(PMIX_HAVE_UNDERSCORE_VA_COPY, $PMIX_HAVE_UNDERSCORE_VA_COPY,
                       [Whether we have __va_copy or not])

    AC_CHECK_DECLS(__func__)

    # checkpoint results
    AC_CACHE_SAVE

    ##################################
    # System-specific tests
    ##################################

    pmix_show_title "System-specific tests"

    AC_C_BIGENDIAN
    PMIX_CHECK_BROKEN_QSORT

    ##################################
    # Visibility
    ##################################

    # Check the visibility declspec at the end to avoid problem with
    # the previous tests that are not necessarily prepared for
    # the visibility feature.
    pmix_show_title "Symbol visibility feature"

    PMIX_CHECK_VISIBILITY

    ##################################
    # Libevent
    ##################################
    pmix_show_title "Libevent"

    PMIX_LIBEVENT_CONFIG

    ##################################
    # ZLIB COMPRESSION
    ##################################
    pmix_show_title "ZLIB"

    PMIX_ZLIB_CONFIG

    ##################################
    # MCA
    ##################################

    pmix_show_title "Modular Component Architecture (MCA) setup"

    AC_MSG_CHECKING([for subdir args])
    PMIX_CONFIG_SUBDIR_ARGS([pmix_subdir_args])
    AC_MSG_RESULT([$pmix_subdir_args])

    PMIX_MCA

    ##################################
    # Dstore Locking
    ##################################

    pmix_show_title "Dstore Locking"

    PMIX_CHECK_DSTOR_LOCK

    ############################################################################
    # final compiler config
    ############################################################################

    pmix_show_subtitle "Set path-related compiler flags"

    #
    # This is needed for VPATH builds, so that it will -I the appropriate
    # include directory.  We delayed doing it until now just so that
    # '-I$(top_srcdir)' doesn't show up in any of the configure output --
    # purely aesthetic.
    #
    # Because pmix_config.h is created by AC_CONFIG_HEADERS, we
    # don't need to -I the builddir for pmix/include. However, if we
    # are VPATH building, we do need to include the source directories.
    #
    if test "$PMIX_top_builddir" != "$PMIX_top_srcdir"; then
        # Note the embedded m4 directives here -- we must embed them
        # rather than have successive assignments to these shell
        # variables, lest the $(foo) names try to get evaluated here.
        # Yuck!
        CPPFLAGS="-I$PMIX_top_builddir -I$PMIX_top_srcdir -I$PMIX_top_srcdir/src -I$PMIX_top_builddir/include -I$PMIX_top_srcdir/include $CPPFLAGS"
    else
        CPPFLAGS="-I$PMIX_top_srcdir -I$PMIX_top_srcdir/src -I$PMIX_top_srcdir/include $CPPFLAGS"
    fi

    # pmixdatadir, pmixlibdir, and pmixinclude are essentially the same as
    # pkg*dir, but will always be */pmix.
    pmixdatadir='${datadir}/pmix'
    pmixlibdir='${libdir}/pmix'
    pmixincludedir='${includedir}/pmix'
    AC_SUBST(pmixdatadir)
    AC_SUBST(pmixlibdir)
    AC_SUBST(pmixincludedir)

    ############################################################################
    # final output
    ############################################################################

    pmix_show_subtitle "Final output"

    AC_CONFIG_FILES(
        pmix_config_prefix[Makefile]
        pmix_config_prefix[config/Makefile]
        pmix_config_prefix[include/Makefile]
        pmix_config_prefix[src/Makefile]
        pmix_config_prefix[src/util/keyval/Makefile]
        pmix_config_prefix[src/mca/base/Makefile]
        )

    # publish any embedded flags so external wrappers can use them
    AC_SUBST(PMIX_EMBEDDED_LIBS)
    AC_SUBST(PMIX_EMBEDDED_LDFLAGS)
    AC_SUBST(PMIX_EMBEDDED_CPPFLAGS)

    # Success
    $2
])dnl

AC_DEFUN([PMIX_DEFINE_ARGS],[
    # do we want dlopen support ?
    AC_MSG_CHECKING([if want dlopen support])
    AC_ARG_ENABLE([dlopen],
        [AC_HELP_STRING([--enable-dlopen],
                        [Whether build should attempt to use dlopen (or
                         similar) to dynamically load components.
                         Disabling dlopen implies --disable-pdl-dlopen
                         (default: enabled)])])
    AS_IF([test "$enable_dlopen" = "no"],
          [enable_mca_dso="no"
           enable_mca_static="yes"
           PMIX_ENABLE_DLOPEN_SUPPORT=0
           AC_MSG_RESULT([no])],
          [PMIX_ENABLE_DLOPEN_SUPPORT=1
           AC_MSG_RESULT([yes])])

    # Embedded mode, or standalone?
    AC_MSG_CHECKING([if embedded mode is enabled])
    AC_ARG_ENABLE([embedded-mode],
        [AC_HELP_STRING([--enable-embedded-mode],
                [Using --enable-embedded-mode causes PMIx to skip a few configure checks and install nothing.  It should only be used when building PMIx within the scope of a larger package.])])
    AS_IF([test ! -z "$enable_embedded_mode" && test "$enable_embedded_mode" = "yes"],
          [pmix_mode=embedded
           AC_MSG_RESULT([yes])],
          [pmix_mode=standalone
           AC_MSG_RESULT([no])])

    # Install tests and examples?
    AC_MSG_CHECKING([if tests and examples are to be installed])
    AC_ARG_WITH([tests-examples],
        [AC_HELP_STRING([--with-tests-examples],
                [Whether or not to install the tests and example programs.])])
    AS_IF([test ! -z "$with_tests_examples" && test "$with_tests_examples" = "no"],
          [pmix_tests=no
           AC_MSG_RESULT([no])],
          [pmix_tests=yes
           AC_MSG_RESULT([yes])])

#
# Is this a developer copy?
#

if test -d .git; then
    PMIX_DEVEL=1
else
    PMIX_DEVEL=0
fi


#
# Developer picky compiler options
#

AC_MSG_CHECKING([if want developer-level compiler pickyness])
AC_ARG_ENABLE(picky,
    AC_HELP_STRING([--enable-picky],
                   [enable developer-level compiler pickyness when building PMIx (default: disabled)]))
if test "$enable_picky" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PICKY_COMPILER=1
else
    AC_MSG_RESULT([no])
    WANT_PICKY_COMPILER=0
fi
#################### Early development override ####################
if test "$WANT_PICKY_COMPILER" = "0" && test -z "$enable_picky" && test "$PMIX_DEVEL" = "1"; then
    WANT_PICKY_COMPILER=1
    echo "--> developer override: enable picky compiler by default"
fi
#################### Early development override ####################

#
# Developer debugging
#

AC_MSG_CHECKING([if want developer-level debugging code])
AC_ARG_ENABLE(debug,
    AC_HELP_STRING([--enable-debug],
                   [enable developer-level debugging code (not for general PMIx users!) (default: disabled)]))
if test "$enable_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_DEBUG=0
fi
#################### Early development override ####################
if test "$WANT_DEBUG" = "0" && test -z "$enable_debug" && test "$PMIX_DEVEL" = "1"; then
    WANT_DEBUG=1
    echo "--> developer override: enable debugging code by default"
fi
#################### Early development override ####################
if test "$WANT_DEBUG" = "0"; then
    CFLAGS="-DNDEBUG $CFLAGS"
    CXXFLAGS="-DNDEBUG $CXXFLAGS"
fi
AC_DEFINE_UNQUOTED(PMIX_ENABLE_DEBUG, $WANT_DEBUG,
                   [Whether we want developer-level debugging code or not])

AC_ARG_ENABLE(debug-symbols,
              AC_HELP_STRING([--disable-debug-symbols],
                             [Disable adding compiler flags to enable debugging symbols if --enable-debug is specified.  For non-debugging builds, this flag has no effect.]))

#
# Do we want to install the internal devel headers?
#
AC_MSG_CHECKING([if want to install project-internal header files])
AC_ARG_WITH(devel-headers,
    AC_HELP_STRING([--with-devel-headers],
                   [normal PMIx users/applications do not need this (pmix.h and friends are ALWAYS installed).  Developer headers are only necessary for authors doing deeper integration (default: disabled).]))
if test "$with_devel_headers" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_INSTALL_HEADERS=1
else
    AC_MSG_RESULT([no])
    WANT_INSTALL_HEADERS=0
fi
AM_CONDITIONAL(WANT_INSTALL_HEADERS, test "$WANT_INSTALL_HEADERS" = 1)

#
# Support per-user config files?
#
AC_ARG_ENABLE([per-user-config-files],
   [AC_HELP_STRING([--enable-per-user-config-files],
      [Disable per-user configuration files, to save disk accesses during job start-up.  This is likely desirable for large jobs.  Note that this can also be acheived by environment variables at run-time.  (default: enabled)])])
if test "$enable_per_user_config_files" = "no" ; then
  result=0
else
  result=1
fi
AC_DEFINE_UNQUOTED([PMIX_WANT_HOME_CONFIG_FILES], [$result],
     [Enable per-user config files])

#
# Do we want the pretty-print stack trace feature?
#

AC_MSG_CHECKING([if want pretty-print stacktrace])
AC_ARG_ENABLE([pretty-print-stacktrace],
              [AC_HELP_STRING([--enable-pretty-print-stacktrace],
                              [Pretty print stacktrace on process signal (default: enabled)])])
if test "$enable_pretty_print_stacktrace" = "no" ; then
    AC_MSG_RESULT([no])
    WANT_PRETTY_PRINT_STACKTRACE=0
else
    AC_MSG_RESULT([yes])
    WANT_PRETTY_PRINT_STACKTRACE=1
fi
AC_DEFINE_UNQUOTED([PMIX_WANT_PRETTY_PRINT_STACKTRACE],
                   [$WANT_PRETTY_PRINT_STACKTRACE],
                   [if want pretty-print stack trace feature])

#
# Do we want the shared memory datastore usage?
#

AC_MSG_CHECKING([if want shared memory datastore])
AC_ARG_ENABLE([dstore],
              [AC_HELP_STRING([--disable-dstore],
                              [Using shared memory datastore (default: enabled)])])
if test "$enable_dstore" == "no" ; then
    AC_MSG_RESULT([no])
    WANT_DSTORE=0
else
    AC_MSG_RESULT([yes])
    WANT_DSTORE=1
fi
AC_DEFINE_UNQUOTED([PMIX_ENABLE_DSTORE],
                 [$WANT_DSTORE],
                 [if want shared memory dstore feature])

#
# Use pthread-based locking
#
DSTORE_PTHREAD_LOCK="1"
AC_MSG_CHECKING([if want dstore pthread-based locking])
AC_ARG_ENABLE([dstore-pthlck],
              [AC_HELP_STRING([--disable-dstore-pthlck],
                              [Disable pthread-based lockig in dstor (default: enabled)])])
if test "$enable_dstore_pthlck" == "no" ; then
    AC_MSG_RESULT([no])
    DSTORE_PTHREAD_LOCK="0"
else
    AC_MSG_RESULT([yes])
    DSTORE_PTHREAD_LOCK="1"
fi

#
# Ident string
#
AC_MSG_CHECKING([if want ident string])
AC_ARG_WITH([ident-string],
            [AC_HELP_STRING([--with-ident-string=STRING],
                            [Embed an ident string into PMIx object files])])
if test "$with_ident_string" = "" || test "$with_ident_string" = "no"; then
    with_ident_string="%VERSION%"
fi
# This is complicated, because $PMIX_VERSION may have spaces in it.
# So put the whole sed expr in single quotes -- i.e., directly
# substitute %VERSION% for (not expanded) $PMIX_VERSION.
with_ident_string="`echo $with_ident_string | sed -e 's/%VERSION%/$PMIX_VERSION/'`"

# Now eval an echo of that so that the "$PMIX_VERSION" token is
# replaced with its value.  Enclose the whole thing in "" so that it
# ends up as 1 token.
with_ident_string="`eval echo $with_ident_string`"

AC_DEFINE_UNQUOTED([PMIX_IDENT_STRING], ["$with_ident_string"],
                   [ident string for PMIX])
AC_MSG_RESULT([$with_ident_string])

#
# Timing support
#
AC_MSG_CHECKING([if want developer-level timing support])
AC_ARG_ENABLE(timing,
              AC_HELP_STRING([--enable-timing],
                             [enable developer-level timing code (default: disabled)]))
if test "$enable_timing" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_TIMING=1
else
    AC_MSG_RESULT([no])
    WANT_TIMING=0
fi

AC_DEFINE_UNQUOTED([PMIX_ENABLE_TIMING], [$WANT_TIMING],
                   [Whether we want developer-level timing support or not])

#
# Install header files
#
AC_MSG_CHECKING([if want to head developer-level header files])
AC_ARG_WITH(devel-headers,
              AC_HELP_STRING([--with-devel-headers],
                             [also install developer-level header files (only for internal PMIx developers, default: disabled)]))
if test "$with_devel_headers" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_INSTALL_HEADERS=1
else
    AC_MSG_RESULT([no])
    WANT_INSTALL_HEADERS=0
fi

AM_CONDITIONAL([WANT_INSTALL_HEADERS], [test $WANT_INSTALL_HEADERS -eq 1])
])dnl

# This must be a standalone routine so that it can be called both by
# PMIX_INIT and an external caller (if PMIX_INIT is not invoked).
AC_DEFUN([PMIX_DO_AM_CONDITIONALS],[
    AS_IF([test "$pmix_did_am_conditionals" != "yes"],[
        AM_CONDITIONAL([PMIX_EMBEDDED_MODE], [test "x$pmix_mode" = "xembedded"])
        AM_CONDITIONAL([PMIX_TESTS_EXAMPLES], [test "x$pmix_tests" = "xyes"])
        AM_CONDITIONAL([PMIX_COMPILE_TIMING], [test "$WANT_TIMING" = "1"])
        AM_CONDITIONAL([PMIX_WANT_MUNGE], [test "$pmix_munge_support" = "1"])
        AM_CONDITIONAL([PMIX_WANT_SASL], [test "$pmix_sasl_support" = "1"])
        AM_CONDITIONAL([WANT_DSTORE],[test "x$enable_dstore" != "xno"])
    ])
    pmix_did_am_conditionals=yes
])dnl
