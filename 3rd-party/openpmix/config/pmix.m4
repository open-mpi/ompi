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
dnl Copyright (c) 2006-2022 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009-2022 IBM Corporation.  All rights reserved.
dnl Copyright (c) 2009      Los Alamos National Security, LLC.  All rights
dnl                         reserved.
dnl Copyright (c) 2009-2011 Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2011-2013 NVIDIA Corporation.  All rights reserved.
dnl Copyright (c) 2013-2023 Intel, Inc.  All rights reserved.
dnl Copyright (c) 2015-2019 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl Copyright (c) 2016      Mellanox Technologies, Inc.
dnl                         All rights reserved.
dnl
dnl Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
dnl Copyright (c) 2018-2022 Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl Copyright (c) 2021      FUJITSU LIMITED.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([PMIX_SETUP_CORE],[

    AC_REQUIRE([AC_USE_SYSTEM_EXTENSIONS])
    AC_REQUIRE([AC_CANONICAL_TARGET])

    # AM_PROG_CC_C_O AC_REQUIREs AC_PROG_CC, so we have to be a little
    # careful about ordering here, and AC_REQUIRE these things so that
    # they get stamped out in the right order.
    AC_REQUIRE([_PMIX_START_SETUP_CC])
    AC_REQUIRE([_PMIX_PROG_CC])
    AC_REQUIRE([AM_PROG_CC_C_O])

    OAC_PUSH_PREFIX([PMIX])

    # initialize
    PMIX_EMBEDDED_LDFLAGS=
    PMIX_EMBEDDED_LIBS=
    PMIX_EMBEDDED_CPPFLAGS=

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
    # real $top_srcdir.  First, go back to the startdir in case the
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

    PMIX_PROXY_BUGREPORT_STRING="https://github.com/openpmix/openpmix"
    AC_SUBST(PMIX_PROXY_BUGREPORT_STRING)
    AC_DEFINE_UNQUOTED([PMIX_PROXY_BUGREPORT_STRING], ["$PMIX_PROXY_BUGREPORT_STRING"],
                       [Where to report bugs])

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

    PMIX_RELEASE_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --release`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_SUBST(PMIX_RELEASE_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_RELEASE_VERSION], [$PMIX_RELEASE_VERSION],
                       [The library release version is always available, contrary to VERSION])

    pmixmajor=${PMIX_MAJOR_VERSION}L
    pmixminor=${PMIX_MINOR_VERSION}L
    pmixrelease=${PMIX_RELEASE_VERSION}L
    pmixnumeric=$(printf 0x%4.4x%2.2x%2.2x $PMIX_MAJOR_VERSION $PMIX_MINOR_VERSION $PMIX_RELEASE_VERSION)
    AC_SUBST(pmixmajor)
    AC_SUBST(pmixminor)
    AC_SUBST(pmixrelease)
    AC_SUBST(pmixnumeric)
    AC_CONFIG_FILES(pmix_config_prefix[include/pmix_version.h])

    PMIX_GREEK_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --greek`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_SUBST(PMIX_GREEK_VERSION)

    AC_MSG_CHECKING([for pmix standard version])
    PMIX_STD_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --std-version`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_MSG_RESULT([$PMIX_STD_VERSION])
    AC_SUBST(PMIX_STD_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_STD_VERSION], ["$PMIX_STD_VERSION"],
                       [The PMIx Standard compliance level])

    AC_MSG_CHECKING([for pmix standard stable ABI version(s)])
    PMIX_STD_ABI_STABLE_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --std-abi-stable-version`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_MSG_RESULT([$PMIX_STD_ABI_STABLE_VERSION])
    AC_SUBST(PMIX_STD_ABI_STABLE_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_STD_ABI_STABLE_VERSION], ["$PMIX_STD_ABI_STABLE_VERSION"],
                       [The PMIx Standard Stable ABI compliance level(s)])

    AC_MSG_CHECKING([for pmix standard provisional ABI version(s)])
    PMIX_STD_ABI_PROVISIONAL_VERSION="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --std-abi-provisional-version`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_MSG_RESULT([$PMIX_STD_ABI_PROVISIONAL_VERSION])
    AC_SUBST(PMIX_STD_ABI_PROVISIONAL_VERSION)
    AC_DEFINE_UNQUOTED([PMIX_STD_ABI_PROVISIONAL_VERSION], ["$PMIX_STD_ABI_PROVISIONAL_VERSION"],
                       [The PMIx Standard Provisional ABI compliance level(s)])

    PMIX_REPO_REV="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --repo-rev`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_SUBST(PMIX_REPO_REV)
    AC_DEFINE_UNQUOTED([PMIX_REPO_REV], ["$PMIX_REPO_REV"],
                       [The OpenPMIx Git Revision])

    PMIX_RELEASE_DATE="`$PMIX_top_srcdir/config/pmix_get_version.sh $PMIX_top_srcdir/VERSION --release-date`"
    if test "$?" != "0"; then
        AC_MSG_ERROR([Cannot continue])
    fi
    AC_SUBST(PMIX_RELEASE_DATE)

    # Note that private/config.h *MUST* be listed first so that it
    # becomes the "main" config header file.  Any AC-CONFIG-HEADERS
    # after that (pmix/config.h) will only have selective #defines
    # replaced, not the entire file.
    AC_CONFIG_HEADERS(pmix_config_prefix[src/include/pmix_config.h])


    #
    # Package/brand string
    #
    AC_MSG_CHECKING([if want package/brand string])
    AC_ARG_WITH([pmix-package-string],
         [AS_HELP_STRING([--with-pmix-package-string=STRING],
                         [Use a branding string throughout PMIx])])
    if test "$with_pmix_package_string" = "" || test "$with_pmix_package_string" = "no"; then
        with_package_string="PMIx $PMIX_CONFIGURE_USER@$PMIX_CONFIGURE_HOST Distribution"
    fi
    AC_DEFINE_UNQUOTED([PMIX_PACKAGE_STRING], ["$with_package_string"],
         [package/branding string for PMIx])
    AC_MSG_RESULT([$with_package_string])


    # GCC specifics.
    if test "x$GCC" = "xyes"; then
        PMIX_GCC_CFLAGS="-Wall -Wmissing-prototypes -Wundef"
        PMIX_GCC_CFLAGS="$PMIX_GCC_CFLAGS -Wpointer-arith -Wcast-align"
    fi

    ############################################################################
    # Check for compilers and preprocessors
    ############################################################################
    pmix_show_title "Compiler and preprocessor tests"

    PMIX_SETUP_CC

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
    AC_CHECK_TYPES(__int128)
    AC_CHECK_TYPES(uint128_t)
    AC_CHECK_TYPES(long long)

    AC_CHECK_TYPES(intptr_t)
    AC_CHECK_TYPES(uintptr_t)
    AC_CHECK_TYPES(ptrdiff_t)

    # check for sockaddr_in (a good sign we have TCP)
    # results used in the pif framework
    AC_CHECK_HEADERS([netdb.h netinet/in.h netinet/tcp.h])
    AC_CHECK_TYPES([struct sockaddr_in],
                   [pmix_found_sockaddr=yes],
                   [pmix_found_sockaddr=no],
                   [AC_INCLUDES_DEFAULT
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif])

    #
    # Check for type sizes
    #

    AC_CHECK_SIZEOF(_Bool)
    AC_CHECK_SIZEOF(short)
    AC_CHECK_SIZEOF(int)
    AC_CHECK_SIZEOF(long)
    AC_CHECK_SIZEOF(void *)
    AS_IF([test "$ac_cv_sizeof_void_p" -eq 4],
          [AC_MSG_WARN([PMIx does not support 32 bit builds.])
           AC_MSG_ERROR([Cannot continue])])
    AC_CHECK_SIZEOF(size_t)
    AC_CHECK_SIZEOF(pid_t)

    #
    # Check for type alignments
    #

    AC_CHECK_ALIGNOF(bool, [AC_INCLUDES_DEFAULT
                            #include <stdbool.h>])
    AC_CHECK_ALIGNOF(int)
    AC_CHECK_ALIGNOF(long)
    AC_CHECK_ALIGNOF(size_t)
    if test "$ac_cv_type_long_long" = yes; then
        AC_CHECK_ALIGNOF(long long)
    fi
    AC_CHECK_ALIGNOF(double)

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

    ##################################
    # Only after setting up
    # C do we check compiler attributes.
    ##################################

    pmix_show_subtitle "Compiler characteristics"

    PMIX_CHECK_ATTRIBUTES
    PMIX_CHECK_COMPILER_VERSION_ID

    # OpenPMIx only supports GCC >=v4.8.1.  Notes:
    #
    # 1. The default compiler that comes with RHEL 7 is v4.8.5
    #    (version ID 264197).
    # 2. We regularly test with GCC v4.8.1 (version ID 264193).
    # 3. GCC 4.8.0 probably also works; we just haven't tested it.
    #
    # Since we regularly test with 4.8.1, that's what we check for.
    AS_IF([test "$pmix_cv_compiler_FAMILYNAME" = "GNU" && \
               test "$pmix_cv_compiler_VERSION" -lt 264193],
          [AC_MSG_WARN([OpenPMIx no longer supports versions of the GNU compiler suite])
           AC_MSG_WARN([less than v4.8.1.])
           AC_MSG_WARN([Please upgrade your GNU compiler suite, or use])
           AC_MSG_WARN([a different compiler to build OpenPMIx.])
           AC_MSG_ERROR([Cannot continue])
          ])

    ##################################
    # Assembler Configuration
    ##################################

    pmix_show_subtitle "Atomics"

    PMIX_CONFIG_ASM


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
                      sys/fcntl.h sys/statfs.h sys/statvfs.h \
                      netdb.h ucred.h zlib.h sys/auxv.h \
                      sys/sysctl.h termio.h termios.h pty.h \
                      libutil.h util.h grp.h sys/cdefs.h utmp.h stropts.h \
                      sys/utsname.h stdatomic.h mntent.h])

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
    AC_LINK_IFELSE([AC_LANG_PROGRAM([[
        void finalize (void) {}
        ]])],
        [AC_MSG_RESULT([yes])
         pmix_ld_have_fini=1],
        [AC_MSG_RESULT([no])
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

    # Darwin doesn't need -lutil, as it's something other than this -lutil.
    PMIX_SEARCH_LIBS_CORE([openpty], [util])

    PMIX_SEARCH_LIBS_CORE([gethostbyname], [nsl])

    PMIX_SEARCH_LIBS_CORE([socket], [socket])

    # IRIX and CentOS have dirname in -lgen, usually in libc
    PMIX_SEARCH_LIBS_CORE([dirname], [gen])

    # Darwin doesn't need -lm, as it's a symlink to libSystem.dylib
    PMIX_SEARCH_LIBS_CORE([ceil], [m])

    # -lrt might be needed for clock_gettime
    PMIX_SEARCH_LIBS_CORE([clock_gettime], [rt])

    AC_CHECK_FUNCS([asprintf snprintf vasprintf vsnprintf strsignal socketpair strncpy_s usleep statfs statvfs getpeereid getpeerucred strnlen posix_fallocate tcgetpgrp setpgid ptsname openpty setenv fork execve waitpid atexit])

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

    #
    # Check out what thread support we have
    #
    PMIX_CONFIG_THREADS

    CFLAGS="$CFLAGS $THREAD_CFLAGS"
    CPPFLAGS="$CPPFLAGS $THREAD_CPPFLAGS"
    LDFLAGS="$LDFLAGS $THREAD_LDFLAGS"
    LIBS="$LIBS $THREAD_LIBS"

    PMIX_WRAPPER_FLAGS_ADD([CFLAGS], [$THREAD_CFLAGS])
    PMIX_WRAPPER_FLAGS_ADD([LDFLAGS], [$THREAD_LDFLAGS])

    #
    # What is the local equivalent of "ln -s"
    #

    AC_PROG_LN_S

    # Check for some common system programs that we need
    AC_PROG_GREP
    AC_PROG_EGREP

    # This check must come after PMIX_CONFIG_THREADS
    AC_CHECK_FUNCS([pthread_setaffinity_np])

    # Setup HTML and man page processing
    OAC_SETUP_SPHINX([$srcdir/docs/_build/html/index.html], [],
                     [$srcdir/docs/requirements.txt])

    AS_IF([test -n "$OAC_MAKEDIST_DISABLE"],
          [AS_IF([test -n "$PMIX_MAKEDIST_DISABLE"],
                 [PMIX_MAKEDIST_DISABLE="$PMIX_MAKEDIST_DISABLE $OAC_MAKEDIST_DISABLE"],
                 [PMIX_MAKEDIST_DISABLE=$OAC_MAKEDIST_DISABLE])])
    AS_IF([test -n "$PMIX_MAKEDIST_DISABLE"],
          [AC_MSG_WARN(["make dist" will be disabled due to: $PMIX_MAKEDIST_DISABLE])])
    AC_SUBST([PMIX_MAKEDIST_DISABLE])

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
    pmix_show_title "Event libraries"

    dnl Only one of Libev or Libevent can be used by OpenPMIX.  The
    dnl selection logic for the two is:
    dnl
    dnl   * libev is used if explicitly requested
    dnl   * libevent is used if explicitly requested
    dnl   * if both are explicitly requested, then we report the error
    dnl     and abort
    dnl   * if neither is explicitly requested, then we default to
    dnl     using libevent if it is available. If libevent isn't
    dnl     available, then we see if libev is available.
    dnl
    dnl poking at $with_libevent and $with_libev is a bit of an
    dnl abstraction break, but makes implementing this logic
    dnl significantly easier.
    pmix_libev_support=0
    pmix_libevent_support=0

    AS_IF([test ! -z "$with_libevent" -a "$with_libevent" != "no"],
          [want_libevent=1])
    AS_IF([test ! -z "$with_libev" -a "$with_libev" != "no"],
          [want_libev=1])

    AS_IF([test "$want_libevent" = "1" -a "$want_libev" = "1"],
          [AC_MSG_WARN([Both libevent and libev support have been specified.])
           AC_MSG_WARN([Only one can be configured against at a time. Please])
           AC_MSG_WARN([remove one from the configure command line.])
           AC_MSG_ERROR([Cannot continue])])

    pmix_found_event_lib=0
    dnl If libevent succeeds, then we don't need libev, but we skip
    dnl libevent if libev was explicitly requested.
    AS_IF([test "$want_libev" != "1"],
          [PMIX_LIBEVENT_CONFIG([pmix_found_event_lib=1])])
    AS_IF([test $pmix_found_event_lib -eq 0],
          [PMIX_LIBEV_CONFIG([pmix_found_event_lib=1])])

    dnl The following must _always_ be defined, regardless of which
    dnl event library was selected/requested
    AC_DEFINE_UNQUOTED([PMIX_HAVE_LIBEV], [$pmix_libev_support], [Whether we are building against libev])
    AC_DEFINE_UNQUOTED([PMIX_HAVE_LIBEVENT], [$pmix_libevent_support], [Whether we are building against libevent])

    AS_IF([test $pmix_found_event_lib -eq 0],
          [AC_MSG_WARN([Either libevent or libev support is required, but neither])
           AC_MSG_WARN([was found. Please use the configure options to point us])
           AC_MSG_WARN([to where we can find one or the other library])
           AC_MSG_ERROR([Cannot continue])])


    ##################################
    # HWLOC
    ##################################
    pmix_show_title "HWLOC"

    PMIX_SETUP_HWLOC


    ##################################
    # JANSSON
    ##################################
    pmix_show_title "JANSSON"

    PMIX_CHECK_JANSSON


    ##################################
    # CURL
    ##################################
    pmix_show_title "CURL"

    PMIX_CHECK_CURL

    ##################################
    # MCA
    ##################################

    pmix_show_title "Modular Component Architecture (MCA) setup"

    #
    # Do we want to show component load error messages by default?
    #

    AC_MSG_CHECKING([for default value of mca_base_component_show_load_errors])
    AC_ARG_WITH([show-load-errors],
                [AS_HELP_STRING([--with-show-load-errors],
                                [Set the default value for the MCA
                                parameter
                                mca_base_component_show_load_errors (but
                                can be overridden at run time by the usual
                                MCA-variable-setting mechansism).
                                (default: "all")])])

    if test -z "$with_show_load_errors" || \
       test "$with_show_load_errors" = "yes"; then
        with_show_load_errors=all
        AC_MSG_RESULT([enabled for all (by default)])
    elif test "$with_show_load_errors" = "no"; then
        with_show_load_errors=none
        AC_MSG_RESULT([disabled for all (by default)])
    fi

    AC_DEFINE_UNQUOTED(PMIX_SHOW_LOAD_ERRORS_DEFAULT, ["$with_show_load_errors"],
                       [Default value for mca_base_component_show_load_errors MCA variable])

    AC_MSG_CHECKING([for subdir args])
    PMIX_CONFIG_SUBDIR_ARGS([pmix_subdir_args])
    AC_MSG_RESULT([$pmix_subdir_args])

    PMIX_MCA


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
        cpp_includes="$PMIX_top_builddir $PMIX_top_srcdir $PMIX_top_srcdir/src $PMIX_top_builddir/include"
    else
        cpp_includes="$PMIX_top_srcdir $PMIX_top_srcdir/src"
    fi
    CPP_INCLUDES="$(echo $cpp_includes | $SED 's/[[^ \]]* */'"$pmix_cc_iquote"'&/g')"
    CPPFLAGS="$CPP_INCLUDES -I$PMIX_top_srcdir/include $CPPFLAGS"

    ############################################################################
    # final wrapper compiler config
    ############################################################################
    pmix_show_subtitle "Wrapper compiler final setup"

    # The PMIx wrapper script (i.e., not the C-compiled
    # executables) need perl.
    AC_PATH_PROG(PERL, perl, perl)

    # What's the suffix of shared libraries?  Inspired by generated
    # Libtool code (even though we don't support several of these
    # platforms, there didn't seem to be any harm in leaving in some of
    # them, alhtough I did remove some that we have never/will never
    # support, like OS/2).
    case $host_os in
    mingw* | pw32* | cegcc*)
        PMIX_DYN_LIB_SUFFIX=dll
        ;;
    darwin* | rhapsody*)
        PMIX_DYN_LIB_SUFFIX=dylib
        ;;
    hpux9* | hpux10* | hpux11*)
        case $host_cpu in
            ia64*)
            PMIX_DYN_LIB_SUFFIX=so
          ;;
        *)
            PMIX_DYN_LIB_SUFFIX=sl
            ;;
        esac
        ;;
    *)
       PMIX_DYN_LIB_SUFFIX=so
       ;;
    esac
    AC_SUBST(PMIX_DYN_LIB_SUFFIX)

    # Need the libtool executable before the rpathify stuff
    LT_OUTPUT

    ############################################################################
    # pmixdatadir, pmixlibdir, and pmixinclude are essentially the same as
    # pkg*dir, but will always be */pmix.
    pmixdatadir='${datadir}/pmix'
    pmixlibdir='${libdir}/pmix'
    pmixincludedir='${includedir}/pmix'
    AC_SUBST(pmixdatadir)
    AC_SUBST(pmixlibdir)
    AC_SUBST(pmixincludedir)

    PMIX_SETUP_WRAPPER_FINAL

    # PMIX_DELAYED_LIBS is used to allow us to add some libraries to the build, but
    # not add them to all the tests that are run through configure, since that
    # can cause some bundled build situations.  This is the last minute, so time
    # to add them to LIBS.
    PMIX_FLAGS_APPEND_MOVE([LIBS], [$PMIX_DELAYED_LIBS])

    ############################################################################
    # setup "make check"
    ############################################################################
    PMIX_BUILT_TEST_PREFIX=$PMIX_top_builddir
    AC_SUBST(PMIX_BUILT_TEST_PREFIX)
    # expose the mca component library paths in the build system
    pathfile=$PMIX_top_srcdir/config/mca_library_paths.txt
    PMIX_COMPONENT_LIBRARY_PATHS=`cat $pathfile`
    AC_SUBST(PMIX_COMPONENT_LIBRARY_PATHS)
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests00.pl], [chmod +x test/run_tests00.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests01.pl], [chmod +x test/run_tests01.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests02.pl], [chmod +x test/run_tests02.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests03.pl], [chmod +x test/run_tests03.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests04.pl], [chmod +x test/run_tests04.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests05.pl], [chmod +x test/run_tests05.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests06.pl], [chmod +x test/run_tests06.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests07.pl], [chmod +x test/run_tests07.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests08.pl], [chmod +x test/run_tests08.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests09.pl], [chmod +x test/run_tests09.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests10.pl], [chmod +x test/run_tests10.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests11.pl], [chmod +x test/run_tests11.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests12.pl], [chmod +x test/run_tests12.pl])
    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests13.pl], [chmod +x test/run_tests13.pl])
#    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests14.pl], [chmod +x test/run_tests14.pl])
#    AC_CONFIG_FILES(pmix_config_prefix[test/run_tests15.pl], [chmod +x test/run_tests15.pl])
    if test "$WANT_PYTHON_BINDINGS" = "1"; then
        AC_CONFIG_FILES(pmix_config_prefix[test/python/run_server.sh], [chmod +x test/python/run_server.sh])
        AC_CONFIG_FILES(pmix_config_prefix[test/python/run_sched.sh], [chmod +x test/python/run_sched.sh])
    fi


    ############################################################################
    # final output
    ############################################################################

    pmix_show_subtitle "Final output"

    AC_CONFIG_HEADERS(pmix_config_prefix[include/pmix_common.h])

    AC_CONFIG_FILES(
        pmix_config_prefix[Makefile]
        pmix_config_prefix[bindings/Makefile]
        pmix_config_prefix[bindings/python/Makefile]
        pmix_config_prefix[config/Makefile]
        pmix_config_prefix[etc/Makefile]
        pmix_config_prefix[include/Makefile]
        pmix_config_prefix[src/Makefile]
        pmix_config_prefix[src/class/Makefile]
        pmix_config_prefix[src/include/Makefile]
        pmix_config_prefix[src/util/Makefile]
        pmix_config_prefix[src/util/keyval/Makefile]
        pmix_config_prefix[src/mca/base/Makefile]
        pmix_config_prefix[src/tools/pevent/Makefile]
        pmix_config_prefix[src/tools/pmix_info/Makefile]
        pmix_config_prefix[src/tools/plookup/Makefile]
        pmix_config_prefix[src/tools/pps/Makefile]
        pmix_config_prefix[src/tools/pattrs/Makefile]
        pmix_config_prefix[src/tools/pquery/Makefile]
        pmix_config_prefix[src/tools/wrapper/Makefile]
        pmix_config_prefix[src/tools/wrapper/pmixcc-wrapper-data.txt]
        pmix_config_prefix[src/tools/palloc/Makefile]
        pmix_config_prefix[src/tools/pctrl/Makefile]
        )

    # Success
    $2
])dnl

AC_DEFUN([PMIX_DEFINE_ARGS],[

    # A hint to tell us if we are working with a build from Git or a tarball.
    # Helpful when preparing diagnostic output.
    if test -e $PMIX_TOP_SRCDIR/.git; then
        AC_DEFINE_UNQUOTED([PMIX_GIT_REPO_BUILD], ["1"],
            [If built from a git repo])
        pmix_git_repo_build=yes
    fi

    # do we want dlopen support ?
    AC_MSG_CHECKING([if want dlopen support])
    AC_ARG_ENABLE([dlopen],
        [AS_HELP_STRING([--enable-dlopen],
                        [Whether build should attempt to use dlopen (or
                         similar) to dynamically load components.
                         (default: enabled)])])
    AS_IF([test "$enable_dlopen" = "unknown"],
          [AC_MSG_WARN([enable_dlopen variable has been overwritten by configure])
           AC_MSG_WARN([This is an internal error that should be reported to PMIx developers])
           AC_MSG_ERROR([Cannot continue])])
    AS_IF([test "$enable_dlopen" = "no"],
          [PMIX_ENABLE_DLOPEN_SUPPORT=0
           AC_MSG_RESULT([no])],
          [PMIX_ENABLE_DLOPEN_SUPPORT=1
           AC_MSG_RESULT([yes])])
    AC_DEFINE_UNQUOTED(PMIX_ENABLE_DLOPEN_SUPPORT, $PMIX_ENABLE_DLOPEN_SUPPORT,
                      [Whether we want to enable dlopen support])

#
# Check the OS flavor here
#
OAC_CHECK_OS_FLAVORS

# Debug mode?
AC_MSG_CHECKING([if want pmix maintainer support])
pmix_debug=0
pmix_debug_msg="disabled"
AS_IF([test "$enable_debug" = "yes"],
      [pmix_debug=1
       pmix_debug_msg="enabled"])
# Grr; we use #ifndef for PMIX_DEBUG!  :-(
AH_TEMPLATE(PMIX_ENABLE_DEBUG, [Whether we are in debugging mode or not])
AS_IF([test "$pmix_debug" = "1"], [AC_DEFINE([PMIX_ENABLE_DEBUG])])
AC_MSG_RESULT([$pmix_debug_msg])
AC_MSG_CHECKING([for pmix directory prefix])
AC_MSG_RESULT(m4_ifval([$1], pmix_config_prefix, [(none)]))

#
# Developer picky compiler options
#

AC_MSG_CHECKING([if want developer-level compiler pickyness])
AC_ARG_ENABLE(devel-check,
    AS_HELP_STRING([--enable-devel-check],
                   [enable developer-level compiler pickyness when building PMIx (default: disabled)]))
if test "$enable_devel_check" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PICKY_COMPILER=1
elif test "$enable_devel_check" = "no"; then
    AC_MSG_RESULT([no])
    WANT_PICKY_COMPILER=0
elif test "$pmix_git_repo_build" = "yes" && test "$pmix_debug" = "1"; then
    AC_MSG_RESULT([yes])
    WANT_PICKY_COMPILER=1
else
    AC_MSG_RESULT([no])
    WANT_PICKY_COMPILER=0
fi

AC_DEFINE_UNQUOTED(PMIX_PICKY_COMPILERS, $WANT_PICKY_COMPILER,
                   [Whether or not we are using picky compiler settings])

AC_MSG_CHECKING([if want memory sanitizers])
AC_ARG_ENABLE(memory-sanitizers,
    AS_HELP_STRING([--memory-sanitizers],
                   [enable developer-level memory sanitizers when building PMIx (default: disabled)]))
if test "$enable_memory_sanitizers" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_MEMORY_SANITIZERS=1
    AC_MSG_WARN([******************************************************])
    AC_MSG_WARN([**** Memory sanitizers may require that you LD_PRELOAD])
    AC_MSG_WARN([**** libasan in order to run an executable.])
    AC_MSG_WARN([******************************************************])
else
    AC_MSG_RESULT([no])
    WANT_MEMORY_SANITIZERS=0
fi

AC_DEFINE_UNQUOTED(PMIX_MEMORY_SANITIZERS, $WANT_MEMORY_SANITIZERS,
                   [Whether or not we are using memory sanitizers])

#
# Developer debugging
#

AC_MSG_CHECKING([if want developer-level debugging code])
AC_ARG_ENABLE(debug,
    AS_HELP_STRING([--enable-debug],
                   [enable developer-level debugging code (not for general PMIx users!) (default: disabled)]))
if test "$enable_debug" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_DEBUG=1
else
    AC_MSG_RESULT([no])
    WANT_DEBUG=0
fi

if test "$WANT_DEBUG" = "0"; then
    CFLAGS="-DNDEBUG $CFLAGS"
fi

AC_DEFINE_UNQUOTED(PMIX_ENABLE_DEBUG, $WANT_DEBUG,
                   [Whether we want developer-level debugging code or not])

AC_ARG_ENABLE(debug-symbols,
              AS_HELP_STRING([--disable-debug-symbols],
                             [Disable adding compiler flags to enable debugging symbols if --enable-debug is specified.  For non-debugging builds, this flag has no effect.]))

AC_MSG_CHECKING([if want to install PMIx header files])
AC_ARG_WITH(pmix-headers,
    AS_HELP_STRING([--with-pmix-headers],
                   [Install the PMIx header files (pmix.h and friends) (default: enabled)]))
if test "$with_pmix_headers" != "no"; then
    AC_MSG_RESULT([yes])
    WANT_PRIMARY_HEADERS=1
    pmix_install_primary_headers=yes
else
    AC_MSG_RESULT([no])
    WANT_PRIMARY_HEADERS=0
    pmix_install_primary_headers=no
fi

# Install tests and examples?
AC_MSG_CHECKING([if tests and examples are to be installed])
AC_ARG_WITH([tests-examples],
    [AS_HELP_STRING([--with-tests-examples],
            [Whether or not to install the tests and example programs.])])
AS_IF([test "$pmix_install_primary_headers" = "no"],
      [AS_IF([test -z "$with_tests_examples" || test "$with_tests_examples" = "no"],
             [pmix_tests=no
              AC_MSG_RESULT([no])],
             [AC_MSG_RESULT([no])
              AC_MSG_WARN([Cannot install tests/examples without installing primary headers.])
              AC_MSG_ERROR([Please correct the configure line and retry])])],
      [AS_IF([test ! -z "$with_tests_examples" && test "$with_tests_examples" = "no"],
             [pmix_tests=no
              AC_MSG_RESULT([no])],
             [pmix_tests=yes
              AC_MSG_RESULT([yes])])])

#
# Support per-user config files?
#
AC_ARG_ENABLE([per-user-config-files],
   [AS_HELP_STRING([--enable-per-user-config-files],
      [Disable per-user configuration files, to save disk accesses during job start-up.  This is likely desirable for large jobs.  Note that this can also be achieved by environment variables at run-time.  (default: enabled)])])
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
              [AS_HELP_STRING([--enable-pretty-print-stacktrace],
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
#
# Ident string
#
AC_MSG_CHECKING([if want ident string])
AC_ARG_WITH([ident-string],
            [AS_HELP_STRING([--with-ident-string=STRING],
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
AC_ARG_ENABLE(pmix-timing,
              AS_HELP_STRING([--enable-pmix-timing],
                             [enable PMIx developer-level timing code (default: disabled)]))
if test "$enable_pmix_timing" = "yes"; then
    AC_MSG_RESULT([yes])
    WANT_PMIX_TIMING=1
else
    AC_MSG_RESULT([no])
    WANT_PMIX_TIMING=0
fi

AC_DEFINE_UNQUOTED([PMIX_ENABLE_TIMING], [$WANT_PMIX_TIMING],
                   [Whether we want developer-level timing support or not])

#
# Do we want to install binaries?
#
AC_MSG_CHECKING([if want to disable binaries])
AC_ARG_ENABLE(pmix-binaries,
              AS_HELP_STRING([--enable-pmix-binaries],
                             [enable PMIx tools]))
if test "$enable_pmix_binaries" = "no"; then
    AC_MSG_RESULT([no])
    WANT_PMIX_BINARIES=0
else
    AC_MSG_RESULT([yes])
    WANT_PMIX_BINARIES=1
fi

AM_CONDITIONAL([PMIX_INSTALL_BINARIES], [test $WANT_PMIX_BINARIES -eq 1])

#
# Install Python bindings?
#
AC_MSG_CHECKING([if want install Python bindings])
AC_ARG_ENABLE(python-bindings,
              AS_HELP_STRING([--enable-python-bindings],
                             [enable Python bindings (default: disabled)]))
if test "$enable_python_bindings" != "yes"; then
    AC_MSG_RESULT([no])
    WANT_PYTHON_BINDINGS=0
else
    AC_MSG_RESULT([yes])
    # cannot build Python bindings if we are doing a purely static build
    if test "$enable_shared" = "no"; then
        AC_MSG_WARN([Python bindings cannot be built in purely])
        AC_MSG_WARN([static configurations. Please either enable])
        AC_MSG_WARN([shared libraries or remove the request to])
        AC_MSG_WARN([build the Python bindings])
        AC_MSG_ERROR([Cannot continue])
    fi
    WANT_PYTHON_BINDINGS=1
    AM_PATH_PYTHON([3.4], [pmix_python_good=yes], [pmix_python_good=no])
fi

AM_CONDITIONAL([WANT_PYTHON_BINDINGS], [test $WANT_PYTHON_BINDINGS -eq 1])
PYTHON=
if test "$WANT_PYTHON_BINDINGS" = "1"; then
    if test "$pmix_python_good" = "no"; then
        AC_MSG_WARN([Python bindings were enabled, but no suitable])
        AC_MSG_WARN([interpreter was found. PMIx requires at least])
        AC_MSG_WARN([Python v3.4 to provide Python bindings])
        AC_MSG_ERROR([Cannot continue])
    fi
    pyvers=`python3 --version`
    python_version=${pyvers#"Python "}

    PMIX_SUMMARY_ADD([Bindings], [Python], [], [yes ($python_version)])

    AC_MSG_CHECKING([if Cython package installed as Python package])
    have_cython=`$srcdir/config/pmix_check_cython.py 2> /dev/null`
    if test "$have_cython" = "0"; then
        AC_MSG_RESULT([yes])
        AC_MSG_CHECKING([Cython version])
        cython_version=`python -c "from Cython.Compiler.Version import version; print(version)"`
        AC_MSG_RESULT([$cython_version])
        PMIX_SUMMARY_ADD([Required Packages], [Cython], [], [yes ($cython_version)])
    else
        AC_MSG_RESULT([no])
        # Cython doesn't have any include or lib files - it is just a binary
        AC_CHECK_PROG(pmix_cython_rpm, cython, [cython])
        if test "$pmix_cython_rpm" != ""; then
            AC_MSG_CHECKING([Cython version])
            cyvers=`cython --version 2>&1`
            cython_version=${cyvers#"Cython version "}
            AC_MSG_RESULT([$cython_version])
            PMIX_SUMMARY_ADD([Bindings], [Cython], [], [yes ($cython_version)])
        else
            AC_MSG_WARN([Python bindings were enabled, but the Cython])
            AC_MSG_WARN([package was not found. PMIx Python bindings])
            AC_MSG_WARN([require that the Cython package be installed])
            AC_MSG_ERROR([Cannot continue])
        fi
    fi

    pmix_pythondir=`eval echo $pythondir`
    AC_SUBST([PMIX_PYTHON_EGG_PATH], [$pmix_pythondir], [Path to installed Python egg])
fi

# If we didn't find a good Python and we don't have dictionary.h, then
# see if we can find an older Python (because construct_dictionary.py
# can use an older Python).
AS_IF([test "$PYTHON" = "" && test ! -f $srcdir/include/dictionary.h],
      [AC_MSG_CHECKING([python])
       PYTHON=
       AM_PATH_PYTHON
       # If we still can't find Python (and we don't have
       # dictionary.h), then give up.
       AC_MSG_RESULT([$PYTHON])
       AS_IF([test "$PYTHON" = ""],
             [AC_MSG_WARN([Could not find a modern enough Python])
              AC_MSG_WARN([Developer builds (e.g., git clones) of OpenPMIx must have Python available])
              AC_MSG_ERROR([Cannot continue])
             ])
       ])

# see if they want to disable non-RTLD_GLOBAL dlopen
AC_MSG_CHECKING([if want to support dlopen of non-global namespaces])
AC_ARG_ENABLE([nonglobal-dlopen],
              AS_HELP_STRING([--enable-nonglobal-dlopen],
                             [enable non-global dlopen (default: enabled)]))
if test "$enable_nonglobal_dlopen" = "no"; then
    AC_MSG_RESULT([no])
    pmix_need_libpmix=0
else
    AC_MSG_RESULT([yes])
    pmix_need_libpmix=1
fi

#
# Do we want PTY support?
#

AC_MSG_CHECKING([if want pty support])
AC_ARG_ENABLE(pty-support,
    AS_HELP_STRING([--enable-pty-support],
                   [Enable/disable PTY support for STDIO forwarding.  (default: enabled)]))
if test "$enable_pty_support" = "no" ; then
    AC_MSG_RESULT([no])
    PMIX_ENABLE_PTY_SUPPORT=0
else
    AC_MSG_RESULT([yes])
    PMIX_ENABLE_PTY_SUPPORT=1
fi
AC_DEFINE_UNQUOTED([PMIX_ENABLE_PTY_SUPPORT], [$PMIX_ENABLE_PTY_SUPPORT],
                   [Whether user wants PTY support or not])

#
# psec/dummy_handshake
#

AC_MSG_CHECKING([if want build psec/dummy_handshake])
AC_ARG_ENABLE(dummy-handshake,
              AS_HELP_STRING([--enable-dummy-handshake],
                             [Enables psec dummy component intended to check the PTL handshake scenario (default: disabled)]))
if test "$enable_dummy_handshake" != "yes"; then
    AC_MSG_RESULT([no])
    eval "DISABLE_psec_dummy_handshake=1"
else
    AC_MSG_RESULT([yes])
    eval "DISABLE_psec_dummy_handshake=0"
fi
AM_CONDITIONAL(MCA_BUILD_PSEC_DUMMY_HANDSHAKE, test "$DISABLE_psec_dummy_handshake" = "0")

#
# Do we want to enable IPv6 support?
#
AC_MSG_CHECKING([if want IPv6 support])
AC_ARG_ENABLE([ipv6],
    [AS_HELP_STRING([--enable-ipv6],
        [Enable IPv6 support, but only if the underlying system supports it (default: disabled)])])
if test "$enable_ipv6" = "yes"; then
    AC_MSG_RESULT([yes])
    pmix_want_ipv6=1
else
    AC_MSG_RESULT([no])
    pmix_want_ipv6=0
fi
AC_DEFINE_UNQUOTED([PMIX_ENABLE_IPV6], [$pmix_want_ipv6],
                   [Enable IPv6 support, but only if the underlying system supports it])


])dnl

# This must be a standalone routine so that it can be called both by
# PMIX_INIT and an external caller (if PMIX_INIT is not invoked).
AC_DEFUN([PMIX_DO_AM_CONDITIONALS],[
    AS_IF([test "$pmix_did_am_conditionals" != "yes"],[
        AM_CONDITIONAL([PMIX_TESTS_EXAMPLES], [test "x$pmix_tests" = "xyes"])
        AM_CONDITIONAL([PMIX_COMPILE_TIMING], [test "$WANT_TIMING" = "1"])
        AM_CONDITIONAL([PMIX_WANT_MUNGE], [test "$pmix_munge_support" = "1"])
        AM_CONDITIONAL([PMIX_WANT_SASL], [test "$pmix_sasl_support" = "1"])
        AM_CONDITIONAL([WANT_PRIMARY_HEADERS], [test "x$pmix_install_primary_headers" = "xyes"])
        AM_CONDITIONAL(NEED_LIBPMIX, [test "$pmix_need_libpmix" = "1"])
        AM_CONDITIONAL([PMIX_HAVE_JANSSON], [test "x$pmix_check_jansson_happy" = "xyes"])
        AM_CONDITIONAL([PMIX_HAVE_CURL], [test "x$pmix_check_curl_happy" = "xyes"])
    ])
    pmix_did_am_conditionals=yes
])dnl
