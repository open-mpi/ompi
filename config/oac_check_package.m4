dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.
dnl                         All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl The macros in this file depend on 3 helper macros being defined.  These are
dnl likely similar to existing macros, so are not defined here with the
dnl expectation the caller will provide them (likely through an m4_copy).  The
dnl three macros are:
dnl  - OAC_LOG_COMMAND([command], [action-if-success], [action-if-fail])
dnl  - OAC_LOG_MSG([msg], [non-empty-string if prefix])
dnl  - OAC_APPEND([variable], [data to append])
dnl
dnl In PMIX, for example, the following would provide the required behavior:
dnl   m4_copy([PMIX_LOG_COMMAND], [OAC_LOG_COMMAND])
dnl   m4_copy([PMIX_LOG_MSG], [OAC_LOG_MSG])
dnl   m4_copy([PMIX_APPEND], [OAC_APPEND])


dnl OAC_CHECK_PACKAGE: Search for the availability of a package
dnl
dnl 1 -> package name
dnl 2 -> prefix value
dnl 3 -> headers (space separated list)
dnl 4 -> libraries (space separated list)
dnl 5 -> function name
dnl 6 -> action if found
dnl 7 -> action if not found
dnl
dnl OAC_CHECK_PACKAGE has an argument length problem.  Technically, M4
dnl macros may only have 9 arguments, as argument values must be in the
dnl form of $X, where X is a single digit.  This means we do some argument
dnl compression (life would be easier if the libraries and headers were
dnl split into primary and support) and use the M4 environment for passing
dnl some infrequently used arguments.
dnl
dnl OAC_CHECK_PACKAGE tries to find the correct CPPFLAGS, LDFLAGS, and libraries
dnl to use a particular package.  It then verifies that the header is available
dnl via AC_CHECK_HEADERS and that the function specified is available via
dnl AC_CHECK_FUNC using the specified flags.  To find the flags and libraries,
dnl OAC_CHECK_PACKAGE follows a 4 step search path:
dnl
dnl   1. If <package name>.pc is available to package config, the package config
dnl      data is used.
dnl   2. If a OMPI-style wrapper compiler is found, the information from the
dnl      wrapper compiler is used (NOTE: THIS IS OFF BY DEFAULT)
dnl   3. If with_<package> and/or with_<package>_libdir are specified, the
dnl      filesystem is examined to look for the specified header and library
dnl      in the specified path.
dnl   4. We try to find the specified header and function with no change
dnl      in CPPFLAGS or LDFLAGS and adding the specified libraries to LIBS.
dnl
dnl It is the resposibility of the caller to register arguments of the form
dnl with-<package name>, with-<package name>-libdir, and with-package name>-incdir.
dnl All three are optional, nothing will break if the caller doesn't specify them
dnl (and indeed, if the package being searched for isn't libnl3, it's likely the
dnl with-<package name>-incdir is a complete waste of energe).
dnl
dnl By default, OAC_CHECK_PACKAGE will use <package name> for the module name to specify
dnl to pkg-config, meaning there is a <package name>.pc in the filesystem.  If a
dnl different module name should be used, add a macro to the M4 environment named
dnl <package name>_pkgconfig_module with the value of the pkgconfig module name
dnl to use.  For exmaple, if the libevent module name is libevent_core, you could
dnl specify:
dnl
dnl    m4_define([libevent_pkgconfig_module], [libevent_core])
dnl
dnl Similarly, by default, OAC_CHECK_PACKAGE will use <package name>cc for the name
dnl of the wrapper compiler to investigate.  This can be modified with the
dnl <package name>_wrapper_compiler macro in the m4 environment.
dnl
dnl Using pkg-config is on by default and using the wrapper compilers is off by
dnl default.  The use of either can be controlled by setting the SHELL environment
dnl variables <package name>_USE_PKG_CONFIG and <package name>_USE_WRAPPER_COMPILER
dnl to 0 (to explicitly disable) or 1 (to explicitly enable).
dnl
dnl On return, <action if found> will be evaluated if it appears that the package is
dnl available.  <action if not found> will be evaluated if it appears that the package
dnl is not available.  If it appears the package is available, the following SHELL
dnl environment variables will be set:
dnl
dnl   <prefix>_CPPFLAGS - CPPFLAGS to add when compiling sources depending on the package
dnl   <prefix>_LDFLAGS - LDFLAGS to add when linking against the package
dnl   <prefix>_STATIC_LDFLAGS - LDFLAGS to add when linking against the package when
dnl                          building a statically linked executable.
dnl   <prefix>_LIBS - Libraries to link to access the package
dnl   <prefix>_STATIC_LIBS - Libraries to link to access the package when building a
dnl                          statically linked executable.
dnl   <prefix>_PC_MODULES - Module name of the pkgconfig module used to generate
dnl                          the build information.  Will be unset by OAC_CHECK_PACKAGE
dnl                          if pkg-config was not used to configure the package.  Note
dnl                          that there is no need for a STATIC_PC_MODULES option,
dnl                          as that functionality is built into pkgconfig modules
dnl                          directly.
dnl   <prefix>_SUMMARY - A summary of the check package output, suitable for inclusion
dnl                          in a configure summary table.  Will start with yes/no.
dnl   <prefix>_DETECT_METHOD - The method used to find the right flags.  Will be one of
dnl                          'pkg-config', 'wrapper compiler', or empty string
dnl
dnl Note that STATIC_LIBS and STATIC_LDFLAGS should not be added to
dnl LIBS and LDFLAGS unnecessarily.  Even if the library being built
dnl is being built as a static library, that does not mean adding
dnl STATIC_LIBS to LIBS is the right call.  Only when the executable
dnl is only linked against static libraries should STATIC_LIBS be
dnl added to LIBS.
AC_DEFUN([OAC_CHECK_PACKAGE],[
# ****************************** START CHECK PACKAGE FOR $1 ******************************
    AC_REQUIRE([OAC_CHECK_PACKAGE_STATIC_CHECK])

    check_package_$2_save_CPPFLAGS="${CPPFLAGS}"
    check_package_$2_save_LDFLAGS="${LDFLAGS}"
    check_package_$2_save_LIBS="${LIBS}"

    $2_CPPFLAGS=
    $2_LDFLAGS=
    $2_STATIC_LDFLAGS=
    $2_LIBS=
    $2_STATIC_LIBS=
    AS_UNSET([$2_PC_MODULES])

    check_package_happy=1
    check_package_have_flags=0
    check_package_type=

    # build a sane environment
    AS_IF([test "$with_$1" = "no"],
          [AC_MSG_NOTICE([Package $1 disabled by user])
           check_package_happy=0],
          [test "${with_$1}" = "yes"],
          [check_package_prefix=],
          [check_package_prefix="${with_$1}"])
    check_package_libdir=
    AS_IF([test "${with_$1_libdir}" = "no" -o "${with_$1_libdir}" = "yes"],
          [AC_MSG_ERROR(["yes" or "no" are not valid arguments for --with-$1-libdir])],
          [test -n "${with_$1_libdir}"],
          [check_package_libdir="${with_$1_libdir}"])
    check_package_incdir=
    AS_IF([test "${with_$1_incdir}" = "no" -o "${with_$1_incdir}" = "yes"],
          [AC_MSG_ERROR(["yes" or "no" are not valid arguments for --with-$1-incdir])],
          [test -n "${with_$1_incdir}"],
          [check_package_incdir="${with_$1_incdir}"])

    AS_IF([test ${check_package_happy} -eq 1 -a ${check_package_have_flags} -eq 0],
          [_OAC_CHECK_PACKAGE_PKGCONFIG([$1], [$2],
                [check_package_type="pkg-config"
                 check_package_have_flags=1])])

    AS_IF([test ${check_package_happy} -eq 1 -a ${check_package_have_flags} -eq 0],
          [_OAC_CHECK_PACKAGE_WRAPPER_COMPILER([$1], [$2],
                [check_package_type="wrapper compiler"
                 check_package_have_flags=1])])

    AS_IF([test ${check_package_happy} -eq 1 -a ${check_package_have_flags} -eq 0],
          [_OAC_CHECK_PACKAGE_GENERIC([$1], [$2], [$3], [$4],
                [check_package_type=""
                 check_package_have_flags=1])])

    AS_IF([test ${check_package_have_flags} -eq 0], [check_package_happy=0])

    AS_IF([test ${check_package_happy} -eq 1 -a "${check_package_cv_static_linker_flag}" = "yes"],
          [AC_MSG_NOTICE([Copying STATIC_LIBS and STATIC_LDFLAGS to LIBS and LDFLAGS because static linking])
           OAC_APPEND([$2_LDFLAGS], [${$2_STATIC_LDFLAGS}])
           OAC_APPEND([$2_LIBS], [${$2_STATIC_LIBS}])])

    AS_IF([test ${check_package_happy} -eq 1],
          [_OAC_CHECK_PACKAGE_VERIFY([$1], [$2], [$3], [$5],
                                 [check_package_happy=1], [check_package_happy=0])])

    $2_DETECT_METHOD="${check_package_type}"
    AS_IF([test -n "${check_package_type}"],
          [check_package_type="${check_package_type}: "])

    AS_IF([test ${check_package_happy} -eq 1],
          [AS_IF([test -z "${check_package_prefix}"],
                 [$2_SUMMARY="yes (${check_package_type}default search paths)"],
                 [$2_SUMMARY="yes (${check_package_type}${check_package_prefix})"])
           $6],
          [AS_IF([test "${with_$1}" = "no"],
                 [$2_SUMMARY="no (explicitly disabled)"],
                 [$2_SUMMARY="no (not found)"])
           AS_UNSET([$2_CPPFLAGS])
           AS_UNSET([$2_LDFLAGS])
           AS_UNSET([$2_STATIC_LDFLAGS])
           AS_UNSET([$2_LIBS])
           AS_UNSET([$2_STATIC_LIBS])
           $7])

    CPPFLAGS="${check_package_$2_save_CPPFLAGS}"
    LDFLAGS="${check_package_$2_save_LDFLAGS}"
    LIBS="${check_package_$2_save_LIBS}"

    AS_UNSET([check_package_$2_save_CPPFLAGS])
    AS_UNSET([check_package_$2_save_LDFLAGS])
    AS_UNSET([check_package_$2_save_LIBS])
    AS_UNSET([check_package_happy])
    AS_UNSET([check_package_have_flags])
    AS_UNSET([check_package_prefix])
    AS_UNSET([check_package_libdir])
    AS_UNSET([check_package_incdir])
    AS_UNSET([check_package_pcfilename])

# ****************************** END CHECK PACKAGE FOR $1 ******************************
])


dnl Retrieve arguments from pkg-config file
dnl
dnl 1 -> package name
dnl 2 -> prefix
dnl 3 -> pcfile name (may be full path)
dnl 4 -> action if found
dnl 5 -> action if not found
dnl
dnl Read pkgconfig module $3 and set build variables based on return
dnl value.  Results are cached based on the value in $1, even if the
dnl pkgconfig module name ($3) changes and that this macro is expanded
dnl inside OAC_CHECK_PACKAGE, which can pollute the results cache.
dnl
dnl On return, <action if found> will be evaluated if it appears that
dnl the pkg-config data is available.  <action if not found> will be
dnl evaluated if it appears that the package is not available.  If it
dnl appears the package is available, the following SHELL environment
dnl variables will be set:
dnl
dnl   <prefix>_CPPFLAGS - CPPFLAGS to add when compiling sources depending on the package
dnl   <prefix>_LDFLAGS - LDFLAGS to add when linking against the package
dnl   <prefix>_STATIC_LDFLAGS - LDFLAGS to add when linking against the package when
dnl                          building a statically linked executable.
dnl   <prefix>_LIBS - Libraries to link to access the package
dnl   <prefix>_STATIC_LIBS - Libraries to link to access the package when building a
dnl                          statically linked executable.
dnl   <prefix>_PC_MODULES - Module name of the pkgconfig module used to generate
dnl                          the build information.  Will be unset by OAC_CHECK_PACKAGE
dnl                          if pkg-config was not used to configure the package.  Note
dnl                          that there is no need for a STATIC_PC_MODULES option,
dnl                          as that functionality is built into pkgconfig modules
dnl                          directly.
AC_DEFUN([OAC_CHECK_PACKAGE_PARSE_PKGCONFIG], [
    AC_REQUIRE([_OAC_CHECK_PACKAGE_PKGCONFIG_INIT])

    AC_CACHE_CHECK([if $1 pkg-config module exists],
         [check_package_cv_$1_pkg_config_exists],
         [_OAC_CHECK_PACKAGE_PKGCONFIG_RUN([$3], [--exists], [check_package_pkgconfig_internal_result],
                    [$2_PC_MODULES=$3
                     check_package_cv_$1_pkg_config_exists=yes],
                    [check_package_cv_$1_pkg_config_exists=no])])

    # if pkg-config --exists works, but getting one of the standard flags fails, we consider
    # that a hard failure.  It should not happen, outside of a weird system configuration
    # issue where we're probably not going to like the results anyway.
    AS_IF([test "${check_package_cv_$1_pkg_config_exists}" = "yes"],
          [AC_CACHE_CHECK([for $1 pkg-config cflags],
                [check_package_cv_$1_pkg_config_cppflags],
                [_OAC_CHECK_PACKAGE_PKGCONFIG_RUN([$3], [--cflags],
                      [check_package_cv_$1_pkg_config_cppflags], [],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 cppflags from pkg-config])])])
           $2_CPPFLAGS="${check_package_cv_$1_pkg_config_cppflags}"

           AC_CACHE_CHECK([for $1 pkg-config ldflags],
                [check_package_cv_$1_pkg_config_ldflags],
                [_OAC_CHECK_PACKAGE_PKGCONFIG_RUN([$3], [--libs-only-L --libs-only-other],
                      [check_package_cv_$1_pkg_config_ldflags], [],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 ldflags from pkg-config])])])
           $2_LDFLAGS="${check_package_cv_$1_pkg_config_ldflags}"

           AC_CACHE_CHECK([for $1 pkg-config static ldflags],
                [check_package_cv_$1_pkg_config_static_ldflags],
                [_OAC_CHECK_PACKAGE_PKGCONFIG_RUN([$3], [--static --libs-only-L --libs-only-other],
                      [check_package_cv_$1_pkg_config_static_ldflags], [],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 static ldflags from pkg-config])])])
           $2_STATIC_LDFLAGS="${check_package_cv_$1_pkg_config_static_ldflags}"

           AC_CACHE_CHECK([for $1 pkg-config libs],
                [check_package_cv_$1_pkg_config_libs],
                [_OAC_CHECK_PACKAGE_PKGCONFIG_RUN([$3], [--libs-only-l],
                      [check_package_cv_$1_pkg_config_libs], [],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 libs from pkg-config])])])
           $2_LIBS="${check_package_cv_$1_pkg_config_libs}"

           AC_CACHE_CHECK([for $1 pkg-config static libs],
                [check_package_cv_$1_pkg_config_static_libs],
                [_OAC_CHECK_PACKAGE_PKGCONFIG_RUN([$3], [--static --libs-only-l],
                      [check_package_cv_$1_pkg_config_static_libs], [],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 libs from pkg-config])])])
           $2_STATIC_LIBS="${check_package_cv_$1_pkg_config_static_libs}"

           $4])

    AS_UNSET([check_package_pkgconfig_internal_result])
])


AC_DEFUN([OAC_CHECK_PACKAGE_STATIC_CHECK], [
    AC_CACHE_CHECK([for static linker flag],
        [check_package_cv_static_linker_flag],
        [check_package_cv_static_linker_flag="no"
         for arg in ${CFLAGS} ${LDFLAGS} ; do
             if test "${arg}" = "-static" -o \
                     "${arg}" = "--static" -o \
                     "${arg}" = "-Bstatic" -o \
                     "${arg}" = "-Wl,-static" -o \
                     "${arg}" = "-Wl,--static" -o \
                     "${arg}" = "-Wl,-Bstatic" ; then
                 check_package_cv_static_linker_flag="yes"
             fi
         done])
])


dnl Invalidate generic cached results (should rarely be needed)
dnl
dnl 1 -> package name
dnl 2 -> prefix value
dnl 3 -> headers (space separated list)
dnl 4 -> function name
dnl
dnl Rarely, packages change linking or in some other way make it
dnl difficult to determine all the correct arguments for
dnl OAC_CHECK_PACKAGE in one try.  The TM interface is a good example
dnl of this, which has changed the name of the library (or its
dnl dependencies) throughtout the years.  Because OAC_CHECK_PACKAGE
dnl makes heavy use of caching (yay!), it is generally not useful to
dnl call OAC_CHECK_PACKAGE multiple times with the same package name,
dnl but different arguments.  This macro may be expanded between calls
dnl to invalidate the caching for the generic (no pkg-config or
dnl wrapper config found) case.
AC_DEFUN([OAC_CHECK_PACKAGE_INVALIDATE_GENERIC_CACHE], [
    dnl today, all we cache in the generic case is the header and func libs
    check_package_verify_search_header=`echo "$3" | cut -f1 -d' '`
    AS_UNSET([ac_cv_header_]AS_TR_SH([${check_package_verify_search_header}]))
    AS_UNSET([ac_cv_func_$4])
    AS_UNSET([check_package_verify_search_header])
])


dnl OAC_CHECK_PACKAGE_VERIFY_COMMANDS - macros to expand during
dnl   verification we have a working package
dnl
dnl 1 -> macro name (must be double quoted)
dnl
dnl If extra verification is required (such as the libnl1 vs. libnl3 disaster),
dnl callers (like the libnl verification code) can register a hook for
dnl every time OAC_CHECK_PACKAGE verifies that a package actually links.  This
dnl check will only be run after it is verified that the header can be found
dnl and that the function specified is found when linking.
dnl
dnl The macro specified must take 6 arguments:
dnl     1 -> package name
dnl     2 -> prefix
dnl     3 -> headers (space separated list)
dnl     4 -> function name
dnl     5 -> action if found
dnl     6 -> action if not found
dnl
dnl The CPPFLAGS / LDFLAGS / LIBS can be retrieved via ${$2_CPPFLAGS},
dnl ${$2_LDFLAGS}, and ${$2_LIBS}, respectively.
dnl
dnl Note that, because M4 really likes to expand macro names, the argument
dnl to OAC_CHECK_PACKAGE_VERIFY_COMMANDS must be overquoted.  That is,
dnl if the macro name to be called is LIBNL_PACKAGE_VERIFY, the call to
dnl register should be:
dnl
dnl    OAC_CHECK_PACKAGE_VERIFY_COMMANDS([[LIBNL_PACKAGE_VERIFY]])
dnl
dnl If you see the macro being invoked without arguments, that almost certainly
dnl means you forgot to double quote.
AC_DEFUN([OAC_CHECK_PACKAGE_VERIFY_COMMANDS],
[m4_append([OAC_CHECK_PACKAGE_VERIFY_COMMAND_LIST], m4_dquote([$1]), [,])])


dnl ************************************* PKG-CONFIG *************************************


dnl no arguments; here for an AC_REQUIRE to set $PKG_CONFIG
AC_DEFUN([_OAC_CHECK_PACKAGE_PKGCONFIG_INIT], [
  AC_CHECK_PROG([PKG_CONFIG], [pkg-config], [pkg-config])
])


dnl 1 -> package
dnl 2 -> prefix
dnl 3 -> action if found flags
AC_DEFUN([_OAC_CHECK_PACKAGE_PKGCONFIG], [
    m4_ifdef([$1_pkgconfig_module],
             [m4_define([pcname], [$1_pkgconfig_module])],
             [m4_define([pcname], [$1])])
    AS_IF([test "${$1_USE_PKG_CONFIG}" != "0"],
          [# search for the package using pkg-config.  If the user provided a
           # --with-$1 or --with-$1-libdir argument, be explicit about where
           # we look for the pkg-config file, so we don't find the wrong one.
           # If they specified --with-$1 only, we look in
           # prefix/lib64/pkgconfig and if we don't find a file there, assume
           # prefix/lib is the right answer.
          AC_CACHE_CHECK([for $1 pkg-config name],
               [check_package_cv_$1_pcfilename],
               [check_package_cv_$1_pcfilename="pcname"
                AS_IF([test -n "${check_package_libdir}"],
                      [check_package_cv_$1_pcfilename="${check_package_libdir}/pkgconfig/pcname.pc"],
                      [test -z "${check_package_prefix}"],
                      [check_package_cv_$1_pcfilename="pcname"],
                      [test -r "${check_package_prefix}/lib/pkgconfig/pcname.pc" -a -r "${check_package_prefix}/lib64/pkgconfig/pcname.pc"],
                      [AC_MSG_ERROR([Found pcname in both ${check_package_prefix}/lib/pkgconfig and
${check_package_prefix}/lib64/pkgconfig.  This is confusing.  Please add --with-$1-libdir=PATH
to configure to help disambiguate.])],
                      [test -r "${check_package_prefix}/lib64/pkgconfig/pcname.pc"],
                      [check_package_cv_$1_pcfilename="${check_package_prefix}/lib64/pkgconfig/pcname.pc"],
                      [check_package_cv_$1_pcfilename="${check_package_prefix}/lib/pkgconfig/pcname.pc"])])
         OAC_CHECK_PACKAGE_PARSE_PKGCONFIG([$1], [$2], [${check_package_cv_$1_pcfilename}], [$3])])
])


dnl 1 -> pc module/filename
dnl 2 -> argument string
dnl 3 -> result assignment string
dnl 4 -> action if found
dnl 5 -> action if not found
AC_DEFUN([_OAC_CHECK_PACKAGE_PKGCONFIG_RUN], [
  check_package_pkgconfig_run_happy=no
  AS_IF([test -n "${PKG_CONFIG}"],
        [OAC_LOG_COMMAND([check_package_pkgconfig_run_results=`${PKG_CONFIG} $2 $1 2>&1`],
             [AS_VAR_COPY([$3], [check_package_pkgconfig_run_results])
              check_package_pkgconfig_run_happy=yes])
         OAC_LOG_MSG([pkg-config output: ${check_package_pkgconfig_run_results}], [1])])
  AS_IF([test "${check_package_pkgconfig_run_happy}" = "yes"], [$4], [$5])
  AS_UNSET([check_package_pkgconfig_run_results])
  AS_UNSET([check_package_pkgconfig_run_happy])
])


dnl ************************************* WRAPPER COMPILERS *************************************


dnl 1 -> package name
dnl 2 -> prefix
dnl 3 -> action if found flags
dnl
dnl wrapper compiler is off by default; must be explicitly enabled
AC_DEFUN([_OAC_CHECK_PACKAGE_WRAPPER_COMPILER], [
    m4_ifdef([$1_wrapper_compiler],
             [m4_define([wrapper_compiler_name], [$1_wrapper_compiler])],
             [m4_define([wrapper_compiler_name], [$1cc])])
    AS_IF([test "${$1_USE_WRAPPER_COMPILER}" == "1"],
          [# search for the package using wrapper compilers.  If the user
           # provided a --with-$1 argument, be explicit about where we look
           # for the compiler, so we don't find the wrong one.
           AC_CACHE_CHECK([for $1 wrapper compiler],
                [check_package_cv_$1_wrapper_compiler],
                [AS_IF([test -z "${check_package_prefix}"],
                       [check_package_cv_$1_wrapper_compiler="wrapper_compiler_name"],
                       [check_package_cv_$1_wrapper_compiler="${check_package_prefix}/bin/wrapper_compiler_name"])])
           _OAC_CHECK_PACKAGE_WRAPPER_INTERNAL([$1], [$2], [${check_package_cv_$1_wrapper_compiler}], [$3])])
])


dnl 1 -> package name
dnl 2 -> prefix
dnl 2 -> wrapper compiler
dnl 3 -> action if found flag
AC_DEFUN([_OAC_CHECK_PACKAGE_WRAPPER_INTERNAL], [
    AC_CACHE_CHECK([if $1 wrapper compiler works],
         [check_package_cv_$1_wrapper_compiler_works],
         [_OAC_CHECK_PACKAGE_WRAPPER_RUN([$3], [--showme:version], [check_package_wrapper_internal_result],
               [check_package_cv_$1_wrapper_compiler_works=yes],
               [check_package_cv_$1_wrapper_compiler_works=no])])

    # if wrapper --showme:version  works, but getting one of the standard flags fails, we consider
    # that a hard failure.  It should not happen, outside of a weird system configuration
    # issue where we're probably not going to like the results anyway.
    AS_IF([test ${check_package_cv_$1_wrapper_compiler_works} = "yes"],
          [AC_CACHE_CHECK([for $1 wrapper compiler cppflags],
                [check_package_cv_$1_wrapper_compiler_cppflags],
                [_OAC_CHECK_PACKAGE_WRAPPER_RUN([$3], [--showme:incdirs],
                      [check_package_wrapper_internal_result],
                      [for check_package_wrapper_internal_tmp in ${check_package_wrapper_internal_result} ; do
                           OAC_APPEND([check_package_cv_$1_wrapper_compiler_cppflags], ["-I${check_package_wrapper_internal_tmp}"])
                       done],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 cppflags from wrapper compiler])])])
           $2_CPPFLAGS="${check_package_cv_$1_wrapper_compiler_cppflags}"

           AC_CACHE_CHECK([for $1 wrapper compiler ldflags],
                [check_package_cv_$1_wrapper_compiler_ldflags],
                [_OAC_CHECK_PACKAGE_WRAPPER_RUN([$3], [--showme:libdirs],
                      [check_package_wrapper_internal_result],
                       [for check_package_wrapper_internal_tmp in ${check_package_wrapper_internal_result} ; do
                            OAC_APPEND([check_package_cv_$1_wrapper_compiler_ldflags], ["-L${check_package_wrapper_internal_tmp}"])
                        done],
                       [AC_MSG_RESULT([error])
                        AC_MSG_ERROR([An error occurred retrieving $1 ldflags from wrapper compiler])])])
           $2_LDFLAGS="${check_package_cv_$1_wrapper_compiler_ldflags}"

           AC_CACHE_CHECK([for $1 wrapper compiler static ldflags],
                [check_package_cv_$1_wrapper_compiler_static_ldflags],
                [_OAC_CHECK_PACKAGE_WRAPPER_RUN([$3], [--showme:libdirs_static],
                      [check_package_wrapper_internal_result],
                       [for check_package_wrapper_internal_tmp in ${check_package_wrapper_internal_result} ; do
                            OAC_APPEND([check_package_cv_$1_wrapper_compiler_static_ldflags], ["-L${check_package_wrapper_internal_tmp}"])
                        done],
                       [AC_MSG_RESULT([error])
                        AC_MSG_ERROR([An error occurred retrieving $1 static ldflags from wrapper compiler])])])
           $2_STATIC_LDFLAGS="${check_package_cv_$1_wrapper_compiler_static_ldflags}"

           AC_CACHE_CHECK([for $1 wrapper compiler libs],
                [check_package_cv_$1_wrapper_compiler_libs],
                [_OAC_CHECK_PACKAGE_WRAPPER_RUN([$3], [--showme:libs],
                      [check_package_wrapper_internal_result],
                      [for check_package_wrapper_internal_tmp in ${check_package_wrapper_internal_result} ; do
                           OAC_APPEND([check_package_cv_$1_wrapper_compiler_libs], ["-l${check_package_wrapper_internal_tmp}"])
                       done],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 libs from wrapper compiler])])])
           $2_LIBS="$check_package_cv_$1_wrapper_compiler_libs"

           AC_CACHE_CHECK([for $1 wrapper compiler static libs],
                [check_package_cv_$1_wrapper_compiler_static_libs],
                [_OAC_CHECK_PACKAGE_WRAPPER_RUN([$3], [--showme:libs_static],
                      [check_package_wrapper_internal_result],
                      [for check_package_wrapper_internal_tmp in ${check_package_wrapper_internal_result} ; do
                           OAC_APPEND([check_package_cv_$1_wrapper_compiler_static_libs], ["-l${check_package_wrapper_internal_tmp}"])
                       done],
                      [AC_MSG_RESULT([error])
                       AC_MSG_ERROR([An error occurred retrieving $1 static libs from wrapper compiler])])])
           $2_STATIC_LIBS="${check_package_cv_$1_wrapper_compiler_static_libs}"

           $4])

    AS_UNSET([check_package_wrapper_internal_result])
    AS_UNSET([check_package_wrapper_internal_tmp])
])


dnl 1 -> wrapper compiler
dnl 2 -> argument string
dnl 3 -> result assignment string
dnl 4 -> action if found
dnl 5 -> action if failed
AC_DEFUN([_OAC_CHECK_PACKAGE_WRAPPER_RUN], [
    OAC_LOG_COMMAND([check_package_wrapper_run_results=`$1 $2 2>&1`],
             [AS_VAR_COPY([$3], [check_package_wrapper_run_results])
              $4],
             [$5])
         OAC_LOG_MSG([wrapper output: ${check_package_wrapper_run_results}], [1])
    AS_UNSET([check_package_wrapper_run_results])
])


dnl ************************************* GENERIC GUESSING *************************************


dnl 1 -> package name
dnl 2 -> prefix
dnl 3 -> headers (space separated list)
dnl 4 -> libraries (space separated list)
dnl 5 -> action if found flags
AC_DEFUN([_OAC_CHECK_PACKAGE_GENERIC], [
    check_package_generic_happy=0

    AS_IF([test -n "${check_package_prefix}"],
          [_OAC_CHECK_PACKAGE_GENERIC_PREFIX([$1], [$2], [$3], [$4], [check_package_generic_happy=1])],
          [AC_MSG_NOTICE([Searching for $1 in default search paths])
           $1_CPPFLAGS=
           $1_LDFLAGS=
           check_package_generic_happy=1])

    AS_IF([test ${check_package_generic_happy} -eq 1],
          [for check_package_generic_lib in $4 ; do
               check_package_generic_lib=`echo ${check_package_generic_lib} | sed -e 's/^-l//'`
               OAC_APPEND([$2_LIBS], ["-l${check_package_generic_lib}"])
               OAC_APPEND([$2_STATIC_LIBS], ["-l${check_package_generic_lib}"])
           done

           AC_MSG_CHECKING([for $1 cppflags])
           AC_MSG_RESULT([$$2_CPPFLAGS])
           AC_MSG_CHECKING([for $1 ldflags])
           AC_MSG_RESULT([$$2_LDFLAGS])
           AC_MSG_CHECKING([for $1 libs])
           AC_MSG_RESULT([$$2_LIBS])
           AC_MSG_CHECKING([for $1 static libs])
           AC_MSG_RESULT([$$2_STATIC_LIBS])

           $5])

    AS_UNSET([check_package_generic_happy])
    AS_UNSET([check_package_generic_lib])
])


dnl 1 -> package name
dnl 2 -> prefix
dnl 3 -> headers (space separated list)
dnl 4 -> libraries (space separated list)
dnl 5 -> action if found flags
AC_DEFUN([_OAC_CHECK_PACKAGE_GENERIC_PREFIX], [
    check_package_generic_search_header=`echo "$3" | cut -f1 -d' '`
    check_package_generic_search_lib=`echo "$4" | cut -f1 -d' ' | sed -e 's/^-l//'`

    check_package_generic_prefix_happy=0
    AS_IF([test -n "${check_package_incdir}"],
          [check_package_generic_incdir="${check_package_incdir}"],
          [check_package_generic_incdir="${check_package_prefix}/include"])
    AC_MSG_CHECKING([for $1 header at ${check_package_generic_incdir}])
    AS_IF([test -r ${check_package_generic_incdir}/${check_package_generic_search_header}],
          [check_package_generic_prefix_happy=1
           $2_CPPFLAGS="-I${check_package_generic_incdir}"
           AC_MSG_RESULT([found])],
          [AC_MSG_RESULT([not found])])

    AS_IF([test ${check_package_generic_prefix_happy} -eq 1],
          [check_package_generic_prefix_happy=0
           AS_IF([test -n "${check_package_libdir}"],
                 [AC_MSG_CHECKING([for $1 library (${check_package_generic_search_lib}) in ${check_package_libdir}])
                  ls ${check_package_libdir}/lib${check_package_generic_search_lib}.*  1>&/dev/null 2>&1
                  AS_IF([test $? -eq 0],
                        [check_package_generic_prefix_happy=1
                         $2_LDFLAGS="-L${check_package_libdir}"
                         AC_MSG_RESULT([foound])],
                        [AC_MSG_RESULT([not found])])],
                 [check_package_generic_prefix_lib=0
                  check_package_generic_prefix_lib64=0

                  ls ${check_package_prefix}/lib/lib${check_package_generic_search_lib}.*  1>&/dev/null 2>&1
                  AS_IF([test $? -eq 0], [check_package_generic_prefix_lib=1])
                  ls ${check_package_prefix}/lib64/lib${check_package_generic_search_lib}.*  1>&/dev/null 2>&1
                  AS_IF([test $? -eq 0], [check_package_generic_prefix_lib64=1])

                  AC_MSG_CHECKING([for $1 library (${check_package_generic_search_lib}) in ${check_package_prefix}])
                  AS_IF([test ${check_package_generic_prefix_lib} -eq 1 -a ${check_package_generic_prefix_lib64} -eq 1],
                        [AC_MSG_ERROR([Found library $check_package_generic_search_lib in both ${check_package_prefix}/lib and
${check_package_prefix}/lib64.  This has confused configure.  Please add --with-$1-libdir=PATH to configure to help
disambiguate.])],
                        [test ${check_package_generic_prefix_lib} -eq 1],
                        [check_package_generic_prefix_happy=1
                         $2_LDFLAGS=-L${check_package_prefix}/lib
                         AC_MSG_RESULT([found -- lib])],
                        [test $check_package_generic_prefix_lib64 -eq 1],
                        [check_package_generic_prefix_happy=1
                         libdir_prefix=${check_package_prefix}/lib64
                         AC_MSG_RESULT([found -- lib64])],
                        [AC_MSG_RESULT([not found])])])])

    AS_IF([test ${check_package_generic_prefix_happy} -eq 1], [$5])

    AS_UNSET([check_package_generic_search_header])
    AS_UNSET([check_package_generic_search_lib])
    AS_UNSET([check_package_generic_incdir])
])


dnl ************************************* OPERATIONAL CHECK *************************************


dnl 1 -> package name
dnl 2 -> prefix
dnl 3 -> headers (space separated list)
dnl 4 -> function name
dnl 5 -> action if found
dnl 6 -> action if not found
AC_DEFUN([_OAC_CHECK_PACKAGE_VERIFY],[
    check_package_verify_search_header=`echo "$3" | cut -f1 -d' '`

    OAC_APPEND([CPPFLAGS], [${$2_CPPFLAGS}])
    OAC_APPEND([LDFLAGS], [${$2_LDFLAGS}])
    OAC_APPEND([LIBS], [${$2_LIBS}])

    check_package_verify_happy=1

    AS_IF([test ${check_package_verify_happy} -eq 1],
          [AC_CHECK_HEADER([${check_package_verify_search_header}],
                           [check_package_verify_happy=1], [check_package_verify_happy=0])])

    dnl Note that we use AC_CHEC_FUNC here instead of AC_CHECK_LIB, because we're pretty sure we've
    dnl found the library already (and have added it to LIBS).  Now we're just trying to verify
    dnl that the library we found contains the bits we need.
    AS_IF([test ${check_package_verify_happy} -eq 1],
          [AC_CHECK_FUNC([$4],  [check_package_verify_happy=1], [check_package_verify_happy=0])])

    m4_ifdef([OAC_CHECK_PACKAGE_VERIFY_COMMAND_LIST],
        [m4_foreach([list_item], [OAC_CHECK_PACKAGE_VERIFY_COMMAND_LIST],
               [AS_IF([test ${check_package_verify_happy} -eq 1],
                      [m4_apply(m4_unquote([list_item]), [[$1], [$2], [$3], [$4],
                                                          [check_package_verify_happy=1],
                                                          [check_package_verify_happy=0]])])])])

    AS_IF([test ${check_package_verify_happy} -eq 1],
          [$5], [$6])

    AS_UNSET([check_package_verify_search_header])
    AS_UNSET([check_package_verify_happy])
])
