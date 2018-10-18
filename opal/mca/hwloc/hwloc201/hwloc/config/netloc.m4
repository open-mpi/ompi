dnl -*- Autoconf -*-
dnl
dnl Copyright © 2014 Cisco Systems, Inc.  All rights reserved.
dnl
dnl Copyright © 2014-2018 Inria.  All rights reserved.
dnl See COPYING in top-level directory.

# Main hwloc m4 macro, to be invoked by the user
#
# Expects:
# 1. Configuration prefix
# 2. What to do upon success
# 3. What to do upon failure
# 4. If non-empty, print the announcement banner
#
AC_DEFUN([NETLOC_SETUP_CORE],[
    AC_REQUIRE([HWLOC_SETUP_CORE])
    AC_REQUIRE([AC_PROG_CC])

    AS_IF([test "x$4" != "x"],
          [cat <<EOF

###
### Configuring netloc core
###
EOF])

    # These flags are specific to netloc, and should not be redundant
    # with hwloc.  I.e., if the flag already exists in hwloc, there's
    # no need to put it here.
    NETLOC_CFLAGS=$HWLOC_CFLAGS
    NETLOC_CPPFLAGS=$HWLOC_CPPFLAGS
    NETLOC_LDFLAGS=$HWLOC_LDFLAGS
    NETLOC_LIBS=
    NETLOC_LIBS_PRIVATE=

    # Setup the individual parts of Netloc
    netloc_happy=yes
    AS_IF([test "$netloc_happy" = "yes"],
          [NETLOC_CHECK_PLATFORM([netloc_happy])])

    AC_SUBST(NETLOC_CFLAGS)
    AC_SUBST(NETLOC_CPPFLAGS)
    AC_SUBST(NETLOC_LDFLAGS)
    AC_SUBST(NETLOC_LIBS)
    AC_SUBST(NETLOC_LIBS_PRIVATE)

    # Set these values explicitly for embedded builds.  Exporting
    # these values through *_EMBEDDED_* values gives us the freedom to
    # do something different someday if we ever need to.  There's no
    # need to fill these values in unless we're in embedded mode.
    # Indeed, if we're building in embedded mode, we want NETLOC_LIBS
    # to be empty so that nothing is linked into libnetloc_embedded.la
    # itself -- only the upper-layer will link in anything required.

    AS_IF([test "$hwloc_mode" = "embedded"],
          [NETLOC_EMBEDDED_CFLAGS=$NETLOC_CFLAGS
           NETLOC_EMBEDDED_CPPFLAGS=$NETLOC_CPPFLAGS
           NETLOC_EMBEDDED_LDADD='$(HWLOC_top_builddir)/netloc/libnetloc_embedded.la'
           NETLOC_EMBEDDED_LIBS=$NETLOC_LIBS
           NETLOC_LIBS=])
    AC_SUBST(NETLOC_EMBEDDED_CFLAGS)
    AC_SUBST(NETLOC_EMBEDDED_CPPFLAGS)
    AC_SUBST(NETLOC_EMBEDDED_LDADD)
    AC_SUBST(NETLOC_EMBEDDED_LIBS)

    AS_IF([test "$netloc_happy" = "yes"],
          [$2],
          [$3])
])dnl

AC_DEFUN([NETLOC_CHECK_PLATFORM], [
    AC_CHECK_FUNC([asprintf])
    AC_MSG_CHECKING([if netloc supports this platform])
    AS_IF([test "$ac_cv_func_asprintf" != "yes"],
          [$1=no netloc_missing_reason=" (asprintf missing)"])
    AS_IF([test "$hwloc_windows" = "yes"],
          [$1=no netloc_missing_reason=" (Windows platform)"])
    AC_MSG_RESULT([$$1$netloc_missing_reason])

    AC_CHECK_LIB(scotch, SCOTCH_archSub,
        [scotch_found_headers=yes;
        AC_DEFINE([NETLOC_SCOTCH], [1],
            [Define to 1 if scotch is netlocscotch is enabled])
    ], [], -lscotcherr)
    AC_CHECK_HEADERS([mpi.h],
            [mpi_found_headers=yes;
            MPI_CPPFLAGS=`mpicc -showme:compile 2>/dev/null`
            MPI_LDADD=`mpicc -showme:link 2>/dev/null`
            AC_SUBST(MPI_CPPFLAGS)
            AC_SUBST(MPI_LDADD)
            break;])

    AC_CHECK_PROG([xz],[xz],[yes],[no])
])dnl

AC_DEFUN([NETLOC_DO_AM_CONDITIONALS], [
    AM_CONDITIONAL([BUILD_NETLOC], [test "$netloc_happy" = "yes"])
    AM_CONDITIONAL([BUILD_NETLOCSCOTCH], [test "x$scotch_found_headers" = "xyes"])
    AM_CONDITIONAL([BUILD_MPITOOLS], [test "x$mpi_found_headers" = "xyes"])
    AM_CONDITIONAL([FOUND_XZ], [test "x$xz" = xyes])
])dnl
