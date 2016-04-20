dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2015-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# OPAL_CHECK_LIBFABRIC(prefix, [action-if-found], [action-if-not-found]
# --------------------------------------------------------
# Check if libfabric support can be found.
#
# Sets prefix_{CPPFLAGS, LDFLAGs, LIBS} as needed and runs
# action-if-found if there is support; otherwise executes
# action-if-not-found.
#
AC_DEFUN([OPAL_CHECK_LIBFABRIC],[
    if test -z "$opal_check_libfabric_happy" ; then
	OPAL_VAR_SCOPE_PUSH([opal_check_libfabric_$1_save_CPPFLAGS opal_check_libfabric_$1_save_LDFLAGS opal_check_libfabric_$1_save_LIBS])

	# Add --with options
	AC_ARG_WITH([libfabric],
		    [AC_HELP_STRING([--with-libfabric=DIR],
				    [Specify location of libfabric installation, adding DIR/include to the default search location for libfabric headers, and DIR/lib or DIR/lib64 to the default search location for libfabric libraries.  Error if libfabric support cannot be found.])])
	AC_ARG_WITH([libfabric-libdir],
		    [AC_HELP_STRING([--with-libfabric-libdir=DIR],
				    [Search for libfabric libraries in DIR])])

		    # Sanity check the --with values
	OPAL_CHECK_WITHDIR([libfabric], [$with_libfabric],
			   [include/rdma/fabric.h])
	OPAL_CHECK_WITHDIR([libfabric-libdir], [$with_libfabric_libdir],
			   [libfabric.*])

	opal_check_libfabric_$1_save_CPPFLAGS=$CPPFLAGS
	opal_check_libfabric_$1_save_LDFLAGS=$LDFLAGS
	opal_check_libfabric_$1_save_LIBS=$LIBS

	opal_check_libfabric_happy=yes
	AS_IF([test "$with_libfabric" = "no"],
              [opal_check_libfabric_happy=no])

	AS_IF([test $opal_check_libfabric_happy = yes],
              [AC_MSG_CHECKING([looking for libfabric in])
               AS_IF([test "$with_libfabric" != "yes"],
                     [opal_libfabric_dir=$with_libfabric
                      AC_MSG_RESULT([($opal_libfabric_dir)])],
                     [AC_MSG_RESULT([(default search paths)])])
               AS_IF([test ! -z "$with_libfabric_libdir" && \
			     test "$with_libfabric_libdir" != "yes"],
                     [opal_libfabric_libdir=$with_libfabric_libdir])
              ])

	AS_IF([test $opal_check_libfabric_happy = yes],
              [OPAL_CHECK_PACKAGE([opal_check_libfabric],
				  [rdma/fabric.h],
				  [fabric],
				  [fi_getinfo],
				  [],
				  [$opal_libfabric_dir],
				  [$opal_libfabric_libdir],
				  [],
				  [opal_check_libfabric_happy=no])])

	CPPFLAGS=$opal_check_libfabric_$1_save_CPPFLAGS
	LDFLAGS=$opal_check_libfabric_$1_save_LDFLAGS
	LIBS=$opal_check_libfabric_$1_save_LIBS

	OPAL_SUMMARY_ADD([[Transports]],[[OpenFabrics Libfabric]],[$1],[$opal_check_libfabric_happy])

	OPAL_VAR_SCOPE_POP
    fi

    if test $opal_check_libfabric_happy = yes ; then
	$1_CPPFLAGS="[$]$1_CPPFLAGS $opal_check_libfabric_CPPFLAGS"
	$1_LIBS="[$]$1_LIBS $opal_check_libfabric_LIBS"
	$1_LDFLAGS="[$]$1_LDFLAGS $opal_check_libfabric_LDFLAGS"

	AC_SUBST($1_CPPFLAGS)
	AC_SUBST($1_LDFLAGS)
	AC_SUBST($1_LIBS)
    fi

    AS_IF([test $opal_check_libfabric_happy = yes],
          [$2],
          [AS_IF([test -n "$with_libfabric" && test "$with_libfabric" != "no"],
                 [AC_MSG_WARN([libfabric support requested (via --with-libfabric), but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $3])
])dnl
