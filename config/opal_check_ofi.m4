dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2015-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2016-2017 Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# OPAL_CHECK_OFI(prefix, [action-if-found], [action-if-not-found]
# --------------------------------------------------------
# Check if libfabric support can be found.
#
# Sets prefix_{CPPFLAGS, LDFLAGs, LIBS} as needed and runs
# action-if-found if there is support; otherwise executes
# action-if-not-found.
#
AC_DEFUN([OPAL_CHECK_OFI],[
    if test -z "$opal_check_libfabric_happy" ; then
	OPAL_VAR_SCOPE_PUSH([opal_check_libfabric_$1_save_CPPFLAGS opal_check_libfabric_$1_save_LDFLAGS opal_check_libfabric_$1_save_LIBS])

	# Add --with options
	AC_ARG_WITH([libfabric],
		    [AC_HELP_STRING([--with-libfabric=DIR],
				    [Deprecated synonym for --with-ofi])])
	AC_ARG_WITH([libfabric-libdir],
		    [AC_HELP_STRING([--with-libfabric-libdir=DIR],
				    [Deprecated synonym for --with-ofi-libdir])])

	AC_ARG_WITH([ofi],
		    [AC_HELP_STRING([--with-ofi=DIR],
				    [Specify location of OFI libfabric installation, adding DIR/include to the default search location for libfabric headers, and DIR/lib or DIR/lib64 to the default search location for libfabric libraries.  Error if libfabric support cannot be found.])])

	AC_ARG_WITH([ofi-libdir],
		    [AC_HELP_STRING([--with-ofi-libdir=DIR],
				    [Search for OFI libfabric libraries in DIR])])

	if test "$with_ofi" = ""; then
	     with_ofi=$with_libfabric
	fi

	if test "$with_ofi_libdir" = ""; then
	     with_ofi_libdir=$with_libfabric_libdir
	fi

	    # Sanity check the --with values
	OPAL_CHECK_WITHDIR([ofi], [$with_ofi],
			   [include/rdma/fabric.h])
	OPAL_CHECK_WITHDIR([ofi-libdir], [$with_ofi_libdir],
			   [libfabric.*])

	opal_check_ofi_$1_save_CPPFLAGS=$CPPFLAGS
	opal_check_ofi_$1_save_LDFLAGS=$LDFLAGS
	opal_check_ofi_$1_save_LIBS=$LIBS

	opal_check_ofi_happy=yes
	AS_IF([test "$with_ofi" = "no"],
              [opal_check_ofi_happy=no])

	AS_IF([test $opal_check_ofi_happy = yes],
              [AC_MSG_CHECKING([looking for OFI libfabric in])
               AS_IF([test "$with_ofi" != "yes"],
                     [opal_ofi_dir=$with_ofi
                      AC_MSG_RESULT([($opal_ofi_dir)])],
                     [AC_MSG_RESULT([(default search paths)])])
               AS_IF([test ! -z "$with_ofi_libdir" && \
			     test "$with_ofi_libdir" != "yes"],
                     [opal_ofi_libdir=$with_ofi_libdir])
              ])

	AS_IF([test $opal_check_ofi_happy = yes],
              [OPAL_CHECK_PACKAGE([opal_check_ofi],
				  [rdma/fabric.h],
				  [fabric],
				  [fi_getinfo],
				  [],
				  [$opal_ofi_dir],
				  [$opal_ofi_libdir],
				  [],
				  [opal_check_ofi_happy=no])])

	CPPFLAGS=$opal_check_ofi_$1_save_CPPFLAGS
	LDFLAGS=$opal_check_ofi_$1_save_LDFLAGS
	LIBS=$opal_check_ofi_$1_save_LIBS

	OPAL_SUMMARY_ADD([[Transports]],[[OpenFabrics Libfabric]],[$1],[$opal_check_ofi_happy])

	OPAL_VAR_SCOPE_POP
    fi

    if test $opal_check_ofi_happy = yes ; then
	$1_CPPFLAGS="[$]$1_CPPFLAGS $opal_check_ofi_CPPFLAGS"
	$1_LIBS="[$]$1_LIBS $opal_check_ofi_LIBS"
	$1_LDFLAGS="[$]$1_LDFLAGS $opal_check_ofi_LDFLAGS"

	AC_SUBST($1_CPPFLAGS)
	AC_SUBST($1_LDFLAGS)
	AC_SUBST($1_LIBS)
    fi

    AS_IF([test $opal_check_ofi_happy = yes],
          [$2],
          [AS_IF([test -n "$with_ofi" && test "$with_ofi" != "no"],
                 [AC_MSG_WARN([OFI libfabric support requested (via --with-ofi or --with-libfabric), but not found.])
                  AC_MSG_ERROR([Cannot continue.])])
           $3])
])dnl
