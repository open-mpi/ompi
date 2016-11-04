dnl -*- shell-script -*-
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
dnl Copyright (c) 2006-2016 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# ORTE_CHECK_TM_LIBS_FLAGS(prefix, [LIBS or LDFLAGS])
# ---------------------------------------------------
AC_DEFUN([ORTE_CHECK_TM_LIBS_FLAGS],[
    OPAL_VAR_SCOPE_PUSH([orte_check_tm_flags])
    orte_check_tm_flags=`$orte_check_tm_pbs_config --libs`
    for orte_check_tm_val in $orte_check_tm_flags; do
        if test "`echo $orte_check_tm_val | cut -c1-2`" = "-l"; then
            if test "$2" = "LIBS"; then
                $1_$2="$$1_$2 $orte_check_tm_val"
            fi
        else
            if test "$2" = "LDFLAGS"; then
                $1_$2="$$1_$2 $orte_check_tm_val"
            fi
        fi
    done
    OPAL_VAR_SCOPE_POP
])


# ORTE_CHECK_TM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
AC_DEFUN([ORTE_CHECK_TM],[
    if test -z $orte_check_tm_happy ; then
	OPAL_VAR_SCOPE_PUSH([orte_check_tm_found orte_check_tm_dir orte_check_tm_pbs_config orte_check_tm_LDFLAGS_save orte_check_tm_CPPFLAGS_save orte_check_tm_LIBS_save])

	AC_ARG_WITH([tm],
                    [AC_HELP_STRING([--with-tm(=DIR)],
                                    [Build TM (Torque, PBSPro, and compatible) support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
	OPAL_CHECK_WITHDIR([tm], [$with_tm], [include/tm.h])

	orte_check_tm_found=no
	AS_IF([test "$with_tm" = "no"],
              [orte_check_tm_happy="no"],
              [orte_check_tm_happy="yes"
               AS_IF([test ! -z "$with_tm" && test "$with_tm" != "yes"],
                     [orte_check_tm_dir="$with_tm"],
                     [orte_check_tm_dir=""])])

	AS_IF([test "$orte_check_tm_happy" = "yes"],
              [AC_MSG_CHECKING([for pbs-config])
               orte_check_tm_pbs_config="not found"
               AS_IF([test "$orte_check_tm_dir" != "" && test -d "$orte_check_tm_dir" && test -x "$orte_check_tm_dir/bin/pbs-config"],
                     [orte_check_tm_pbs_config="$orte_check_tm_dir/bin/pbs-config"],
                     [AS_IF([pbs-config --prefix >/dev/null 2>&1],
                            [orte_check_tm_pbs_config="pbs-config"])])
               AC_MSG_RESULT([$orte_check_tm_pbs_config])])

	# If we have pbs-config, get the flags we need from there and then
	# do simplistic tests looking for the tm headers and symbols

	AS_IF([test "$orte_check_tm_happy" = "yes" && test "$orte_check_tm_pbs_config" != "not found"],
              [orte_check_tm_CPPFLAGS=`$orte_check_tm_pbs_config --cflags`
               OPAL_LOG_MSG([orte_check_tm_CPPFLAGS from pbs-config: $orte_check_tm_CPPFLAGS], 1)

               ORTE_CHECK_TM_LIBS_FLAGS([orte_check_tm], [LDFLAGS])
               OPAL_LOG_MSG([orte_check_tm_LDFLAGS from pbs-config: $orte_check_tm_LDFLAGS], 1)

               ORTE_CHECK_TM_LIBS_FLAGS([orte_check_tm], [LIBS])
               OPAL_LOG_MSG([orte_check_tm_LIBS from pbs-config: $orte_check_tm_LIBS], 1)

               # Now that we supposedly have the right flags, try them out.

               orte_check_tm_CPPFLAGS_save="$CPPFLAGS"
               orte_check_tm_LDFLAGS_save="$LDFLAGS"
               orte_check_tm_LIBS_save="$LIBS"

               CPPFLAGS="$CPPFLAGS $orte_check_tm_CPPFLAGS"
               LIBS="$LIBS $orte_check_tm_LIBS"
               LDFLAGS="$LDFLAGS $orte_check_tm_LDFLAGS"

               AC_CHECK_HEADER([tm.h],
			       [AC_CHECK_FUNC([tm_finalize],
					      [orte_check_tm_found="yes"])])

               CPPFLAGS="$orte_check_tm_CPPFLAGS_save"
               LDFLAGS="$orte_check_tm_LDFLAGS_save"
               LIBS="$orte_check_tm_LIBS_save"])

	# If we don't have pbs-config, then we have to look around
	# manually.

	# Note that Torque 2.1.0 changed the name of their back-end
	# library to "libtorque".  So we have to check for both libpbs and
	# libtorque.  First, check for libpbs.

	orte_check_package_$1_save_CPPFLAGS="$CPPFLAGS"
	orte_check_package_$1_save_LDFLAGS="$LDFLAGS"
	orte_check_package_$1_save_LIBS="$LIBS"

	AS_IF([test "$orte_check_tm_found" = "no"],
              [AS_IF([test "$orte_check_tm_happy" = "yes"],
                     [_OPAL_CHECK_PACKAGE_HEADER([orte_check_tm],
						 [tm.h],
						 [$orte_check_tm_dir],
						 [orte_check_tm_found="yes"],
						 [orte_check_tm_found="no"])])

               AS_IF([test "$orte_check_tm_found" = "yes"],
                     [_OPAL_CHECK_PACKAGE_LIB([orte_check_tm],
					      [pbs],
					      [tm_init],
					      [],
					      [$orte_check_tm_dir],
					      [$orte_check_tm_libdir],
					      [orte_check_tm_found="yes"],
					      [_OPAL_CHECK_PACKAGE_LIB([orte_check_tm],
								       [torque],
								       [tm_init],
								       [],
								       [$orte_check_tm_dir],
								       [$orte_check_tm_libdir],
								       [orte_check_tm_found="yes"],
								       [orte_check_tm_found="no"])])])])

	CPPFLAGS="$orte_check_package_$1_save_CPPFLAGS"
	LDFLAGS="$orte_check_package_$1_save_LDFLAGS"
	LIBS="$orte_check_package_$1_save_LIBS"

	if test "$orte_check_tm_found" = "no" ; then
	    orte_check_tm_happy=no
	fi

	OPAL_SUMMARY_ADD([[Resource Managers]],[[Torque]],[$1],[$orte_check_tm_happy])

	OPAL_VAR_SCOPE_POP
    fi

    # Did we find the right stuff?
    AS_IF([test "$orte_check_tm_happy" = "yes"],
          [$1_LIBS="[$]$1_LIBS $orte_check_tm_LIBS"
	   $1_LDFLAGS="[$]$1_LDFLAGS $orte_check_tm_LDFLAGS"
	   $1_CPPFLAGS="[$]$1_CPPFLAGS $orte_check_tm_CPPFLAGS"
	   # add the TM libraries to static builds as they are required
	   $1_WRAPPER_EXTRA_LDFLAGS=[$]$1_LDFLAGS
	   $1_WRAPPER_EXTRA_LIBS=[$]$1_LIBS
	   $2],
          [AS_IF([test ! -z "$with_tm" && test "$with_tm" != "no"],
                 [AC_MSG_ERROR([TM support requested but not found.  Aborting])])
	   orte_check_tm_happy="no"
	   $3])
])
