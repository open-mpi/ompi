# -*- shell-script -*-
#
# Copyright (c) 2009      Sun Microsystems, Inc.  All rights reserved.
# $COPYRIGHT$
# 
# Additional copyrights may follow
# 
# $HEADER$
#

AC_DEFUN([MCA_installdirs_autodetect_COMPILE_MODE], [
    AC_MSG_CHECKING([for MCA component $2:$3 compile mode])
    $4="static"
    AC_MSG_RESULT([$$4])
])


# MCA_installdirs_autodetect_CONFIG(action-if-can-compile, 
#                        [action-if-cant-compile])
# ------------------------------------------------
#
# Solaris uses a binary-format /proc/$$/map file that contains a
# sequence of prmap_t structures.  Those can be used to look up
# associated files in /proc/$$/path, which are symbolic links to
# the actual files.
#
# AIX has /proc/$$/map and /proc/$$/object, but no /proc/$$/path.
# I don't know how to achieve autodetect functionality on AIX.
#
# Linux has /proc/self/maps that contains text with both virtual
# addresses and paths.  (Under Linux 2.0 there are no paths.  I
# don't know how to achieve autodetect functionality on such
# systems.)

AC_DEFUN([MCA_installdirs_autodetect_CONFIG],[
    AC_CHECK_HEADERS(procfs.h,
        [AC_CHECK_FILE(/proc/$$/path,
                [procfs_path_happy="yes"
                 OMPI_INSTALLDIRS_AUTODETECT_PATH=opal_installdirs_solaris.lo],
                [procfs_path_happy="no"])],
        [AC_CHECK_FILE(/proc/self/maps,
                [procfs_path_happy="yes"
                 OMPI_INSTALLDIRS_AUTODETECT_PATH=opal_installdirs_linux.lo],
                [procfs_path_happy="no"])])

    AS_IF([test "$procfs_path_happy" = "yes"],[
	# The following check is from opal/mca/backtrace/execinfo/configure.m4
	AC_CHECK_HEADERS([execinfo.h],[
	    # FreeBSD has backtrace in -lexecinfo, usually in libc
	    OMPI_CHECK_FUNC_LIB([backtrace], [execinfo],
		[findpc_happy="yes"
		 OMPI_INSTALLDIRS_AUTODETECT_PC=opal_installdirs_backtrace.lo],
		[findpc_happy="no"])])

	AS_IF([test "$backtrace_execinfo_happy" = "no"],[
	    AC_CHECK_HEADERS([ucontext.h],[
		OMPI_CHECK_FUNC_LIB([walkcontext],,
		    [findpc_happy="yes"
		     OMPI_INSTALLDIRS_AUTODETECT_PC=opal_installdirs_walkcontext.lo],
		    [findpc_happy="no"])])])])

    AS_IF([test "$procfs_path_happy" = "yes" -a "$findpc_happy" = "yes"],[
            AC_SUBST([OMPI_INSTALLDIRS_AUTODETECT_PATH])
            AC_SUBST([OMPI_INSTALLDIRS_AUTODETECT_PC])
            $1],[$2])])
