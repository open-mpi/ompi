dnl
dnl Copyright (C) 2022      Advanced Micro Devices, Inc. All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl


# OMPI_CHECK_ROCM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if ROCM support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found


#
# Check for ROCm  support
#
AC_DEFUN([OPAL_CHECK_ROCM],[

     OPAL_VAR_SCOPE_PUSH([opal_check_rocm_happy rocm_save_CPPFLAGS rocm_save_LDFLAGS rocm_CPPFLAGS rocm_LDFLAGS])

     rocm_save_CPPFLAGS="$CPPFLAGS"
     rocm_save_LDFLAGS="$LDFLAGS"
     
     # Get some configuration information
     AC_ARG_WITH([rocm],
        [AS_HELP_STRING([--with-rocm(=DIR)],
        [Build ROCm support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])


     AS_IF([ test -n "$with_rocm" && test "$with_rocm" = "yes" ],
           [ with_rocm="/opt/rocm"] )

     rocm_CPPFLAGS="-D__HIP_PLATFORM_AMD__"
     rocm_LDFLAGS="-L${with_rocm}/lib/hip"

     AS_IF([ test -n "$with_rocm" && test "$with_rocm" != "no" ],
           [ OPAL_APPEND([CPPFLAGS], [$rocm_CPPFLAGS])
	     OPAL_APPEND([LDFLAGS], [$rocm_LDFLAGS]) ])

     OAC_CHECK_PACKAGE([rocm],
                       [$1],
                       [hip/hip_runtime.h],
                       [amdhip64],
		       [hipFree],
                       [opal_check_rocm_happy="yes"],
                       [opal_check_rocm_happy="no"])

     LDFLAGS="$rocm_save_LDFLAGS"
     OPAL_APPEND([CPPFLAGS], [${$1_CPPFLAGS}] )
     
     AS_IF([ test "$opal_check_rocm_happy" = "no" ],
           [ CPPFLAGS="$rocm_save_CPPFLAGS"])

     AS_IF([ test "$opal_check_rocm_happy" = "yes" ],
           [ AC_DEFINE_UNQUOTED([OPAL_ROCM_SUPPORT], [1], [Enable ROCm support])
             ROCM_SUPPORT=1 ],
           [ AC_DEFINE_UNQUOTED([OPAL_ROCM_SUPPORT], [0], [Disable ROCm support])
             ROCM_SUPPORT=0 ])

     AS_IF([ test "$opal_check_rocm_happy" = "yes" ],
            [$2],
            [AS_IF([test -n "$with_rocm" && test "$with_rocm" != "no"],
                   [AC_MSG_ERROR([ROCm support requested but not found.  Aborting])])
            $3])

     AM_CONDITIONAL([OPAL_rocm_support], [test "$opal_check_rocm_happy" = "yes"])
     OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OPAL_CHECK_ROCM_AFTER_OPAL_DL],[
    # We cannot have ROCm support without OPAL DL support.  Error out
    # if the user wants Rocm but we do not have OPAL DL support.
     AS_IF([test $OPAL_HAVE_DL_SUPPORT -eq 0 && test "$opal_check_rocm_happy" = "yes"],
          [AC_MSG_WARN([--with-rocm was specified, but dlopen support is disabled.])
           AC_MSG_WARN([You must reconfigure Open MPI with dlopen ("dl") support.])
           AC_MSG_ERROR([Cannot continue.])])

])
