#
# 2022     Copyright (C) Advanced Micro Devices, Inc. All rights reserved.
#


# OMPI_CHECK_ROCM(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if ROCM support can be found.  sets prefix_{CPPFLAGS,
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found


# ROCM_BUILD_FLAGS(ARG, VAR_LIBS, VAR_LDFLAGS, VAR_CPPFLAGS)
# ----------------------------------------------------------
# Parse value of ARG into appropriate LIBS, LDFLAGS, and
# CPPFLAGS variables.
AC_DEFUN([ROCM_BUILD_FLAGS],
    $4="-D__HIP_PLATFORM_AMD__ -I$1/include/hip -I$1/include"
    $3="-L$1/hip/lib -L$1/lib"
    $2="-lamdhip64"
)

#
# Check for ROCm  support
#
AC_DEFUN([OPAL_CHECK_ROCM],[

     OPAL_VAR_SCOPE_PUSH([opal_check_rocm_happy])

    # Get some configuration information
     AC_ARG_WITH([rocm],
        [AS_HELP_STRING([--with-rocm(=DIR)],
        [Build ROCm support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
     OPAL_CHECK_WITHDIR([rocm], [$with_rocm], [include/hip/hip_runtime.h])

     AS_IF([test "$with_rocm" = "no"],
           [opal_check_rocm_happy="no"],
           [AS_IF([test -n "$with_rocm" && test "$with_rocm" != "yes"],
                  [opal_check_rocm_dir=$with_rocm])

          SAVE_CPPFLAGS="$CPPFLAGS"
          SAVE_LDFLAGS="$LDFLAGS"
          SAVE_LIBS="$LIBS"

          ROCM_BUILD_FLAGS([$with_rocm],
                           [common_rocm_LIBS], [common_rocm_LDFLAGS], [common_rocm_CPPFLAGS])


          save_common_rocm_CPPFLAGS=$common_rocm_CPPFLAGS
          save_common_rocm_LFFLAGS=$common_rocm_LDFLAGS
          save_common_rocm_LIBS=$common_rocm_LIBS

          CPPFLAGS="$common_rocm_CPPFLAGS $CPPFLAGS"
          LDFLAGS="$common_rocm_LDFLAGS $LDFLAGS"
          LIBS="$common_rocm_LIBS $LIBS"

          OAC_CHECK_PACKAGE([rocm], [$1], [hip/hip_runtime.h],
	                    [amdhip64], [hipFree],
                            [opal_check_rocm_happy="yes"],
                            [opal_check_rocm_happy="no"])
          CPPFLAGS="$SAVE_CPPFLAGS"
          LDFLAGS="$SAVE_LDFLAGS"
          LIBS="$SAVE_LIBS"

          common_rocm_CPPFLAGS=$save_common_rocm_CPPFLAGS
          common_rocm_LFFLAGS=$save_common_rocm_LDFLAGS
          common_rocm_LIBS=$save_common_rocm_LIBS
     ])

     AS_IF([test "$opal_check_rocm_happy" = "yes"],
           AC_DEFINE([OPAL_ROCM_SUPPORT], 1, [Enable ROCM support]),
	   [AC_DEFINE([OPAL_ROCM_SUPPORT], 0, [Enable ROCM support])
	    ROCM_CPPFLAGS=
	    ROCM_LDFLAGS=
	    ROCM_LIBS=
	    ])

     AS_IF([test "$opal_check_rocm_happy" = "yes"],
            [$2],
            [AS_IF([test -n "$with_rocm" && test "$with_rocm" != "no"],
                   [AC_MSG_ERROR([ROCm support requested but not found.  Aborting])])
            $3])

     AM_CONDITIONAL([OPAL_rocm_support], [test "$opal_check_rocm_happy" = "yes"])
     OPAL_VAR_SCOPE_POP
])
