dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2016      IBM Corporation.  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Parameters: (use a version "1.1.4rc2" as the example)
# * prefix
#   Will export a variable $prefix_version_cmp
#   - action_if_less         => "less"
#   - action_if_equal        => "equal"
#   - action_if_equal_series => "series"
#   - action_if_greater      => "greater"
# * version_actual
#   Actual version string
# * version_desired
#   Desired version string to check against
# * action_if_less
#   Action to take if the version is strictly less than
#   "1.1.3" < "1.1.4rc2"
# * action_if_equal
#   Action to take if the version matches exactly
#   "1.1.4rc2" = "1.1.4rc2"
# * action_if_equal_series
#   Action to take if the version matches to this series
#   "1.1.4rc1" ~=~ "1.1.4rc2"
#   "1.1.4" ~=~ "1.1.4rc2"
# * action_if_greater
#   Action to take if the version is strictly greater than
#   "1.1.5" > "1.1.4rc2"
#   "2.0" > "1.1.4rc2"
#
# See documentation on m4_version_compare and AS_VERSION_COMPARE for more
# precise definitions
# OPAL_CHECK_VERSION(prefix, version_actual, version_desired,
#                    action_if_less, action_if_equal, action_if_equal_series,
#                    action_if_greater)
# ----------------------------------------------------
AC_DEFUN([OPAL_CHECK_VERSION],[
    version_actual=$2
    version_desired=$3

    AC_MSG_CHECKING([Checking library version is $version_desired])
    #
    # Example: If version_desired=1.1.4 and
    # version_actual=1.1.3    -> -1
    # version_actual=1.1.4    ->  0
    # version_actual=1.1.4rc1 ->  1
    # version_actual=1.1.5    ->  1 (need further check)
    #
    AS_VERSION_COMPARE(["$version_actual"], [$version_desired],
            [AC_MSG_RESULT([Earlier than expected ($version_actual < $$version_desired)])
             $1_version_cmp="less"
             $4],
            [AC_MSG_RESULT([Equal])
             $1_version_cmp="equal"
             $5],
            [
             # Need further check to make sure we are < 1.1.5
             # version_actual=1.1.4rc1 ->  -1
             # version_actual=1.1.4    ->   0 (caught above)
             # version_actual=1.1.5    ->   1
             AS_VERSION_COMPARE(["$version_actual"], [$version_desired"zzzz"],
                 [AC_MSG_RESULT([Within release series ($version_actual)])
                  $1_version_cmp="series"
                  $6],
                 [AC_MSG_RESULT([Within release series ($version_actual)])
                  $1_version_cmp="series"
                  $6],
                 [AC_MSG_RESULT([Later than expected ($version_actual > $$version_desired)])
                  $1_version_cmp="greater"
                  $7]
             )]
         )
])dnl
