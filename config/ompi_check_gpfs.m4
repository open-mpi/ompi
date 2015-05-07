# OMPI_CHECK_GPFS(prefix, [action-if-found], [action-if-not-found])
# --------------------------------------------------------
# check if GPFS support can be found. Sets prefix_{CPPFLAGS, 
# LDFLAGS, LIBS} as needed and runs action-if-found if there is
# support, otherwise executes action-if-not-found

AC_DEFUN([OMPI_CHECK_GPFS],[

    check_gpfs_CPPFLAGS=
    check_gpfs_LDFLAGS=
    check_gpfs_LIBS=

    check_gpfs_save_LIBS="$LIBS" 
    check_gpfs_save_LDFLAGS="$LDFLAGS"
    check_gpfs_save_CPPFLAGS="$CPPFLAGS"

    check_gpfs_configuration="none"
    ompi_check_gpfs_happy="yes"


    # Get some configuration information
    AC_ARG_WITH([gpfs],
        [AC_HELP_STRING([--with-gpfs(=DIR)],
             [Build GPFS support, optionally adding DIR/include, DIR/lib, and DIR/lib64 to the search path for headers and libraries])])
    OPAL_CHECK_WITHDIR([gpfs], [$with_gpfs], [include/gpfs.h])

    AS_IF([test -z "$with_gpfs"],
          [ompi_check_gpfs_dir="/usr"],
          [ompi_check_gpfs_dir="$with_gpfs"])

    if test -e "$ompi_check_gpfs_dir/lib64" ; then
        ompi_check_gpfs_libdir="$ompi_check_gpfs_dir/lib64"
    else
        ompi_check_gpfs_libdir="$ompi_check_gpfs_dir/lib"
    fi

    # Add correct -I and -L flags
    OPAL_CHECK_PACKAGE([$1], [gpfs.h], [gpfs], [gpfs_lib_init], [],
                       [$ompi_check_gpfs_dir], [$ompi_check_gpfs_libdir], [ompi_check_gpfs_happy="yes"],
                       [ompi_check_gpfs_happy="no"], [#include <gpfs.h>])

    AS_IF([test "$ompi_check_gpfs_happy" = "yes"],
          [$2],
          [AS_IF([test ! -z "$with_gpfs" -a "$with_gpfs" != "no"],
                  [echo GPFS support not found])
              $3])
])
