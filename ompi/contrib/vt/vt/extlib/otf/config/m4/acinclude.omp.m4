AC_DEFUN([CHECK_OMP],
[
    check_omp="yes"
    have_omp="no"

    AC_ARG_VAR(OPENMP_CXXFLAGS, [C++ compiler flags to enable support for OpenMP])

dnl Disable OpenMP if the PGI compiler is used to work around the following errors:
dnl compiler version  compiler error
dnl < 9.0-3           PGCC-S-0000-Internal compiler error. calc_dw_tag:no tag
dnl (see Technical Problem Report 4337 at http://www.pgroup.com/support/release_tprs_90.htm)
dnl 10.1 - 10.6       this kind of pragma may not be used here
dnl                   #pargma omp barrier
    case `$CC -V 2>&1` in
        *pgcc*)
            check_omp="no"
            ;;
    esac

    AS_IF([test "$check_omp" = "yes"],
    [
        AC_LANG_SAVE
        AC_LANG_CPLUSPLUS
        AX_OPENMP([have_omp="yes"])
        AC_LANG_RESTORE
    ])
])
