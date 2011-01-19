dnl /*D
dnl PAC_F77_WORKS_WITH_CPP
dnl
dnl Checks if Fortran 77 compiler works with C preprocessor
dnl
dnl Most systems allow the Fortran compiler to process .F and .F90 files
dnl using the C preprocessor.  However, some systems either do not
dnl allow this or have serious bugs (OSF Fortran compilers have a bug
dnl that generates an error message from cpp).  The following test
dnl checks to see if .F works, and if not, whether "cpp -P -C" can be used
dnl D*/
AC_DEFUN([PAC_F77_WORKS_WITH_CPP],[
AC_REQUIRE([AC_PROG_CPP])
AC_MSG_CHECKING([whether Fortran 77 compiler processes .F files with C preprocessor])
AC_LANG_PUSH([Fortran 77])
saved_f77_ext=${ac_ext}
ac_ext="F"
saved_FFLAGS="$FFLAGS"
FFLAGS="$FFLAGS $CPPFLAGS"
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        program main
#define ASIZE 10
        integer a(ASIZE)
        end
    ])
])
AC_COMPILE_IFELSE([],[
    pac_cv_f77_accepts_F=yes
    ifelse([$1],[],[],[$1=""])
],[
    pac_cv_f77_accepts_F=no
    ifelse([$1],[],[:],[$1="false"])
])
# Restore Fortran 77's ac_ext but not FFLAGS
ac_ext="$saved_f77_ext"

if test "$pac_cv_f77_accepts_F" != "yes" ; then
    pac_cpp_f77="$ac_cpp -C -P conftest.F > conftest.$ac_ext"
    PAC_RUNLOG_IFELSE([$pac_cpp_f77],[
        if test -s conftest.${ac_ext} ; then
            AC_COMPILE_IFELSE([],[
                pac_cv_f77_accepts_F="no, use cpp"
                ifelse([$1],[],[],[$1="$CPP -C -P"])
            ],[])
            rm -f conftest.${ac_ext}
        fi
    ],[])
fi
FFLAGS="$saved_FFLAGS"
rm -f conftest.F
AC_LANG_POP([Fortran 77])
AC_MSG_RESULT([$pac_cv_f77_accepts_F])
])
