
dnl Macro to add --enable-coverage option (disabled by default) and add
dnl appropriate compiler flags to permit usage of gcov if that option is
dnl enabled.  If WRAPPER_xFLAGS variables are set then the flags will also be
dnl added to those variables.
dnl
dnl Sets "pac_cv_use_coverage=yes" and AC_DEFINEs USE_COVERAGE if coverage was
dnl successfully enabled.  Also creates an AM_CONDITIONAL with the name
dnl "BUILD_COVERAGE" that is true iff pac_cv_use_coverage=yes.
dnl
dnl Usage: PAC_CONFIG_SUBDIR_ARGS
dnl
dnl Assumes that all of the compiler macros have already been invoked
dnl (AC_PROG_CC and friends).
AC_DEFUN([PAC_ENABLE_COVERAGE],[

AC_ARG_VAR([GCOV],[name/path for the gcov utility])
AC_CHECK_PROGS([GCOV],[gcov])

AC_ARG_ENABLE([coverage],
              [AC_HELP_STRING([--enable-coverage],
                              [Turn on coverage analysis using gcc and gcov])],
              [],[enable_coverage=no])

if test "$enable_coverage" = "yes" ; then
    if test "$ac_cv_prog_gcc" = "yes" ; then
        CFLAGS="$CFLAGS -fprofile-arcs -ftest-coverage"
        LIBS="$LIBS -lgcov"
        if test ${WRAPPER_CFLAGS+set} = set ; then
            WRAPPER_CFLAGS="$WRAPPER_CFLAGS -fprofile-arcs -ftest-coverage"
        fi
    else
        AC_MSG_WARN([--enable-coverage only supported for GCC])
    fi
    if test "$enable_cxx" = "yes" ; then
        if test "$ac_cv_cxx_compiler_gnu" = "yes" ; then
            CXXFLAGS="$CXXFLAGS -fprofile-arcs -ftest-coverage"
            LIBS="$LIBS -lgcov"
            if test ${WRAPPER_CXXFLAGS+set} = set ; then
                WRAPPER_CXXFLAGS="$WRAPPER_CXXFLAGS -fprofile-arcs -ftest-coverage"
            fi
        else
            AC_MSG_WARN([--enable-coverage only supported for GCC])
        fi
    fi
    # Add similar options for g77 so that the Fortran tests will also
    # 
    if test "$enable_f77" = yes ; then
        if test "$ac_cv_f77_compiler_gnu" = "yes" ; then
             FFLAGS="$FFLAGS -fprofile-arcs -ftest-coverage"
             LIBS="$LIBS -lgcov"
             if test ${WRAPPER_FFLAGS+set} = set ; then
                 WRAPPER_FFLAGS="$WRAPPER_FFLAGS -fprofile-arcs -ftest-coverage"
             fi
        else
            AC_MSG_WARN([--enable-coverage only supported for G77/GFORTRAN])
        fi
    fi
    if test "$enable_fc" = yes ; then
        if test "$ac_cv_fc_compiler_gnu" = "yes" ; then
             FCFLAGS="$FCFLAGS -fprofile-arcs -ftest-coverage"
             LIBS="$LIBS -lgcov"
             if test ${WRAPPER_FCFLAGS+set} = set ; then
                 WRAPPER_FCFLAGS="$WRAPPER_FCFLAGS -fprofile-arcs -ftest-coverage"
             fi
        else
            AC_MSG_WARN([--enable-coverage only supported for GFORTRAN])
        fi
    fi
    # On some platforms (e.g., Mac Darwin), we must also *link* 
    # with the -fprofile-args -ftest-coverage option.
    AC_MSG_CHECKING([whether compilation with coverage analysis enabled works])
    AC_LINK_IFELSE([AC_LANG_SOURCE([int main(int argc, char **argv){return 1;}])],
                   [AC_MSG_RESULT([yes])],
                   [AC_MSG_RESULT([no])
                    AC_MSG_ERROR([Unable to link programs when coverage analysis enabled])])
    
    # Test for the routines that we need to use to ensure that the
    # data files are (usually) written out
    # FIXME: Some versions of Linux provide usleep, but it rounds times
    # up to the next second (!)
    AC_CHECK_FUNCS([usleep])

    # NOTE: using a "pac_cv_" prefix but not caching because of xFLAGS "side effects"
    pac_cv_use_coverage=yes
    AC_DEFINE([USE_COVERAGE],[1],[Define if performing coverage tests])
fi
AM_CONDITIONAL([BUILD_COVERAGE],[test "X$pac_cv_use_coverage" = "Xyes"])
])

