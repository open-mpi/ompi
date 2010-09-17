AC_DEFUN([CHECK_OMP],
[
    have_omp="no"

    AC_ARG_VAR(OPENMP_CXXFLAGS, [C++ compiler flags to enable support for OpenMP])

    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    AX_OPENMP([have_omp="yes"])
    AC_LANG_RESTORE
])
