dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2019      Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# Check whether or not the C compiler supports ISO_Fortran_binding.h
# Also check whether C and Fortran compiler interoperate.
#
# OMPI_FORTRAN_CHECK_TS([action if found], [action if not found])
# ----------------------------------------------------
AC_DEFUN([OMPI_FORTRAN_CHECK_TS],[
    AS_VAR_PUSHDEF([fortran_ts], [ompi_cv_fortran_have_ts])

    AC_CHECK_HEADERS([ISO_Fortran_binding.h],
      [AC_CACHE_CHECK([if Fortran and C compilers support ISO_Fortran_binding.h], fortran_ts,
         [mkdir conftest.$$
          cd conftest.$$

          # Try to compile the C bindings
          cat > conftest_c.c << EOF
#include <ISO_Fortran_binding.h>

int is_contiguous_c(CFI_cdesc_t* x) {
    return CFI_is_contiguous(x);
}
EOF
          OPAL_LOG_COMMAND([$CC $CCFLAGS -c conftest_c.c],
              [cat > conftest.f90 << EOF
module MOD_IS_CONTIGUOUS

interface

function is_contiguous(buf) BIND(C, name="is_contiguous_c")
   implicit none
   type(*), dimension(..) :: buf
   integer :: is_contiguous
end function is_contiguous

end interface

end module

program test_is_contiguous
   use MOD_IS_CONTIGUOUS
   implicit none
   integer :: a0, a1(2), a2(2,2), a3(2,2,2)
   write (*,*) is_contiguous(a0)
   write (*,*) is_contiguous(a1)
   write (*,*) is_contiguous(a2)
   write (*,*) is_contiguous(a3)
end program
EOF
               OPAL_LOG_COMMAND([$FC $FCFLAGS $FCFLAGS_f90 -o conftest conftest.f90 conftest_c.o $LDFLAGS $LIBS],
                   [AS_VAR_SET(fortran_ts, yes)],
                   [AS_VAR_SET(fortran_ts, no)])],
              [AS_VAR_SET(fortran_ts, no)])
          cd ..
          rm -rf conftest.$$])],
         [AS_VAR_SET(fortran_ts, no)])

    AS_VAR_IF(fortran_ts, [yes], [$1], [$2])
    AS_VAR_POPDEF([fortran_ts])dnl
])
