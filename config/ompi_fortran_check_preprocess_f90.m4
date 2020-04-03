dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([OMPI_FORTRAN_CHECK_PREPROCESS_F90], [
    AC_MSG_CHECKING([if Fortran compilers preprocess .F90 files without additional flag])
    cat > conftest_f.F90 << EOF
#if 0
#error The source file was not preprocessed
#endif
      program bogus
      end program
EOF
    OPAL_LOG_COMMAND([$FC $FCFLAGS -c conftest_f.F90],
                     [AC_MSG_RESULT([yes])],
                     [AC_MSG_RESULT([no])
                      AC_MSG_CHECKING([if -fpp flag works])
                      OPAL_LOG_COMMAND([$FC $FCFLAGS -fpp -c conftest_f.F90],
                                       [AC_MSG_RESULT([yes])
                                        FCFLAGS="$FCFLAGS -fpp"],
                                       [AC_MSG_RESULT(no)
                                        AC_MSG_ERROR([cannot preprocess Fortran files, Aborting])])])
    rm -f conftest*
])dnl
