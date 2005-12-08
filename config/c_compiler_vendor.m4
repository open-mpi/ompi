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
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

AC_DEFUN([AC_C_COMPILER_VENDOR],
#
# Arguments: 
#   optional 1 and 2 for the compiler vendor and the compiler nickname
#
# Depdencies: None
#
# Check to see if the C++ compiler can handle exceptions
#
# Sets OMPI_CXX_EXCEPTIONS to 1 if compiler has exceptions, 0 if not
#
  [AC_REQUIRE([OMPI_SETUP_CC])
   AC_CACHE_CHECK([the C compiler vendor],
                  [ac_cv_c_compiler_vendor],
                  [AC_LANG_PUSH([C])
                   dnl GNU C
                   _AC_C_IFDEF([__GNU__],
                               [ac_cv_c_compiler_vendor=gnu],
                               [dnl SGI CC
                                _AC_C_IFDEF([__sgi],
                                            [ac_cv_c_compiler_vendor=sgi],
                                            [_AC_C_IFDEF([_MSC_VER],
                                                        [ac_cv_c_compiler_vendor=microsoft],
                                                        [ac_cv_c_compiler_vendor=unknown]
                                                       )
                                            ]
                                           )
                               ]
                              )
                   AC_LANG_POP([C])
                  ]
   )

   ifelse([$1], , [], [$1="$ac_cv_c_compiler_vendor"])

   dnl The compiler nickname
   ifelse([$2], , [],
     [case "$ac_cv_c_compiler_vendor" in
        gnu)       $2=gcc;;
        microsoft) $2=cl;;
        *)         $2=unknown;;
      esac])
])dnl
