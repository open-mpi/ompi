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

AC_DEFUN([_AC_C_IFDEF],
  [AC_COMPILE_IFELSE([#ifndef $1
                      # error "Macro $1 is undefined!"
                      /* For some compilers (eg. SGI's CC), #error is not
                         enough...  */
                      please, do fail
                      #endif],
                     [$2], [$3])])

AC_DEFUN([AC_CXX_COMPILER_VENDOR],
#
# Arguments:
#   Optional 1 and 2 : compiler vendor and compiler nickname.
#
# Depdencies: None
#
# Check to see if the C++ compiler can handle exceptions
#
# Sets OMPI_CXX_EXCEPTIONS to 1 if compiler has exceptions, 0 if not
#
  [AC_REQUIRE([AC_PROG_CXX])
   AC_REQUIRE([AC_PROG_CXXCPP])
   AC_CACHE_CHECK([the C++ compiler vendor],
    [ac_cv_cxx_compiler_vendor],

    [AC_LANG_PUSH([C++])

     dnl GNU C++
     _AC_C_IFDEF([__GNUG__],
       [ac_cv_cxx_compiler_vendor=gnu],
       [_AC_C_IFDEF([__DECCXX],
         [ac_cv_cxx_compiler_vendor=compaq],
         [dnl HP's aCC
          _AC_C_IFDEF([__HP_aCC],
           [ac_cv_cxx_compiler_vendor=hp],
           [dnl SGI CC
            _AC_C_IFDEF([__sgi],
             [ac_cv_cxx_compiler_vendor=sgi],
             [dnl Note:  We are using the C compiler because VC++ doesn't
              dnl recognize `.cc'(which is used by `configure') as a C++ file
              dnl extension and requires `/TP' to be passed.
              AC_LANG_PUSH([C])
              _AC_C_IFDEF([_MSC_VER],
                [ac_cv_cxx_compiler_vendor=microsoft],
                [ac_cv_cxx_compiler_vendor=unknown])
              AC_LANG_POP()])])])])

     AC_LANG_POP()])
   ifelse([$1], , [], [$1="$ac_cv_cxx_compiler_vendor"])

   dnl The compiler nickname
   ifelse([$2], , [],
     [case "$ac_cv_cxx_compiler_vendor" in
        gnu)       $2=g++;;
        compaq)    $2=cxx;;
        hp)        $2=aCC;;
        sgi)       $2=CC;;
        microsoft) $2=cl;;
        *)         $2=unknown;;
      esac])])dnl
