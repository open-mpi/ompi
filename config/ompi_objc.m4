dnl -*- autoconf -*-
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
dnl Copyright (c) 2006      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

# XXX - FIX ME - when we drop support for 2.58 and 2.59, 
# remove this stuff

# Autoconf 2.59 and prior don't support Objective C code, so hack 
# in the minimal amount of support needed to build the XGrid
# code on Mac OS X.  Since the Objective C and C compilers are
# one and the same on OS X, there isn't a whole lot of support
# we really need here...  Loosely based on how 2.59 treats
# C++ source, plus lots of trial and error...
m4_if(m4_version_compare(m4_defn([m4_PACKAGE_VERSION]), [2.60]), -1,
[
  m4_define([AC_LANG(Objective C)],
  [ac_ext=m
  ac_cpp='$OBJCCPP $CPPFLAGS'
  ac_compile='$OBJC -c $OBJCFLAGS $CPPFLAGS conftest.$ac_ext >&AS_MESSAGE_LOG_FD'
  ac_link='$OBJC -o conftest$ac_exeext $OBJCFLAGS $CPPFLAGS $LDFLAGS conftest.$ac_ext $LIBS >&AS_MESSAGE_LOG_FD'
  ac_compiler_gnu=$ac_cv_objc_compiler_gnu
  ])

  AU_DEFUN([AC_LANG_OBJC], [AC_LANG(Objective C)])

  m4_define([_AC_LANG_ABBREV(Objective C)], [objc])

  m4_define([_AC_LANG_PREFIX(Objective C)], [OBJC])

  m4_copy([AC_LANG_SOURCE(C)], [AC_LANG_SOURCE(Objective C)])
  m4_copy([AC_LANG_PROGRAM(C)], [AC_LANG_PROGRAM(Objective C)])

  # m4_defun instead of AC_DEFUN because there's a bad character for
  # the shell in the macro name and AM 1.9.6 appears to really not
  # like that behavior...
  m4_defun([AC_LANG_COMPILER(Objective C)], 
    [AC_REQUIRE([AC_PROG_OBJC])])

  AC_DEFUN([AC_PROG_OBJC],
    [AC_LANG_PUSH(Objective C)dnl
     AC_ARG_VAR([OBJC],      [Objective C compiler command])dnl
     AC_ARG_VAR([OBJCFLAGS], [Objective C compiler flags])dnl
     _AC_ARG_VAR_LDFLAGS()dnl
     _AC_ARG_VAR_CPPFLAGS()dnl
     _AC_ARG_VAR_PRECIOUS([OBJC])dnl
     AC_CHECK_TOOLS(OBJC,
                   [m4_default([$1], [gcc objcc cc])], gcc)
     # Provide some information about the compiler.
     echo "$as_me:$LINENO:" \
       "checking for _AC_LANG compiler version" >&AS_MESSAGE_LOG_FD
     ac_compiler=`set X $ac_compile; echo $[2]`
     _AC_EVAL([$ac_compiler --version </dev/null >&AS_MESSAGE_LOG_FD])
     _AC_EVAL([$ac_compiler -v </dev/null >&AS_MESSAGE_LOG_FD])
     _AC_EVAL([$ac_compiler -V </dev/null >&AS_MESSAGE_LOG_FD])

     m4_expand_once([_AC_COMPILER_EXEEXT])[]dnl
     m4_expand_once([_AC_COMPILER_OBJEXT])[]dnl
     _AC_LANG_COMPILER_GNU
     GOBJC=`test $ac_compiler_gnu = yes && echo yes`
     AC_LANG_POP(Objective C)dnl
    _AM_DEPENDENCIES(OBJC)
  ])
])
