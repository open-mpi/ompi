dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Los Alamos National Security, LLC.  All rights
dnl                         reserved. 
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009-2014 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl
dnl OMPI_SETUP_FC
dnl

# This is REQUIREd, below.
AC_DEFUN_ONCE([_OMPI_SETUP_FC_BANNER],[
    ompi_show_subtitle "Fortran compiler" 
])

#############################################################################

# This is REQUIREd, below.
AC_DEFUN_ONCE([_OMPI_SETUP_FC_COMPILER],[
    OPAL_VAR_SCOPE_PUSH([ompi_fcflags_save])
    ompi_fcflags_save="$FCFLAGS"
    # Note that AC_PROG_FC will look for *any* fortran compiler, and
    # we don't want it to find an F77-only compiler.  The AC docs
    # don't recommend using the "dialect" feature of AC_PROG_FC, so
    # instead use the optional first parameter and steal the list of
    # Fortran compilers (excluding the f77 compiler names) from AC's
    # default list of compilers and use it here.  This is the main
    # reason we have an OMPI-ized version of the PROG_FC macro.
    AC_PROG_FC([gfortran f95 fort xlf95 ifort ifc efc pgfortran pgf95 lf95 f90 xlf90 pgf90 epcf90])
    FCFLAGS="$ompi_fcflags_save"
    OPAL_VAR_SCOPE_POP
])

#############################################################################

# General Fortran compiler setup
AC_DEFUN([OMPI_SETUP_FC],[
    OPAL_VAR_SCOPE_PUSH([ompi_fc_happy LDFLAGS_save fc_version])

    # Force the intro banner to be displayed first
    AC_REQUIRE([_OMPI_SETUP_FC_BANNER])

    # Unfortunately, we must REQUIRE this for the reasons cited in the
    # Autoconf (version >= 2.64) docs.
    AC_REQUIRE([_OMPI_SETUP_FC_COMPILER])

    AS_IF([test -z "$FC"],
          [AC_MSG_WARN([*** All Fortran MPI bindings disabled (could not find compiler)])
           ompi_fc_happy=0],
          [ompi_fc_happy=1])

    AS_IF([test $ompi_fc_happy -eq 1 -a "$WANT_DEBUG" = "1" -a "$enable_debug_symbols" != "no"],
          [FCFLAGS="$FCFLAGS -g"
           OPAL_FLAGS_UNIQ(FCFLAGS)
           AC_MSG_WARN([-g has been added to FCFLAGS (--enable-debug)])
          ])

    # Make sure the compiler actually works, if not cross-compiling.
    # Don't just use the AC macro so that we can have a pretty
    # message.
    AS_IF([test $ompi_fc_happy -eq 1],
          [OMPI_CHECK_COMPILER_WORKS([Fortran], [], [], [],
              [AC_MSG_ERROR([Could not run a simple Fortran program.  Aborting.])])])
    
    # OS X before 10.3 (deployment target) does not allow undefined common
    # symbols in shared libraries.  Because we can't figure out how to
    # implement MPI_STATUSES_IGNORE and friends wihtout common symbols, on
    # OS X we can't build the F90 bindings as a shared library.
    OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS=
    AS_IF([test $ompi_fc_happy -eq 1],
          [AC_MSG_CHECKING([for extra arguments to build a shared library])
           case "$host" in
               *apple-darwin*)
                   if test -z ${MACOSX_DEPLOYMENT_TARGET} ; then
                       AC_MSG_RESULT([impossible -- -static])
                       OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS="-static"
                   else
                       case ${MACOSX_DEPLOYMENT_TARGET} in
                       10.[012])
                           AC_MSG_RESULT([impossible -- -static])
                           OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS="-static"
                           ;;
                       10.*)
                           AC_MSG_RESULT([-Wl,-single_module])
                           OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS="-Wl,-single_module"
                       esac
                   fi
               ;;
               *)
                   AC_MSG_RESULT([none needed])
                   OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS=""
               ;;
           esac])
    AC_SUBST(OMPI_FORTRAN_EXTRA_SHARED_LIBRARY_FLAGS)
    
    # If we're still good, then save the extra file types.  Do this last
    # because it implies tests that should be invoked by the above tests
    # (e.g., running the fortran compiler).
    AS_IF([test $ompi_fc_happy -eq 1],
           [AC_FC_SRCEXT(f)
            AC_FC_SRCEXT(f90)])

    # Per trac #1982, on OS X, we may need some esoteric linker flags
    # in the wrapper compilers.  However, per
    # https://github.com/open-mpi/ompi/issues/259, we need to use
    # -Wl,-flat_namespace when *building* the library (and
    # -Wl,-commons,use_dylibs isn't quite sufficient).
    AS_IF([test $ompi_fc_happy -eq 1],
          [AC_MSG_CHECKING([to see if Fortran compilers need additional linker flags])
           case "$host" in
           *apple-darwin*)
               # Test whether -Wl,-flat_namespace works; if it does,
               # both use it to build the libraries, and also put it
               # in the wrapper compiler LDFLAGS.
               LDFLAGS_save=$LDFLAGS
               LDFLAGS="$LDFLAGS -Wl,-flat_namespace"
               AC_LANG_PUSH([Fortran])
               AC_LINK_IFELSE([AC_LANG_SOURCE([[program test
  integer :: i
end program]])],
                              [LDFLAGS_save=$LDFLAGS
                               OMPI_FORTRAN_WRAPPER_FLAGS="-Wl,-flat_namespace"
                               OPAL_WRAPPER_FLAGS_ADD([FCFLAGS], [$OMPI_FORTRAN_WRAPPER_FLAGS])],
                              [OMPI_FORTRAN_WRAPPER_FLAGS=none])
               AC_LANG_POP([Fortran])
               LDFLAGS=$LDFLAGS_save
               AC_MSG_RESULT([$OMPI_FORTRAN_WRAPPER_FLAGS])
               ;;
           *)
               AC_MSG_RESULT([none])
               ;;
           esac
          ])

    # Get our Fortran symbol mangling scheme
    AS_IF([test $ompi_fc_happy -eq 1],
          [OMPI_FORTRAN_FIND_EXT_SYMBOL_CONVENTION])

    # Make sure we can link with C code.
    AS_IF([test $ompi_fc_happy -eq 1],
          [OMPI_LANG_LINK_WITH_C([Fortran], [],
              [cat <<EOF
**********************************************************************
It appears that your Fortran compiler is unable to link against
object files created by your C compiler.  This typically indicates
one of a few possibilities:

  - A conflict between CFLAGS and FCFLAGS
  - A problem with your compiler installation(s)
  - Different default build options between compilers (e.g., C
    building for 32 bit and Fortran building for 64 bit)
  - Incompatible compilers

Such problems can usually be solved by picking compatible compilers
and/or CFLAGS and FCFLAGS.  More information (including exactly what
command was given to the compilers and what error resulted when the
commands were executed) is available in the config.log file in this
directory.
**********************************************************************
EOF
             AC_MSG_ERROR([C and Fortran compilers are not link compatible.  Can not continue.])])])

    # Test to see if the Fortran compilers likes the C++ exceptions
    # flags.  If it doesn't, just abort.  We *could* handle this
    # scenario (e.g., probe the Fortran compiler for what flags would
    # be necessary), but we're kinda assuming that no one will care.
    # If they do, they'll e-mail us.
    AS_IF([test $ompi_fc_happy -eq 1],
          [AC_MSG_CHECKING([to see if Fortran compiler likes the C++ exception flags])
           AS_IF([test "$OMPI_CXX_EXCEPTIONS_CXXFLAGS" = ""],
                 [AC_MSG_RESULT([skipped (no C++ exceptions flags)])],
                 [FCFLAGS="$FCFLAGS $OMPI_CXX_EXCEPTIONS_CXXFLAGS"
                  AC_LANG_PUSH([Fortran])
                  AC_COMPILE_IFELSE([AC_LANG_PROGRAM([], [[ 
INTEGER I
I = 3]])],
                      [AC_MSG_RESULT([yes])],
                      [AC_MSG_RESULT([no])
                       AC_MSG_WARN([C++ exception flags are different between the C and Fortran compilers; this configure script cannot currently handle this scenario.  Either disable C++ exception support or send mail to the Open MPI users list.])
                       AC_MSG_ERROR([*** Cannot continue])])
                  AC_LANG_POP
                 ])
          ])

    # Per #1982, on OS X, we may need some esoteric linker flags in the
    # Fortran wrapper compiler.
    AC_MSG_CHECKING([to see if mpifort compiler needs additional linker flags])
    case "$host" in
    *apple-darwin*)
        # Test whether -Wl,-commons,use_dylibs works; if it does, use it.
        LDFLAGS_save=$LDFLAGS
        LDFLAGS="$LDFLAGS -Wl,-commons,use_dylibs"
        AC_LANG_PUSH([Fortran])
        AC_LINK_IFELSE([AC_LANG_SOURCE([[program test
    integer :: i
end program]])],
                       [OMPI_FORTRAN_WRAPPER_FLAGS="-Wl,-commons,use_dylibs"
                        OPAL_WRAPPER_FLAGS_ADD([FCFLAGS], [$OMPI_FORTRAN_WRAPPER_FLAGS])],
                       [OMPI_FORTRAN_WRAPPER_FLAGS=none])
        AC_LANG_POP([Fortran])
        LDFLAGS=$LDFLAGS_save
        AC_MSG_RESULT([$OMPI_FORTRAN_WRAPPER_FLAGS])
        ;;
    *)
        AC_MSG_RESULT([none])
        ;;
    esac

    # All done
    AS_IF([test $ompi_fc_happy -eq 1],
          [$1], [$2])

    OPAL_VAR_SCOPE_POP
])dnl
