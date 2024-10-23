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
dnl Copyright (c) 2009-2020 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2015-2020 Research Organization for Information Science
dnl                         and Technology (RIST).  All rights reserved.
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
    opal_show_subtitle "Fortran compiler"
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
    AC_PROG_FC([gfortran f95 fort xlf95 ifort ifc efc pgfortran pgf95 lf95 f90 xlf90 pgf90 epcf90 nagfor nvfortran])
    FCFLAGS="$ompi_fcflags_save"
    OPAL_VAR_SCOPE_POP
])

#############################################################################

dnl On macOS with Xcode, test whether -Wl,-commons,use_dylibs works
dnl by itself or whether it also needs -Wl,-ld_classic.
dnl
dnl Backstory
dnl
dnl The history is that for a long time (decades),
dnl -Wl,-commons,use_dylibs worked by itself.
dnl
dnl XCode 15 introduced a a new linker (either "the new linker" or
dnl "ld_prime", according to
dnl https://developer.apple.com/forums/thread/715385). The new linker
dnl originally did not support "-commons use_dylibs", but Apple recently
dnl added support for that feature to the new linker in the XCode 16
dnl beta. "-ld_classic" forces using the old linker (which doesn't support
dnl some other features that customers might like, but Open MPI doesn't
dnl use for its Fortran bindings, like mergable libraries).
dnl
dnl Sidenode: Open MPI needs this "-commons use_dylibs" functionality
dnl because Fortran sentinel values (e.g., MPI_BOTTOM) are implemented
dnl with Fortran common blocks.
dnl
dnl So there's three cases:
dnl
dnl 1. Customer's default linker is the classic linker, which always
dnl    supported "-commons use_dylibs".
dnl 2. Customer's default linker is the new linker, but not new enough
dnl    to support "-commons use_dylibs", so we need to force using the old
dnl    linker via "-ld_classic".
dnl 3. Customer's default linker is the new linker, new enough to support
dnl    "-commons use_dylibs", so we do not want to force using the old
dnl    linker.
dnl
dnl We have to use a slightly complex test code that will actually
dnl fail if the version of Xcode being used requires "-ld_classic"
dnl with "-commons,use_dylibs".
dnl
dnl 1. Build a shared library (with C source code) with a public
dnl    symbol that can be used as a Fortran common block symbol.
dnl 2. Compile a Fortran program that calls a function in the shared
dnl    library, and link it against the shared library.
dnl
dnl Note: This is a linker test; we are checking to see if this all
dnl compiles and links properly.  The logic in the C / Fortran code
dnl below specifically does not test for correctness because we do not
dnl actually run the code.
AC_DEFUN([_OMPI_SETUP_FC_XCODE_COMMONS_LDFLAGS],[
    OPAL_VAR_SCOPE_PUSH([xcode_flags])

    # This variable is used by the invoking macro to display the
    # results via AC RESULT (just to keep the symmetry of
    # MSG_CHECKING / RESULT in the same upper-level macro).
    OMPI_FORTRAN_WRAPPER_FLAGS=

    xcode_flags="-Wl,-commons,use_dylibs"
    _OMPI_SETUP_FC_XCODE_COMMONS_LDFLAGS_BACKEND(
        [$xcode_flags],
        [OMPI_FORTRAN_WRAPPER_FLAGS=$xcode_flags], [])
    AS_IF([test -z "$OMPI_FORTRAN_WRAPPER_FLAGS"],
          [xcode_flags="-Wl,-commons,use_dylibs -Wl,-ld_classic"
           _OMPI_SETUP_FC_XCODE_COMMONS_LDFLAGS_BACKEND(
               [$xcode_flags],
               [OMPI_FORTRAN_WRAPPER_FLAGS=$xcode_flags], [])])
    AS_IF([test -z "$OMPI_FORTRAN_WRAPPER_FLAGS"],
          [OMPI_FORTRAN_WRAPPER_FLAGS="none"])

    OPAL_VAR_SCOPE_POP
])

dnl Companion to _OMPI SETUP_FC_XCODE_COMMONS_LDFLAGS;
dnl see that macro for an explanation of this macro.
dnl
dnl $1: LDFLAGS to test
dnl $2: action to perform upon success
dnl $3: action to perform upon failure
AC_DEFUN([_OMPI_SETUP_FC_XCODE_COMMONS_LDFLAGS_BACKEND],[
    OPAL_VAR_SCOPE_PUSH([xcode_happy xcode_dir LDFLAGS_save_xcode LIBS_save_xcode])

    xcode_dir=conftest.$$
    rm -rf $xcode_dir
    mkdir -p $xcode_dir
    cd $xcode_dir

    LIBS_save_xcode=$LIBS
    LDFLAGS_save_xcode=$LDFLAGS
    LDFLAGS="$LDFLAGS -L. $1"

    # Note: we use COMPILE_IFELSE and LANG_SOURCE below, which assume
    # that confdefs.h exists.  This is being invoked extremely early
    # in the configure sequence, so we haven't AC DEFINE'ed anything
    # yet, and therefore confdefs.h won't be automatically created
    # yet.  So we'll make an empty confdefs.h to avoid some error
    # messages (it'll be removed with the whole tempdir, later).
    touch confdefs.h

    # Step 1: make a C library with some public symbols
    xcode_happy=0
    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([AC_LANG_SOURCE([[
/* Must end the symbol in _ (remember: we are specifically targeting
   the MacOS compilation environment, so it is ok to target a specific
   Fortran symbol convention), otherwise the Fortran linker will not
   find it, and will just create a new Fortran symbol for it */
int ompi_mpi_bottom_ = 42;

void ompi_init_f(int *bogus);

/* Empty / useless function that still ensures that this compilation
   unit will not be optimized out */
void ompi_init_f(int *bogus)
{
    *bogus = ompi_mpi_bottom_;
}
]])],
        [ # If the above compiled successfully, Then use
          # conftest.OBJEXT to make the library.  Note that
          # conftest.OBJEXT will automatically be deleted upon exit of
          # COMPILE_IFELSE.
          #
          # NOTE: this is pretty gross -- we're manually making a
          # shared library.  But the libtool binary doesn't exist yet,
          # so this is the best that we can do.
         OPAL_LOG_COMMAND([$CC -dynamiclib -Wl,-undefined -Wl,dynamic_lookup $LDFLAGS conftest.$OBJEXT -o libconftest.dylib],
                          [xcode_happy=1])])
    AC_LANG_POP

    # Now compile and link a Fortran program against this shared
    # library.
    AC_LANG_PUSH([Fortran])
    AS_IF([test $xcode_happy -eq 1],
          [LIBS="$LIBS -lconftest"
           AC_LINK_IFELSE([AC_LANG_SOURCE([
program test
  integer :: mpi_bottom
  common/ompi_mpi_bottom/mpi_bottom

  interface
     subroutine ompi_init(bogus) BIND(C, name="ompi_init_f")
       implicit none
       integer bogus
     end subroutine ompi_init
  end interface

  integer bogus
  call ompi_init(bogus)
end program
])],

                          [],
                          [xcode_happy=0])])
    AC_LANG_POP

    # Exit the temp dir
    cd ..
    rm -rf $xcode_dir

    # LIBS was set specifically for the artificial conditions of this
    # test, so reset it
    LIBS=$LIBS_save_xcode

    AS_IF([test $xcode_happy -eq 1],
          [ # Restore LDFLAGS + the new flags (i.e., get rid of the
            # "-L." we added for this test)
           LDFLAGS="$LDFLAGS_save_xcode $1"
           $2],
          [ # If we failed the test, reset LDFLAGS back to its
            # original value.
           LDFLAGS=$LDFLAGS_save_xcode
           $3])

    OPAL_VAR_SCOPE_POP
])

#############################################################################

# General Fortran compiler setup
AC_DEFUN([OMPI_SETUP_FC],[
    OPAL_VAR_SCOPE_PUSH([ompi_fc_happy LDFLAGS_save fc_version OMPI_FORTRAN_WRAPPER_FLAGS])

    # Force the intro banner to be displayed first
    AC_REQUIRE([_OMPI_SETUP_FC_BANNER])

    # Unfortunately, we must REQUIRE this for the reasons cited in the
    # Autoconf (version >= 2.64) docs.
    AC_REQUIRE([_OMPI_SETUP_FC_COMPILER])

    # If $FC is "no", that's another way of the user telling us "I
    # don't want any Fortran compiler".  That being said, there are
    # already existing code paths that expect an empty $FC to mean "no
    # Fortran compiler", so turn "no" into "" here.
    AS_IF([test "$FC" = "no"], [FC=])

    AS_IF([test -z "$FC"],
          [AC_MSG_WARN([*** All Fortran MPI bindings disabled (could not find compiler)])
           ompi_fc_happy=0],
          [ompi_fc_happy=1])

    # Make sure the compiler actually works, if not cross-compiling.
    # Don't just use the AC macro so that we can have a pretty
    # message.
    AS_IF([test $ompi_fc_happy -eq 1],
          [OPAL_CHECK_COMPILER_WORKS([Fortran], [], [], [],
              [AC_MSG_ERROR([Could not run a simple Fortran program.  Aborting.])])])

    # OS X before 10.3 (deployment target) does not allow undefined common
    # symbols in shared libraries.  Because we can't figure out how to
    # implement MPI_STATUSES_IGNORE and friends without common symbols, on
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

    # The Absoft compiler does not like the fact that we use lots of
    # "ignore TKR" comment pragmas that it doesn't understand, and
    # will warn about them.  From Tony Goetz at Absoft, we can use the
    # -Z790 flag to quell these warnings.
    # The NAG compiler is too picky about naming conventions, so use the
    # -mismatch flag to keep it happy
    AC_MSG_CHECKING([for $FC warnings flags])
    fc_version=`$FC --version 2>&1`
    case "$fc_version" in
    *Absoft*)
        AC_MSG_RESULT([-Z790])
        FCFLAGS="$FCFLAGS -Z790"
        ;;
    *NAG*)
        AC_MSG_RESULT([-mismatch])
        FCFLAGS="$FCFLAGS -mismatch"
        ;;
    *)
        AC_MSG_RESULT([none])
        ;;
    esac

    # If we're still good, then save the extra file types.  Do this last
    # because it implies tests that should be invoked by the above tests
    # (e.g., running the fortran compiler).
    AS_IF([test $ompi_fc_happy -eq 1],
           [AC_FC_SRCEXT(f)
            AC_FC_SRCEXT(f90)])

    # Check to see if we need additional compiler flags for
    # preprocessing .F90 files.
    AS_IF([test $ompi_fc_happy -eq 1],
          [OMPI_FORTRAN_CHECK_PREPROCESS_F90])

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
          [OPAL_LANG_LINK_WITH_C([Fortran], [],
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

    # Per Trac #1982, on OS X, we may need some esoteric linker flags in the
    # Fortran wrapper compiler.
    AC_MSG_CHECKING([to see if mpifort compiler needs additional linker flags])
    case "$host" in
    *apple-darwin*)
        _OMPI_SETUP_FC_XCODE_COMMONS_LDFLAGS
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
