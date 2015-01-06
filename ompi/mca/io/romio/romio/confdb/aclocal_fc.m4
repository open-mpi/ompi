dnl PAC_FC_SEARCH_LIST - expands to a whitespace separated list of modern
dnl fortran compilers for use with AC_PROG_FC that is more suitable for HPC
dnl software packages
AC_DEFUN([PAC_FC_SEARCH_LIST],
         [ifort pgf90 pathf90 pathf95 xlf90 xlf95 xlf2003 gfortran f90 epcf90 f95 fort lf95 g95 ifc efc gfc])
dnl /*D
dnl PAC_PROG_FC([COMPILERS])
dnl
dnl COMPILERS is a space separated list of Fortran 77 compilers to search
dnl for.  Fortran 95 isn't strictly backwards-compatible with Fortran 77,
dnl but `f95' is worth trying.
dnl
dnl Compilers are ordered by
dnl  1. F77, F90, F95
dnl  2. Good/tested native compilers, bad/untested native compilers
dnl  3. Wrappers around f2c go last.
dnl
dnl `fort77' is a wrapper around `f2c'.
dnl It is believed that under HP-UX `fort77' is the name of the native
dnl compiler.  On some Cray systems, fort77 is a native compiler.
dnl frt is the Fujitsu F77 compiler.
dnl pgf77 and pgf90 are the Portland Group F77 and F90 compilers.
dnl xlf/xlf90/xlf95/xlf2003 are IBM (AIX) F77/F90/F95/F2003 compilers.
dnl lf95 is the Lahey-Fujitsu compiler.
dnl fl32 is the Microsoft Fortran "PowerStation" compiler.
dnl af77 is the Apogee F77 compiler for Intergraph hardware running CLIX.
dnl epcf90 is the "Edinburgh Portable Compiler" F90.
dnl fort is the Compaq Fortran 90 (now 95) compiler for Tru64 and Linux/Alpha.
dnl pathf90 is the Pathscale Fortran 90 compiler
dnl ifort is another name for the Intel f90 compiler
dnl efc - An older Intel compiler (?)
dnl ifc - An older Intel compiler
dnl fc  - A compiler on some unknown system.  This has been removed because
dnl       it may also be the name of a command for something other than
dnl       the Fortran compiler (e.g., fc=file system check!)
dnl gfortran - The GNU Fortran compiler (not the same as g95) 
dnl gfc - An alias for gfortran recommended in cygwin installations
dnl NOTE: this macro suffers from a basically intractable "expanded before it
dnl was required" problem when libtool is also used
dnl D*/
AC_DEFUN([PAC_PROG_FC],[
PAC_PUSH_FLAG([FCFLAGS])
AC_PROG_FC([m4_default([$1],[PAC_FC_SEARCH_LIST])])
PAC_POP_FLAG([FCFLAGS])
])
dnl
dnl PAC_FC_EXT checks for the default Fortran 90 program extension, f90 then f.
dnl This could be replaced by AC_FC_SRCEXT but since AC_FC_SRCEXT
dnl adds FCFLAGS_ext, which is used to modify FCFLAGS or Makefile.in.
dnl So will do this later.
dnl
AC_DEFUN([PAC_FC_EXT],[
AC_MSG_CHECKING([for extension for Fortran 90 programs])
ac_fc_srcext="f90"
AC_LANG_PUSH(Fortran)
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM()
],[
    AC_MSG_RESULT([f90])
],[
    ac_fc_srcext="f" 
    AC_COMPILE_IFELSE([
        AC_LANG_PROGRAM()
    ],[
        AC_MSG_RESULT([f])
    ],[
        AC_MSG_RESULT([unknown!])
    ])
])
AC_LANG_POP(Fortran)
])
dnl
dnl Internal routine for testing F90
dnl PAC_PROG_FC_WORKS()
dnl
AC_DEFUN([PAC_PROG_FC_WORKS],[
AC_REQUIRE([PAC_FC_EXT])
AC_LANG_PUSH(Fortran)
AC_MSG_CHECKING([whether the Fortran 90 compiler ($FC $FCFLAGS $LDFLAGS) works])
AC_LINK_IFELSE([
    AC_LANG_SOURCE([
        program conftest
        integer, dimension(10) :: n
        end
    ])
],[
    pac_cv_prog_fc_works="yes"
    AC_MSG_RESULT([$pac_cv_prog_fc_works])
    AC_MSG_CHECKING([whether the Fortran 90 compiler ($FC $FCFLAGS $LDFLAGS) is a cross-compiler])
    AC_RUN_IFELSE([],
    [pac_cv_prog_fc_cross="no"],
    [pac_cv_prog_fc_cross="yes"],
    [pac_cv_prog_fc_cross="$cross_compiling"]
    )
    AC_MSG_RESULT($pac_cv_prog_fc_cross)
],[
    pac_cv_prog_fc_works="no"
    AC_MSG_WARN([installation or configuration problem: Fortran 90 compiler cannot create executables.])
])
# The intel compiler sometimes generates these work.pc and .pcl files
rm -f work.pc work.pcl
AC_LANG_POP(Fortran)
dnl cross_compiling no longer maintained by autoconf as part of the
dnl AC_LANG changes.  If we set it here, a later AC_LANG may not 
dnl restore it (in the case where one compiler claims to be a cross compiler
dnl and another does not)
dnl cross_compiling=$pac_cv_prog_f90_cross
])
dnl/*D 
dnl PAC_PROG_FC_INT_KIND - Determine kind parameter for an integer with
dnl the specified number of bytes.
dnl
dnl Synopsis:
dnl  PAC_PROG_FC_INT_KIND(variable-to-set,number-of-bytes,[cross-size])
dnl
dnl D*/
AC_DEFUN([PAC_PROG_FC_INT_KIND],[
# Set the default
$1=-1
if test "$pac_cv_prog_fc_cross" = "yes" ; then
    AS_IF([test -z "$3"],[AC_MSG_ERROR(['$3' is empty])])
    $1="$3"
else
    AC_LANG_PUSH(Fortran)
    AC_MSG_CHECKING([for Fortran 90 integer kind for $2-byte integers])
    # Convert bytes to digits
    case $2 in 
        1) sellen=2 ;;
        2) sellen=4 ;;
        4) sellen=8 ;;
        8) sellen=16 ;;
       16) sellen=30 ;;
        *) sellen=8 ;;
    esac
    # Check for cached value
    eval testval=\$"pac_cv_prog_fc_int_kind_$sellen"
    if test -n "$testval" ; then 
        AC_MSG_RESULT([$testval (cached)])
        $1=$testval
    else
        KINDVAL="unavailable"
        eval "pac_cv_prog_fc_int_kind_$sellen"=-1
        AC_RUN_IFELSE([
            AC_LANG_SOURCE([
                program main
                integer ii
                ii = selected_int_kind($sellen)
                open(8, file="conftest1.out", form="formatted")
                write (8,*) ii
                close(8)
                stop
                end
            ])
        ],[pac_run_ok=yes],[pac_run_ok=no])
        if test "$pac_run_ok" = "yes" ; then
            if test -s conftest1.out ; then
                # Because of write, there may be a leading blank.
                KINDVAL=`cat conftest1.out | sed 's/ //g'`
                eval "pac_cv_prog_fc_int_kind_$sellen"=$KINDVAL
                $1=$KINDVAL
            fi
        fi
        AC_MSG_RESULT([$KINDVAL])
    fi # not cached
    AC_LANG_POP(Fortran)
fi # is not cross compiling
])dnl
dnl
dnl ------------------------------------------------------------------------
dnl Special characteristics that have no autoconf counterpart but that
dnl we need as part of the Fortran 90 support.  To distinquish these, they
dnl have a [PAC] prefix.
dnl 
dnl At least one version of the Cray compiler needs the option -em to
dnl generate a separate module file, rather than including the module
dnl information in the object (.o) file.
dnl
dnl
dnl PAC_FC_MODULE_EXT(action if found,action if not found)
dnl
AC_DEFUN([PAC_FC_MODULE_EXT],
[AC_CACHE_CHECK([for Fortran 90 module extension],
pac_cv_fc_module_ext,[
pac_cv_fc_module_case="unknown"
AC_LANG_PUSH(Fortran)
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        module conftest
        integer n
        parameter (n=1)
        end module conftest
    ])
],[
    # Look for module name
    # First, try to find known names.  This avoids confusion caused by
    # additional files (like <name>.stb created by some versions of pgf90)
    # Early versions of the Intel compiler used d as the module extension;
    # we include that just to finish the test as early as possible.
    for name in conftest CONFTEST ; do
        for ext in mod MOD d ; do
            if test -s $name.$ext ; then
                if test $name = conftest ; then
                    pac_cv_fc_module_case=lower
                else
                    pac_cv_fc_module_case=upper
                fi
                pac_cv_fc_module_ext=$ext
                pac_MOD=$ext
                break
            fi
        done
        if test -n "$pac_cv_fc_module_ext" ; then break ; fi
    done
    if test -z "$pac_MOD" ; then
        # The test on .err is needed for Cray Fortran.
        pac_MOD=`ls conftest.* 2>&1 | grep -v conftest.${ac_fc_srcext} | grep -v conftest.o | grep -v conftest.err`
        pac_MOD=`echo $pac_MOD | sed -e 's/conftest\.//g'`
        pac_cv_fc_module_case="lower"
        if test "X$pac_MOD" = "X" ; then
            pac_MOD=`ls CONFTEST* 2>&1 | grep -v CONFTEST.${ac_fc_srcext} | grep -v CONFTEST.o | grep -v CONFTEST.err`
            pac_MOD=`echo $pac_MOD | sed -e 's/CONFTEST\.//g'`
            if test -n "$pac_MOD" -a -s "CONFTEST.$pac_MOD" ; then
                pac_cv_fc_module_case="upper"
            else
                # Clear because we must have gotten an error message
                pac_MOD=""
            fi
        fi
        if test -z "$pac_MOD" ; then 
            pac_cv_fc_module_ext="unknown"
        else
            pac_cv_fc_module_ext=$pac_MOD
        fi
    fi
],[
    pac_cv_fc_module_ext="unknown"
])

if test "$pac_cv_fc_module_ext" = "unknown" ; then
    # Try again, but with an -em option.  Abbreviated, because we're
    # just looking for the Cray option
    saveFCFLAGS=$FCFLAGS
    FCFLAGS="$FCFLAGS -em"
    AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        module conftest
        integer n
        parameter (n=1)
        end module conftest
    ])
],[
    if test -s conftest.mod ; then
        pac_cv_fc_module_ext="mod"
        pac_cv_fc_module_case="lower"
    elif test -s CONFTEST.mod ; then
        pac_cv_fc_module_ext="mod"
        pac_cv_fc_module_case="upper"
    fi
],[
    :
    # do nothing - already have the unknown default value
])
    if test "$pac_cv_fc_module_ext" = "unknown" ; then
        # The additional command line option did not help - restore
        # the original flags.
        FCFLAGS=$saveFCFLAGS
    fi
fi
AC_LANG_POP(Fortran)
])
#
AC_SUBST(FCMODEXT)
if test "$pac_cv_fc_module_ext" = "unknown" ; then
    ifelse($2,,:,[$2])
else
    ifelse($1,,FCMODEXT=$pac_MOD,[$1])
fi
])
dnl
dnl
dnl PAC_FC_MODULE_INCFLAG
AC_DEFUN([PAC_FC_MODULE_INCFLAG],[
AC_REQUIRE([PAC_FC_MODULE_EXT])
AC_CACHE_CHECK([for Fortran 90 module include flag],
pac_cv_fc_module_incflag,[
AC_LANG_PUSH(Fortran)
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        module conf
        integer n
        parameter (n=1)
        end module conf
    ])
])
pac_madedir="no"
if test ! -d conf ; then mkdir conftestdir ; pac_madedir="yes"; fi
if test "$pac_cv_fc_module_case" = "upper" ; then
    pac_module="CONF.$pac_cv_fc_module_ext"
else
    pac_module="conf.$pac_cv_fc_module_ext"
fi
AC_COMPILE_IFELSE([],[
    if test -s "$pac_module" ; then
        mv $pac_module conftestdir
        # Remove any temporary files, and hide the work.pc file
        # (if the compiler generates them)
        if test -f work.pc ; then 
            mv -f work.pc conftest.pc
        fi
        rm -f work.pcl
    else
        AC_MSG_WARN([Unable to build a simple Fortran 90 module])
        # echo "configure: failed program was:" >&AS_MESSAGE_LOG_FD
        # cat conftest.$ac_ext >&AS_MESSAGE_LOG_FD
        _AC_MSG_LOG_CONFTEST
    fi
],[])
# Remove the conftest* after AC_LANG_CONFTEST
rm -rf conftest.dSYM
rm -f conftest.$ac_ext

dnl Create the conftest here so the test isn't created everytime inside loop.
AC_LANG_CONFTEST([AC_LANG_PROGRAM([],[use conf])])

# Save the original FCFLAGS
saved_FCFLAGS="$FCFLAGS"
pac_cv_fc_module_incflag=""
for inchdr in '-I' '-M' '-p' ; do
    FCFLAGS="$saved_FCFLAGS ${inchdr}conftestdir"
    AC_COMPILE_IFELSE([],[pac_cv_fc_module_incflag="$inchdr" ; break])
done
if test "X$pac_cv_fc_module_incflag" = "X" ; then
    if test -s conftest.pc ; then
        mv conftest.pc conftestdir/mpimod.pc
        echo "mpimod.pc" > conftestdir/mpimod.pcl
        echo "`pwd`/conftestdir/mpimod.pc" >> conftestdir/mpimod.pcl
        inchdr='-cl,'
        FCFLAGS="$save_FCFLAGS ${inchdr}conftestdir"
        AC_COMPILE_IFELSE([], [pac_fcompile_ok=yes], [pac_fcompile_ok=no])
        if test "$pac_fcompile_ok" = "yes" ; then
            pac_cv_fc_module_incflag="$inchdr"
            # Not quite right; see the comments that follow
            AC_MSG_RESULT([-cl,filename where filename contains a list of files and directories])
            FC_WORK_FILES_ARG="-cl,mpimod.pcl"
            FCMODINCSPEC="-cl,<dir>/<file>mod.pcl"
        else 
            # The version of the Intel compiler that I have refuses to let
            # you put the "work catalog" list anywhere but the current directory.
            pac_cv_fc_module_incflag="Unavailable!"
        fi
    else
        # Early versions of the Intel ifc compiler required a *file*
        # containing the names of files that contained the names of the
        # 
        # -cl,filename.pcl
        #   filename.pcl contains
        #     fullpathname.pc
        # The "fullpathname.pc" is generated, I believe, when a module is 
        # compiled.  
        # Intel compilers use a wierd system: -cl,filename.pcl .  If no file is
        # specified, work.pcl and work.pc are created.  However, if you specify
        # a file, it must contain the name of a file ending in .pc .  Ugh!
        pac_cv_fc_module_incflag="unknown"
    fi
fi
# Restore the original FCFLAGS
FCFLAGS="$saved_FCFLAGS"
if test "$pac_madedir" = "yes" ; then rm -rf conftestdir ; fi
# Remove the conftest* after AC_LANG_CONFTEST
# This is needed for Mac OSX 10.5
rm -rf conftest.dSYM
rm -f conftest*
AC_LANG_POP(Fortran)
])
AC_SUBST(FC_WORK_FILES_ARG)
AC_SUBST(FCMODINCFLAG)
FCMODINCFLAG=$pac_cv_fc_module_incflag
])
dnl
dnl
dnl
AC_DEFUN([PAC_FC_MODULE],[
PAC_FC_MODULE_EXT
PAC_FC_MODULE_INCFLAG
PAC_FC_MODULE_OUTFLAG
])
dnl
dnl PAC_FC_MODULE_OUTFLAG
AC_DEFUN([PAC_FC_MODULE_OUTFLAG],[
AC_REQUIRE([PAC_FC_MODULE_EXT])
AC_CACHE_CHECK([for Fortran 90 module output directory flag],
               [pac_cv_fc_module_outflag],
[
AC_LANG_PUSH([Fortran])
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        module conf
        integer n
        parameter (n=1)
        end module conf
    ])
])
pac_madedir="no"
if test ! -d conf ; then mkdir conftestdir ; pac_madedir="yes"; fi
if test "$pac_cv_fc_module_case" = "upper" ; then
    pac_module="CONF.$pac_cv_fc_module_ext"
else
    pac_module="conf.$pac_cv_fc_module_ext"
fi

# check base case that the compiler can create modules and that they endup in
# the current directory
AC_COMPILE_IFELSE([],[
    if test -s "$pac_module" ; then
        rm -f "$pac_module"
        # Remove any temporary files, and hide the work.pc file
        # (if the compiler generates them)
        if test -f work.pc ; then 
            mv -f work.pc conftest.pc
        fi
        rm -f work.pcl
    else
        AC_MSG_WARN([Unable to build a simple Fortran 90 module])
        # echo "configure: failed program was:" >&AS_MESSAGE_LOG_FD
        # cat conftest.$ac_ext >&AS_MESSAGE_LOG_FD
        _AC_MSG_LOG_CONFTEST
    fi
],[])

# known flags for reasonably recent versions of various f90 compilers:
#   gfortran -J${dir}
#   xlf -qmoddir=${dir}
#   pgf90 -module ${dir}
#   ifort -module ${dir}
#   nagfor -mdir ${dir}
#   ftn -J ${dir}              ## the Cray fortran compiler
#   ftn -em -J${dir}           ## the Cray fortran compiler (crayftn, in 2013)
#      For this above case, we must have added -em to FCFLAGS, since other
#      module tests do not always use the module output flag.  See
#      FC_MODULE_EXT , where this is determined.
#   f95 -YMOD_OUT_DIR=${dir}   ## the Absoft fortran compiler
#   lf95 -Am -mod ${dir}       ## the Lahey/Fujitsu fortran compiler
#   f90 -moddir=${dir}         ## the Sun f90 compiler
#   g95 -fmod=${dir}
#
# If there are any compilers still out there that are totally brain-dead and
# don't support an output directory flag, we can write a wrapper script to tell
# users to use.  Alternatively they can use an older version of MPICH.

pac_cv_fc_module_outflag=
for mod_flag in '-J' '-J ' '-qmoddir=' '-module ' '-YMOD_OUT_DIR=' '-mdir ' '-moddir=' '-fmod=' ; do
    rm -f conftestdir/NONEXISTENT conftestdir/*
    PAC_PUSH_FLAG([FCFLAGS])
    FCFLAGS="$FCFLAGS ${mod_flag}conftestdir"
    AC_COMPILE_IFELSE([],[pac_build_success=yes],[pac_build_success=no])
    AS_IF([test "X$pac_build_success" = Xyes],
          [AS_IF([test -s "conftestdir/${pac_module}"],
                 [pac_cv_fc_module_outflag="$mod_flag"])])
    PAC_POP_FLAG([FCFLAGS])
    AS_IF([test "X$pac_cv_fc_module_outflag" = X],[:],[break])
done

# Remove the conftest* after AC_LANG_CONFTEST
rm -rf conftest.dSYM
rm -f conftest.$ac_ext

if test "$pac_madedir" = "yes" ; then rm -rf conftestdir ; fi
AS_UNSET([pac_madedir])
# Remove the conftest* after AC_LANG_CONFTEST
# This is needed for Mac OSX 10.5
rm -rf conftest.dSYM
rm -f conftest*
AC_LANG_POP(Fortran)
])dnl end AC_CACHE_CHECK

AC_SUBST([FCMODOUTFLAG],[$pac_cv_fc_module_outflag])
])dnl end AC_DEFUN([PAC_FC_MODULE_OUTFLAG])
dnl
dnl PAC_FC_AND_F77_COMPATIBLE([action-if-true],[action-if-false])
dnl
dnl Determine whether object files compiled with Fortran 77 can be 
dnl linked to Fortran 90 main programs.
dnl
dnl The test uses a name that includes an underscore unless the 3rd
dnl argument provides another routine name.
dnl
AC_DEFUN([PAC_FC_AND_F77_COMPATIBLE],[
AC_REQUIRE([PAC_PROG_FC_WORKS])
AC_CACHE_CHECK([whether Fortran 90 compiler works with Fortran 77 compiler],
pac_cv_fc_and_f77,[
pacTestRoutine=foo_abc
ifelse([$3],,,[eval pacTestRoutine=$3])
pac_cv_fc_and_f77="unknown"
# compile the f77 program and link with the f90 program
# The reverse may not work because the Fortran 90 environment may
# expect to be in control (and to provide library files unknown to any other
# environment, even Fortran 77!)
AC_LANG_PUSH(Fortran 77)
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        subroutine ${pacTestRoutine}(b)
        integer b
        b = b + 1
        end
    ])
],[
    # pac_f77compile_ok=yes
    PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
    # Save original LIBS, prepend previously generated object file to LIBS
    saved_LIBS="$LIBS"
    LIBS="pac_f77conftest.$OBJEXT $LIBS"
    AC_LANG_PUSH(Fortran)
    AC_LINK_IFELSE([
        AC_LANG_SOURCE([
            program main
            integer a
            a = 1
            call ${pacTestRoutine}(a)
            end
        ])
    ],[pac_cv_fc_and_f77=yes],[pac_cv_fc_and_f77=no])
    # Some versions of the Intel compiler produce these two files
    rm -f work.pc work.pcl
    # Restore LIBS
    LIBS="$saved_LIBS"
    AC_LANG_POP(Fortran)
    # remove previously generated object file.
    rm -f pac_f77conftest.$OBJEXT
], [
    # pac_f77compile_ok=no
    pac_cv_fc_and_f77=no
])
AC_LANG_POP(Fortran 77)
# Perform the requested action based on whether the test succeeded
if test "$pac_cv_fc_and_f77" = yes ; then
    ifelse($1,,:,[$1])
else
    ifelse($2,,:,[$2])
    AC_MSG_WARN([See config.log for the failed test program and its output.])
fi
])
dnl
])
dnl
dnl
dnl /*D 
dnl PAC_PROG_FC_CRAY_POINTER - Check if Fortran supports Cray-style pointer.
dnl                            If so, set pac_cv_prog_fc_has_pointer to yes
dnl                            and find out if any extra compiler flag is
dnl                            needed and set it as CRAYPTR_FCFLAGS.
dnl                            i.e. CRAYPTR_FCFLAGS is meaningful only if
dnl                            pac_cv_prog_fc_has_pointer = yes.
dnl
dnl Synopsis:
dnl   PAC_PROG_FC_CRAY_POINTER([action-if-true],[action-if-false])
dnl D*/
AC_DEFUN([PAC_PROG_FC_CRAY_POINTER],[
AC_CACHE_CHECK([whether Fortran 90 supports Cray-style pointer],
pac_cv_prog_fc_has_pointer,[
AC_LANG_PUSH([Fortran])
AC_LANG_CONFTEST([
    AC_LANG_PROGRAM([],[
        integer M
        pointer (MPTR,M)
        data MPTR/0/
    ])
])
saved_FCFLAGS="$FCFLAGS"
pac_cv_prog_fc_has_pointer=no
CRAYPTR_FCFLAGS=""
for ptrflag in '' '-fcray-pointer' ; do
    FCFLAGS="$saved_FCFLAGS $ptrflag"
    AC_COMPILE_IFELSE([],[
        pac_cv_prog_fc_has_pointer=yes
        CRAYPTR_FCFLAGS="$ptrflag"
        break
    ])
done
dnl Restore FCFLAGS first, since user may not want to modify FCFLAGS
FCFLAGS="$saved_FCFLAGS"
dnl remove conftest after ac_lang_conftest
rm -f conftest.$ac_ext
AC_LANG_POP([Fortran])
])
if test "$pac_cv_prog_fc_has_pointer" = "yes" ; then
    AC_MSG_CHECKING([for Fortran 90 compiler flag for Cray-style pointer])
    if test "X$CRAYPTR_FCFLAGS" != "X" ; then
        AC_MSG_RESULT([$CRAYPTR_FCFLAGS])
    else
        AC_MSG_RESULT([none])
    fi
    ifelse([$1],[],[:],[$1])
else
    ifelse([$2],[],[:],[$2])
fi
])
dnl
dnl
dnl
AC_DEFUN([PAC_PROG_FC_AND_C_STDIO_LIBS],[
AC_REQUIRE([AC_HEADER_STDC])
# To simply the code in the cache_check macro, chose the routine name
# first, in case we need it
confname=conf1_
case "$pac_cv_prog_f77_name_mangle" in
    "lower underscore")       confname=conf1_  ;;
    "upper stdcall")          confname=CONF1   ;;
    upper)                    confname=CONF1   ;;
    "lower doubleunderscore") confname=conf1_  ;;
    lower)                    confname=conf1   ;;
    "mixed underscore")       confname=conf1_  ;;
    mixed)                    confname=conf1   ;;
esac

AC_CACHE_CHECK([what libraries are needed to link Fortran90 programs with C routines that use stdio],pac_cv_prog_fc_and_c_stdio_libs,[
pac_cv_prog_fc_and_c_stdio_libs=unknown

AC_LANG_PUSH(C)
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
int $confname( int a )
{ printf( "The answer is %d\n", a ); fflush(stdout); return 0; }
    ])
],[
    pac_compile_ok=yes
    PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
    # Save LIBS and prepend object file to LIBS
    saved_LIBS="$LIBS"
    LIBS="pac_conftest.$OBJEXT $LIBS"
    AC_LANG_PUSH(Fortran)
    AC_LINK_IFELSE([
        AC_LANG_PROGRAM([],[call conf1(0)])
    ],[
        pac_cv_prog_fc_and_c_stdio_libs=none
    ],[
        # Try again with -lSystemStubs
        LIBS="$LIBS -lSystemStubs"
        AC_LINK_IFELSE([],[
            pac_cv_prog_fc_and_c_stdio_libs="-lSystemStubs"
        ],[])
    ])
    LIBS="$saved_LIBS"
    AC_LANG_POP(Fortran)
    rm -f pac_conftest.$OBJEXT
])
AC_LANG_POP(C)
dnl
if test "$pac_cv_prog_fc_and_c_stdio_libs" != none -a \
        "$pac_cv_prog_fc_and_c_stdio_libs" != unknown ; then
    FC_OTHER_LIBS="$FC_OTHER_LIBS $pac_cv_prog_fc_and_c_stdio_libs"    
fi
])
dnl
])
dnl
dnl/*D
dnl PAC_FC_CHECK_COMPILER_OPTION - Check that a FC compiler option is
dnl accepted without warning messages
dnl
dnl Synopsis:
dnl PAC_FC_CHECK_COMPILER_OPTION(optionname,action-if-ok,action-if-fail)
dnl
dnl Output Effects:
dnl
dnl If no actions are specified, a working value is added to 'FCOPTIONS'
dnl
dnl Notes:
dnl This is now careful to check that the output is different, since 
dnl some compilers are noisy.
dnl 
dnl We are extra careful to prototype the functions in case compiler options
dnl that complain about poor code are in effect.
dnl
dnl Because this is a long script, we have ensured that you can pass a 
dnl variable containing the option name as the first argument.
dnl D*/
AC_DEFUN([PAC_FC_CHECK_COMPILER_OPTION],[
AC_MSG_CHECKING([whether Fortran 90 compiler accepts option $1])
pac_opt="$1"
AC_LANG_PUSH(Fortran)
FCFLAGS_orig="$FCFLAGS"
FCFLAGS_opt="$pac_opt $FCFLAGS"
pac_result="unknown"

AC_LANG_CONFTEST([AC_LANG_PROGRAM()])
FCFLAGS="$FCFLAGS_orig"
rm -f pac_test1.log
PAC_LINK_IFELSE_LOG([pac_test1.log], [], [
    FCFLAGS="$FCFLAGS_opt"
    rm -f pac_test2.log
    PAC_LINK_IFELSE_LOG([pac_test2.log], [], [
        PAC_RUNLOG_IFELSE([diff -b pac_test1.log pac_test2.log],
                          [pac_result=yes], [pac_result=no])
    ],[
        pac_result=no
    ])
], [
    pac_result=no
])
AC_MSG_RESULT([$pac_result])
dnl Delete the conftest created by AC_LANG_CONFTEST.
rm -f conftest.$ac_ext
#
if test "$pac_result" = "yes" ; then
    AC_MSG_CHECKING([whether routines compiled with $pac_opt can be linked with ones compiled without $pac_opt])
    pac_result=unknown
    FCFLAGS="$FCFLAGS_orig"
    rm -f pac_test3.log
    PAC_COMPILE_IFELSE_LOG([pac_test3.log], [
        AC_LANG_SOURCE([
            subroutine try()
            end
        ])
    ],[
        PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
        saved_LIBS="$LIBS"
        LIBS="pac_conftest.$OBJEXT $LIBS"

        FCFLAGS="$FCFLAGS_opt"
        rm -f pac_test4.log
        PAC_LINK_IFELSE_LOG([pac_test4.log], [AC_LANG_PROGRAM()], [
            PAC_RUNLOG_IFELSE([diff -b pac_test2.log pac_test4.log],
                              [pac_result=yes], [pac_result=no])
        ],[
            pac_result=no
        ])
        LIBS="$saved_LIBS"
        rm -f pac_conftest.$OBJEXT
    ],[
        pac_result=no
    ])
    AC_MSG_RESULT([$pac_result])
    rm -f pac_test3.log pac_test4.log
fi
rm -f pac_test1.log pac_test2.log

dnl Restore FCFLAGS before 2nd/3rd argument commands are executed,
dnl as 2nd/3rd argument command could be modifying FCFLAGS.
FCFLAGS="$FCFLAGS_orig"
if test "$pac_result" = "yes" ; then
     ifelse([$2],[],[FCOPTIONS="$FCOPTIONS $1"],[$2])
else
     ifelse([$3],[],[:],[$3])
fi
AC_LANG_POP(Fortran)
])
dnl /*D
dnl PAC_FC_WORKS_WITH_CPP
dnl
dnl Checks if Fortran 90 compiler works with C preprocessor
dnl
dnl Most systems allow the Fortran compiler to process .F and .F90 files
dnl using the C preprocessor.  However, some systems either do not
dnl allow this or have serious bugs (OSF Fortran compilers have a bug
dnl that generates an error message from cpp).  The following test
dnl checks to see if .F works, and if not, whether "cpp -P -C" can be used
dnl D*/
AC_DEFUN([PAC_FC_WORKS_WITH_CPP],[
AC_REQUIRE([AC_PROG_CPP])
AC_MSG_CHECKING([whether Fortran 90 compiler processes .F90 files with C preprocessor])
AC_LANG_PUSH([Fortran])
saved_fc_ext=${ac_ext}
ac_ext="F90"
saved_FCFLAGS="$FCFLAGS"
FCFLAGS="$FCFLAGS $CPPFLAGS"
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        program main
#define ASIZE 10
        integer a(ASIZE)
        end
    ])
])
AC_COMPILE_IFELSE([],[
    pac_cv_fc_accepts_F90=yes
    ifelse([$1],[],[],[$1=""])
],[
    pac_cv_fc_accepts_F90=no
    ifelse([$1],[],[:],[$1="false"])
])
# Restore Fortran's ac_ext but not FCFLAGS
ac_ext="$saved_fc_ext"

if test "$pac_cv_fc_accepts_F90" != "yes" ; then
    pac_cpp_fc="$ac_cpp -C -P conftest.F90 > conftest.$ac_ext"
    PAC_RUNLOG_IFELSE([$pac_cpp_fc],[
        if test -s conftest.${ac_ext} ; then
            AC_COMPILE_IFELSE([],[
                pac_cv_fc_accepts_F90="no, use cpp"
                ifelse([$1],[],[],[$1="$CPP -C -P"])
            ],[])
            rm -f conftest.${ac_ext}
        fi
    ],[])
fi
FCFLAGS="$saved_FCFLAGS"
rm -f conftest.F90
AC_LANG_POP([Fortran])
AC_MSG_RESULT([$pac_cv_fc_accepts_F90])
])
dnl
dnl PAC_FC_VENDOR:
dnl Try to get a version string for the F90 compiler.  We may
dnl need this to find likely command-line arguments for accessing
dnl shared libraries
dnl
AC_DEFUN([PAC_FC_VENDOR],[
AC_MSG_CHECKING([for Fortran 90 compiler vendor])
# This is complicated by some compilers (such as the Intel 8.1 ifort)
# that return a non-zero status even when they accept the -V option
# (a zero status is returned only if there is a file).
pac_cv_fc_vendor="unknown"
for arg in --version -V -v ; do
    rm -f conftest.txt
    PAC_RUNLOG([$FC $arg </dev/null >conftest.txt 2>&1])
    # Ignore the return code, because some compilers set the
    # return code to zero on invalid arguments and some to 
    # non-zero on success (with no files to compile)
    if test -f conftest.txt ; then
        if grep 'Portland Group' conftest.txt >/dev/null 2>&1 ; then
            pac_cv_fc_vendor=pgi
        elif grep 'Sun Workshop' conftest.txt >/dev/null 2>&1 ; then
            pac_cv_fc_vendor=sun
	elif grep 'Sun Fortran 9' conftest.txt >/dev/null 2>&1 ; then 
	    pac_cv_fc_vendor=sun
        elif grep 'Absoft' conftest.txt >/dev/null 2>&1 ; then
            pac_cv_fc_vendor=absoft
        elif grep 'G95' conftest.txt >/dev/null 2>&1 ; then
            pac_cv_fc_vendor=gnu
        elif grep 'GNU Fortran' conftest.txt >/dev/null 2>&1 ; then
            # This is gfortran
            pac_cv_fc_vendor=gnu
        elif grep Intel conftest.txt >/dev/null 2>&1 ; then
            pac_cv_fc_vendor=intel
        fi
    fi
    if test "$pac_cv_fc_vendor" != "unknown" ; then break ; fi
done
if test "$pac_cv_fc_vendor" = "unknown" ; then
    # Try to use the compiler name
    if test "$FC" = "ifort" -o "$FC" = "ifc" ; then
        pac_cv_fc_vendor=intel
    elif test "$FC" = "pgf90" ; then
        pac_cv_fc_vendor=pgi
    elif test "$FC" = "xlf90" -o "$FC" = "xlf90_r" ; then
        pac_cv_fc_vendor=ibm
    elif test "$FC" = "xlf95" -o "$FC" = "xlf95_r" ; then
        pac_cv_fc_vendor=ibm
    fi
fi
AC_MSG_RESULT([$pac_cv_fc_vendor])
rm -f conftest.txt
# End of checking for F90 compiler vendor
])
dnl
dnl PAC_F77_IS_FC([ACTION_IF_TRUE],[ACTION_IF_FALSE])
dnl Check if F77 is a Fortran 90 compiler.
dnl
AC_DEFUN([PAC_F77_IS_FC],[
AC_MSG_CHECKING([whether $F77 is a Fortran 90 compiler])
AC_LANG_PUSH([Fortran 77])
saved_ac_ext=$ac_ext
ac_ext="f90"
AC_LINK_IFELSE([
    AC_LANG_SOURCE([
        program main
        integer, dimension(10) :: n
        integer k
        print *,  range(k)
        end
    ])
],[
    pac_cv_prog_f77_is_fc=yes
    ifelse([$1],[],[],[$1])
],[
    pac_cv_prog_f77_is_fc=no
    ifelse([$2],[],[],[$2])
])
AC_MSG_RESULT([$pac_cv_prog_f77_is_fc])
AC_LANG_POP([Fortran 77])
])
dnl
dnl PAC_FC_FLOAT_MODEL(float_type, [variable-set-if-successful-test])
dnl variable-set-if-successful-test is optional variable.
dnl
dnl This is a runtime test.
dnl
AC_DEFUN([PAC_FC_FLOAT_MODEL],[
type="$1"
AC_MSG_CHECKING([for precision and range of $type])
AC_LANG_PUSH([Fortran])
rm -f pac_fconftest.out
AC_RUN_IFELSE([
    AC_LANG_SOURCE([
        program main
        $type aa
        open(8, file="pac_fconftest.out", form="formatted")
        write(8,*) precision(aa), ",", range(aa)
        close(8)
        end
    ])
],[
    if test -s pac_fconftest.out ; then
        pac_fc_num_model="`sed -e 's/  */ /g' pac_fconftest.out`"
        AC_MSG_RESULT([$pac_fc_num_model])
        ifelse([$2],[],[],[$2=$pac_fc_num_model])
    else
        AC_MSG_RESULT([Error])
        AC_MSG_WARN([No output from test program!])
    fi
    rm -f pac_fconftest.out
],[
    AC_MSG_RESULT([Error])
    AC_MSG_WARN([Failed to run program to determine the precision and range of $type])
])
AC_LANG_POP([Fortran])
])
dnl
dnl PAC_FC_SIMPLE_NUMBER_MODEL(message, Fortran-type, Fortran-write,
dnl                            [variable-set-if-successful-test],
dnl                            [cross-value])
dnl
dnl message        : message of what test-fc-code is checking
dnl Fortran-type   : Fortran90 type's data model to be examined.
dnl Fortran-write  : Fortran90 type's write statement used with write(N,*).
dnl variable-set-if-successful-test :
dnl                  The optional variable to be set if the codelet:
dnl                  "Fortran-type" + "write(N,*) Fortran-write"
dnl                  is successful in returning the simple data model.
dnl cross-value    : value to be used for above variable when
dnl                  cross_compiling=yes
dnl
dnl This is a runtime test.
dnl
AC_DEFUN([PAC_FC_SIMPLE_NUMBER_MODEL],[
pac_msg="$1"
AC_MSG_CHECKING([for $pac_msg])
AC_LANG_PUSH([Fortran])
rm -f pac_fconftest.out
AC_RUN_IFELSE([
    AC_LANG_SOURCE([
        program main
        $2
        open(8, file="pac_fconftest.out", form="formatted")
        write(8,*) $3
        close(8)
        end
    ])
],[
    if test -s pac_fconftest.out ; then
        pac_fc_num_model="`sed -e 's/  */ /g' pac_fconftest.out`"
        AC_MSG_RESULT([$pac_fc_num_model])
        ifelse([$4],[],[],[$4=$pac_fc_num_model])
    else
        AC_MSG_RESULT([Error])
        AC_MSG_WARN([No output from test program!])
    fi
    rm -f pac_fconftest.out
],[
    AC_MSG_RESULT([Error])
    AC_MSG_WARN([Failed to run program to determine $pac_msg])
],[
    AC_MSG_RESULT([$5])
    ifelse([$4],[],[],[$4=$5])
])
AC_LANG_POP([Fortran])
])
dnl
dnl PAC_FC_AVAIL_INTEGER_MODELS([INTEGER-MODELS-FLAG],[CROSS-VARIABLE])
dnl
dnl INTEGER-MODELS-FLAG : an optional variable to be set if provided.
dnl                       If it isn't provided, PAC_FC_ALL_INTEGER_MODELS
dnl                       will be set.
dnl CROSS-VALUE         : value will be used to set INTEGER-MODELS-FLAG
dnl                       or PAC_FC_ALL_INTEGER_MODELS if cross_compiling=yes.
dnl
dnl This is a runtime test.
dnl
AC_DEFUN([PAC_FC_AVAIL_INTEGER_MODELS],[
AC_MSG_CHECKING([for available integer kinds])
AC_LANG_PUSH([Fortran])
rm -f pac_fconftest.out
AC_RUN_IFELSE([
    AC_LANG_SOURCE([
        program main
        integer r, lastkind
        lastkind=selected_int_kind(1)
        open(8, file="pac_fconftest.out", form="formatted")
        do r=2,30
             k = selected_int_kind(r)
             if (k .ne. lastkind) then
                  write(8,*) r-1, ",", lastkind
                  lastkind = k
             endif
             if (k .le. 0) then
                 exit
             endif
        enddo
        if (k.ne.lastkind) then
            write(8,*) 31, ",", k
        endif
        close(8)
        end
    ])
],[
    if test -s pac_fconftest.out ; then
        pac_flag="`sed -e 's/  */ /g' pac_fconftest.out | tr '\012' ','`"
        AC_MSG_RESULT([$pac_flag])
        pac_validKinds="`sed -e 's/  */ /g' pac_fconftest.out | tr '\012' ':'`"
        ifelse([$1],[],[PAC_FC_ALL_INTEGER_MODELS=$pac_flag],[$1=$pac_flag])
    else
        AC_MSG_RESULT([Error])
        AC_MSG_WARN([No output from test program!])
    fi
    rm -f pac_fconftest.out
],[
    AC_MSG_RESULT([Error])
    AC_MSG_WARN([Failed to run program to determine available integer models])
],[
    dnl Even when cross_compiling=yes,
    dnl pac_validKinds needs to be set for PAC_FC_INTEGER_MODEL_MAP()
    pac_validKinds="`echo \"$2\" | tr ',' ':'`"
    AC_MSG_RESULT([$2])
    ifelse([$1],[],[PAC_FC_ALL_INTEGER_MODELS=$2],[$1=$2])
])
AC_LANG_POP([Fortran])
])
dnl
dnl PAC_FC_INTEGER_MODEL_MAP([INTEGER-MODEL-MAP-FLAG],[CROSS-VALUE]))
dnl
dnl INTEGER-MODEL-MAP-FLAG : an optional variable to be set if provided.
dnl                          If it isn't provided, PAC_FC_INTEGER_MODEL_MAP
dnl                          will be set.
dnl CROSS-VALUE            : value will be used to set INTEGER-MODEL-MAP-FLAG
dnl                          or PAC_FC_INTEGER_MODEL_MAP if cross_compiling=yes.
dnl
dnl This test requires $pac_validKinds set by PAC_FC_ALL_INTEGER_MODELS().
dnl
dnl This is a runtime test.
dnl
dnl Compile the C subroutine as pac_conftest.o and Link it with a Fortran main.
AC_DEFUN([PAC_FC_INTEGER_MODEL_MAP],[
AC_REQUIRE([PAC_FC_AVAIL_INTEGER_MODELS])
AC_MSG_CHECKING([for available integer ranges])
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
#ifdef F77_NAME_UPPER
#define cisize_ CISIZE
#define isize_ ISIZE
#elif defined(F77_NAME_LOWER) || defined(F77_NAME_MIXED)
#define cisize_ cisize
#define isize_ isize
#endif
int cisize_(char *,char*);
int cisize_(char *i1p, char *i2p)
{
    int isize_val=0;
    isize_val = (int)(i2p - i1p);
    return isize_val;
}
    ])
],[
    PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
    pac_ccompile_ok=yes
],[
    pac_ccompile_ok=no
])
AC_LANG_POP([C])
dnl
if test "$pac_ccompile_ok" = "yes" ; then
    saved_LIBS="$LIBS"
    LIBS="pac_conftest.$OBJEXT $LIBS"
    saved_IFS=$IFS
    IFS=:
    AC_LANG_PUSH([Fortran])
    pac_flag=""
    for rangekind in $pac_validKinds ; do
        kind="`echo $rangekind | sed -e 's/.*,//'`"
        range="`echo $rangekind | sed -e 's/,.*//'`"
        AC_LANG_CONFTEST([
            AC_LANG_SOURCE([
                program main
                integer (kind=$kind) a(2)
                integer cisize
                open(8, file="pac_fconftest.out", form="formatted")
                write(8,*) $range, ",", $kind, ",", cisize( a(1), a(2) )
                close(8)
                end
            ])
        ])
        IFS=$saved_IFS
        rm -f pac_fconftest.out
        AC_RUN_IFELSE([],[
            if test -s pac_fconftest.out ; then
                sizes="`sed -e 's/  */ /g' pac_fconftest.out`"
                pac_flag="$pac_flag { $sizes },"
            else
                AC_MSG_WARN([No output from test program!])
            fi
            rm -f pac_fconftest.out
        ],[
            AC_MSG_WARN([Fortran program fails to build or run!])
        ],[
            pac_flag="$2"
        ])
        IFS=:
    done
    IFS=$saved_IFS
    AC_MSG_RESULT([$pac_flag])
    ifelse([$1],[],[PAC_FC_INTEGER_MODEL_MAP=$pac_flag],[$1=$pac_flag])
    AC_LANG_POP([Fortran])
    LIBS="$saved_LIBS"
    rm -f pac_conftest.$OBJEXT
fi
])


AC_DEFUN([PAC_FC_2008_SUPPORT],[
AC_MSG_CHECKING([for Fortran 2008 support])

AC_LANG_PUSH([C])
f08_works=yes
AC_COMPILE_IFELSE([
	AC_LANG_SOURCE(
[[
#include <ISO_Fortran_binding.h>

int foo_c(CFI_cdesc_t * a_desc, CFI_cdesc_t * b_desc)
{
	char * a_row = (char*) a_desc->base_addr;
	if (a_desc->type != CFI_type_int) { return 1; }
	if (a_desc->rank != 2) { return 2; }
	if (a_desc->dim[1].extent != b_desc->dim[0].extent) { return 3; }
	return 0;
}
]])],[mv conftest.$OBJEXT conftest1.$OBJEXT],[f08_works=no])
AC_LANG_POP([C])

AC_LANG_PUSH([Fortran])
PAC_PUSH_FLAG([LIBS])
LIBS="conftest1.$OBJEXT $LIBS"
AC_LINK_IFELSE([
    AC_LANG_SOURCE([
MODULE F08TS_MODULE
IMPLICIT NONE

! Test public, private, protected
REAL, PUBLIC       :: x
REAL, PRIVATE      :: y
LOGICAL, PROTECTED :: z

! Test abstract
ABSTRACT INTERFACE
    SUBROUTINE user_func(x, y)
        INTEGER  :: x(*)
        REAL     :: y
    END SUBROUTINE
END INTERFACE

! Test TS 29113 assumed type , assumed rank and bind(C)
INTERFACE
    FUNCTION FOO(A, B, C) &
        BIND(C,name="foo_c") RESULT(err)
        USE, intrinsic :: iso_c_binding, ONLY : c_int
        TYPE(*), DIMENSION(..) :: A, B, C
        INTEGER(c_int) :: err
    END FUNCTION FOO
END INTERFACE

CONTAINS

! Test TS 29113 asychronous attribute and optional
SUBROUTINE test1(buf, count, ierr)
    INTEGER, ASYNCHRONOUS :: buf(*)
    INTEGER               :: count
    INTEGER, OPTIONAL     :: ierr
END SUBROUTINE

! Test procedure type and non-bind(c) x in C_FUNCLOC(x)
SUBROUTINE test2(func)
    USE, INTRINSIC :: ISO_C_BINDING, ONLY : C_FUNLOC, C_FUNPTR
    PROCEDURE(user_func)  :: func
    TYPE(C_FUNPTR) :: errhandler_fn
    errhandler_fn = C_FUNLOC(func)
END SUBROUTINE

! Test intrinsic storage_size
SUBROUTINE test3(x, size)
    CHARACTER, DIMENSION(..) :: x
    INTEGER, INTENT(OUT) :: size
    size = storage_size(x)/8
END SUBROUTINE test3

END MODULE

!==============================================
PROGRAM MAIN
USE :: F08TS_MODULE, ONLY : FOO
IMPLICIT NONE

INTEGER, DIMENSION(4,4) :: A, B
INTEGER, DIMENSION(2,2) :: C
INTEGER                 :: ERRCODE

! Test contiguous and non-contiguous array section passing
! and linkage with C code
ERRCODE = FOO(A(1:4:2, :), B(:, 2:4:2), C)

END PROGRAM
    ])],[],[f08_works=no])
PAC_POP_FLAG([LIBS])
AC_LANG_POP([Fortran])

if test "$f08_works" = "yes" ; then
   $1
else
   $2
fi
rm -f conftest1.$OBJEXT
AC_MSG_RESULT([$f08_works])
])
