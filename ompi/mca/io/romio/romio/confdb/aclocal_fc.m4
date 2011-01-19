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
dnl xlf/xlf90/xlf95 are IBM (AIX) F77/F90/F95 compilers.
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
dnl D*/
AC_DEFUN([PAC_PROG_FC],[
PAC_PUSH_FLAG([FCFLAGS])
AC_PROG_FC([m4_default([$1],
           [ifort pgf90 pathf90 pathf95 xlf90 xlf95 f90 epcf90 f95 fort lf95 \
            gfortran g95 ifc efc])])
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
    dnl Look for module name
    dnl First, try to find known names.  This avoids confusion caused by
    dnl additional files (like <name>.stb created by some versions of pgf90)
    dnl Early versions of the Intel compiler used "d" as the module extension;
    dnl we include that just to finish the test as early as possible.
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
        pac_MOD=`ls conftest.* 2>&1 | grep -v conftest.${ac_fc_srcext} | grep -v conftest.o`
        pac_MOD=`echo $pac_MOD | sed -e 's/conftest\.//g'`
        pac_cv_fc_module_case="lower"
        if test "X$pac_MOD" = "X" ; then
            pac_MOD=`ls CONFTEST* 2>&1 | grep -v CONFTEST.f | grep -v CONFTEST.o`
            pac_MOD=`echo $pac_MOD | sed -e 's/CONFTEST\.//g'`
            if test -n "$pac_MOD" -a -s "CONFTEST.$pac_MOD" ; then
                testname="CONFTEST"
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
AC_LANG_POP(Fortran)
])
dnl
dnl
dnl
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
AC_CACHE_CHECK([for Fortran 90 module include flag],
pac_cv_fc_module_incflag,[
AC_REQUIRE([PAC_FC_MODULE_EXT])
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
    ],
    []
)
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
        # a file, it must contain a the name of a file ending in .pc .  Ugh!
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
])
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
    mv conftest.$OBJEXT pac_f77conftest.$OBJEXT
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
dnl PAC_PROG_FC_HAS_POINTER - Determine if Fortran allows pointer type
dnl
dnl Synopsis:
dnl   PAC_PROG_FC_HAS_POINTER(action-if-true,action-if-false)
dnl D*/
AC_DEFUN([PAC_PROG_FC_HAS_POINTER],[
AC_CACHE_CHECK([whether Fortran 90 has Cray-style pointer declaration],
pac_cv_prog_fc_has_pointer,[
AC_LANG_PUSH(Fortran)
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM([],[
        integer M
        pointer (MPTR,M)
        data MPTR/0/
    ])
],[
    pac_cv_prog_fc_has_pointer="yes"
],[
    pac_cv_prog_fc_has_pointer="no"
]) dnl Endof AC_COMPILE_IFELSE
AC_LANG_POP(Fortran)
])
if test "$pac_cv_prog_fc_has_pointer" = "yes" ; then
    ifelse([$1],,:,[$1])
else
    ifelse([$2],,:,[$2])
fi
])
dnl
dnl
dnl
AC_DEFUN([PAC_PROG_FC_AND_C_STDIO_LIBS],[
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
#include <stdio.h>
int $confname( int a )
{ printf( "The answer is %d\n", a ); fflush(stdout); return 0; }
    ])
],[
    pac_compile_ok=yes
    mv conftest.$OBJEXT pac_conftest.$OBJEXT
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
dnl Instead of defining our own ac_link and ac_compile and do AC_TRY_EVAL
dnl on these variables.  We modify ac_link and ac_compile used by AC_*_IFELSE
dnl by piping the output of the command to a logfile.  The reason is that
dnl 1) AC_TRY_EVAL is discouraged by Autoconf. 2) defining our ac_link and
dnl ac_compile could mess up the usage and order of FCFLAGS, LDFLAGS
dnl and LIBS in these commands, i.e. deviate from how GNU standard uses
dnl these variables.
dnl
dnl Replace " >&AS_MESSAGE_LOG_FD" by "> file 2>&1" in ac_link and ac_compile
pac_link="`echo $ac_link | sed -e 's|>.*$|> $pac_logfile 2>\&1|g'`"
dnl echo "ac_link=\"$ac_link\""
dnl echo "pac_link=\"$pac_link\""
saved_ac_link="$ac_link"
ac_link="$pac_link"
dnl echo "ac_link=\"$ac_link\""

pac_compile="`echo $ac_compile | sed -e 's|>.*$|> $pac_logfile 2>\&1|g'`"
dnl echo "ac_compile=\"$ac_compile\""
dnl echo "pac_compile=\"$pac_compile\""
saved_ac_compile="$ac_compile"
ac_compile="$pac_compile"
dnl echo "ac_compile=\"$ac_compile\""

FCFLAGS_orig="$FCFLAGS"
FCFLAGS_opt="$pac_opt $FCFLAGS"
pac_result="unknown"
AC_LANG_CONFTEST([AC_LANG_PROGRAM()])
FCFLAGS="$FCFLAGS_orig"
pac_logfile="pac_test1.log"
rm -f $pac_logfile
AC_LINK_IFELSE([], [
    FCFLAGS="$FCFLAGS_opt"
    pac_logfile="pac_test2.log"
    rm -f $pac_logfile
    AC_LINK_IFELSE([], [
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
    pac_logfile="pac_test3.log"
    rm -f $pac_logfile
    AC_COMPILE_IFELSE([
        AC_LANG_SOURCE([
            subroutine try()
            end
        ])
    ],[
        mv conftest.$OBJEXT pac_conftest.$OBJEXT
        saved_LIBS="$LIBS"
        LIBS="pac_conftest.$OBJEXT $LIBS"

        FCFLAGS="$FCFLAGS_opt"
        pac_logfile="pac_test4.log"
        rm -f $pac_logfile
        AC_LINK_IFELSE([AC_LANG_PROGRAM()], [
            diffcmd='diff -b pac_test3.log pac_test4.log'
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

dnl Restore everything in AC that has been overwritten
ac_link="$saved_ac_link"
ac_compile="$saved_ac_compile"
dnl echo "ac_link=\"$ac_link\""
dnl echo "ac_compile=\"$ac_compile\""
dnl Restore FCFLAGS before 2nd/3rd argument commands are executed,
dnl as 2nd/3rd argument command could be modifying FCFLAGS.
FCFLAGS="$FCFLAGS_orig"
if test "$pac_result" = "yes" ; then
     ifelse([$2],[],[FCOPTIONS="$FCOPTIONS $1"],[$2])
else
     ifelse([$3],[],[:],[$3])
fi

# This is needed for Mac OSX 10.5
rm -rf conftest.dSYM
rm -f conftest*
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
AC_LINK_IFELSE([
    AC_LANG_SOURCE([
        program main
        $type aa
        print *, precision(aa), ",", range(aa)
        end
    ])
],[
    rm -f pac_conftest.out
    PAC_RUNLOG([./conftest$EXEEXT > pac_conftest.out])
    if test -s pac_conftest.out ; then
        pac_fc_num_model="`cat pac_conftest.out | sed -e 's/  */ /g'`"
        AC_MSG_RESULT([$pac_fc_num_model])
        ifelse([$2],[],[],[$2=$pac_fc_num_model])
    else
        AC_MSG_WARN([No output from test program!])
    fi
    rm -f pac_conftest.out
],[
    AC_MSG_WARN([Failed to build program to determine the precision and range of $type])
])
AC_LANG_POP([Fortran])
])
dnl
dnl PAC_FC_SIMPLE_NUMBER_MODEL(message,test-fc-code,
dnl                            [variable-set-if-successful-test])
dnl message      : message of what test-fc-code is checking
dnl test-fc-code : Fortran 90 code to check a float or integer type's data model
dnl variable-set-if-successful-test :
dnl                The optional variable to be set if the test-fc-code
dnl                is successful in returning the simple data model.
dnl
dnl This is a runtime test.
dnl
AC_DEFUN([PAC_FC_SIMPLE_NUMBER_MODEL],[
pac_msg="$1"
AC_MSG_CHECKING([for $pac_msg])
AC_LANG_PUSH([Fortran])
AC_LINK_IFELSE([
    AC_LANG_PROGRAM([],[
        $2
    ])
],[
    rm -f pac_conftest.out
    PAC_RUNLOG([./conftest$EXEEXT > pac_conftest.out])
    if test -s pac_conftest.out ; then
        pac_fc_num_model="`cat pac_conftest.out | sed -e 's/  */ /g'`"
        AC_MSG_RESULT([$pac_fc_num_model])
        ifelse([$3],[],[],[$3=$pac_fc_num_model])
    else
        AC_MSG_WARN([No output from test program!])
    fi
    rm -f pac_conftest.out
],[
    AC_MSG_WARN([Failed to build program to determine $pac_msg])
])
AC_LANG_POP([Fortran])
])
dnl
dnl PAC_FC_AVAIL_INTEGER_MODELS([INTEGER-MODELS-FLAG])
dnl Both INTEGER-MODELS-FLAG is an optional variable to be set if provided.
dnl If it isn't provided, PAC_FC_ALL_INTEGER_MODELS will be set.
dnl
dnl This is a runtime test.
dnl
AC_DEFUN([PAC_FC_AVAIL_INTEGER_MODELS],[
AC_MSG_CHECKING([for available integer kinds])
AC_LANG_PUSH([Fortran])
AC_LINK_IFELSE([
    AC_LANG_SOURCE([
        program main
        integer r, lastkind
        lastkind=selected_int_kind(1)
        do r=2,30
             k = selected_int_kind(r)
             if (k .ne. lastkind) then
                  print *, r-1,",",lastkind
                  lastkind = k
             endif
             if (k .le. 0) then
                 exit
             endif
        enddo
        if (k.ne.lastkind) then
            print *, 31, ",", k
        endif
        end
    ])
],[
    rm -f pac_conftest.out
    PAC_RUNLOG([./conftest$EXEEXT > pac_conftest.out])
    if test -s pac_conftest.out ; then
        pac_flag=`cat pac_conftest.out | sed -e 's/  */ /g'| tr '\012' ','`
        AC_MSG_RESULT([$pac_flag])
        pac_validKinds="`sed -e 's/  */ /g' pac_conftest.out | tr '\012' ':'`"
        ifelse([$1],[],[PAC_FC_ALL_INTEGER_MODELS=$pac_flag],[$1=$pac_flag])
    else
        AC_MSG_WARN([No output from test program!])
    fi
    rm -f pac_conftest.out
],[
    AC_MSG_WARN([Failed to build program to determine available integer models])
])
AC_LANG_POP([Fortran])
])
dnl
dnl PAC_FC_INTEGER_MODEL_MAP([INTEGER-MODEL-MAP-FLAG])
dnl Both INTEGER-MODEL-MAP-FLAG is an optional variable to be set if provided.
dnl If it isn't provided, PAC_FC_INTEGER_MODEL_MAP will be set.
dnl
dnl This test expect pac_validKinds set by PAC_FC_ALL_INTEGER_MODELS.
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
#include <stdio.h>
#include "confdefs.h"
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
                print *, $range, ",", $kind, ",", cisize( a(1), a(2) )
                end
            ])
        ])
        IFS=$saved_IFS
        AC_LINK_IFELSE([],[
            rm -f pac_conftest.out
            PAC_RUNLOG([./conftest$EXEEXT > pac_conftest.out])
            if test -s pac_conftest.out ; then
                sizes="`cat pac_conftest.out | sed -e 's/  */ /g'`"
                pac_flag="$pac_flag { $sizes },"
            else
                AC_MSG_WARN([No output from test program!])
            fi
            rm -f pac_conftest.out
        ],[
            AC_MSG_WARN([Fortran program fails to build!])
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
