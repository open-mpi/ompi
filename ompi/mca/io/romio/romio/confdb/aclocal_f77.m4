dnl PAC_F77_SEARCH_LIST - expands to a whitespace separated list of fortran 77
dnl compilers for use with AC_PROG_F77 that is more suitable for HPC software
dnl packages
AC_DEFUN([PAC_F77_SEARCH_LIST],[ifort pgf77 af77 xlf frt cf77 fort77 fl32 fort ifc efc ftn gfortran f77 g77])
dnl PAC_PROG_F77 - reprioritize the F77 compiler search order
dnl NOTE: this macro suffers from a basically intractable "expanded before it
dnl was required" problem when libtool is also used
AC_DEFUN([PAC_PROG_F77],[
PAC_PUSH_FLAG([FFLAGS])
AC_PROG_F77([PAC_F77_SEARCH_LIST])
PAC_POP_FLAG([FFLAGS])
])
dnl
dnl/*D
dnl PAC_PROG_F77_NAME_MANGLE - Determine how the Fortran compiler mangles
dnl names 
dnl
dnl Synopsis:
dnl PAC_PROG_F77_NAME_MANGLE([action])
dnl
dnl Output Effect:
dnl If no action is specified, one of the following names is defined:
dnl.vb
dnl If fortran names are mapped:
dnl   lower -> lower                  F77_NAME_LOWER
dnl   lower -> lower_                 F77_NAME_LOWER_USCORE
dnl   lower -> UPPER                  F77_NAME_UPPER
dnl   lower_lower -> lower__          F77_NAME_LOWER_2USCORE
dnl   mixed -> mixed                  F77_NAME_MIXED
dnl   mixed -> mixed_                 F77_NAME_MIXED_USCORE
dnl   mixed -> UPPER@STACK_SIZE       F77_NAME_UPPER_STDCALL
dnl.ve
dnl If an action is specified, it is executed instead.
dnl 
dnl Notes:
dnl We assume that if lower -> lower (any underscore), upper -> upper with the
dnl same underscore behavior.  Previous versions did this by 
dnl compiling a Fortran program and running strings -a over it.  Depending on 
dnl strings is a bad idea, so instead we try compiling and linking with a 
dnl C program, since that is why we are doing this anyway.  A similar approach
dnl is used by FFTW, though without some of the cases we check (specifically, 
dnl mixed name mangling).  STD_CALL not only specifies a particular name
dnl mangling convention (adding the size of the calling stack into the function
dnl name, but also the stack management convention (callee cleans the stack,
dnl and arguments are pushed onto the stack from right to left)
dnl
dnl One additional problem is that some Fortran implementations include 
dnl references to the runtime (like pgf90_compiled for the pgf90 compiler
dnl used as the "Fortran 77" compiler).  This is not yet solved.
dnl
dnl D*/
dnl
AC_DEFUN([PAC_PROG_F77_NAME_MANGLE],[
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
AC_CACHE_CHECK([for Fortran 77 name mangling],
pac_cv_prog_f77_name_mangle,[
# Initialize pac_found to indicate if name mangling scheme has been found
pac_found=no
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        subroutine MY_name( ii )
        return
        end
    ])
],[
    PAC_RUNLOG([mv conftest.$OBJEXT f77conftest.$OBJEXT])
    saved_LIBS="$LIBS"
    dnl  FLIBS is set by AC_F77_LIBRARY_LDFLAGS
    LIBS="f77conftest.$OBJEXT $FLIBS $LIBS"
    AC_LANG_PUSH([C])
    for call in "" __stdcall ; do
        for sym in my_name_ my_name__ my_name MY_NAME MY_name MY_name_ NONE ; do
            AC_LINK_IFELSE([
                AC_LANG_PROGRAM([extern void ${call} ${sym}(int);],[${sym}(0);])
            ],[
                pac_found=yes
                break
            ])
        done
        test "$pac_found" = "yes" && break
    done
    AC_LANG_POP([C])
    LIBS="$saved_LIBS"
    rm -f f77conftest.$OBJEXT
])
AC_LANG_POP([Fortran 77])
dnl
# If we got to here and pac_cv_prog_f77_name_mangle is still NOT definable,
# it may be that the programs have to be linked with the Fortran compiler,
# not the C compiler.  Try reversing the language used for the test
if test  "$pac_found" != "yes" ; then
    AC_LANG_PUSH([C])
    for call in "" __stdcall ; do
        for sym in my_name_ my_name__ my_name MY_NAME MY_name MY_name_ NONE ; do
            AC_COMPILE_IFELSE([
                AC_LANG_SOURCE([void ${call} ${sym}(int a) {}])
            ],[
                PAC_RUNLOG([mv conftest.$OBJEXT cconftest.$OBJEXT])
                saved_LIBS="$LIBS"
                LIBS="cconftest.$OBJEXT $LIBS"
                AC_LANG_PUSH([Fortran 77])
                AC_LINK_IFELSE([
                    AC_LANG_PROGRAM([],[      call my_name(0)])
                ],[
                    pac_found=yes
                ])
                AC_LANG_POP([Fortran 77])
                LIBS="$saved_LIBS"
                rm -f cconftest.$OBJEXT
                test "$pac_found" = "yes" && break
            ])
        done
        test "$pac_found" = "yes" && break
    done
    AC_LANG_POP([C])
fi
if test "$pac_found" = "yes" ; then
    case ${sym} in
        my_name_)
            pac_cv_prog_f77_name_mangle="lower uscore" ;;
        my_name__)
            pac_cv_prog_f77_name_mangle="lower 2uscore" ;;
        my_name)
            pac_cv_prog_f77_name_mangle="lower" ;;
        MY_NAME)
            pac_cv_prog_f77_name_mangle="upper" ;;
        MY_name)
            pac_cv_prog_f77_name_mangle="mixed" ;;
        MY_name_)
            pac_cv_prog_f77_name_mangle="mixed uscore" ;;
        *)
            pac_cv_prog_f77_name_mangle=""
            pac_found=no;
            ;;
    esac
    if test "X$pac_cv_prog_f77_name_mangle" != "X" ; then
        if test "$call" = "__stdcall" ; then
            pac_cv_prog_f77_name_mangle="$pac_cv_prog_f77_name_mangle stdcall"
        fi
    fi
fi
])
dnl Endof ac_cache_check
case $pac_cv_prog_f77_name_mangle in
    *stdcall)
        F77_STDCALL="__stdcall" ;;
    *)
        F77_STDCALL="" ;;
esac
# Get the standard call definition
# FIXME: This should use F77_STDCALL, not STDCALL (non-conforming name)
F77_STDCALL="$call"
AC_DEFINE_UNQUOTED(STDCALL,[$F77_STDCALL],[Define calling convention])

# new_name="`echo $name | tr ' ' '_' | tr [a-z] [A-Z]`"
# We could have done the character conversion with 'tr'
# which may not be portable, e.g. solaris's /usr/ucb/bin/tr.
# So use a conservative approach.

# Replace blank with underscore
name_scheme="`echo $pac_cv_prog_f77_name_mangle | sed 's% %_%g'`"
# Turn lowercase into uppercase.
name_scheme="`echo $name_scheme | sed -e 'y%abcdefghijklmnopqrstuvwxyz%ABCDEFGHIJKLMNOPQRSTUVWXYZ%'`"
F77_NAME_MANGLE="F77_NAME_${name_scheme}"
AC_DEFINE_UNQUOTED([$F77_NAME_MANGLE])
AC_SUBST(F77_NAME_MANGLE)
if test "X$pac_cv_prog_f77_name_mangle" = "X" ; then
    AC_MSG_WARN([Unknown Fortran naming scheme])
fi
dnl
dnl Define the macros that is needed by AC_DEFINE_UNQUOTED([$F77_NAME_MANGLE])
AH_TEMPLATE([F77_NAME_LOWER],
    [Fortran names are lowercase with no trailing underscore])
AH_TEMPLATE([F77_NAME_LOWER_USCORE],
    [Fortran names are lowercase with one trailing underscore])
AH_TEMPLATE([F77_NAME_LOWER_2USCORE],
    [Fortran names are lowercase with two trailing underscores])
AH_TEMPLATE([F77_NAME_MIXED],
    [Fortran names preserve the original case])
AH_TEMPLATE([F77_NAME_MIXED_USCORE],
    [Fortran names preserve the original case with one trailing underscore])
AH_TEMPLATE([F77_NAME_UPPER],
    [Fortran names are uppercase])
AH_TEMPLATE([F77_NAME_LOWER_STDCALL],
    [Fortran names are lowercase with no trailing underscore in stdcall])
AH_TEMPLATE([F77_NAME_LOWER_USCORE_STDCALL],
    [Fortran names are lowercase with one trailing underscore in stdcall])
AH_TEMPLATE([F77_NAME_LOWER_2USCORE_STDCALL],
    [Fortran names are lowercase with two trailing underscores in stdcall])
AH_TEMPLATE([F77_NAME_MIXED_STDCALL],
    [Fortran names preserve the original case in stdcall])
AH_TEMPLATE([F77_NAME_MIXED_USCORE_STDCALL],
    [Fortran names preserve the original case with one trailing underscore in stdcall])
AH_TEMPLATE([F77_NAME_UPPER_STDCALL],
    [Fortran names are uppercase in stdcall])
])
dnl
dnl/*D
dnl PAC_PROG_F77_CHECK_SIZEOF - Determine the size in bytes of a Fortran
dnl type
dnl
dnl Synopsis:
dnl PAC_PROG_F77_CHECK_SIZEOF(type,[cross-size])
dnl
dnl Output Effect:
dnl Sets SIZEOF_F77_uctype to the size if bytes of type.
dnl If type is unknown, the size is set to 0.
dnl If cross-compiling, the value cross-size is used (it may be a variable)
dnl For example 'PAC_PROG_F77_CHECK_SIZEOF(real)' defines
dnl 'SIZEOF_F77_REAL' to 4 on most systems.  The variable 
dnl 'pac_cv_sizeof_f77_<type>' (e.g., 'pac_cv_sizeof_f77_real') is also set to
dnl the size of the type. 
dnl If the corresponding variable is already set, that value is used.
dnl If the name has an '*' in it (e.g., 'integer*4'), the defined name 
dnl replaces that with an underscore (e.g., 'SIZEOF_F77_INTEGER_4').
dnl
dnl Notes:
dnl If the 'cross-size' argument is not given, 'autoconf' will issue an error
dnl message.  You can use '0' to specify undetermined.
dnl
dnl D*/
AC_DEFUN([PAC_PROG_F77_CHECK_SIZEOF],[
AC_REQUIRE([AC_HEADER_STDC])
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
changequote(<<, >>)dnl
dnl The name to #define.
dnl dnl If the arg value contains a variable, we need to update that
define(<<PAC_TYPE_NAME>>, translit(sizeof_f77_$1, [a-z *], [A-Z__]))dnl
dnl The cache variable name.
define(<<PAC_CV_NAME>>, translit(pac_cv_f77_sizeof_$1, [ *], [__]))dnl
changequote([, ])dnl
AC_CACHE_CHECK([for size of Fortran type $1],PAC_CV_NAME,[
AC_REQUIRE([PAC_PROG_F77_NAME_MANGLE])
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        subroutine isize()
        $1 i(2)
        call cisize( i(1), i(2) )
        end
    ])
],[
    # pac_f77compile_ok=yes
    PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
    # Save original LIBS, prepend previously generated object file to LIBS
    saved_LIBS="$LIBS"
    LIBS="pac_f77conftest.$OBJEXT $FLIBS $LIBS"
    AC_LANG_PUSH([C])
    AC_RUN_IFELSE([
        AC_LANG_PROGRAM([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
#ifdef F77_NAME_UPPER
#define cisize_ CISIZE
#define isize_ ISIZE
#elif defined(F77_NAME_LOWER) || defined(F77_NAME_MIXED)
#define cisize_ cisize
#define isize_ isize
#endif
static int isize_val=0;
void cisize_(char *,char*);
void isize_(void);
void cisize_(char *i1p, char *i2p)
{ 
   isize_val = (int)(i2p - i1p);
}
        ],[
    FILE *f = fopen("conftestval", "w");
    if (!f) return 1;
    isize_();
    fprintf(f,"%d\n", isize_val);
        ])
        dnl Endof ac_lang_program
    ],[
        eval PAC_CV_NAME=`cat conftestval`
    ],[
        eval PAC_CV_NAME=0
    ],[
        # Use -9999 as value to emit a warning message after the cache_check.
        ifelse([$2],[],[eval PAC_CV_NAME=-9999],[eval PAC_CV_NAME=$2])
    ])
    dnl Endof ac_run_ifelse
    AC_LANG_POP([C])
    LIBS="$saved_LIBS"
    # remove previously generated object file.
    rm -f pac_f77conftest.$OBJEXT
],[
    # pac_f77compile_ok=no
    ifelse([$2],,eval PAC_CV_NAME=0,eval PAC_CV_NAME=$2)
])  Endof ac_compile_ifelse
AC_LANG_POP([Fortran 77])
])
dnl Endof ac_cache_check
if test "$PAC_CV_NAME" = "-9999" ; then
     AC_MSG_WARN([No value provided for size of $1 when cross-compiling])
fi
AC_DEFINE_UNQUOTED(PAC_TYPE_NAME,$PAC_CV_NAME,[Define size of PAC_TYPE_NAME])
undefine([PAC_TYPE_NAME])
undefine([PAC_CV_NAME])
])
dnl
dnl This version uses a Fortran program to link programs.
dnl This is necessary because some compilers provide shared libraries
dnl that are not within the default linker paths (e.g., our installation
dnl of the Portland Group compilers)
dnl
AC_DEFUN([PAC_PROG_F77_CHECK_SIZEOF_EXT],[
changequote(<<,>>)dnl
dnl The name to #define.
dnl If the arg value contains a variable, we need to update that
define(<<PAC_TYPE_NAME>>, translit(sizeof_f77_$1, [a-z *], [A-Z__]))dnl
dnl The cache variable name.
define(<<PAC_CV_NAME>>, translit(pac_cv_f77_sizeof_$1, [ *], [__]))dnl
changequote([,])dnl
AC_CACHE_CHECK([for size of Fortran type $1],PAC_CV_NAME,[
AC_REQUIRE([AC_HEADER_STDC])
AC_REQUIRE([PAC_PROG_F77_NAME_MANGLE])
dnl if test "$cross_compiling" = yes ; then
dnl     ifelse([$2],[],
dnl         [AC_MSG_WARN([No value provided for size of $1 when cross-compiling])],
dnl         [eval PAC_CV_NAME=$2])
dnl fi
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
#ifdef F77_NAME_UPPER
#define cisize_ CISIZE
#define isize_ ISIZE
#elif defined(F77_NAME_LOWER) || defined(F77_NAME_MIXED)
#define cisize_ cisize
#define isize_ isize
#endif
int cisize_(char *,char*);
int cisize_(char *i1p, char *i2p) {
    int isize_val=0;
    FILE *f = fopen("conftestval", "w");
    if (!f) return 1;
    isize_val = (int)(i2p - i1p);
    fprintf(f,"%d\n", isize_val);
    fclose(f);
    return 0;
}
    ])
    dnl Endof ac_lang_source
],[
    # pac_compile_ok=yes
    PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
    # Save LIBS and prepend object file to LIBS
    saved_LIBS="$LIBS"
    LIBS="pac_conftest.$OBJEXT $LIBS"
    AC_LANG_PUSH([Fortran 77])
    AC_RUN_IFELSE([
        AC_LANG_SOURCE([
            program main
            $1 a(2)
            integer irc, cisize
            irc = cisize(a(1),a(2))
            end
        ])
    ],[
        eval PAC_CV_NAME=`cat conftestval`
    ],[
        eval PAC_CV_NAME=0
    ],[
        # Use -9999 as value to emit a warning message after the cache_check.
        ifelse([$2],[],[eval PAC_CV_NAME=-9999],[eval PAC_CV_NAME=$2])
    ])
    AC_LANG_POP([Fortran 77])
    LIBS="$saved_LIBS"
    # remove previously generated object file.
    rm -f pac_conftest.$OBJEXT
],[
    AC_MSG_WARN([Unable to compile the C routine for finding the size of a $1])
])
AC_LANG_POP([C])
])
dnl Endof ac_cache_check
if test "$PAC_CV_NAME" = "-9999" ; then
     AC_MSG_WARN([No value provided for size of $1 when cross-compiling])
fi
AC_DEFINE_UNQUOTED(PAC_TYPE_NAME,$PAC_CV_NAME,[Define size of PAC_TYPE_NAME])
undefine([PAC_TYPE_NAME])
undefine([PAC_CV_NAME])
])
dnl
dnl/*D
dnl PAC_PROG_F77_EXCLAIM_COMMENTS
dnl
dnl Synopsis:
dnl PAC_PROG_F77_EXCLAIM_COMMENTS([action-if-true],[action-if-false])
dnl
dnl Notes:
dnl Check whether '!' may be used to begin comments in Fortran.
dnl
dnl This macro requires a version of autoconf `after` 2.13; the 'acgeneral.m4'
dnl file contains an error in the handling of Fortran programs in 
dnl 'AC_TRY_COMPILE' (fixed in our local version).
dnl
dnl D*/
AC_DEFUN([PAC_PROG_F77_EXCLAIM_COMMENTS],[
AC_CACHE_CHECK([whether Fortran 77 accepts ! for comments],
pac_cv_prog_f77_exclaim_comments,[
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([
     AC_LANG_PROGRAM([],[!        This is a comment])
],[
    pac_cv_prog_f77_exclaim_comments="yes"
],[
    pac_cv_prog_f77_exclaim_comments="no"
])
AC_LANG_POP([Fortran 77])
])
if test "$pac_cv_prog_f77_exclaim_comments" = "yes" ; then
    ifelse([$1],[],[:],[$1])
else
    ifelse([$2],[],[:],[$2])
fi
])dnl
dnl
dnl/*D
dnl PAC_F77_CHECK_COMPILER_OPTION - Check that a F77 compiler option is
dnl accepted without warning messages
dnl
dnl Synopsis:
dnl PAC_F77_CHECK_COMPILER_OPTION(optionname,action-if-ok,action-if-fail)
dnl
dnl Output Effects:
dnl
dnl If no actions are specified, a working value is added to 'FOPTIONS'
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
AC_DEFUN([PAC_F77_CHECK_COMPILER_OPTION],[
AC_MSG_CHECKING([whether Fortran 77 compiler accepts option $1])
pac_opt="$1"
AC_LANG_PUSH([Fortran 77])
FFLAGS_orig="$FFLAGS"
FFLAGS_opt="$pac_opt $FFLAGS"
pac_result="unknown"

AC_LANG_CONFTEST([AC_LANG_PROGRAM()])
FFLAGS="$FFLAGS_orig"
rm -f pac_test1.log
PAC_LINK_IFELSE_LOG([pac_test1.log], [], [
    FFLAGS="$FFLAGS_opt"
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
    FFLAGS="$FFLAGS_orig"
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

        FFLAGS="$FFLAGS_opt"
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

dnl Restore FFLAGS before 2nd/3rd argument commands are executed,
dnl as 2nd/3rd argument command could be modifying FFLAGS.
FFLAGS="$FFLAGS_orig"
if test "$pac_result" = "yes" ; then
     ifelse([$2],[],[FOPTIONS="$FOPTIONS $1"],[$2])
else
     ifelse([$3],[],[:],[$3])
fi
AC_LANG_POP([Fortran 77])
])
dnl
dnl/*D
dnl PAC_PROG_F77_LIBRARY_DIR_FLAG - Determine the flag used to indicate
dnl the directories to find libraries in
dnl
dnl Notes:
dnl Many compilers accept '-Ldir' just like most C compilers.  
dnl Unfortunately, some (such as some HPUX Fortran compilers) do not, 
dnl and require instead either '-Wl,-L,dir' or something else.  This
dnl command attempts to determine what is accepted.  The flag is 
dnl placed into 'F77_LIBDIR_LEADER'.
dnl
dnl D*/
dnl
dnl An earlier version of this only tried the arguments without using
dnl a library.  This failed when the HP compiler complained about the
dnl arguments, but produced an executable anyway.
AC_DEFUN([PAC_PROG_F77_LIBRARY_DIR_FLAG],[
AC_CACHE_CHECK([for Fortran 77 flag for library directories],
pac_cv_prog_f77_library_dir_flag,[
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        subroutine f1conf
        end
    ])
],[
    # pac_f77compile_ok=yes
    PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
    PAC_RUNLOG([test -d conftestdir || mkdir conftestdir])
    PAC_RUNLOG([${AR-ar} ${AR_FLAGS-cr} conftestdir/libf77conftest.a pac_f77conftest.$OBJEXT])
    PAC_RUNLOG([${RANLIB-ranlib} conftestdir/libf77conftest.a])
    # Save original LIBS, prepend previously generated object file to LIBS
    saved_LIBS="$LIBS"
    LIBS="-lf77conftest $LIBS"
    saved_LDFLAGS="$LDFLAGS"
    pac_cv_prog_f77_library_dir_flag="none"
    for ldir in "-L" "-Wl,-L," ; do
        LDFLAGS="${ldir}conftestdir $saved_LDFLAGS"
        AC_LINK_IFELSE([
            AC_LANG_SOURCE([
                program main
                call f1conf
                end
            ])
        ],[pac_cv_prog_f77_library_dir_flag="$ldir";break])
    done
    LDFLAGS="$saved_LDFLAGS"
    LIBS="$saved_LIBS"
    rm -rf conftestdir
    rm -f pac_f77conftest.$OBJEXT
],[])
AC_LANG_POP([Fortran 77])
])
dnl Endof ac_cache_check
if test "X$pac_cv_prog_f77_library_dir_flag" != "Xnone" ; then
    F77_LIBDIR_LEADER="$pac_cv_prog_f77_library_dir_flag"
    AC_SUBST(F77_LIBDIR_LEADER)
fi
])
dnl
dnl/*D 
dnl PAC_PROG_F77_HAS_INCDIR - Check whether Fortran accepts -Idir flag
dnl
dnl Syntax:
dnl   PAC_PROG_F77_HAS_INCDIR(directory,action-if-true,action-if-false)
dnl
dnl Output Effect:
dnl  Sets 'F77_INCDIR' to the flag used to choose the directory.  
dnl
dnl Notes:
dnl This refers to the handling of the common Fortran include extension,
dnl not to the use of '#include' with the C preprocessor.
dnl If directory does not exist, it will be created.  In that case, the 
dnl directory should be a direct descendant of the current directory.
dnl
dnl D*/
AC_DEFUN([PAC_PROG_F77_HAS_INCDIR],[
ifelse([$1],[],[checkdir=f77tmpdir],[checkdir=$1;checkdir_is_given=yes])
AC_CACHE_CHECK([for include directory flag for Fortran],
pac_cv_prog_f77_has_incdir,[
test -d $checkdir || mkdir $checkdir
dnl PAC_RUNLOG([echo '       call sub()' > $checkdir/conftestf.h])
echo '       call sub()' > $checkdir/conftestf.h
AC_LANG_PUSH([Fortran 77])
saved_FFLAGS="$FFLAGS"
pac_cv_prog_f77_has_incdir="none"
# SGI wants -Wf,-I
for idir in "-I" "-Wf,-I" ; do
    FFLAGS="${idir} $checkdir $saved_FFLAGS"
    AC_COMPILE_IFELSE([
        AC_LANG_SOURCE([
            program main
            include 'conftestf.h'
            end
        ])
    ],[pac_cv_prog_f77_has_incdir="$idir"; break])
done
FFLAGS="$saved_FFLAGS"
AC_LANG_POP([Fortran 77])
if test "$checkdir_is_given" = "yes" ; then
    rm -f $checkdir/conftestf.h
else
    rm -rf $checkdir
fi
])
dnl Endof ac_cache_check
if test "X$pac_cv_prog_f77_has_incdir" != "Xnone" ; then
    F77_INCDIR="$pac_cv_prog_f77_has_incdir"
    AC_SUBST(F77_INCDIR)
fi
])
dnl
dnl/*D
dnl PAC_PROG_F77_ALLOWS_UNUSED_EXTERNALS - Check whether the Fortran compiler
dnl allows unused and undefined functions to be listed in an external 
dnl statement
dnl
dnl Syntax:
dnl   PAC_PROG_F77_ALLOWS_UNUSED_EXTERNALS(action-if-true,action-if-false)
dnl
dnl D*/
AC_DEFUN([PAC_PROG_F77_ALLOWS_UNUSED_EXTERNALS],[
AC_CACHE_CHECK([whether Fortran allows unused externals],
pac_cv_prog_f77_allows_unused_externals,[
AC_LANG_PUSH([Fortran 77])
AC_LINK_IFELSE([
    AC_LANG_SOURCE([
        program main
        external bar
        end
    ])
],[
    pac_cv_prog_f77_allows_unused_externals="yes"
],[
    pac_cv_prog_f77_allows_unused_externals="no"
])
AC_LANG_POP([Fortran 77])
])
dnl Endof ac_cache_check
if test "X$pac_cv_prog_f77_allows_unused_externals" = "Xyes" ; then
   ifelse([$1],[],[:],[$1])
else
   ifelse([$2],[],[:],[$2])
fi
])
dnl PAC_PROG_F77_RUN_PROC_FROM_C( c main program, fortran routine,
dnl                               [action-if-works], [action-if-fails],
dnl                               [cross-action] )
dnl Fortran routine MUST be named ftest unless you include code
dnl to select the appropriate Fortran name.
dnl 
AC_DEFUN([PAC_PROG_F77_RUN_PROC_FROM_C],[
AC_REQUIRE([AC_HEADER_STDC])
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([$2])
],[
    # pac_f77compile_ok=yes
    PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
    # Save original LIBS, prepend previously generated object file to LIBS
    saved_LIBS="$LIBS"
    LIBS="pac_f77conftest.$OBJEXT $FLIBS $LIBS"
    AC_LANG_PUSH([C])
    AC_RUN_IFELSE([
        AC_LANG_SOURCE([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
#ifdef F77_NAME_UPPER
#define ftest_ FTEST
#elif defined(F77_NAME_LOWER) || defined(F77_NAME_MIXED)
#define ftest_ ftest
#endif
$1
        ])
    ],[
        ifelse([$3],[],[:],[$3])
    ],[
        ifelse([$4],[],[:],[$4])
    ],[
        ifelse([$5],[],[:],[$5])
    ])
    AC_LANG_POP([C])
    LIBS="$saved_LIBS"
    rm -f pac_f77conftest.$OBJEXT
],[
])
AC_LANG_POP([Fortran 77])
])
dnl PAC_PROG_F77_IN_C_LIBS
dnl
dnl Find the essential libraries that are needed to use the C linker to 
dnl create a program that includes a trival Fortran code.  
dnl
dnl For example, all pgf90 compiled objects include a reference to the
dnl symbol pgf90_compiled, found in libpgf90 .
dnl
dnl There is an additional problem.  To *run* programs, we may need 
dnl additional arguments; e.g., if shared libraries are used.  Even
dnl with autoconf 2.52, the autoconf macro to find the library arguments
dnl doesn't handle this, either by detecting the use of -rpath or
dnl by trying to *run* a trivial program.  It only checks for *linking*.
dnl 
dnl
AC_DEFUN([PAC_PROG_F77_IN_C_LIBS],[
AC_REQUIRE([AC_HEADER_STDC])
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
AC_MSG_CHECKING([for which Fortran libraries are needed to link C with Fortran])
F77_IN_C_LIBS="invalid"
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        subroutine ftest
        end
    ])
],[
    # pac_f77compile_ok=yes
    PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
    # Save original LIBS, prepend previously generated object file to LIBS
    saved_LIBS="$LIBS"
    LIBS="pac_f77conftest.$OBJEXT $FLIBS $saved_LIBS"
    AC_LANG_PUSH([C])

    # Create conftest for all link tests.
    AC_LANG_CONFTEST([
        AC_LANG_PROGRAM([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
        ],[
#ifdef F77_NAME_UPPER
#define ftest_ FTEST
#elif defined(F77_NAME_LOWER) || defined(F77_NAME_MIXED)
#define ftest_ ftest
#endif
extern void ftest_(void);
ftest_();
        ])
    ])

    F77_IN_C_LIBS=""
    AC_LINK_IFELSE([],[:],[
        flibdirs=`echo $FLIBS | tr ' ' '\012' | grep '\-L' | tr '\012' ' '`
        fliblibs=`echo $FLIBS | tr ' ' '\012' | grep -v '\-L' | tr '\012' ' '`
        for flibs in $fliblibs ; do
            LIBS="pac_f77conftest.$OBJEXT $flibdirs $flibs $saved_LIBS"
            AC_LINK_IFELSE([],[F77_IN_C_LIBS="$flibdirs $flibs"; break])
        done
        if test "X$F77_IN_C_LIBS" = "X" ; then
            flibscat=""
            for flibs in $fliblibs ; do
                flibscat="$flibscat $flibs"
                LIBS="pac_f77conftest.$OBJEXT $flibdirs $flibscat $saved_LIBS"
                AC_LINK_IFELSE([],[F77_IN_C_LIBS="$flibdirs $flibscat";break])
            done
        fi
    ])

    # remove conftest created by ac_lang_conftest
    rm -f conftest.$ac_ext
    AC_LANG_POP([C])
    LIBS="$saved_LIBS"
    rm -f pac_f77conftest.$OBJEXT
])
AC_LANG_POP([Fortran 77])
if test "X$F77_IN_C_LIBS" = "X" ; then
    AC_MSG_RESULT(none)
else
    AC_MSG_RESULT($F77_IN_C_LIBS)
fi
])
dnl
dnl Test to see if we should use C or Fortran to link programs whose
dnl main program is in Fortran.  We may find that neither work because 
dnl we need special libraries in each case.
dnl
AC_DEFUN([PAC_PROG_F77_LINKER_WITH_C],[
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
AC_MSG_CHECKING([for linker for Fortran main program])
dnl Create a C program that uses multiplication and division
dnl in case that requires special libraries
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([
    AC_LANG_PROGRAM([],[long long a;])
],[
    AC_DEFINE(HAVE_LONG_LONG,1,[Define if long long allowed])
])
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
#ifdef HAVE_LONG_LONG
int f(int a, long long b) { int c; c = a * ( b / 3 ) / (b-1); return c ; }
#else
int f(int a, long b) { int c; c = a * b / (b-1); return c ; }
#endif
    ])
])
AC_LANG_POP([C])

dnl Create a Fortran program for test
AC_LANG_PUSH([Fortran 77])
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        program main
        double precision d
        print *, "hi"
        end
    ])
])
AC_LANG_POP([Fortran 77])

dnl Initialize flags
pac_linkwithf77=no
pac_linkwithC=no

dnl Use F77 as a linker to compile a Fortran main and C subprogram.
if test "$pac_linkwithC" != "yes" ; then
    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([],[
        PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
        saved_LIBS="$LIBS"
        LIBS="pac_conftest.$OBJEXT $saved_LIBS"
        AC_LANG_PUSH([Fortran 77])
        AC_LINK_IFELSE([],[
            AC_MSG_RESULT([Use Fortran to link programs])
            pac_linkwithf77=yes
        ])
        AC_LANG_POP([Fortran 77])
        LIBS="$saved_LIBS"
        rm -f pac_conftest.$OBJEXT
    ])
    AC_LANG_POP([C])
fi

dnl Use C as a linker and FLIBS to compile a Fortran main and C subprogram.
if test "$pac_linkwithf77" != "yes" ; then
    AC_LANG_PUSH([Fortran 77])
    AC_COMPILE_IFELSE([],[
        PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
        saved_LIBS="$LIBS"
        LIBS="pac_f77conftest.$OBJEXT $FLIBS $saved_LIBS"
        AC_LANG_PUSH([C])
        AC_LINK_IFELSE([],[
            pac_linkwithC=yes
            AC_MSG_RESULT([Use C with FLIBS to link programs])
            F77LINKER="$CC"
            F77_LDFLAGS="$F77_LDFLAGS $FLIBS"
        ])
        AC_LANG_POP([C])
        LIBS="$saved_LIBS"
        rm -f pac_f77conftest.$OBJEXT
    ])
    AC_LANG_POP([Fortran 77])
fi

AC_LANG_PUSH([Fortran 77])
rm -f conftest.$ac_ext
AC_LANG_POP([Fortran 77])

AC_LANG_PUSH([C])
rm -f conftest.$ac_ext
AC_LANG_POP([C])

if test "$pac_linkwithf77" != "yes" -a "$pac_linkwithC" != "yes" ; then
    AC_MSG_ERROR([Could not determine a way to link a Fortran test program!])
fi
])
dnl
dnl Check to see if a C program can be linked when using the libraries
dnl needed by C programs
dnl
AC_DEFUN([PAC_PROG_F77_CHECK_FLIBS],[
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
AC_MSG_CHECKING([whether $CC links with FLIBS found by autoconf])
AC_LANG_PUSH([C])
# Create a simple C program for the tests.
AC_LANG_CONFTEST([
    AC_LANG_PROGRAM([],[int a;])
])
# Try to link a C program with all of these libraries
saved_LIBS="$LIBS"
LIBS="$FLIBS $saved_LIBS"
AC_LINK_IFELSE([],[
    AC_MSG_RESULT([yes])
],[
    AC_MSG_RESULT([no])
    AC_MSG_CHECKING([for which libraries can be used])
    pac_ldirs=""
    pac_libs=""
    pac_other=""
    for name in $FLIBS ; do
        case $name in 
        -l*) pac_libs="$pac_libs $name"   ;;
        -L*) pac_ldirs="$pac_ldirs $name" ;;
          *) pac_other="$pac_other $name" ;;
        esac
    done
    keep_libs=""
    for name in $pac_libs ; do
        LIBS="$saved_LIBS $pac_ldirs $pac_other $name"
        AC_LINK_IFELSE([],[
            keep_libs="$keep_libs $name"
        ])
    done
    AC_MSG_RESULT($keep_libs)
    FLIBS="$pac_ldirs $pac_other $keep_libs"
])
LIBS="$saved_LIBS"
rm -f conftest.$ac_ext
AC_LANG_PUSH([C])
])
dnl
dnl Test for extra libraries needed when linking C routines that use
dnl stdio with Fortran.  This test was created for OSX, which 
dnl sometimes requires -lSystemStubs.  If another library is needed,
dnl add it to F77_OTHER_LIBS
dnl
AC_DEFUN([PAC_PROG_F77_AND_C_STDIO_LIBS],[
AC_REQUIRE([AC_HEADER_STDC])
AC_REQUIRE([PAC_PROG_F77_NAME_MANGLE])
# To simply the code in the cache_check macro, chose the routine name
# first, in case we need it
confname=conf1_
case "$pac_cv_prog_f77_name_mangle" in
    "lower underscore")       confname=conf1_ ;;
    "upper stdcall")          confname=CONF1  ;;
    "upper")                  confname=CONF1  ;;
    "lower doubleunderscore") confname=conf1_ ;;
    "lower")                  confname=conf1  ;;
    "mixed underscore")       confname=conf1_ ;;
    "mixed")                  confname=conf1  ;;
esac

AC_CACHE_CHECK([for libraries to link Fortran main with C stdio routines],
pac_cv_prog_f77_and_c_stdio_libs,[
pac_cv_prog_f77_and_c_stdio_libs=unknown
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
int $confname(int a) {
    printf( "The answer is %d\n", a ); fflush(stdout); return 0;
}
    ])
],[
    PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
    saved_LIBS="$LIBS"
    AC_LANG_PUSH([Fortran 77])
    AC_LANG_CONFTEST([
        AC_LANG_SOURCE([
            program main
            call conf1(0)
            end
        ])
    ])
    for extralib in "" "-lSystemStubs" ; do
        LIBS="pac_conftest.$OBJEXT $saved_LIBS $extralib"
        AC_LINK_IFELSE([],[
            pac_cv_prog_f77_and_c_stdio_libs="$extralib"; break
        ])
    done
    if test "X$pac_cv_prog_f77_and_c_stdio_libs" = "X" ; then
        pac_cv_prog_f77_and_c_stdio_libs=none
    fi
    rm -f conftest.$ac_ext
    AC_LANG_POP([Fortran 77])
    LIBS="$saved_LIBS"
    rm -f pac_conftest.$OBJEXT
])
AC_LANG_POP([C])
])
dnl Endof ac_cache_check
if test "$pac_cv_prog_f77_and_c_stdio_libs" != "none" \
     -a "$pac_cv_prog_f77_and_c_stdio_libs" != "unknown" ; then
    F77_OTHER_LIBS="$F77_OTHER_LIBS $pac_cv_prog_f77_and_c_stdio_libs"
fi
])
dnl
dnl Check that the FLIBS determined by AC_F77_LIBRARY_LDFLAGS is valid.
dnl That macro (at least as of autoconf 2.59) attempted to parse the output
dnl of the compiler when asked to be verbose; in the case of the Fujitsu
dnl frt Fortran compiler, it included files that frt looked for and then
dnl discarded because they did not exist.
dnl
AC_DEFUN([PAC_PROG_F77_FLIBS_VALID],[
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
AC_MSG_CHECKING([whether $F77 accepts the FLIBS found by autoconf])
pac_cv_f77_flibs_valid=unknown
AC_LANG_PUSH([Fortran 77])
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        program main
        end
    ])
])
AC_LINK_IFELSE([],[
    AC_MSG_RESULT([yes])
],[
    AC_MSG_RESULT([no])
    AC_MSG_CHECKING([for valid entries in FLIBS])
    goodFLIBS=""
    saveFLIBS=$FLIBS
    FLIBS=""
    for arg in $saveFLIBS ; do
        FLIBS="$goodFLIBS $arg"
        AC_LINK_IFELSE([],[goodFLIBS=$FLIBS])
    done
    FLIBS=$goodFLIBS
    AC_MSG_RESULT($FLIBS)
])
rm -f conftest.$ac_ext
AC_LANG_POP([Fortran 77])
])
dnl
dnl Check if the Fortran 77 and C objects are compatible in linking.
dnl e.g. On some intel x86_64 Mac, Fortran compiler's default binary format
dnl is different from C, so either -m64 or -m32 is needed in either CFLAGS
dnl or FFLAGS.
dnl
AC_DEFUN([PAC_PROG_F77_OBJ_LINKS_WITH_C],[
AC_REQUIRE([AC_F77_LIBRARY_LDFLAGS])
AC_MSG_CHECKING([whether Fortran 77 and C objects are compatible])
AC_LANG_PUSH([C])
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
/* lower */
void c_subpgm( int *rc );
void c_subpgm( int *rc ) { *rc = 1; }

/* lower underscore */
void c_subpgm_( int *rc );
void c_subpgm_( int *rc ) { *rc = 2; }

/* upper */
void C_SUBPGM( int *rc );
void C_SUBPGM( int *rc ) { *rc = 3; }

/* lower doubleunderscore */
void c_subpgm__( int *rc );
void c_subpgm__( int *rc ) { *rc = 4; }

/* mixed */
void C_subpgm( int *rc );
void C_subpgm( int *rc ) { *rc = 5; }

/* mixed underscore */
void C_subpgm_( int *rc );
void C_subpgm_( int *rc ) { *rc = 6; }
    ])
])
AC_LANG_POP([C])

AC_LANG_PUSH([Fortran 77])
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        program test
        integer rc
        rc = -1
        call c_subpgm( rc )
        write(6,*) "rc=", rc
        end
    ])
])
AC_LANG_POP([Fortran 77])

dnl Initialize flags
pac_linkwithf77=no
pac_linkwithC=no

dnl Use F77 as a linker to compile a Fortran main and C subprogram.
if test "$pac_linkwithC" != "yes" ; then
    AC_LANG_PUSH([C])
    AC_COMPILE_IFELSE([],[
        PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
        saved_LIBS="$LIBS"
        LIBS="pac_conftest.$OBJEXT $saved_LIBS"
        AC_LANG_PUSH([Fortran 77])
        AC_LINK_IFELSE([],[
            pac_linkwithf77=yes
            AC_MSG_RESULT([yes])
        ])
        AC_LANG_POP([Fortran 77])
        LIBS="$saved_LIBS"
        if test "$pac_linkwithf77" = "yes" ; then
            rm -f pac_conftest.$OBJEXT
        fi
    ])
    AC_LANG_POP([C])
fi

dnl Use C as a linker and FLIBS to compile a Fortran main and C subprogram.
if test "$pac_linkwithf77" != "yes" ; then
    AC_LANG_PUSH([Fortran 77])
    AC_COMPILE_IFELSE([],[
        PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
        saved_LIBS="$LIBS"
        LIBS="pac_f77conftest.$OBJEXT $FLIBS $saved_LIBS"
        AC_LANG_PUSH([C])
        AC_LINK_IFELSE([],[
            pac_linkwithC=yes
            AC_MSG_RESULT([yes])
        ])
        AC_LANG_POP([C])
        LIBS="$saved_LIBS"
        if test "$pac_linkwithC" = "yes" ; then
            rm -f pac_f77conftest.$OBJEXT
        fi
    ])
    AC_LANG_POP([Fortran 77])
fi

AC_LANG_PUSH([Fortran 77])
rm -f conftest.$ac_ext
AC_LANG_POP([Fortran 77])

AC_LANG_PUSH([C])
rm -f conftest.$ac_ext
AC_LANG_POP([C])

if test "$pac_linkwithf77" != "yes" -a "$pac_linkwithC" != "yes" ; then
    AC_MSG_RESULT(no)
    AC_CHECK_PROG(FILE, file, file, [])
    if test "X$FILE" != "X" ; then
        fobjtype="`${FILE} pac_f77conftest.$OBJEXT | sed -e \"s|pac_f77conftest\.$OBJEXT||g\"`"
        cobjtype="`${FILE} pac_conftest.$OBJEXT | sed -e \"s|pac_conftest\.$OBJEXT||g\"`"
        if test "$fobjtype" != "$cobjtype" ; then
            AC_MSG_ERROR([****  Incompatible Fortran and C Object File Types!  ****
F77 Object File Type produced by \"${F77} ${FFLAGS}\" is : ${fobjtype}.
 C  Object File Type produced by \"${CC} ${CFLAGS}\" is : ${cobjtype}.])
        fi
    fi
fi
])
dnl
dnl /*D
dnl PAC_F77_WORKS_WITH_CPP
dnl
dnl Checks if Fortran 77 compiler works with C preprocessor
dnl
dnl Most systems allow the Fortran compiler to process .F and .F90 files
dnl using the C preprocessor.  However, some systems either do not
dnl allow this or have serious bugs (OSF Fortran compilers have a bug
dnl that generates an error message from cpp).  The following test
dnl checks to see if .F works, and if not, whether "cpp -P -C" can be used
dnl D*/
AC_DEFUN([PAC_F77_WORKS_WITH_CPP],[
AC_REQUIRE([AC_PROG_CPP])
AC_MSG_CHECKING([whether Fortran 77 compiler processes .F files with C preprocessor])
AC_LANG_PUSH([Fortran 77])
saved_f77_ext=${ac_ext}
ac_ext="F"
saved_FFLAGS="$FFLAGS"
FFLAGS="$FFLAGS $CPPFLAGS"
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        program main
#define ASIZE 10
        integer a(ASIZE)
        end
    ])
])
AC_COMPILE_IFELSE([],[
    pac_cv_f77_accepts_F=yes
    ifelse([$1],[],[],[$1=""])
],[
    pac_cv_f77_accepts_F=no
    ifelse([$1],[],[:],[$1="false"])
])
# Restore Fortran 77's ac_ext but not FFLAGS
ac_ext="$saved_f77_ext"

if test "$pac_cv_f77_accepts_F" != "yes" ; then
    pac_cpp_f77="$ac_cpp -C -P conftest.F > conftest.$ac_ext"
    PAC_RUNLOG_IFELSE([$pac_cpp_f77],[
        if test -s conftest.${ac_ext} ; then
            AC_COMPILE_IFELSE([],[
                pac_cv_f77_accepts_F="no, use cpp"
                ifelse([$1],[],[],[$1="$CPP -C -P"])
            ],[])
            rm -f conftest.${ac_ext}
        fi
    ],[])
fi
FFLAGS="$saved_FFLAGS"
rm -f conftest.F
AC_LANG_POP([Fortran 77])
AC_MSG_RESULT([$pac_cv_f77_accepts_F])
])
dnl
dnl /*D
dnl PAC_PROG_F77_CRAY_POINTER - Check if Fortran 77 supports Cray-style pointer.
dnl                             If so, set pac_cv_prog_f77_has_pointer to yes
dnl                             and find out if any extra compiler flag is
dnl                             needed and set it as CRAYPTR_FFLAGS.
dnl                             i.e. CRAYPTR_FFLAGS is meaningful only if
dnl                             pac_cv_prog_f77_has_pointer = yes.
dnl
dnl Synopsis:
dnl   PAC_PROG_F77_CRAY_POINTER([action-if-true],[action-if-false])
dnl D*/
AC_DEFUN([PAC_PROG_F77_CRAY_POINTER],[
AC_CACHE_CHECK([whether Fortran 77 supports Cray-style pointer],
pac_cv_prog_f77_has_pointer,[
AC_LANG_PUSH([Fortran 77])
AC_LANG_CONFTEST([
    AC_LANG_PROGRAM([],[
        integer M
        pointer (MPTR,M)
        data MPTR/0/
    ])
])
saved_FFLAGS="$FFLAGS"
pac_cv_prog_f77_has_pointer=no
CRAYPTR_FFLAGS=""
for ptrflag in '' '-fcray-pointer' ; do
    FFLAGS="$saved_FFLAGS $ptrflag"
    AC_COMPILE_IFELSE([], [
        pac_cv_prog_f77_has_pointer=yes
        CRAYPTR_FFLAGS="$ptrflag"
        break
    ])
done
dnl Restore FFLAGS first, since user may not want to modify FFLAGS
FFLAGS="$saved_FFLAGS"
dnl remove conftest after ac_lang_conftest
rm -f conftest.$ac_ext
AC_LANG_POP([Fortran 77])
])
if test "$pac_cv_prog_f77_has_pointer" = "yes" ; then
    AC_MSG_CHECKING([for Fortran 77 compiler flag for Cray-style pointer])
    if test "X$CRAYPTR_FFLAGS" != "X" ; then
        AC_MSG_RESULT([$CRAYPTR_FFLAGS])
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
dnl PAC_F77_INIT_WORKS_WITH_C
dnl
AC_DEFUN([PAC_F77_INIT_WORKS_WITH_C],[
AC_REQUIRE([AC_HEADER_STDC])
AC_MSG_CHECKING([whether Fortran init will work with C])
pac_f_init_works_with_c=unknown
AC_LANG_PUSH([Fortran 77])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
        subroutine minit()
        common /m1/ vc, vc2
        character*1 vc(1,1), vc2(1)
        common /m2/ vd
        integer vd
        save /m1/, /m2/
        call minitc( vc, vc2, vd )
        end
    ])
],[
    PAC_RUNLOG([mv conftest.$OBJEXT pac_f77conftest.$OBJEXT])
    saved_LIBS="$LIBS"
    # This test checks if Fortran init can be done in pure C environment,
    # i.e. no FLIBS in linking, so don't put FLIBS in LIBS below
    dnl LIBS="pac_f77conftest.$OBJEXT $FLIBS $LIBS"
    LIBS="pac_f77conftest.$OBJEXT $LIBS"
    AC_LANG_PUSH([C])
    AC_LINK_IFELSE([
        AC_LANG_SOURCE([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
#ifdef F77_NAME_UPPER
#define minit_ MINIT
#elif defined(F77_NAME_LOWER) || defined(F77_NAME_MIXED)
#define minit_ minit
#endif
extern void minit_(void);
int main( int argc, char **argv )
{
    minit_();
    return 0;
}
char *v1 = 0;
char *vc2 = 0;
int  v2 = 0;
void minitc_( char *dv1, int d, char *dv2, int d2, int dv3 );
void minitc_( char *dv1, int d, char *dv2, int d2, int dv3 )
{
v1 = dv1;
v2 = dv3;
vc2 = dv2;
*vc2 = ' ';
}
        ])
    ],[pac_f_init_works_with_c=yes],[pac_f_init_works_with_c=no])
    AC_LANG_POP([C])
    LIBS="$saved_LIBS"
    rm -f pac_f77conftest.$OBJEXT
])
AC_LANG_POP([Fortran 77])
AC_MSG_RESULT([$pac_f_init_works_with_c])
])
dnl
dnl PAC_F77_LOGICALS_IN_C(MPI_FINT)
dnl
dnl where MPI_FINT is the C type for Fortran integer.
dnl
dnl Use a Fortran main program.  This simplifies some steps,
dnl since getting all of the Fortran libraries (including shared
dnl libraries that are not in the default library search path) can
dnl be tricky.  Specifically, The PROG_F77_RUN_PROC_FROM_C failed with
dnl some installations of the Portland group compiler.
dnl
dnl We'd also like to check other values for .TRUE. and .FALSE. to see
dnl if the compiler allows (or uses) more than one value (some DEC compilers,
dnl for example, used the high (sign) bit to indicate true and false; the
dnl rest of the bits were ignored.  For now, we'll assume that there are
dnl unique true and false values.
dnl
AC_DEFUN([PAC_F77_LOGICALS_IN_C],[
AC_REQUIRE([AC_HEADER_STDC])
AC_REQUIRE([PAC_PROG_F77_NAME_MANGLE])
pac_mpi_fint="$1"
AC_MSG_CHECKING([for values of Fortran logicals])
AC_CACHE_VAL(pac_cv_prog_f77_true_false_value,[
pac_cv_prog_f77_true_false_value=""
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
#if defined(HAVE_STDLIB_H) || defined(STDC_HEADERS)
#include <stdlib.h>
#endif
#ifdef F77_NAME_UPPER
#define ftest_ FTEST
#elif defined(F77_NAME_LOWER) || defined(F77_NAME_MIXED)
#define ftest_ ftest
#endif
void ftest_( $pac_mpi_fint *, $pac_mpi_fint *);
void ftest_( $pac_mpi_fint *itrue, $pac_mpi_fint *ifalse )
{
  FILE *f = fopen("conftestval","w");
  if (!f) exit(1);
  fprintf( f, "%d %d\n", *itrue, *ifalse );
  fclose(f);
}
    ])
],[
    PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
    saved_LIBS="$LIBS"
    LIBS="pac_conftest.$OBJEXT $saved_LIBS"
    AC_LANG_PUSH([Fortran 77])
    AC_RUN_IFELSE([
        AC_LANG_SOURCE([
            program main
            logical itrue, ifalse
            itrue = .TRUE.
            ifalse = .FALSE.
            call ftest( itrue, ifalse )
            end
        ])
    ],[
        pac_cv_prog_f77_true_false_value="`cat conftestval`"
    ],[
        AC_MSG_WARN([Failed to build/run program to determine Fortran logical values.])
    ],[
        # Cross-Compiling.  Allow the user to set the values
        if test -n "$CROSS_F77_TRUE_VALUE" -a -n "$CROSS_F77_FALSE_VALUE" ; then
            pac_cv_prog_f77_true_false_value="$CROSS_F77_TRUE_VALUE $CROSS_F77_FALSE_VALUE"
        else
            AC_MSG_WARN([Either CROSS_F77_TRUE_VALUE="$CROSS_F77_TRUE_VALUE" or CROSS_F77_FALSE_VALUE="$CROSS_F77_FALSE_VALUE" is not set.])
        fi
    ])
    AC_LANG_POP([Fortran 77])
    LIBS="$saved_LIBS"
    rm -f pac_conftest.$OBJEXT
])
AC_LANG_POP([C])
])
dnl Endof ac_cache_val
if test "X$pac_cv_prog_f77_true_false_value" != "X" ; then
    true_val="`echo $pac_cv_prog_f77_true_false_value | sed -e 's/ .*//g'`"
    false_val="`echo $pac_cv_prog_f77_true_false_value | sed -e 's/.*  *//g'`"
    if test -n "$true_val" -a -n "$false_val" ; then
        AC_MSG_RESULT([True is $true_val and False is $false_val])
    else
        AC_MSG_RESULT([could not determine])
    fi
fi
if test -n "$true_val" -a -n "$false_val" ; then
    AC_DEFINE(F77_TRUE_VALUE_SET,1,[Define if we know the value of Fortran true and false])
    AC_DEFINE_UNQUOTED(F77_TRUE_VALUE,$true_val,[The value of true in Fortran])
    AC_DEFINE_UNQUOTED(F77_FALSE_VALUE,$false_val,[The value of false in Fortran])
fi
])
dnl/*D
dnl PAC_PROG_F77_MISMATCHED_ARGS([option],[AllOnly]) - Determine whether the
dnl Fortran compiler allows routines to be called with different
dnl argument types.  If not, attempts to determine a command-line argument
dnl that permits such use
dnl (The Fortran standard prohibits this usage)
dnl
dnl option is set to the compiler option to use.
dnl if AllOnly is yes (literal, not variable with value), then only consider
dnl options that turn off checking
dnl for all routines
dnl
dnl The NAG Fortran compiler, nagfor, is known to enforce this part of the
dnl Fortran standard.
dnl D*/
AC_DEFUN([PAC_PROG_F77_MISMATCHED_ARGS],[
AC_MSG_CHECKING([whether $F77 allows mismatched arguments])
if test "X$pac_cv_prog_f77_mismatched_args" = X ; then
    pac_cv_prog_f77_mismatched_args_parm=""
    pac_cv_prog_f77_mismatched_args=no
    AC_LANG_PUSH([Fortran 77])
    AC_COMPILE_IFELSE([
       AC_LANG_SOURCE([
        program main
        integer a
        real b
        character c
        call foo1(a)
        call foo1(b)
        call foo1(c)
        end
])],[pac_cv_prog_f77_mismatched_args=yes])
    if test "$pac_cv_prog_f77_mismatched_args" != "yes" ; then
        # try again with -wmismatch=foo1
        save_FFLAGS="$FFLAGS"
	# The best solution is to turn off errors on particular routines
	# if that isn't possible (e.g., too many of them), then
	# just try arguments that turn off all checking
	for flags in ifelse($2,yes,,"-wmismatch=foo1") "-mismatch" ; do
            testok=no
            FFLAGS="$FFLAGS $flags"
            AC_COMPILE_IFELSE([
            AC_LANG_SOURCE([
        program main
        integer a
        real b
        character c
        call foo1(a)
        call foo1(b)
        call foo1(c)
        end
])],[testok=yes])
            FFLAGS="$save_FFLAGS"
            if test "$testok" = yes ; then break ; fi
        done
        if test "$testok" = yes ; then
	    pac_cv_prog_f77_mismatched_args_parm="$flags"
            pac_cv_prog_f77_mismatched_args="yes, with $pac_cv_prog_f77_mismatched_args_parm"
        fi
    fi
    AC_LANG_POP([Fortran 77])
fi
AC_MSG_RESULT($pac_cv_prog_f77_mismatched_args)
if test "$pac_cv_prog_f77_mismatched_args" = no ; then
    AC_MSG_ERROR([The Fortran compiler $F77 will not compile files that call
the same routine with arguments of different types.])
fi

ifelse($1,,,[$1=$pac_cv_prog_f77_mismatched_args_parm])
])
