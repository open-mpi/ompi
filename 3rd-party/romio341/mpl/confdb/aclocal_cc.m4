dnl AC_PROG_CC_GNU
ifdef([AC_PROG_CC_GNU],,[AC_DEFUN([AC_PROG_CC_GNU],)])

dnl
dnl/*D
dnl PAC_C_CHECK_COMPILER_OPTION - Check that a compiler option is accepted
dnl without warning messages
dnl
dnl Synopsis:
dnl PAC_C_CHECK_COMPILER_OPTION(optionname,action-if-ok,action-if-fail)
dnl
dnl Output Effects:
dnl
dnl If no actions are specified, a working value is added to 'COPTIONS'
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
dnl
dnl D*/
AC_DEFUN([PAC_C_CHECK_COMPILER_OPTION],[
AC_MSG_CHECKING([whether C compiler accepts option $1])
pac_opt="$1"
AC_LANG_PUSH([C])
CFLAGS_orig="$CFLAGS"
CFLAGS_opt="$pac_opt $CFLAGS"
pac_result="unknown"

AC_LANG_CONFTEST([
	AC_LANG_PROGRAM([[#include <stdio.h>
                          const char hw[] = "Hello, World\n";]],
		[[fputs (hw, stdout);]])
])
CFLAGS="$CFLAGS_orig"
rm -f pac_test1.log
PAC_LINK_IFELSE_LOG([pac_test1.log], [], [
    CFLAGS="$CFLAGS_opt"
    rm -f pac_test2.log
    PAC_LINK_IFELSE_LOG([pac_test2.log], [], [
        PAC_RUNLOG_IFELSE([diff -b pac_test1.log pac_test2.log],
                          [pac_result=yes],[pac_result=no])
    ],[
        pac_result=no
    ])
], [
    pac_result=no
])
AC_MSG_RESULT([$pac_result])
dnl Delete the conftest created by AC_LANG_CONFTEST.
rm -f conftest.$ac_ext

if test "$pac_result" = "yes" ; then
    AC_MSG_CHECKING([whether routines compiled with $pac_opt can be linked with ones compiled without $pac_opt])
    pac_result=unknown
    CFLAGS="$CFLAGS_orig"
    rm -f pac_test3.log
    PAC_COMPILE_IFELSE_LOG([pac_test3.log], [
        AC_LANG_SOURCE([
            int foo(void);
            int foo(void){return 0;}
        ])
    ],[
        PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
        saved_LIBS="$LIBS"
        LIBS="pac_conftest.$OBJEXT $LIBS"

        rm -f pac_test4.log
        PAC_LINK_IFELSE_LOG([pac_test4.log], [AC_LANG_PROGRAM()], [
            CFLAGS="$CFLAGS_opt"
            rm -f pac_test5.log
            PAC_LINK_IFELSE_LOG([pac_test5.log], [AC_LANG_PROGRAM()], [
                PAC_RUNLOG_IFELSE([diff -b pac_test4.log pac_test5.log],
                                  [pac_result=yes], [pac_result=no])
            ],[
                pac_result=no
            ])
        ],[
            pac_result=no
        ])
        LIBS="$saved_LIBS"
        rm -f pac_conftest.$OBJEXT
    ],[
        pac_result=no
    ])
    AC_MSG_RESULT([$pac_result])
    rm -f pac_test3.log pac_test4.log pac_test5.log
fi
rm -f pac_test1.log pac_test2.log

dnl Restore CFLAGS before 2nd/3rd argument commands are executed,
dnl as 2nd/3rd argument command could be modifying CFLAGS.
CFLAGS="$CFLAGS_orig"
if test "$pac_result" = "yes" ; then
     ifelse([$2],[],[COPTIONS="$COPTIONS $1"],[$2])
else
     ifelse([$3],[],[:],[$3])
fi
AC_LANG_POP([C])
])
dnl
dnl/*D
dnl PAC_C_OPTIMIZATION - Determine C options for producing optimized code
dnl
dnl Synopsis
dnl PAC_C_OPTIMIZATION([action if found])
dnl
dnl Output Effect:
dnl Adds options to 'COPTIONS' if no other action is specified
dnl 
dnl Notes:
dnl This is a temporary standin for compiler optimization.
dnl It should try to match known systems to known compilers (checking, of
dnl course), and then falling back to some common defaults.
dnl Note that many compilers will complain about -g and aggressive
dnl optimization.  
dnl D*/
AC_DEFUN([PAC_C_OPTIMIZATION],[
    for copt in "-O4 -Ofast" "-Ofast" "-fast" "-O3" "-xO3" "-O" ; do
        PAC_C_CHECK_COMPILER_OPTION($copt,found_opt=yes,found_opt=no)
        if test "$found_opt" = "yes" ; then
	    ifelse($1,,COPTIONS="$COPTIONS $copt",$1)
	    break
        fi
    done
    if test "$ac_cv_prog_gcc" = "yes" ; then
	for copt in "-fomit-frame-pointer" "-finline-functions" \
		 "-funroll-loops" ; do
	    PAC_C_CHECK_COMPILER_OPTION($copt,found_opt=yes,found_opt=no)
	    if test "$found_opt" = "yes" ; then
	        ifelse($1,,COPTIONS="$COPTIONS $copt",$1)
	        # no break because we're trying to add them all
	    fi
	done
	# We could also look for architecture-specific gcc options
    fi

])

dnl/*D
dnl PAC_PROG_C_UNALIGNED_DOUBLES - Check that the C compiler allows unaligned
dnl doubles
dnl
dnl Synopsis:
dnl   PAC_PROG_C_UNALIGNED_DOUBLES(action-if-true,action-if-false,
dnl       action-if-unknown)
dnl
dnl Notes:
dnl 'action-if-unknown' is used in the case of cross-compilation.
dnl D*/
AC_DEFUN([PAC_PROG_C_UNALIGNED_DOUBLES],[
AC_CACHE_CHECK([whether C compiler allows unaligned doubles],
pac_cv_prog_c_unaligned_doubles,[
AC_TRY_RUN([
void fetch_double( v )
double *v;
{
*v = 1.0;
}
int main( argc, argv )
int argc;
char **argv;
{
int p[4];
double *p_val;
fetch_double( (double *)&(p[0]) );
p_val = (double *)&(p[0]);
if (*p_val != 1.0) return 1;
fetch_double( (double *)&(p[1]) );
p_val = (double *)&(p[1]);
if (*p_val != 1.0) return 1;
return 0;
}
],pac_cv_prog_c_unaligned_doubles="yes",pac_cv_prog_c_unaligned_doubles="no",
pac_cv_prog_c_unaligned_doubles="unknown")])
ifelse($1,,,if test "X$pac_cv_prog_c_unaligned_doubles" = "yes" ; then 
$1
fi)
ifelse($2,,,if test "X$pac_cv_prog_c_unaligned_doubles" = "no" ; then 
$2
fi)
ifelse($3,,,if test "X$pac_cv_prog_c_unaligned_doubles" = "unknown" ; then 
$3
fi)
])

dnl/*D 
dnl PAC_PROG_C_WEAK_SYMBOLS - Test whether C supports weak alias symbols.
dnl
dnl Synopsis
dnl PAC_PROG_C_WEAK_SYMBOLS(action-if-true,action-if-false)
dnl
dnl Output Effect:
dnl Defines one of the following if a weak symbol pragma is found:
dnl.vb
dnl    HAVE_PRAGMA_WEAK - #pragma weak
dnl    HAVE_PRAGMA_HP_SEC_DEF - #pragma _HP_SECONDARY_DEF
dnl    HAVE_PRAGMA_CRI_DUP  - #pragma _CRI duplicate x as y
dnl.ve
dnl May also define
dnl.vb
dnl    HAVE_WEAK_ATTRIBUTE
dnl.ve
dnl if functions can be declared as 'int foo(...) __attribute__ ((weak));'
dnl sets the shell variable pac_cv_attr_weak to yes.
dnl Also checks for __attribute__((weak_import)) which is supported by
dnl Apple in Mac OSX (at least in Darwin).  Note that this provides only
dnl weak symbols, not weak aliases
dnl 
dnl D*/
AC_DEFUN([PAC_PROG_C_WEAK_SYMBOLS],[
pragma_extra_message=""
AC_CACHE_CHECK([for type of weak symbol alias support],
pac_cv_prog_c_weak_symbols,[
# Test for weak symbol support...
# We can't put # in the message because it causes autoconf to generate
# incorrect code
AC_TRY_LINK([
extern int PFoo(int);
#pragma weak PFoo = Foo
int Foo(int a) { return a; }
],[return PFoo(1);],has_pragma_weak=yes)
#
# Some systems (Linux ia64 and ecc, for example), support weak symbols
# only within a single object file!  This tests that case.
# Note that there is an extern int PFoo declaration before the
# pragma.  Some compilers require this in order to make the weak symbol
# externally visible.  
if test "$has_pragma_weak" = yes ; then
    PAC_COMPLINK_IFELSE([
        AC_LANG_SOURCE([
extern int PFoo(int);
#pragma weak PFoo = Foo
int Foo(int);
int Foo(int a) { return a; }
        ])
    ],[
        AC_LANG_SOURCE([
extern int PFoo(int);
int main(int argc, char **argv) {
return PFoo(0);}
        ])
    ],[
        PAC_COMPLINK_IFELSE([
            AC_LANG_SOURCE([
extern int PFoo(int);
#pragma weak PFoo = Foo
int Foo(int);
int Foo(int a) { return a; }
            ])
        ],[
            AC_LANG_SOURCE([
extern int Foo(int);
int PFoo(int a) { return a+1;}
int main(int argc, char **argv) {
return Foo(0);}
            ])
        ],[
            pac_cv_prog_c_weak_symbols="pragma weak"
        ],[
            has_pragma_weak=0
            pragma_extra_message="pragma weak accepted but does not work (probably creates two non-weak entries)"
        ])
    ],[
        has_pragma_weak=0
        pragma_extra_message="pragma weak accepted but does not work (probably creates two non-weak entries)"
    ])
fi
dnl
if test -z "$pac_cv_prog_c_weak_symbols" ; then 
    AC_TRY_LINK([
extern int PFoo(int);
#pragma _HP_SECONDARY_DEF Foo  PFoo
int Foo(int a) { return a; }
],[return PFoo(1);],pac_cv_prog_c_weak_symbols="pragma _HP_SECONDARY_DEF")
fi
dnl
if test -z "$pac_cv_prog_c_weak_symbols" ; then
    AC_TRY_LINK([
extern int PFoo(int);
#pragma _CRI duplicate PFoo as Foo
int Foo(int a) { return a; }
],[return PFoo(1);],pac_cv_prog_c_weak_symbols="pragma _CRI duplicate x as y")
fi
dnl
if test -z "$pac_cv_prog_c_weak_symbols" ; then
    pac_cv_prog_c_weak_symbols="no"
fi
dnl
dnl If there is an extra explanatory message, echo it now so that it
dnl doesn't interfere with the cache result value
if test -n "$pragma_extra_message" ; then
    echo $pragma_extra_message
fi
dnl
])
if test "$pac_cv_prog_c_weak_symbols" != "no" ; then
    case "$pac_cv_prog_c_weak_symbols" in
        "pragma weak") AC_DEFINE(HAVE_PRAGMA_WEAK,1,[Supports weak pragma])
        ;;
        "pragma _HP")  AC_DEFINE(HAVE_PRAGMA_HP_SEC_DEF,1,[HP style weak pragma])
        ;;
        "pragma _CRI") AC_DEFINE(HAVE_PRAGMA_CRI_DUP,1,[Cray style weak pragma])
        ;;
    esac
fi
AC_CACHE_CHECK([whether __attribute__ ((weak)) allowed],
pac_cv_attr_weak,[
AC_TRY_COMPILE([int foo(int) __attribute__ ((weak));],[int a;],
pac_cv_attr_weak=yes,pac_cv_attr_weak=no)])
# Note that being able to compile with weak_import doesn't mean that
# it works.
AC_CACHE_CHECK([whether __attribute__ ((weak_import)) allowed],
pac_cv_attr_weak_import,[
AC_TRY_COMPILE([int foo(int) __attribute__ ((weak_import));],[int a;],
pac_cv_attr_weak_import=yes,pac_cv_attr_weak_import=no)])
# Check if the alias option for weak attributes is allowed
AC_CACHE_CHECK([whether __attribute__((weak,alias(...))) allowed],
pac_cv_attr_weak_alias,[
PAC_PUSH_FLAG([CFLAGS])
# force an error exit if the weak attribute isn't understood
CFLAGS=-Werror
AC_TRY_COMPILE([int __foo(int a){return 0;} int foo(int) __attribute__((weak,alias("__foo")));],[int a;],
pac_cv_attr_weak_alias=yes,pac_cv_attr_weak_alias=no)
# Restore original CFLAGS
PAC_POP_FLAG([CFLAGS])])
if test "$pac_cv_attr_weak_alias" = "yes" ; then
    AC_DEFINE(HAVE_WEAK_ATTRIBUTE,1,[Attribute style weak pragma])
fi
if test "$pac_cv_prog_c_weak_symbols" = "no" -a "$pac_cv_attr_weak_alias" = "no" ; then
    ifelse([$2],,:,[$2])
else
    ifelse([$1],,:,[$1])
fi
])

#
# This is a replacement that checks that FAILURES are signaled as well
# (later configure macros look for the .o file, not just success from the
# compiler, but they should not HAVE to
#
dnl --- insert 2.52 compatibility here ---
dnl 2.52 does not have AC_PROG_CC_WORKS
ifdef([AC_PROG_CC_WORKS],,[AC_DEFUN([AC_PROG_CC_WORKS],)])
dnl
AC_DEFUN([PAC_PROG_CC_WORKS],
[AC_PROG_CC_WORKS
AC_MSG_CHECKING([whether the C compiler sets its return status correctly])
AC_LANG_SAVE
AC_LANG_C
AC_TRY_COMPILE(,[int a = bzzzt;],notbroken=no,notbroken=yes)
AC_MSG_RESULT($notbroken)
if test "$notbroken" = "no" ; then
    AC_MSG_ERROR([installation or configuration problem: C compiler does not
correctly set error code when a fatal error occurs])
fi
])

dnl/*D 
dnl PAC_PROG_C_MULTIPLE_WEAK_SYMBOLS - Test whether C and the
dnl linker allow multiple weak symbols.
dnl
dnl Synopsis
dnl PAC_PROG_C_MULTIPLE_WEAK_SYMBOLS(action-if-true,action-if-false)
dnl
dnl 
dnl D*/
AC_DEFUN([PAC_PROG_C_MULTIPLE_WEAK_SYMBOLS],[
AC_CACHE_CHECK([for multiple weak symbol support],
pac_cv_prog_c_multiple_weak_symbols,[
# Test for multiple weak symbol support...
PAC_COMPLINK_IFELSE([
    AC_LANG_SOURCE([
extern int PFoo(int);
extern int PFoo_(int);
extern int pfoo_(int);
#pragma weak PFoo = Foo
#pragma weak PFoo_ = Foo
#pragma weak pfoo_ = Foo
int Foo(int);
int Foo(a) { return a; }
    ])
],[
    AC_LANG_SOURCE([
extern int PFoo(int), PFoo_(int), pfoo_(int);
int main() {
return PFoo(0) + PFoo_(1) + pfoo_(2);}
    ])
],[
    pac_cv_prog_c_multiple_weak_symbols="yes"
])
dnl
])
if test "$pac_cv_prog_c_multiple_weak_symbols" = "yes" ; then
    ifelse([$1],,:,[$1])
else
    ifelse([$2],,:,[$2])
fi
])

dnl Use the value of enable-strict to update CFLAGS
dnl pac_cc_strict_flags contains the strict flags.
dnl
dnl -std=c89 is used to select the C89 version of the ANSI/ISO C standard.
dnl As of this writing, many C compilers still accepted only this version,
dnl not the later C99 version. When all compilers accept C99, this 
dnl should be changed to the appropriate standard level.  Note that we've
dnl had trouble with gcc 2.95.3 accepting -std=c89 but then trying to 
dnl compile program with a invalid set of options 
dnl (-D __STRICT_ANSI__-trigraphs)
AC_DEFUN([PAC_CC_STRICT],[
export enable_strict_done
if test "$enable_strict_done" != "yes" ; then
    # make sure we don't add the below flags multiple times
    enable_strict_done=yes

    # Some comments on strict warning options.
    # These were added to improve portability
    #   -Wstack-usage=262144 -- 32 bit FreeBSD did not like the mprobe test
    #       allocating a big variable on the stack. (See tt#2160).  The "right"
    #       value requires further investigation; 1 MiB would have at least
    #       caught #2160 at compile-time, and only two other tests show a
    #       warning at 256k.
    #
    # These were added to reduce warnings:
    #   -Wno-missing-field-initializers  -- We want to allow a struct to be 
    #       initialized to zero using "struct x y = {0};" and not require 
    #       each field to be initialized individually.
    #   -Wno-unused-parameter -- For portability, some parameters go unused
    #	    when we have different implementations of functions for 
    #	    different platforms
    #   -Wno-unused-label -- We add fn_exit: and fn_fail: on all functions, 
    #	    but fn_fail may not be used if the function doesn't return an 
    #	    error.
    #   -Wno-sign-compare -- read() and write() return bytes read/written
    #       as a signed value, but we often compare this to size_t (or
    #	    msg_sz_t) variables.
    # These were removed to reduce warnings:
    #   -Wcast-qual -- Sometimes we need to cast "volatile char*" to 
    #	    "char*", e.g., for memcpy.
    #   -Wpadded -- We catch struct padding with asserts when we need to
    #   -Wredundant-decls -- Having redundant declarations is benign and the 
    #	    code already has some.
    #   -Waggregate-return -- This seems to be a performance-related warning
    #       aggregate return values are legal in ANSI C, but they may be returned
    #	    in memory rather than through a register.  We do use aggregate return
    #	    values, but they are structs of a single basic type (used to enforce
    #	    type checking for relative vs. absolute ptrs), and with optimization
    #	    the aggregate value is converted to a scalar.
    #   -Wdeclaration-after-statement -- This is a C89
    #       requirement. When compiling with C99, this should be
    #       disabled.
    #   -Wfloat-equal -- There are places in hwloc that set a float var to 0, then 
    #       compare it to 0 later to see if it was updated.  Also when using strtod()
    #       one needs to compare the return value with 0 to see whether a conversion
    #       was performed.
    #   -Werror-implicit-function-declaration -- implicit function declarations
    #       should never be tolerated.  This also ensures that we get quick
    #       compilation failures rather than later link failures that usually
    #       come from a function name typo.
    #   -Wcast-align -- Casting alignment warnings.  This is an
    #       important check, but is temporarily disabled, since it is
    #       throwing too many (correct) warnings currently, causing us
    #       to miss other warnings.
    #   -Wshorten-64-to-32 -- Bad type-casting warnings.  This is an
    #       important check, but is temporarily disabled, since it is
    #       throwing too many (correct) warnings currently, causing us
    #       to miss other warnings.
    #
    # This was removed because it masks important failures (see ticket #2094).
    # However, since Intel compiler currently does not include -Wtype-limits
    # in -Wextra, -Wtype-limits was added to handle warnings with the Intel
    # compiler.
    #   -Wno-type-limits -- There are places where we compare an unsigned to 
    #	    a constant that happens to be zero e.g., if x is unsigned and 
    #	    MIN_VAL is zero, we'd like to do "MPIR_Assert(x >= MIN_VAL);".
    #       Note this option is not supported by gcc 4.2.  This needs to be added 
    #	    after most other warning flags, so that we catch a gcc bug on 32-bit 
    #	    that doesn't give a warning that this is unsupported, unless another
    #	    warning is triggered, and then if gives an error.
    # the embedded newlines in this string are safe because we evaluate each
    # argument in the for-loop below and append them to the CFLAGS with a space
    # as the separator instead
    pac_common_strict_flags="
        -Wall
        -Wextra
        -Wno-missing-field-initializers
        -Wstrict-prototypes
        -Wmissing-prototypes
        -DGCC_WALL
        -Wno-unused-parameter
        -Wno-unused-label
        -Wshadow
        -Wmissing-declarations
        -Wno-long-long
        -Wundef
        -Wno-endif-labels
        -Wpointer-arith
        -Wbad-function-cast
        -Wwrite-strings
        -Wno-sign-compare
        -Wold-style-definition
        -Wno-multichar
        -Wno-deprecated-declarations
        -Wnested-externs
        -Winvalid-pch
        -Wno-pointer-sign
        -Wvariadic-macros
        -Wtype-limits
        -Werror-implicit-function-declaration
        -Wstack-usage=262144
        -diag-disable=all
    "

    if test -z "$1"; then
        flags=no
    else
        flags="`echo $1 | sed -e 's/:/ /g' -e 's/,/ /g'`"
    fi
    add_cflags=yes
    c_std=c99
    posix_std=2001
    enable_opt=yes
    pac_cc_strict_werror=no
    for flag in ${flags}; do
        case "$flag" in
             error)
                # note: we can't enable -Werror early as it will break many config tests
                #       Need apply to CFLAGS at the end of configure.
                pac_cc_strict_werror=yes
                ;;
	     stdc89)
	        c_std=c89
		;;
	     stdc99)
	        c_std=c99
		;;
	     stdgnu99)
	        c_std=gnu99
		;;
	     nostdc)
		c_std=none
		;;
	     posix1995)
		posix_std=1995
		;;
	     posix2001)
		posix_std=2001
		;;
	     posix2008)
		posix_std=2008
		;;
	     noposix)
		posix_std=none
		;;
	     opt)
		enable_opt=yes
		;;
	     noopt)
		enable_opt=no
		;;
	     all|yes)
		# leave the defaults
	        ;;
	     no|none)
		add_cflags=no
		;;
	     *)
		if test -n "$flag" ; then
		   AC_MSG_WARN([Unrecognized value for enable-strict:$flag])
		fi
		;;
	esac
    done

    pac_cc_strict_flags=""
    if test "${add_cflags}" = "yes" ; then
       # common flags
       pac_cc_strict_flags="$pac_cc_strict_flags $pac_common_strict_flags"

       # optimization flags
       if test "${enable_opt}" = "yes" ; then
	  PAC_APPEND_FLAG([-O2],[pac_cc_strict_flags])
       fi

       # stdc flags
       case "${c_std}" in
	    none)
		:
		;;
	    c89)
		PAC_APPEND_FLAG([-std=c89],[pac_cc_strict_flags])
		PAC_APPEND_FLAG([-Wdeclaration-after-statement],[pac_cc_strict_flags])
		;;
	    c99)
		PAC_APPEND_FLAG([-std=c99],[pac_cc_strict_flags])
		# Use -D_STDC_C99= for Solaris compilers. See
		# http://lists.gnu.org/archive/html/autoconf/2010-12/msg00059.html
		# for discussion on why not to use -xc99
		PAC_APPEND_FLAG([-D_STDC_C99=],[pac_cc_strict_flags])
		;;
	    gnu99)
		PAC_APPEND_FLAG([-std=gnu99],[pac_cc_strict_flags])
		;;
	    *)
		AC_MSG_ERROR([internal error, unexpected C std version: '$c_std'])
		;;
       esac

       # posix flags
       case "${posix_std}" in
            none) : ;;
            1995) PAC_APPEND_FLAG([-D_POSIX_C_SOURCE=199506L],[pac_cc_strict_flags]) ;;
            2001) PAC_APPEND_FLAG([-D_POSIX_C_SOURCE=200112L],[pac_cc_strict_flags]) ;;
            2008) PAC_APPEND_FLAG([-D_POSIX_C_SOURCE=200809L],[pac_cc_strict_flags]) ;;
            *)    AC_MSG_ERROR([internal error, unexpected POSIX version: '$posix_std']) ;;
       esac
       if test "${posix_std}" != "none" ; then
           AS_CASE([$host],[*-*-darwin*], [PAC_APPEND_FLAG([-D_DARWIN_C_SOURCE],[pac_cc_strict_flags])])
       fi
    fi

    # See if the above options work with the compiler
    accepted_flags=""
    for flag in $pac_cc_strict_flags ; do
        PAC_PUSH_FLAG([CFLAGS])
	CFLAGS="$CFLAGS $accepted_flags"
        PAC_C_CHECK_COMPILER_OPTION([$flag],[accepted_flags="$accepted_flags $flag"],)
        PAC_POP_FLAG([CFLAGS])
    done
    pac_cc_strict_flags=$accepted_flags
fi
])

dnl/*D
dnl PAC_ARG_STRICT - Add --enable-strict to configure.  
dnl
dnl Synopsis:
dnl PAC_ARG_STRICT
dnl 
dnl Output effects:
dnl Adds '--enable-strict' to the command line.
dnl
dnl D*/
AC_DEFUN([PAC_ARG_STRICT],[
AC_ARG_ENABLE(strict,
	AC_HELP_STRING([--enable-strict], [Turn on strict compilation testing]),,enable_strict=no)
PAC_CC_STRICT($enable_strict)
CFLAGS="$CFLAGS $pac_cc_strict_flags"
export CFLAGS
])

dnl Return the integer structure alignment in pac_cv_c_max_integer_align
dnl Possible values include
dnl	packed
dnl	two
dnl	four
dnl	eight
dnl
dnl In addition, a "Could not determine alignment" and a "error!"
dnl return is possible.  
AC_DEFUN([PAC_C_MAX_INTEGER_ALIGN],[
AC_CACHE_CHECK([for max C struct integer alignment],
pac_cv_c_max_integer_align,[
AC_TRY_RUN([
#include <stdio.h>
#define DBG(a,b,c)
int main( int argc, char *argv[] )
{
    FILE *cf;
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;
    struct { char a; int b; } char_int;
    struct { char a; short b; } char_short;
    struct { char a; long b; } char_long;
    struct { char a; int b; char c; } char_int_char;
    struct { char a; short b; char c; } char_short_char;
#ifdef HAVE_LONG_LONG_INT
    struct { long long int a; char b; } lli_c;
    struct { char a; long long int b; } c_lli;
#endif
    int size, extent, extent2;

    /* assume max integer alignment isn't 8 if we don't have
     * an eight-byte value :)
     */
#ifdef HAVE_LONG_LONG_INT
    if (sizeof(int) < 8 && sizeof(long) < 8 && sizeof(long long int) < 8)
	is_eight = 0;
#else
    if (sizeof(int) < 8 && sizeof(long) < 8) is_eight = 0;
#endif

    size = sizeof(char) + sizeof(int);
    extent = sizeof(char_int);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0) is_four = 0;
    if (sizeof(int) == 8 && (extent % 8) != 0) is_eight = 0;
    DBG("char_int",size,extent);

    size = sizeof(char) + sizeof(short);
    extent = sizeof(char_short);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if (sizeof(short) == 4 && (extent % 4) != 0) is_four = 0;
    if (sizeof(short) == 8 && (extent % 8) != 0) is_eight = 0;
    DBG("char_short",size,extent);

    size = sizeof(char) + sizeof(long);
    extent = sizeof(char_long);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0) is_four = 0;
    if (sizeof(long) == 8 && (extent % 8) != 0) is_eight = 0;
    DBG("char_long",size,extent);

#ifdef HAVE_LONG_LONG_INT
    size = sizeof(char) + sizeof(long long int);
    extent = sizeof(lli_c);
    extent2 = sizeof(c_lli);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(long long int) >= 8 && (extent % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
#endif

    size = sizeof(char) + sizeof(int) + sizeof(char);
    extent = sizeof(char_int_char);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if ( (extent % 4) != 0) is_four = 0;
    if (sizeof(int) == 8 && (extent % 8) != 0) is_eight = 0;
    DBG("char_int_char",size,extent);

    size = sizeof(char) + sizeof(short) + sizeof(char);
    extent = sizeof(char_short_char);
    if (size != extent) is_packed = 0;
    if ( (extent % 2) != 0) is_two = 0;
    if (sizeof(short) == 4 && (extent % 4) != 0) is_four = 0;
    if (sizeof(short) == 8 && (extent % 8) != 0) is_eight = 0;
    DBG("char_short_char",size,extent);

    /* If aligned mod 8, it will be aligned mod 4 */
    if (is_eight) { is_four = 0; is_two = 0; }

    if (is_four) is_two = 0;

    /* Tabulate the results */
    cf = fopen( "ctest.out", "w" );
    if (is_packed + is_two + is_four + is_eight == 0) {
	fprintf( cf, "Could not determine alignment\n" );
    }
    else {
	if (is_packed + is_two + is_four + is_eight != 1) {
	    fprintf( cf, "error!\n" );
	}
	else {
	    if (is_packed) fprintf( cf, "packed\n" );
	    if (is_two) fprintf( cf, "two\n" );
	    if (is_four) fprintf( cf, "four\n" );
	    if (is_eight) fprintf( cf, "eight\n" );
	}
    }
    fclose( cf );
    return 0;
}],
pac_cv_c_max_integer_align=`cat ctest.out`,
pac_cv_c_max_integer_align="unknown",
pac_cv_c_max_integer_align="$CROSS_ALIGN_STRUCT_INT")
rm -f ctest.out
])
if test -z "$pac_cv_c_max_integer_align" ; then
    pac_cv_c_max_integer_align="unknown"
fi
])

dnl Return the floating point structure alignment in
dnl pac_cv_c_max_fp_align.
dnl
dnl Possible values include:
dnl	packed
dnl	two
dnl	four
dnl	eight
dnl     sixteen
dnl
dnl In addition, a "Could not determine alignment" and a "error!"
dnl return is possible.  
AC_DEFUN([PAC_C_MAX_FP_ALIGN],[
AC_CACHE_CHECK([for max C struct floating point alignment],
pac_cv_c_max_fp_align,[
AC_TRY_RUN([
#include <stdio.h>
#define DBG(a,b,c)
int main( int argc, char *argv[] )
{
    FILE *cf;
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;
    int is_sixteen = 1;
    struct { char a; float b; } char_float;
    struct { float b; char a; } float_char;
    struct { char a; double b; } char_double;
    struct { double b; char a; } double_char;
#ifdef HAVE_LONG_DOUBLE
    struct { char a; long double b; } char_long_double;
    struct { long double b; char a; } long_double_char;
    struct { long double a; int b; char c; } long_double_int_char;
#endif
    int size, extent1, extent2;

    size = sizeof(char) + sizeof(float);
    extent1 = sizeof(char_float);
    extent2 = sizeof(float_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(float) == 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
    DBG("char_float",size,extent1);

    size = sizeof(char) + sizeof(double);
    extent1 = sizeof(char_double);
    extent2 = sizeof(double_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(double) == 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
    DBG("char_double",size,extent1);

#ifdef HAVE_LONG_DOUBLE
    size = sizeof(char) + sizeof(long double);
    extent1 = sizeof(char_long_double);
    extent2 = sizeof(long_double_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(long double) >= 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
    if (sizeof(long double) > 8 && (extent1 % 16) != 0
	&& (extent2 % 16) != 0) is_sixteen = 0;
    DBG("char_long-double",size,extent1);

    extent1 = sizeof(long_double_int_char);
    if ( (extent1 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0) is_four = 0;
    if (sizeof(long double) >= 8 && (extent1 % 8) != 0)	is_eight = 0;
    if (sizeof(long double) > 8 && (extent1 % 16) != 0) is_sixteen = 0;
#else
    is_sixteen = 0;
#endif

    if (is_sixteen) { is_eight = 0; is_four = 0; is_two = 0; }

    if (is_eight) { is_four = 0; is_two = 0; }

    if (is_four) is_two = 0;

    /* Tabulate the results */
    cf = fopen( "ctest.out", "w" );
    if (is_packed + is_two + is_four + is_eight + is_sixteen == 0) {
	fprintf( cf, "Could not determine alignment\n" );
    }
    else {
	if (is_packed + is_two + is_four + is_eight + is_sixteen != 1) {
	    fprintf( cf, "error!\n" );
	}
	else {
	    if (is_packed) fprintf( cf, "packed\n" );
	    if (is_two) fprintf( cf, "two\n" );
	    if (is_four) fprintf( cf, "four\n" );
	    if (is_eight) fprintf( cf, "eight\n" );
	    if (is_sixteen) fprintf( cf, "sixteen\n" );
	}
    }
    fclose( cf );
    return 0;
}],
pac_cv_c_max_fp_align=`cat ctest.out`,
pac_cv_c_max_fp_align="unknown",
pac_cv_c_max_fp_align="$CROSS_ALIGN_STRUCT_FP")
rm -f ctest.out
])
if test -z "$pac_cv_c_max_fp_align" ; then
    pac_cv_c_max_fp_align="unknown"
fi
])

dnl Return the floating point structure alignment in
dnl pac_cv_c_max_double_fp_align.
dnl
dnl Possible values include:
dnl	packed
dnl	two
dnl	four
dnl	eight
dnl
dnl In addition, a "Could not determine alignment" and a "error!"
dnl return is possible.  
AC_DEFUN([PAC_C_MAX_DOUBLE_FP_ALIGN],[
AC_CACHE_CHECK([for max C struct alignment of structs with doubles],
pac_cv_c_max_double_fp_align,[
AC_TRY_RUN([
#include <stdio.h>
#define DBG(a,b,c)
int main( int argc, char *argv[] )
{
    FILE *cf;
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;
    struct { char a; float b; } char_float;
    struct { float b; char a; } float_char;
    struct { char a; double b; } char_double;
    struct { double b; char a; } double_char;
    int size, extent1, extent2;

    size = sizeof(char) + sizeof(float);
    extent1 = sizeof(char_float);
    extent2 = sizeof(float_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(float) == 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
    DBG("char_float",size,extent1);

    size = sizeof(char) + sizeof(double);
    extent1 = sizeof(char_double);
    extent2 = sizeof(double_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(double) == 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
    DBG("char_double",size,extent1);

    if (is_eight) { is_four = 0; is_two = 0; }

    if (is_four) is_two = 0;

    /* Tabulate the results */
    cf = fopen( "ctest.out", "w" );
    if (is_packed + is_two + is_four + is_eight == 0) {
	fprintf( cf, "Could not determine alignment\n" );
    }
    else {
	if (is_packed + is_two + is_four + is_eight != 1) {
	    fprintf( cf, "error!\n" );
	}
	else {
	    if (is_packed) fprintf( cf, "packed\n" );
	    if (is_two) fprintf( cf, "two\n" );
	    if (is_four) fprintf( cf, "four\n" );
	    if (is_eight) fprintf( cf, "eight\n" );
	}
    }
    fclose( cf );
    return 0;
}],
pac_cv_c_max_double_fp_align=`cat ctest.out`,
pac_cv_c_max_double_fp_align="unknown",
pac_cv_c_max_double_fp_align="$CROSS_ALIGN_STRUCT_DOUBLE_FP")
rm -f ctest.out
])
if test -z "$pac_cv_c_max_double_fp_align" ; then
    pac_cv_c_max_double_fp_align="unknown"
fi
])
AC_DEFUN([PAC_C_MAX_LONGDOUBLE_FP_ALIGN],[
AC_CACHE_CHECK([for max C struct floating point alignment with long doubles],
pac_cv_c_max_longdouble_fp_align,[
AC_TRY_RUN([
#include <stdio.h>
#define DBG(a,b,c)
int main( int argc, char *argv[] )
{
    FILE *cf;
    int is_packed  = 1;
    int is_two     = 1;
    int is_four    = 1;
    int is_eight   = 1;
    int is_sixteen = 1;
    struct { char a; long double b; } char_long_double;
    struct { long double b; char a; } long_double_char;
    struct { long double a; int b; char c; } long_double_int_char;
    int size, extent1, extent2;

    size = sizeof(char) + sizeof(long double);
    extent1 = sizeof(char_long_double);
    extent2 = sizeof(long_double_char);
    if (size != extent1) is_packed = 0;
    if ( (extent1 % 2) != 0 && (extent2 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0 && (extent2 % 4) != 0) is_four = 0;
    if (sizeof(long double) >= 8 && (extent1 % 8) != 0 && (extent2 % 8) != 0)
	is_eight = 0;
    if (sizeof(long double) > 8 && (extent1 % 16) != 0
	&& (extent2 % 16) != 0) is_sixteen = 0;
    DBG("char_long-double",size,extent1);

    extent1 = sizeof(long_double_int_char);
    if ( (extent1 % 2) != 0) is_two = 0;
    if ( (extent1 % 4) != 0) is_four = 0;
    if (sizeof(long double) >= 8 && (extent1 % 8) != 0)	is_eight = 0;
    if (sizeof(long double) > 8 && (extent1 % 16) != 0) is_sixteen = 0;

    if (is_sixteen) { is_eight = 0; is_four = 0; is_two = 0; }

    if (is_eight) { is_four = 0; is_two = 0; }

    if (is_four) is_two = 0;

    /* Tabulate the results */
    cf = fopen( "ctest.out", "w" );
    if (is_packed + is_two + is_four + is_eight + is_sixteen == 0) {
	fprintf( cf, "Could not determine alignment\n" );
    }
    else {
	if (is_packed + is_two + is_four + is_eight + is_sixteen != 1) {
	    fprintf( cf, "error!\n" );
	}
	else {
	    if (is_packed) fprintf( cf, "packed\n" );
	    if (is_two) fprintf( cf, "two\n" );
	    if (is_four) fprintf( cf, "four\n" );
	    if (is_eight) fprintf( cf, "eight\n" );
	    if (is_sixteen) fprintf( cf, "sixteen\n" );
	}
    }
    fclose( cf );
    return 0;
}],
pac_cv_c_max_longdouble_fp_align=`cat ctest.out`,
pac_cv_c_max_longdouble_fp_align="unknown",
pac_cv_c_max_longdouble_fp_align="$CROSS_ALIGN_STRUCT_LONGDOUBLE_FP")
rm -f ctest.out
])
if test -z "$pac_cv_c_max_longdouble_fp_align" ; then
    pac_cv_c_max_longdouble_fp_align="unknown"
fi
])

dnl Other tests assume that there is potentially a maximum alignment
dnl and that if there is no maximum alignment, or a type is smaller than
dnl that value, then we align on the size of the value, with the exception
dnl of the "position-based alignment" rules we test for separately.
dnl
dnl It turns out that these assumptions have fallen short in at least one
dnl case, on MacBook Pros, where doubles are aligned on 4-byte boundaries
dnl even when long doubles are aligned on 16-byte boundaries. So this test
dnl is here specifically to handle this case.
dnl
dnl Puts result in pac_cv_c_double_alignment_exception.
dnl
dnl Possible values currently include no and four.
dnl
AC_DEFUN([PAC_C_DOUBLE_ALIGNMENT_EXCEPTION],[
AC_CACHE_CHECK([if double alignment breaks rules, find actual alignment],
pac_cv_c_double_alignment_exception,[
AC_TRY_RUN([
#include <stdio.h>
#define DBG(a,b,c)
int main( int argc, char *argv[] )
{
    FILE *cf;
    struct { char a; double b; } char_double;
    struct { double b; char a; } double_char;
    int extent1, extent2, align_4 = 0;

    extent1 = sizeof(char_double);
    extent2 = sizeof(double_char);

    /* we're interested in the largest value, will let separate test
     * deal with position-based issues.
     */
    if (extent1 < extent2) extent1 = extent2;
    if ((sizeof(double) == 8) && (extent1 % 8) != 0) {
       if (extent1 % 4 == 0) {
          align_4 = 1;
       }
    }

    cf = fopen( "ctest.out", "w" );

    if (align_4) fprintf( cf, "four\n" );
    else fprintf( cf, "no\n" );

    fclose( cf );
    return 0;
}],
pac_cv_c_double_alignment_exception=`cat ctest.out`,
pac_cv_c_double_alignment_exception="unknown",
pac_cv_c_double_alignment_exception="$CROSS_ALIGN_DOUBLE_EXCEPTION")
rm -f ctest.out
])
if test -z "$pac_cv_c_double_alignment_exception" ; then
    pac_cv_c_double_alignment_exception="unknown"
fi
])

dnl Test for odd struct alignment rule that only applies max.
dnl padding when double value is at front of type.
dnl Puts result in pac_cv_c_double_pos_align.
dnl
dnl Search for "Power alignment mode" for more details.
dnl
dnl Possible values include yes, no, and unknown.
dnl
AC_DEFUN([PAC_C_DOUBLE_POS_ALIGN],[
AC_CACHE_CHECK([if alignment of structs with doubles is based on position],
pac_cv_c_double_pos_align,[
AC_TRY_RUN([
#include <stdio.h>
#define DBG(a,b,c)
int main( int argc, char *argv[] )
{
    FILE *cf;
    int padding_varies_by_pos = 0;
    struct { char a; double b; } char_double;
    struct { double b; char a; } double_char;
    int extent1, extent2;

    extent1 = sizeof(char_double);
    extent2 = sizeof(double_char);
    if (extent1 != extent2) padding_varies_by_pos = 1;

    cf = fopen( "ctest.out", "w" );
    if (padding_varies_by_pos) fprintf( cf, "yes\n" );
    else fprintf( cf, "no\n" );

    fclose( cf );
    return 0;
}],
pac_cv_c_double_pos_align=`cat ctest.out`,
pac_cv_c_double_pos_align="unknown",
pac_cv_c_double_pos_align="$CROSS_ALIGN_DOUBLE_POS")
rm -f ctest.out
])
if test -z "$pac_cv_c_double_pos_align" ; then
    pac_cv_c_double_pos_align="unknown"
fi
])

dnl Test for odd struct alignment rule that only applies max.
dnl padding when long long int value is at front of type.
dnl Puts result in pac_cv_c_llint_pos_align.
dnl
dnl Search for "Power alignment mode" for more details.
dnl
dnl Possible values include yes, no, and unknown.
dnl
AC_DEFUN([PAC_C_LLINT_POS_ALIGN],[
AC_CACHE_CHECK([if alignment of structs with long long ints is based on position],
pac_cv_c_llint_pos_align,[
AC_TRY_RUN([
#include <stdio.h>
#define DBG(a,b,c)
int main( int argc, char *argv[] )
{
    FILE *cf;
    int padding_varies_by_pos = 0;
#ifdef HAVE_LONG_LONG_INT
    struct { char a; long long int b; } char_llint;
    struct { long long int b; char a; } llint_char;
    int extent1, extent2;

    extent1 = sizeof(char_llint);
    extent2 = sizeof(llint_char);
    if (extent1 != extent2) padding_varies_by_pos = 1;
#endif

    cf = fopen( "ctest.out", "w" );
    if (padding_varies_by_pos) fprintf( cf, "yes\n" );
    else fprintf( cf, "no\n" );

    fclose( cf );
    return 0;
}],
pac_cv_c_llint_pos_align=`cat ctest.out`,
pac_cv_c_llint_pos_align="unknown",
pac_cv_c_llint_pos_align="$CROSS_ALIGN_LLINT_POS")
rm -f ctest.out
])
if test -z "$pac_cv_c_llint_pos_align" ; then
    pac_cv_c_llint_pos_align="unknown"
fi
])

dnl/*D
dnl PAC_FUNC_NEEDS_DECL - Set NEEDS_<funcname>_DECL if a declaration is needed
dnl
dnl Synopsis:
dnl PAC_FUNC_NEEDS_DECL(headerfiles,funcname)
dnl
dnl Output Effect:
dnl Sets 'NEEDS_<funcname>_DECL' if 'funcname' is not declared by the 
dnl headerfiles.
dnl
dnl Approach:
dnl Attempt to assign library function to function pointer.  If the function
dnl is not declared in a header, this will fail.  Use a non-static global so
dnl the compiler does not warn about an unused variable.
dnl
dnl Simply calling the function is not enough because C89 compilers allow
dnl calls to implicitly-defined functions.  Re-declaring a library function
dnl with an incompatible prototype is also not sufficient because some
dnl compilers (notably clang-3.2) only produce a warning in this case.
dnl
dnl D*/
AC_DEFUN([PAC_FUNC_NEEDS_DECL],[
AC_CACHE_CHECK([whether $2 needs a declaration],
pac_cv_func_decl_$2,[
AC_TRY_COMPILE([$1
void (*fptr)(void) = (void(*)(void))$2;],[],
pac_cv_func_decl_$2=no,pac_cv_func_decl_$2=yes)])
if test "$pac_cv_func_decl_$2" = "yes" ; then
changequote(<<,>>)dnl
define(<<PAC_FUNC_NAME>>, translit(NEEDS_$2_DECL, [a-z *], [A-Z__]))dnl
changequote([, ])dnl
    AC_DEFINE_UNQUOTED(PAC_FUNC_NAME,1,[Define if $2 needs a declaration])
undefine([PAC_FUNC_NAME])
fi
])

dnl PAC_C_GNU_ATTRIBUTE - See if the GCC __attribute__ specifier is allow.
dnl Use the following
dnl #ifndef HAVE_GCC_ATTRIBUTE
dnl #define __attribute__(a)
dnl #endif
dnl If *not*, define __attribute__(a) as null
dnl
dnl We start by requiring Gcc.  Some other compilers accept __attribute__
dnl but generate warning messages, or have different interpretations 
dnl (which seems to make __attribute__ just as bad as #pragma) 
dnl For example, the Intel icc compiler accepts __attribute__ and
dnl __attribute__((pure)) but generates warnings for __attribute__((format...))
dnl
AC_DEFUN([PAC_C_GNU_ATTRIBUTE],[
AC_REQUIRE([AC_PROG_CC_GNU])
if test "$ac_cv_prog_gcc" = "yes" ; then
    AC_CACHE_CHECK([whether __attribute__ allowed],
pac_cv_gnu_attr_pure,[
AC_TRY_COMPILE([int foo(int) __attribute__ ((pure));],[int a;],
pac_cv_gnu_attr_pure=yes,pac_cv_gnu_attr_pure=no)])
AC_CACHE_CHECK([whether __attribute__((format)) allowed],
pac_cv_gnu_attr_format,[
AC_TRY_COMPILE([int foo(char *,...) __attribute__ ((format(printf,1,2)));],[int a;],
pac_cv_gnu_attr_format=yes,pac_cv_gnu_attr_format=no)])
    if test "$pac_cv_gnu_attr_pure" = "yes" -a "$pac_cv_gnu_attr_format" = "yes" ; then
        AC_DEFINE(HAVE_GCC_ATTRIBUTE,1,[Define if GNU __attribute__ is supported])
    fi
fi
])

#
# determine if the compiler defines a symbol containing the function name
#
# These tests check not only that the compiler defines some symbol, such
# as __FUNCTION__, but that the symbol correctly names the function.
#
# Defines 
#   HAVE__FUNC__      (if __func__ defined)
#   HAVE_CAP__FUNC__  (if __FUNC__ defined)
#   HAVE__FUNCTION__  (if __FUNCTION__ defined)
#
AC_DEFUN([PAC_CC_FUNCTION_NAME_SYMBOL],[
AC_CACHE_CHECK([whether the compiler defines __func__],
pac_cv_have__func__,[
tmp_am_cross=no
AC_RUN_IFELSE([
AC_LANG_SOURCE([
#include <string.h>
int foo(void);
int foo(void)
{
    return (strcmp(__func__, "foo") == 0);
}
int main(int argc, char ** argv)
{
    return (foo() ? 0 : 1);
}
])
], pac_cv_have__func__=yes, pac_cv_have__func__=no,tmp_am_cross=yes)
if test "$tmp_am_cross" = yes ; then
    AC_LINK_IFELSE([
    AC_LANG_SOURCE([
#include <string.h>
int foo(void);
int foo(void)
{
    return (strcmp(__func__, "foo") == 0);
}
int main(int argc, char ** argv)
{
    return (foo() ? 0 : 1);
}
    ])
], pac_cv_have__func__=yes, pac_cv_have__func__=no)
fi
])

if test "$pac_cv_have__func__" = "yes" ; then
    AC_DEFINE(HAVE__FUNC__,,[define if the compiler defines __func__])
fi

AC_CACHE_CHECK([whether the compiler defines __FUNC__],
pac_cv_have_cap__func__,[
tmp_am_cross=no
AC_RUN_IFELSE([
AC_LANG_SOURCE([
#include <string.h>
int foo(void);
int foo(void)
{
    return (strcmp(__FUNC__, "foo") == 0);
}
int main(int argc, char ** argv)
{
    return (foo() ? 0 : 1);
}
])
], pac_cv_have_cap__func__=yes, pac_cv_have_cap__func__=no,tmp_am_cross=yes)
if test "$tmp_am_cross" = yes ; then
    AC_LINK_IFELSE([
    AC_LANG_SOURCE([
#include <string.h>
int foo(void);
int foo(void)
{
    return (strcmp(__FUNC__, "foo") == 0);
}
int main(int argc, char ** argv)
{
    return (foo() ? 0 : 1);
}
    ])
], pac_cv_have__func__=yes, pac_cv_have__func__=no)
fi
])

if test "$pac_cv_have_cap__func__" = "yes" ; then
    AC_DEFINE(HAVE_CAP__FUNC__,,[define if the compiler defines __FUNC__])
fi

AC_CACHE_CHECK([whether the compiler sets __FUNCTION__],
pac_cv_have__function__,[
tmp_am_cross=no
AC_RUN_IFELSE([
AC_LANG_SOURCE([
#include <string.h>
int foo(void);
int foo(void)
{
    return (strcmp(__FUNCTION__, "foo") == 0);
}
int main(int argc, char ** argv)
{
    return (foo() ? 0 : 1);
}
])
], pac_cv_have__function__=yes, pac_cv_have__function__=no,tmp_am_cross=yes)
if test "$tmp_am_cross" = yes ; then
    AC_LINK_IFELSE([
    AC_LANG_SOURCE([
#include <string.h>
int foo(void);
int foo(void)
{
    return (strcmp(__FUNCTION__, "foo") == 0);
}
int main(int argc, char ** argv)
{
    return (foo() ? 0 : 1);
}
    ])
], pac_cv_have__func__=yes, pac_cv_have__func__=no)
fi
])

if test "$pac_cv_have__function__" = "yes" ; then
    AC_DEFINE(HAVE__FUNCTION__,,[define if the compiler defines __FUNCTION__])
fi

])


dnl Check structure alignment
AC_DEFUN([PAC_STRUCT_ALIGNMENT],[
	# Initialize alignment checks
	is_packed=1
	is_two=1
	is_four=1
	is_eight=1
	is_largest=1

	# See if long double exists
	AC_TRY_COMPILE(,[long double a;],have_long_double=yes,have_long_double=no)

	# Get sizes of regular types
	AC_CHECK_SIZEOF(char)
	AC_CHECK_SIZEOF(int)
	AC_CHECK_SIZEOF(short)
	AC_CHECK_SIZEOF(long)
	AC_CHECK_SIZEOF(float)
	AC_CHECK_SIZEOF(double)
	AC_CHECK_SIZEOF(long double)

	# char_int comparison
	AC_CHECK_SIZEOF(char_int, 0, [typedef struct { char a; int b; } char_int; ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_int`
	extent=$ac_cv_sizeof_char_int
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_int`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "`expr $extent % 4`" != "0" ; then is_four=0 ; fi
	if test "$ac_cv_sizeof_int" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi

	# char_short comparison
	AC_CHECK_SIZEOF(char_short, 0, [typedef struct { char a; short b; } char_short; ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_short`
	extent=$ac_cv_sizeof_char_short
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_short`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "$ac_cv_sizeof_short" = "4" -a "`expr $extent % 4`" != "0" ; then
	   is_four=0
	fi
	if test "$ac_cv_sizeof_short" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi

	# char_long comparison
	AC_CHECK_SIZEOF(char_long, 0, [typedef struct { char a; long b; } char_long; ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_long`
	extent=$ac_cv_sizeof_char_long
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_long`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "`expr $extent % 4`" != "0" ; then is_four=0 ; fi
	if test "$ac_cv_sizeof_long" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi

	# char_float comparison
	AC_CHECK_SIZEOF(char_float, 0, [typedef struct { char a; float b; } char_float; ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_float`
	extent=$ac_cv_sizeof_char_float
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_float`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "`expr $extent % 4`" != "0" ; then is_four=0 ; fi
	if test "$ac_cv_sizeof_float" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi

	# char_double comparison
	AC_CHECK_SIZEOF(char_double, 0, [typedef struct { char a; double b; } char_double; ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_double`
	extent=$ac_cv_sizeof_char_double
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_double`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "`expr $extent % 4`" != "0" ; then is_four=0 ; fi
	if test "$ac_cv_sizeof_double" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi

	# char_long_double comparison
	if test "$have_long_double" = "yes"; then
	AC_CHECK_SIZEOF(char_long_double, 0, [
				       typedef struct {
				       	       char a;
					       long double b;
				       } char_long_double;
				       ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_long_double`
	extent=$ac_cv_sizeof_char_long_double
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_long_double`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "`expr $extent % 4`" != "0" ; then is_four=0 ; fi
	if test "$ac_cv_sizeof_long_double" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi
	fi

	# char_int_char comparison
	AC_CHECK_SIZEOF(char_int_char, 0, [
				       typedef struct {
				       	       char a;
					       int b;
					       char c;
				       } char_int_char;
				       ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_int + $ac_cv_sizeof_char`
	extent=$ac_cv_sizeof_char_int_char
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_int`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "`expr $extent % 4`" != "0" ; then is_four=0 ; fi
	if test "$ac_cv_sizeof_int" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi

	# char_short_char comparison
	AC_CHECK_SIZEOF(char_short_char, 0, [
				       typedef struct {
				       	       char a;
					       short b;
					       char c;
				       } char_short_char;
				       ])
	size=`expr $ac_cv_sizeof_char + $ac_cv_sizeof_short + $ac_cv_sizeof_char`
	extent=$ac_cv_sizeof_char_short_char
	if test "$size" != "$extent" ; then is_packed=0 ; fi
	if test "`expr $extent % $ac_cv_sizeof_short`" != "0" ; then is_largest=0 ; fi
	if test "`expr $extent % 2`" != "0" ; then is_two=0 ; fi
	if test "$ac_cv_sizeof_short" = "4" -a "`expr $extent % 4`" != "0" ; then
	   is_four=0
	fi
	if test "$ac_cv_sizeof_short" = "8" -a "`expr $extent % 8`" != "0" ; then
	   is_eight=0
	fi

	# If aligned mod 8, it will be aligned mod 4
	if test $is_eight = 1 ; then is_four=0 ; is_two=0 ; fi
	if test $is_four = 1 ; then is_two=0 ; fi

	# Largest supersedes 8
	if test $is_largest = 1 ; then is_eight=0 ; fi

	# Find the alignment
	if test "`expr $is_packed + $is_largest + $is_two + $is_four + $is_eight`" = "0" ; then
	   pac_cv_struct_alignment="unknown"
	elif test "`expr $is_packed + $is_largest + $is_two + $is_four + $is_eight`" != "1" ; then
	   pac_cv_struct_alignment="unknown"
	elif test $is_packed = 1 ; then
	   pac_cv_struct_alignment="packed"
	elif test $is_largest = 1 ; then
	   pac_cv_struct_alignment="largest"
	elif test $is_two = 1 ; then
	   pac_cv_struct_alignment="two"
	elif test $is_four = 1 ; then
	   pac_cv_struct_alignment="four"
	elif test $is_eight = 1 ; then
	   pac_cv_struct_alignment="eight"
	fi
])
dnl
dnl PAC_C_MACRO_VA_ARGS
dnl
dnl will AC_DEFINE([HAVE_MACRO_VA_ARGS]) if the compiler supports C99 variable
dnl length argument lists in macros (#define foo(...) bar(__VA_ARGS__))
AC_DEFUN([PAC_C_MACRO_VA_ARGS],[
    AC_MSG_CHECKING([for variable argument list macro functionality])
    AC_LINK_IFELSE([AC_LANG_PROGRAM([
        #include <stdio.h>
        #define conftest_va_arg_macro(...) printf(__VA_ARGS__)
    ],
    [conftest_va_arg_macro("a test %d", 3);])],
    [AC_DEFINE([HAVE_MACRO_VA_ARGS],[1],[Define if C99-style variable argument list macro functionality])
     AC_MSG_RESULT([yes])],
    [AC_MSG_RESULT([no])])
])dnl

# Will AC_DEFINE([HAVE_BUILTIN_EXPECT]) if the compiler supports __builtin_expect.
AC_DEFUN([PAC_C_BUILTIN_EXPECT],[
AC_MSG_CHECKING([if C compiler supports __builtin_expect])

AC_TRY_LINK(, [
    return __builtin_expect(1, 1) ? 1 : 0
], [
    have_builtin_expect=yes
    AC_MSG_RESULT([yes])
], [
    have_builtin_expect=no
    AC_MSG_RESULT([no])
])
if test x$have_builtin_expect = xyes ; then
    AC_DEFINE([HAVE_BUILTIN_EXPECT], [1], [Define to 1 if the compiler supports __builtin_expect.])
fi
])

dnl
dnl PAC_C_STATIC_ASSERT - Test whether C11 _Static_assert is supported
dnl
dnl will AC_DEFINE([HAVE_C11__STATIC_ASSERT]) if C11 _Static_assert is supported.
dnl
AC_DEFUN([PAC_C_STATIC_ASSERT], [
    AC_MSG_CHECKING([for C11 _Static_assert functionality])
    AC_LINK_IFELSE([AC_LANG_SOURCE([
    int main(){
        _Static_assert(1, "The impossible happened!");
        return 0;
    }
    ])],[
    AC_DEFINE([HAVE_C11__STATIC_ASSERT],[1],[Define if C11 _Static_assert is supported.])
    AC_MSG_RESULT([yes])
    ],[
    AC_MSG_RESULT([no])
    ])
])

dnl
dnl PAC_CC_CHECK_TLS - Test for thread local storage support
dnl
dnl will AC_DEFINE([TLS]) to a compiler supported TLS keyword
dnl
AC_DEFUN([PAC_CC_CHECK_TLS], [
    AC_CACHE_CHECK([for thread local storage], [pac_cv_tls], [
    if test -z $pac_cv_tls ; then
        AC_LINK_IFELSE([AC_LANG_PROGRAM([_Thread_local int foo=0;],[foo=1])],
            [pac_cv_tls=_Thread_local])
    fi
    if test -z $pac_cv_tls ; then
        AC_LINK_IFELSE( [AC_LANG_PROGRAM([__thread int foo=0;],[foo=1])],
            [pac_cv_tls=__thread])
    fi
    if test -z $pac_cv_tls ; then
        AC_LINK_IFELSE( [AC_LANG_PROGRAM([__declspec(thread) int foo=0;],[foo=1])],
            [pac_cv_tls="__declspec(thread)"])
    fi])
    if test -z $pac_cv_tls ; then
        AC_MSG_WARN([Compiler does not support thread local storage])
    else
        AC_DEFINE_UNQUOTED([COMPILER_TLS], [$pac_cv_tls], [Defined the keyword for thread-local storage.])
    fi
])

dnl Test whether pointers can be aligned on a int boundary or require
dnl a pointer boundary.
AC_DEFUN([PAC_CHECK_PTR_ALIGN]), [
    AC_MSG_CHECKING([for alignment restrictions on pointers])
    AC_TRY_RUN(
    changequote(<<,>>)
    struct foo { int a; void *b; };
    int main() {
        int buf[10];
        struct foo *p1;
        p1=(struct foo*)&buf[0];
        p1->b = (void *)0;
        p1=(struct foo*)&buf[1];
        p1->b = (void *)0;
        return 0;
    changequote([,])
    },pac_cv_pointers_have_int_alignment=yes,pac_cv_pointers_have_int_alignment=no,pac_cv_pointers_have_int_alignment=unknown)

    if test "$pac_cv_pointers_have_int_alignment" != "yes" ; then
        AC_DEFINE(NEEDS_POINTER_ALIGNMENT_ADJUST,1,[define if pointers must be aligned on pointer boundaries])
        AC_MSG_RESULT([pointer])
    else
        AC_MSG_RESULT([int or better])
    fi
])

dnl PAC_ARG_ATOMIC_PRIMITIVES
dnl  - Provide configure option to select atomic primitives. Defaults to auto.
AC_DEFUN([PAC_ARG_ATOMIC_PRIMITIVES], [
     AC_ARG_WITH([mpl-atomic-primitives],
     [  --with-mpl-atomic-primitives=package  Atomic primitives to use. The following is include:
        auto - Automatically choose the best one (default)
        c11 - C11 atomics
        gcc_atomic - GCC atomic builtins
        gcc_sync - GCC sync builtins
        win - Windows builtins
        lock - Mutex-based synchronization
        no|none - atomic operations are performed without synchronization
     ],,with_mpl_atomic_primitives=auto)])
