dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
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


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_TEXT
dnl
dnl Determine how to set current mode as text.
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_TEXT],[
    AC_MSG_CHECKING([directive for setting text section])
    ompi_cv_asm_text=""
    case $host in
        *-aix*)
            ompi_cv_asm_text=[".csect .text[PR]"]
        ;;
        *)
            ompi_cv_asm_text=".text"
        ;;
    esac
    AC_MSG_RESULT([$ompi_cv_asm_text])
    AC_DEFINE_UNQUOTED([OMPI_ASM_TEXT], ["$ompi_cv_asm_text"],
                       [Assembly directive for setting text section])
    OMPI_ASM_TEXT="$ompi_cv_asm_text"
    AC_SUBST(OMPI_ASM_TEXT)
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_GLOBAL
dnl
dnl Sets OMPI_ASM_GLOBAL to the value to prefix global values
dnl
dnl I'm sure if I don't have a test for this, there will be some
dnl dumb platform that uses something else
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_GLOBAL],[
    AC_MSG_CHECKING([directive for exporting symbols])
    ompi_cv_asm_global=""
    case $host in
        *)
                ompi_cv_asm_global=".globl"
        ;;
    esac
    AC_MSG_RESULT([$ompi_cv_asm_global])
    AC_DEFINE_UNQUOTED([OMPI_ASM_GLOBAL], ["$ompi_cv_asm_global"],
                       [Assembly directive for exporting symbols])
    OMPI_ASM_GLOBAL="$ompi_cv_asm_global"
    AC_SUBST(OMPI_AS_GLOBAL)
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_LSYM
dnl
dnl Sets OMPI_ASM_LSYM to the prefix value on a symbol to make it
dnl an internal label (jump target and whatnot)
dnl
dnl We look for L .L $ L$ (in that order) for something that both
dnl assembles and does not leave a label in the output of nm.  Fall
dnl back to L if nothing else seems to work :/
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_LSYM],[
    AC_REQUIRE([OMPI_CHECK_ASM_LABEL_SUFFIX])
    AC_REQUIRE([AC_PROG_NM])

    AC_MSG_CHECKING([prefix for lsym labels])
    ompi_cv_asm_lsym="L"

    for sym in L .L $ L$ ; do
        asm_result=0
        echo "configure: trying $sym" >& AC_FD_CC
        OMPI_TRY_ASSEMBLE([foobar$ompi_cv_asm_label_suffix
${sym}mytestlabel$ompi_cv_asm_label_suffix],
            [# ok, we succeeded at assembling.  see if we can nm, 
             # throwing the results in a file
            if $NM conftest.$OBJEXT > conftest.out 2>&AC_FD_CC ; then
                if test "`grep mytestlabel conftest.out`" = "" ; then
                    # there was no symbol...  looks promising to me
                    ompi_cv_asm_lsym="$sym"
                    asm_result=1
                elif test ["`grep ' [Nt] .*mytestlabel' conftest.out`"] = "" ; then
                    # see if we have a non-global-ish symbol
                    # but we should see if we can do better.
                    ompi_cv_asm_lsym="$sym"
                fi
            else
                # not so much on the NM goodness :/
                echo "$NM failed.  Output from NM was:" >& AC_FD_CC
                cat conftest.out > AC_FD_CC
                AC_MSG_WARN([$NM could not read object file])
            fi
            ])
        if test "$asm_result" = "1" ; then
            break
        fi
    done
    rm -f conftest.out

    AC_MSG_RESULT([$ompi_cv_asm_lsym])
    AC_DEFINE_UNQUOTED([OMPI_ASM_LSYM], ["$ompi_cv_asm_lsym"],
                       [Assembly prefix for lsym labels])
    OMPI_ASM_LSYM="$ompi_cv_asm_lsym"
    AC_SUBST(OMPI_ASM_LSYM)
    unset asm_result sym
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_GSYM
dnl
dnl Sets OMPI_ASM_GSYM to the prefix value on a symbol to make it
dnl a global linkable from C.  Basically, an _ or not.
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_GSYM],[
    AC_REQUIRE([OMPI_CHECK_ASM_TEXT])
    AC_REQUIRE([OMPI_CHECK_ASM_GLOBAL])
    AC_REQUIRE([OMPI_CHECK_ASM_LABEL_SUFFIX])

    AC_MSG_CHECKING([prefix for global symbol labels])
    ompi_cv_asm_gsym="none"

    for sym in "_" "" "." ; do
        asm_result=0
        echo "configure: trying $sym" >& AC_FD_CC
cat > conftest_c.c <<EOF
#ifdef __cplusplus
extern "C" {
#endif
void gsym_test_func(void);
#ifdef __cplusplus
}
#endif
int
main(int argc, char *argv[[]])
{
    gsym_test_func();
    return 0;
}
EOF
        OMPI_TRY_ASSEMBLE([
$ompi_cv_asm_text
$ompi_cv_asm_global ${sym}gsym_test_func
${sym}gsym_test_func${ompi_cv_asm_label_suffix}],
            [ompi_compile="$CC $CFLAGS -I. conftest_c.c -c > conftest.cmpl 2>&1"
             if AC_TRY_EVAL(ompi_compile) ; then
                # save the warnings
                 cat conftest.cmpl >&AC_FD_CC
                 ompi_link="$CC $CFLAGS conftest_c.$OBJEXT conftest.$OBJEXT -o conftest > conftest.link 2>&1"
                 if AC_TRY_EVAL(ompi_link) ; then
                     # save the warnings
                     cat conftest.link >&AC_FD_CC
                     asm_result=1
                 else
                     cat conftest.link >&AC_FD_CC
                     echo "configure: failed C program was: " >&AC_FD_CC
                     cat conftest_c.c >&AC_FD_CC
                     echo "configure: failed ASM program was: " >&AC_FD_CC
                     cat conftest.s >&AC_FD_CC
                     asm_result=0
                 fi
             else
                # save output and failed program
                 cat conftest.cmpl >&AC_FD_CC
                 echo "configure: failed C program was: " >&AC_FD_CC
                 cat conftest.c >&AC_FD_CC
                 asm_result=0
             fi], 
            [asm_result=0])
        if test "$asm_result" = "1" ; then
            ompi_cv_asm_gsym="$sym"
            break
        fi
    done
    rm -f conftest.*

    AC_MSG_RESULT([$ompi_cv_asm_gsym])

    if test "$ompi_cv_asm_gsym" = "none" ; then
       AC_MSG_ERROR([Could not determine global symbol label prefix])
    fi

    AC_DEFINE_UNQUOTED([OMPI_ASM_GSYM], ["$ompi_cv_asm_gsym"],
                       [Assembly prefix for lsym labels])
    OMPI_ASM_GSYM="$ompi_cv_asm_gsym"
    AC_SUBST(OMPI_ASM_GSYM)
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_LABEL_SUFFIX
dnl
dnl Sets OMPI_ASM_LABEL_SUFFIX to the value to suffix for labels
dnl
dnl I'm sure if I don't have a test for this, there will be some
dnl dumb platform that uses something else
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_LABEL_SUFFIX],[
    AC_MSG_CHECKING([suffix for labels])
    ompi_cv_asm_label_suffix=""
    case $host in
        *)
                ompi_cv_asm_label_suffix=":"
        ;;
    esac
    AC_MSG_RESULT([$ompi_cv_asm_label_suffix])
    AC_DEFINE_UNQUOTED([OMPI_ASM_LABEL_SUFFIX], ["$ompi_cv_asm_label_suffix"],
                       [Assembly suffix for labels])
    OMPI_ASM_LABEL_SUFFIX="$ompi_cv_asm_label_suffix"
    AC_SUBST(OMPI_AS_LABEL_SUFFIX)
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_ALIGN_LOG
dnl
dnl Sets OMPI_ASM_ALIGN_LOG to 1 if align is specified 
dnl logarithmically, 0 otherwise
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_ALIGN_LOG],[
    AC_REQUIRE([OMPI_CHECK_ASM_TEXT])
    AC_REQUIRE([OMPI_CHECK_ASM_GLOBAL])
    AC_REQUIRE([OMPI_CHECK_ASM_LABEL_SUFFIX])
    AC_REQUIRE([AC_PROG_NM])

    ompi_cv_asm_align_log=0
    asm_result="no"
    AC_MSG_CHECKING([if .align directive takes logarithmic value])
    OMPI_TRY_ASSEMBLE([        $ompi_cv_asm_text
        .align 4
        $ompi_cv_asm_global foo
        .byte 1
        .align 4
foo$ompi_cv_asm_label_suffix
        .byte 2], 
        [ompi_asm_addr=[`$NM conftest.$OBJEXT | grep foo | sed -e 's/.*\([0-9a-fA-F][0-9a-fA-F]\).*foo.*/\1/'`]],
        [ompi_asm_addr=""])
    # test for both 16 and 10 (decimal and hex notations)
    echo "configure: .align test address offset is $ompi_asm_addr" >& AC_FD_CC
    if test "$ompi_asm_addr" = "16" -o "$ompi_asm_addr" = "10" ; then
       ompi_cv_asm_align_log=1
       asm_result="yes"
    fi
    AC_MSG_RESULT([$asm_result])

    AC_DEFINE_UNQUOTED([OMPI_ASM_ALIGN_LOG],
                       [$ompi_cv_asm_align_log],
                       [Assembly align directive expects logarithmic value])

    unset omp_asm_addr asm_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_TYPE
dnl
dnl Sets OMPI_ASM_TYPE to the prefix for the function type to 
dnl set a symbol's type as function (needed on ELF for shared
dnl libaries).  If no .type directive is needed, sets OMPI_ASM_TYPE
dnl to an empty string
dnl
dnl We look for @ \# %
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_TYPE],[
    AC_MSG_CHECKING([prefix for function in .type])
    ompi_cv_asm_type=""

    case "${host}" in
    *-sun-solaris*)
        # GCC on solaris seems to accept just about anything, not
        # that what it defines actually works...  So just hardwire
        # to the right answer
        ompi_cv_asm_type="#"
    ;;
    *)
        for type  in @ \# % ; do
            asm_result=0
            echo "configure: trying $type" >& AC_FD_CC
            OMPI_TRY_ASSEMBLE([     .type mysym, ${type}function],
                [# ok, we succeeded at assembling.  see if there was
                 # a warning in the output.
                 if test "`cat conftest.out`" = "" ; then
                    ompi_cv_asm_type="${type}"
                    asm_result=1
                 fi])
            if test "$asm_result" = "1" ; then
                break
            fi
        done
    ;;
    esac
    rm -f conftest.out

    AC_MSG_RESULT([$ompi_cv_asm_type])
    AC_DEFINE_UNQUOTED([OMPI_ASM_TYPE], ["$ompi_cv_asm_type"],
                       [How to set function type in .type directive])
    OMPI_ASM_TYPE="$ompi_cv_asm_type"
    AC_SUBST(OMPI_ASM_TYPE)
    unset asm_result type
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_ASM_SIZE
dnl
dnl Sets OMPI_ASM_SIZE to 1 if we should set .size directives for
dnl each function, 0 otherwise.
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_ASM_SIZE],[
    AC_MSG_CHECKING([if .size is needed])
    ompi_cv_asm_size=0
    asm_result="no"

    OMPI_TRY_ASSEMBLE([     .size mysym, 1],
            [# ok, we succeeded at assembling.  see if there was
             # a warning in the output.
             if test "`cat conftest.out`" = "" ; then
                ompi_cv_asm_size=1
                asm_result="yes"
             fi])
    rm -f conftest.out

    AC_MSG_RESULT([$asm_result])
    AC_DEFINE_UNQUOTED([OMPI_ASM_SIZE], ["$ompi_cv_asm_size"],
                       [Do we need to give a .size directive?])
    OMPI_ASM_SIZE="$ompi_cv_asm_size"
    AC_SUBST(OMPI_ASM_TYPE)
    unset asm_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_POWERPC_REG
dnl
dnl See if the notation for specifying registers is X (most everyone)
dnl or rX (OS X)
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_POWERPC_REG],[
    AC_REQUIRE([OMPI_CHECK_ASM_TEXT])
    AC_MSG_CHECKING([if PowerPC registers have r prefix])
    OMPI_TRY_ASSEMBLE([$ompi_cv_asm_text
        addi 1,1,0],
        [ompi_cv_asm_powerpc_r_reg=0],
        OMPI_TRY_ASSEMBLE([$ompi_cv_asm_text
        addi r1,r1,0],
            [ompi_cv_asm_powerpc_r_reg=1],
            AC_MSG_ERROR([Can not determine how to use PPC registers])))
    if test "$ompi_cv_asm_powerpc_r_reg" = "1" ; then
        AC_MSG_RESULT([yes])
    else
        AC_MSG_RESULT([no])
    fi

    AC_DEFINE_UNQUOTED([OMPI_POWERPC_R_REGISTERS],
                       [$ompi_cv_asm_powerpc_r_reg],
                       [Whether r notation is used for ppc registers])
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_POWERPC_64BIT
dnl
dnl On some powerpc chips (the PPC970 or G5), the OS usually runs in
dnl 32 bit mode, even though the hardware can do 64bit things.  If
dnl the compiler will let us, emit code for 64bit test and set type
dnl operations (on a long long).
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_POWERPC_64BIT],[
    AC_REQUIRE([OMPI_CHECK_ASM_TEXT])

    AC_MSG_CHECKING([for 64-bit PowerPC assembly support])
    ppc64_result=0
    if test "$ompi_cv_asm_powerpc_r_reg" = "1" ; then
        ldarx_asm="        ldarx r1,r1,r1";
    else
        ldarx_asm="        ldarx 1,1,1";
    fi
    OMPI_TRY_ASSEMBLE([$ompi_cv_asm_text
        $ldarx_asm],
                    [ppc64_result=1],
                    [ppc64_result=0])
    if test "$ppc64_result" = "1" ; then
        AC_MSG_RESULT([yes])
        ifelse([$1],,:,[$1])
    else
        AC_MSG_RESULT([no])
        ifelse([$2],,:,[$2])
    fi

    unset ppc64_result ldarx_asm
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_SPARCV8PLUS
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_SPARCV8PLUS],[
    AC_REQUIRE([OMPI_CHECK_ASM_TEXT])

    AC_MSG_CHECKING([if have Sparc v8+/v9 support])
    sparc_result=0
    OMPI_TRY_ASSEMBLE([$ompi_cv_asm_text
	casa [%o0] 0x80, %o1, %o2],
                [sparc_result=1],
                [sparc_result=0])
    if test "$sparc_result" = "1" ; then
        AC_MSG_RESULT([yes])
        ifelse([$1],,:,[$1])
    else
        AC_MSG_RESULT([no])
        ifelse([$2],,:,[$2])
    fi

    unset sparc_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_INLINE_GCC
dnl
dnl Check if the compiler is capable of doing GCC-style inline
dnl assembly.  Some compilers emit a warning and ignore the inline
dnl assembly (xlc on OS X) and compile without error.  Therefore,
dnl the test attempts to run the emited code to check that the
dnl assembly is actually run.  To run this test, one argument to
dnl the macro must be an assembly instruction in gcc format to move 
dnl the value 0 into the register containing the variable ret.  
dnl For PowerPC, this would be:
dnl
dnl   "li %0,0" : "=&r"(ret)
dnl
dnl DEFINE OMPI_GCC_INLINE_ASSEMBLY to 0 or 1 depending on GCC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_INLINE_GCC],[
    assembly="$1"
    asm_result="unknown"

    AC_MSG_CHECKING([if $CC supports GCC inline assembly])

    if test ! "$assembly" = "" ; then
        AC_RUN_IFELSE(AC_LANG_PROGRAM([[
AC_INCLUDES_DEFAULT]],
[[int ret = 1;
__asm__ __volatile__ ($assembly);
return ret;]]),
            [asm_result="yes"], [asm_result="no"], 
            [asm_result="unknown"])
    else
        assembly="test skipped - assuming no"
    fi

    # if we're cross compiling, just try to compile and figure good enough
    if test "$asm_result" = "unknown" ; then
        AC_LINK_IFELSE(AC_LANG_PROGRAM([[
AC_INCLUDES_DEFAULT]],
[[int ret = 1;
__asm__ __volatile__ ($assembly);
return ret;]]),
            [asm_result="yes"], [asm_result="no"])
    fi

    AC_MSG_RESULT([$asm_result])

    if test "$asm_result" = "yes" ; then
        OMPI_GCC_INLINE_ASSEMBLY=1
    else
        OMPI_GCC_INLINE_ASSEMBLY=0
    fi

    AC_DEFINE_UNQUOTED([OMPI_GCC_INLINE_ASSEMBLY],
                       [$OMPI_GCC_INLINE_ASSEMBLY],
                       [Whether compiler supports GCC style inline assembly])

    unset OMPI_GCC_INLINE_ASSEMBLY assembly asm_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_INLINE_DEC
dnl
dnl DEFINE OMPI_DEC to 0 or 1 depending on DEC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_INLINE_DEC],[

    AC_MSG_CHECKING([if $CC supports DEC inline assembly])

    AC_LINK_IFELSE(AC_LANG_PROGRAM([[
AC_INCLUDES_DEFAULT
#include <c_asm.h>]],
[[asm("");
return 0;]]),
        [asm_result="yes"], [asm_result="no"])

    AC_MSG_RESULT([$asm_result])

    if test "$asm_result" = "yes" ; then
        OMPI_DEC_INLINE_ASSEMBLY=1
    else
        OMPI_DEC_INLINE_ASSEMBLY=0
    fi

    AC_DEFINE_UNQUOTED([OMPI_DEC_INLINE_ASSEMBLY],
                       [$OMPI_DEC_INLINE_ASSEMBLY],
                       [Whether compiler supports DEC style inline assembly])

    unset OMPI_DEC_INLINE_ASSEMBLY asm_result
])dnl


dnl #################################################################
dnl
dnl OMPI_CHECK_INLINE_XLC
dnl
dnl DEFINE OMPI_XLC to 0 or 1 depending on XLC
dnl                support
dnl
dnl #################################################################
AC_DEFUN([OMPI_CHECK_INLINE_XLC],[

    AC_MSG_CHECKING([if $CC supports XLC inline assembly])

    OMPI_XLC_INLINE_ASSEMBLY=0
    asm_result="no"
    if test "$CC" = "xlc" ; then
        if test "$CXX" = "xlC" -o "$CXX" = "xlc++" ; then
            OMPI_XLC_INLINE_ASSEMBLY=1
            asm_result="yes"
        fi
    fi

    AC_MSG_RESULT([$asm_result])
    AC_DEFINE_UNQUOTED([OMPI_XLC_INLINE_ASSEMBLY],
                       [$OMPI_XLC_INLINE_ASSEMBLY],
                       [Whether compiler supports XLC style inline assembly])

    unset OMPI_XLC_INLINE_ASSEMBLY
])dnl


dnl #################################################################
dnl
dnl OMPI_CONFIG_ASM
dnl
dnl DEFINE OMPI_ASSEMBLY_ARCH to something in sys/architecture.h
dnl DEFINE OMPI_ASSEMBLY_FORMAT to string containing correct
dnl                             format for assembly (not user friendly)
dnl SUBST OMPI_ASSEMBLY_FORMAT to string containing correct
dnl                             format for assembly (not user friendly)
dnl
dnl #################################################################
AC_DEFUN([OMPI_CONFIG_ASM],[
    AC_REQUIRE([OMPI_SETUP_CC])
    AC_REQUIRE([OMPI_SETUP_CXX])
    AC_REQUIRE([AM_PROG_AS])
    AC_REQUIRE([OMPI_CHECK_ASM_TEXT])
    AC_REQUIRE([OMPI_CHECK_ASM_GLOBAL])
    AC_REQUIRE([OMPI_CHECK_ASM_GSYM])
    AC_REQUIRE([OMPI_CHECK_ASM_LSYM])
    AC_REQUIRE([OMPI_CHECK_ASM_TYPE])
    AC_REQUIRE([OMPI_CHECK_ASM_SIZE])
    AC_REQUIRE([OMPI_CHECK_ASM_LABEL_SUFFIX])
    AC_REQUIRE([OMPI_CHECK_ASM_ALIGN_LOG])

AC_MSG_CHECKING([whether to enable smp locks])
AC_ARG_ENABLE(smp-locks, 
    AC_HELP_STRING([--enable-smp-locks],
                   [disable smp locks in atomic ops (default: enabled)]))
if test "$enable_smp_locks" != "no"; then
    AC_MSG_RESULT([yes])
    want_smp_locks=1
else
    AC_MSG_RESULT([no])
    want_smp_locks=0
fi
AC_DEFINE_UNQUOTED([OMPI_WANT_SMP_LOCKS], [$want_smp_locks],
                   [whether we want to have smp locks in atomic ops or not])


# find our architecture for purposes of assembly stuff
ompi_cv_asm_arch="UNSUPPORTED"
OMPI_GCC_INLINE_ASSIGN=""
OMPI_ASM_SUPPORT_64BIT=0
case "${host}" in
    *-winnt*)
        ompi_cv_asm_arch="WINDOWS"
    ;;

    i?86-*)
        ompi_cv_asm_arch="IA32"
        OMPI_ASM_SUPPORT_64BIT=1
        OMPI_GCC_INLINE_ASSIGN='"movl [$]0, %0" : "=&r"(ret)'
    ;;

    x86_64*)
        ompi_cv_asm_arch="AMD64"
        OMPI_ASM_SUPPORT_64BIT=1
        OMPI_GCC_INLINE_ASSIGN='"movl [$]0, %0" : "=&r"(ret)'
    ;;

    ia64-*)
        ompi_cv_asm_arch="IA64"
        OMPI_ASM_SUPPORT_64BIT=1
        OMPI_GCC_INLINE_ASSIGN='"mov %0=r0\n;;\n" : "=&r"(ret)'
    ;;

    alpha-*)
        ompi_cv_asm_arch="ALPHA"
        OMPI_ASM_SUPPORT_64BIT=1
        OMPI_GCC_INLINE_ASSIGN='"bis zero,zero,%0" : "=&r"(ret)'
    ;;

    mips-*)
        # Should really find some way to make sure that we are on
        # a MIPS III machine (r4000 and later)
        ompi_cv_asm_arch="MIPS"
        OMPI_ASM_SUPPORT_64BIT=1
        OMPI_GCC_INLINE_ASSIGN='"or %0,[$]0,[$]0" : "=&r"(ret)'
    ;;

    powerpc-*|powerpc64-*)
        OMPI_CHECK_POWERPC_REG
        if test "$ac_cv_sizeof_long" = "4" ; then
            ompi_cv_asm_arch="POWERPC32"

            # Note that on some platforms (Apple G5), even if we are
            # compiling in 32 bit mode (and therefore should assume
            # sizeof(long) == 4), we can use the 64 bit test and set
            # operations.
            OMPI_CHECK_POWERPC_64BIT(OMPI_ASM_SUPPORT_64BIT=1)
        elif test "$ac_cv_sizeof_long" = "8" ; then
            OMPI_ASM_SUPPORT_64BIT=1
            ompi_cv_asm_arch="POWERPC64"
        else
            AC_MSG_ERROR([Could not determine PowerPC word size: $ac_cv_sizeof_long])
        fi
        OMPI_GCC_INLINE_ASSIGN='"1: li %0,0" : "=&r"(ret)'
    ;;

    sparc*-*)
        # SPARC v9 (and above) are the only ones with 64bit support
        # if compiling 32 bit, see if we are v9 (aka v8plus) or
        # earlier (casa is v8+/v9). 
        if test "$ac_cv_sizeof_long" = "4" ; then
            have_v8plus=0
            OMPI_CHECK_SPARCV8PLUS([have_v8plus=1])
            if test "$have_v8plus" = "0" ; then
                OMPI_ASM_SUPPORT_64BIT=0
                ompi_cv_asm_arch="SPARC"
AC_MSG_WARN([Using SPARC V8 assembly for atomic operations.  This])
AC_MSG_WARN([may result in reduced performance on UltraSparc platforms.])
AC_MSG_WARN([If you are compiling for the UltraSparc, consider ])
AC_MSG_WARN([specifying the architecture v8plus (cc: -xarch=v8plus, ])
AC_MSG_WARN([gcc: -mv8plus) when compiling Open MPI, as you may see a])
AC_MSG_WARN([significant performance increase.])

            else
                OMPI_ASM_SUPPORT_64BIT=1
                ompi_cv_asm_arch="SPARCV9_32"
            fi

        elif test "$ac_cv_sizeof_long" = "8" ; then
            OMPI_ASM_SUPPORT_64BIT=1
            ompi_cv_asm_arch="SPARCV9_64"
        else
          AC_MSG_ERROR([Could not determine Sparc word size: $ac_cv_sizeof_long])
        fi
        OMPI_GCC_INLINE_ASSIGN='"mov 0,%0" : "=&r"(ret)'
    ;;

    *)
        AC_MSG_ERROR([No atomic primitives available for $host])
    ;;
esac

AC_DEFINE_UNQUOTED([OMPI_ASM_SUPPORT_64BIT],
                   [$OMPI_ASM_SUPPORT_64BIT],
                   [Whether we can do 64bit assembly operations or not.  Should not be used outside of the assembly header files])
AC_SUBST([OMPI_ASM_SUPPORT_64BIT])

#
# figure out if we need any special function start / stop code
#
case $host_os in
    aix*)
        ompi_asm_arch_config="aix"
    ;;
    *)
        ompi_asm_arch_config="default"
    ;;
esac


# now that we know our architecture, try to inline assemble
OMPI_CHECK_INLINE_GCC([$OMPI_GCC_INLINE_ASSIGN])
OMPI_CHECK_INLINE_DEC
OMPI_CHECK_INLINE_XLC

# format:
#   config_file-text-global-label_suffix-gsym-lsym-type-size-align_log-ppc_r_reg-64_bit
asm_format="${ompi_asm_arch_config}"
asm_format="${asm_format}-${ompi_cv_asm_text}-${ompi_cv_asm_global}"
asm_format="${asm_format}-${ompi_cv_asm_label_suffix}-${ompi_cv_asm_gsym}"
asm_format="${asm_format}-${ompi_cv_asm_lsym}"
asm_format="${asm_format}-${ompi_cv_asm_type}-${ompi_cv_asm_size}"
asm_format="${asm_format}-${ompi_cv_asm_align_log}"
if test "$ompi_cv_asm_arch" = "POWERPC32" -o "$ompi_cv_asm_arch" = "POWERPC64" ; then
    asm_format="${asm_format}-${ompi_cv_asm_powerpc_r_reg}"
else
    asm_format="${asm_format}-1"
fi
ompi_cv_asm_format="${asm_format}-${OMPI_ASM_SUPPORT_64BIT}"
OMPI_ASSEMBLY_FORMAT="$ompi_cv_asm_format"

AC_MSG_CHECKING([for assembly format])
AC_MSG_RESULT([$OMPI_ASSEMBLY_FORMAT])
AC_DEFINE_UNQUOTED([OMPI_ASSEMBLY_FORMAT], ["$OMPI_ASSEMBLY_FORMAT"],
                   [Format of assembly file])
AC_SUBST([OMPI_ASSEMBLY_FORMAT])

result="OMPI_$ompi_cv_asm_arch"
OMPI_ASSEMBLY_ARCH="$ompi_cv_asm_arch"
AC_MSG_CHECKING([for asssembly architecture])
AC_MSG_RESULT([$ompi_cv_asm_arch])
AC_DEFINE_UNQUOTED([OMPI_ASSEMBLY_ARCH], [$result],
                   [Architecture type of assembly to use for atomic operations])
AC_SUBST([OMPI_ASSEMBLY_ARCH])

OMPI_ASM_FIND_FILE

unset result asm_format
])dnl


dnl #################################################################
dnl
dnl OMPI_ASM_FIND_FILE
dnl
dnl
dnl do all the evil mojo to provide a working assembly file
dnl
dnl #################################################################
AC_DEFUN([OMPI_ASM_FIND_FILE], [
    AC_REQUIRE([AC_PROG_FGREP])
    AC_CHECK_PROG([PERL], [perl], [perl])

    # see if we have a pre-built one already
    AC_MSG_CHECKING([for pre-built assembly file])
    ompi_cv_asm_file=""
    if grep "$ompi_cv_asm_arch" "${top_ompi_srcdir}/src/asm/asm-data.txt" | $FGREP "$ompi_cv_asm_format" >conftest.out 2>&1 ; then
        ompi_cv_asm_file="`cut -f3 conftest.out`"
        if test ! "$ompi_cv_asm_file" = "" ; then
            ompi_cv_asm_file="atomic-${ompi_cv_asm_file}.s"
            if test -f "${top_ompi_srcdir}/src/asm/generated/${ompi_cv_asm_file}" ; then
                AC_MSG_RESULT([yes ($ompi_cv_asm_file)])
            else
                AC_MSG_RESULT([no ($ompi_cv_asm_file not found)])
                ompi_cv_asm_file=""
            fi
        fi
    else
        AC_MSG_RESULT([no (not in asm-data)])
    fi
    rm -f conftest.*

    if test "$ompi_cv_asm_file" = "" ; then
        if test ! "$PERL" = "" ; then
            # we have perl...  Can we generate a file?
            AC_MSG_CHECKING([whether possible to generate assembly file])
            mkdir -p src/asm/generated
            ompi_cv_asm_file="atomic-local.s"
            ompi_try="$PERL \"$top_ompi_srcdir/src/asm/generate-asm.pl\" \"$ompi_cv_asm_arch\" \"$ompi_cv_asm_format\" \"$top_ompi_srcdir/src/asm/base\" \"$top_ompi_builddir/src/asm/generated/$ompi_cv_asm_file\" >conftest.out 2>&1"
            if AC_TRY_EVAL(ompi_try) ; then
                # save the warnings
                cat conftest.out >&AC_FD_CC
                AC_MSG_RESULT([yes])
            else
                # save output
                cat conftest.out >&AC_FD_CC
                ompi_cv_asm_file=""
                AC_MSG_RESULT([failed])
                AC_MSG_WARN([Could not build atomic operations assembly file.])
                AC_MSG_WARN([There will be no atomic operations for this build.])
            fi
        else
            AC_MSG_WARN([Could not find prebuilt atomic operations file and could not])
            AC_MSG_WARN([find perl to attempt to generate a custom assembly file.])
            AC_MSG_WARN([There will be no atomic operations for this build.])
        fi
    fi
    rm -f conftest.*

    AC_MSG_CHECKING([for atomic assembly filename])
    if test "$ompi_cv_asm_file" = "" ; then
        AC_MSG_RESULT([none])
        result=0
    else
        AC_MSG_RESULT([$ompi_cv_asm_file])
        result=1
    fi

    AC_DEFINE_UNQUOTED([OMPI_HAVE_ASM_FILE], [$result],
                       [Whether there is an atomic assembly file available])
    AM_CONDITIONAL([OMPI_HAVE_ASM_FILE], [test "$result" = "1"])

    OMPI_ASM_FILE=$ompi_cv_asm_file
    AC_SUBST(OMPI_ASM_FILE)
])dnl
