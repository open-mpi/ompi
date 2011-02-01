dnl
dnl Check for BSD or POSIZ style global symbol lister, nm.
dnl If found, pac_path_NM_G contains the absolute athname of nm + options
dnl pac_path_NM_G_type will be either POSIX or BSD.  NM_G will be
dnl pac_path_NM_G without the absolute path.  Preference is BSD style.
dnl
dnl The test checks if nm accepts the known options and also if nm produces
dnl the expected BSD or POSIX output format.
dnl
AC_DEFUN([PAC_PATH_NM_G],[
AC_MSG_CHECKING([for BSD/POSIX style global symbol lister])
AC_LANG_PUSH(C)
AC_PATH_PROGS_FEATURE_CHECK(NM_G, nm, [
    # Check if nm accepts -g and BSD or POSIX compatible flag.
    # Use the `sed 1q' to avoid HP-UX's unknown option message:
    #   nm: unknown option "B" ignored
    # Tru64's nm complains that /dev/null is an invalid object file
    #
    # AIX's sed does not accept \+, 1) instead of doing 's|a\+||', do 's|aa*||'
    # or 2) instead of 's|A \+B|AB|g', do 's|A  *B|AB|g' 

    # Check if nm accepts -g
    case `${ac_path_NM_G} -g /dev/null 2>&1 | sed '1q'` in
    */dev/null* | *'Invalid file or object type'*)
        ac_path_NM_G="${ac_path_NM_G} -g"
        # Check if nm accepts -B
        case `${ac_path_NM_G} -B /dev/null 2>&1 | sed '1q'` in
        */dev/null* | *'Invalid file or object type'*)
            AC_COMPILE_IFELSE([
                AC_LANG_SOURCE([int iglobal;])
            ],[
                changequote(,)
                case `${ac_path_NM_G} -B conftest.$OBJEXT | sed -e 's|[0-9][0-9]*  *[A-Z]  *iglobal|XXXX|g'` in
                *XXXX*)
                    pac_path_NM_G="${ac_path_NM_G} -B"
                    pac_path_NM_G_type="BSD"
                    ;;
                esac
                changequote([,])
            ])
            ;;
        *)
            # Check if nm accepts -P
            case `${ac_path_NM_G} -P /dev/null 2>&1 | sed '1q'` in
            */dev/null* | *'Invalid file or object type'*)
                AC_COMPILE_IFELSE([
                    AC_LANG_SOURCE([int iglobal;])
                ],[
                    changequote(,)
                    case `${ac_path_NM_G} -P conftest.$OBJEXT | sed -e 's|iglobal  *[A-Z]  *[0-9][0-9]*|XXXX|g'` in
                    *XXXX*)
                        pac_path_NM_G="${ac_path_NM_G} -P"
                        pac_path_NM_G_type="POSIX"
                        ;;
                    esac
                    changequote([,])
                ])
                ;;
            esac  # Endof case `${ac_path_NM_G} -P
            ;;
        esac   # Endof case `${ac_path_NM_G} -B
        ;;
    esac  # Endof case `${ac_path_NM_G} -g
    if test "X$pac_path_NM_G" != "X" ; then
        AC_MSG_RESULT([$pac_path_NM_G_type style,$pac_path_NM_G])
        NM_G="`echo $pac_path_NM_G | sed -e 's|^.*nm |nm |g'`"
    else
        AC_MSG_RESULT(no)
    fi
    ac_cv_path_NM_G=${ac_path_NM_G}
    ac_path_NM_G_found=:
], [AC_MSG_RESULT(no)],
[$PATH$PATH_SEPARATOR/usr/ccs/bin/elf$PATH_SEPARATOR/usr/ccs/bin$PATH_SEPARATOR/usr/ucb$PATH_SEPARATOR/bin])
AC_LANG_POP(C)
]) dnl Endof AC_DEFUN([PAC_PATH_NM_G]
dnl
dnl PAC_C_MULTI_ATTR_ALIAS()
dnl
dnl The checks if multiple __attribute__((alias)) is available
dnl If the multiple __attribute((alias)) support is found,
dnl pac_c_multi_attr_alias=yes is set.
dnl
dnl The default is to do a runtime test.  When cross_compiling=yes,
dnl pac_path_NM_G will be used to determine the test result.
dnl If CFLAGS(or CPPFLAGS) contains ATTR_ALIAS_DEBUG, the runtime will print
dnl out addresses of struct(s) for debugging purpose.
dnl
dnl
AC_DEFUN([PAC_C_MULTI_ATTR_ALIAS],[
AC_REQUIRE([PAC_PATH_NM_G])
AC_LANG_PUSH(C)
AC_CHECK_HEADERS([stdio.h])
AC_MSG_CHECKING([for multiple __attribute__((alias)) support])

#Compile the "other" __attribute__ object file.
AC_COMPILE_IFELSE([
    AC_LANG_SOURCE([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif

struct mpif_cmblk_t_ { int imember; };
typedef struct mpif_cmblk_t_ mpif_cmblk_t;

/* NOT initialize these structure so it appears in BSS or as COMMON symbols */
mpif_cmblk_t mpifcmb;
mpif_cmblk_t MPIFCMB;

/*
   Do the test in this file instead in the file
   where __attribute__((alias)) is used. 
   This is needed for pgcc since pgcc seems to
   define aliased symbols if they are in the same file.
*/
/*
    We can't do the following comparision in one test:

    ilogical = (( &mpifcmb == ptr && &MPIFCMB == ptr ) ? TRUE : FALSE) ;

    because some compiler, like gcc 4.4.2's -O* optimizes the code
    such that the ilogical expression is FALSE. The likely reason is that
    mpifcmb and MPIFCMB are defined in the same scope in which C optimizer
    may have treated them as different objects (with different addresses),
    &mpifcmb != &MPIFCMB, before actually running the test and hence the
    illogical expression is assumed to be always FALSE.  The solution taken
    here is to prevent the optimizer the opportunity to equate &mpifcmb and
    &MPIFCMB (in same scope), e.g. using 2 separate tests and combine the
    test results in a different scope.
*/
int same_addrs1( void *ptr );
int same_addrs1( void *ptr )
{
#if defined(ATTR_ALIAS_DEBUG)
    printf( "others: addr(mpifcmb)=%p, addr(input ptr)=%p\n", &mpifcmb, ptr );
#endif
    return ( &mpifcmb == ptr ? 1 : 0 );
}

int same_addrs2( void *ptr );
int same_addrs2( void *ptr )
{
#if defined(ATTR_ALIAS_DEBUG)
    printf( "others: addr(MPIFCMB)=%p, addr(input ptr)=%p\n", &MPIFCMB, ptr );
#endif
    return ( &MPIFCMB == ptr ? 1 : 0 );
}

    ])
],[
    rm -f pac_conftest_other.$OBJEXT
    PAC_RUNLOG([cp conftest.$OBJEXT pac_conftest_other.$OBJEXT])
    test -s pac_conftest_other.$OBJEXT && pac_c_attr_alias_other=yes
dnl     cp conftest.$ac_ext pac_conftest_other.$ac_ext
dnl     echo
dnl     echo "pac_conftest_other.$OBJEXT"
dnl     nm -P -g pac_conftest_other.$OBJEXT | grep -i "mpifcmb"
],[
    pac_c_attr_alias_other=no
])  dnl Endof AC_COMPILE_IFELSE

pac_c_attr_alias_main=no
if test "$pac_c_attr_alias_other" = "yes" ; then

#   Save LIBS for later restoration.
    saved_LIBS="$LIBS"
    LIBS="pac_conftest_other.$OBJEXT $LIBS"

#   Link the "other" __attribute__ object file.
    AC_LINK_IFELSE([
        AC_LANG_PROGRAM([
#if defined(HAVE_STDIO_H) || defined(STDC_HEADERS)
#include <stdio.h>
#endif
 
struct mpif_cmblk_t_ { int imember; };
typedef struct mpif_cmblk_t_ mpif_cmblk_t;

mpif_cmblk_t mpifcmbr = {0};
extern mpif_cmblk_t MPIFCMB __attribute__ ((alias("mpifcmbr")));
extern mpif_cmblk_t mpifcmb __attribute__ ((alias("mpifcmbr")));

extern int same_addrs1( void *ptr );
extern int same_addrs2( void *ptr );

        ],[
    int iaddr = 0;
#if defined(ATTR_ALIAS_DEBUG)
    printf( "main: addr(mpifcmbr) = %p\n", &mpifcmbr );
    printf( "main: addr(mpifcmb) = %p\n", &mpifcmb );
    printf( "main: addr(MPIFCMB) = %p\n", &MPIFCMB );
#endif
    iaddr = same_addrs1( &mpifcmbr ) && same_addrs2( &mpifcmbr );
    FILE *file = fopen( "pac_conftestval", "w" );
    if (!file) return 1;
    fprintf( file, "%d\n", iaddr );
        ])
    ],[
        rm -f pac_conftest_main$EXEEXT
        PAC_RUNLOG([cp conftest$EXEEXT pac_conftest_main$EXEEXT])
        test -x pac_conftest_main$EXEEXT && pac_c_attr_alias_main=yes
dnl         cp conftest.$ac_ext pac_conftest_main.$ac_ext
dnl         echo
dnl         echo "pac_conftest_main$EXEEXT"
dnl         nm -P -g pac_conftest_main$EXEEXT | grep -i "mpifcmb"
    ],[
        pac_c_attr_alias_main=no
dnl         cp conftest.$ac_ext pac_conftest_main.$ac_ext
    ])  dnl Endof AC_LINK_IFELSE

# Restore the previously saved LIBS
    LIBS="$saved_LIBS"
    rm -f pac_conftest_other.$OBJEXT
fi dnl Endof if test "$pac_c_attr_alias_other" = "yes"

if test "$pac_c_attr_alias_main" = "yes" ; then
    if test "$cross_compiling" = "yes" ; then
        changequote(,)
        # echo "PAC CROSS-COMPILING" dnl
        # POSIX NM = nm -P format dnl
        if test "$pac_path_NM_G_type" = "POSIX" ; then
            addrs=`${pac_path_NM_G} ./pac_conftest_main$EXEEXT | grep -i mpifcmb | sed -e 's% *[a-zA-Z][a-zA-Z]*  *[a-zA-Z]  *\([0-9abcdef][0-9abcdef]*\).*%\1%g'`
        fi

        # BSD NM = nm -B format dnl
        if test "$pac_path_NM_G_type" = "BSD" ; then
            addrs=`${pac_path_NM_G} -g ./pac_conftest_main$EXEEXT | grep -i mpifcmb | sed -e "s% *\([0-9abcdef][0-9abcdef]*\)  *[a-zA-Z]  *[a-zA-Z][a-zA-A]*.*%\1%g"`
        fi
        changequote([,])

        cmp_addr=""
        diff_addrs=no
        for addr in ${addrs} ; do
            if test "X${cmp_addr}" != "X" ; then
                if test "${cmp_addr}" != "${addr}" ; then
                    diff_addrs=yes
                    break
                fi
            else
                cmp_addr=${addr}
            fi
        done
        
        if test "$diff_addrs" != "yes" ; then
            dnl echo "Same addresses. Multiple aliases support"
            AC_MSG_RESULT([${NM_G} says yes])
            pac_c_multi_attr_alias=yes
        else
            dnl echo "Different addresses. No multiple aliases support."
            AC_MSG_RESULT([${NM_G} says no])
            pac_c_multi_attr_alias=no
        fi

    else # if test "$cross_compiling" != "yes"
        rm -f pac_conftestval
        ac_try="./pac_conftest_main$EXEEXT"
        if AC_TRY_EVAL(ac_try) ; then
            pac_c_attr_alias_val=0
            if test -s pac_conftestval ; then
                eval pac_c_attr_alias_val=`cat pac_conftestval`
            fi
            if test "$pac_c_attr_alias_val" -eq 1 ; then
                AC_MSG_RESULT(yes)
                pac_c_multi_attr_alias=yes
            else
                AC_MSG_RESULT(no)
                pac_c_multi_attr_alias=no
            fi
            rm -f pac_conftestval
        fi
    fi
    dnl Endof if test "$cross_compiling" = "yes"
    rm -f pac_conftest_main$EXEEXT
else
    AC_MSG_RESULT(no! link failure)
    pac_c_multi_attr_alias=no
fi dnl Endof if test "$pac_c_attr_alias_main" = "yes"

AC_LANG_POP(C)

]) dnl  Endof AC_DEFUN([PAC_C_MULTI_ATTR_ALIAS]
dnl
dnl PAC_C_ATTR_ALIGNED()
dnl
dnl Check if __attribute__((aligned)) support is there.
dnl If so, set pac_c_attr_aligned=yes.
dnl
dnl Do a link test instead of compile test to check if the linker
dnl would emit an error.
dnl
AC_DEFUN([PAC_C_ATTR_ALIGNED],[
AC_LANG_PUSH(C)
AC_MSG_CHECKING([for __attribute__((aligned)) support])
#Link the __attribute__ object file.
AC_LINK_IFELSE([
    AC_LANG_PROGRAM([
struct mpif_cmblk_t_ { int imembers[5]; };
typedef struct mpif_cmblk_t_ mpif_cmblk_t;
mpif_cmblk_t mpifcmbr __attribute__((aligned)) = {0};
    ],[])
],[pac_c_attr_aligned=yes], [pac_c_attr_aligned=no])
AC_MSG_RESULT([$pac_c_attr_aligned])
AC_LANG_POP(C)
])
dnl
dnl PAC_F2C_ATTR_ALIGNED_SIZE(ARRAY_SIZE, [OUTPUT_VAR], [MIN_ALIGNMENT])
dnl
dnl ARRAY_SIZE    : Size of the integer array within the fortran commmon block.
dnl OUTPUT_VAR    : Optional variable to be set.
dnl                 if test succeeds, set OUTPUT_VAR=$pac_f2c_attr_aligned_str.
dnl                 if test fails, set OUTPUT_VAR="unknown".
dnl MIN_ALIGNMENT : Optional value.
dnl                 Minimum alignment size to be used in OUTPUT_VAR.
dnl                 pac_f2c_attr_aligned_str won't be modified.
dnl
dnl "pac_f2c_attr_aligned_str" will be set with
dnl 1) __attribute__((aligned(ALIGNMENT_SIZE))),
dnl 2) __attribute__((aligned)).
dnl 3) "", i.e. empty string.
dnl
dnl 2) means the test can't find a good alignment value, but both the Fortran
dnl    and C compilers are OK with "aligned" which in principle means the C
dnl    compiler will pick the maximum useful alignment supported by the
dnl    architecture.
dnl 3) means that the test has failed to find the alignment.
dnl
AC_DEFUN([PAC_F2C_ATTR_ALIGNED_SIZE],[
cmblksize=$1
AC_MSG_CHECKING([the minimum alignment of Fortran common block of $cmblksize integers])
dnl To find the minmium alignment of Fortran common block (of integer array)
dnl as seen by C object file, C object files of various (typical) alignments
dnl are linked to the Fortran code using the common block of integer array.
#
dnl Since the incorrect alignment results only a warning messages from the
dnl fortran compiler(or linker), so we use "diff" to compare the fortran
dnl compiler/linker output.  We cannot use AC_LANG_WERROR,
dnl i.e. ac_fc_werror_flag=yes, because compiler like pgf77 at version 10.x)
dnl has non-zero stderr output if a fortran program is used in the linking.
dnl The stderr contains the name of fortran program even if the linking is
dnl successful.  We could avoid the non-zero stderr output in pgf77 by
dnl compiling everthing into object files and linking all the object files
dnl with pgf77.  Doing that would need us to use AC_TRY_EVAL instead of
dnl AC_LINK_IFELSE, so "diff" approach is used instead.
#
dnl Using diff of compiler(linker) output requires a reference output file
dnl as the base of diff.  The process of creating this reference output file
dnl has to be exactly the same as the testing process, because pgf77 has
dnl the following weird behavour
dnl pgf77 -o ftest ftest.f         => when $?=0 with zero stderr output
dnl pgf77 -o ftest ftest.f dummy.o => when $?=0 with non-zero stderr output.
dnl                                   stderr has "ftest.f:".
dnl 
# First create a fortran CONFTEST which will be used repeatedly.
AC_LANG_PUSH([Fortran]) dnl AC_LANG_PUSH([Fortran 77])
AC_LANG_CONFTEST([
    AC_LANG_SOURCE([
        program fconftest
        integer isize
        parameter (isize=$cmblksize)
        integer status_array(isize)
        common /mpifcmb/ status_array
        save /mpifcmb/
        end
    ])
])
AC_LANG_POP([Fortran]) dnl AC_LANG_POP([Fortran 77])
dnl
dnl
dnl
# Compile a C dummy.$OBJEXT and link with Fortran test program to create
# a reference linker output file, pac_align0.log, as the base of "diff".
AC_LANG_PUSH([C])
AC_COMPILE_IFELSE([AC_LANG_SOURCE([])],[
    cp conftest.$ac_ext pac_conftest.c
    PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
    saved_LIBS="$LIBS"
    LIBS="pac_conftest.$OBJEXT $LIBS"
    AC_LANG_PUSH([Fortran]) dnl AC_LANG_PUSH([Fortran 77])
    saved_ac_link="$ac_link"
    ac_link="`echo $saved_ac_link | sed -e 's|>.*$|> $pac_logfile 2>\&1|g'`"
    pac_logfile="pac_align0.log"
    rm -f $pac_logfile
    AC_LINK_IFELSE([],[
        pac_f2c_alignedn_diffbase=yes
    ],[
        pac_f2c_alignedn_diffbase=no
    ])
    # Be sure NOT to remove the conftest.f which is still needed for later use.
    # rm -f conftest.$ac_ext 
    # Restore everything in autoconf that has been overwritten
    ac_link="$saved_ac_link"
    # restore previously saved LIBS
    LIBS="$saved_LIBS"
    AC_LANG_POP([Fortran]) dnl AC_LANG_POP([Fortran 77])
],[
    pac_f2c_alignedn_diffbase=no
])
AC_LANG_POP([C])
dnl
dnl
if test "$pac_f2c_alignedn_diffbase" = "yes" ; then
    # Initialize pac_result_str to empty string since part of the test
    # depends on pac_result_str is empty or non-empty string.
    pac_result_str=""
    # Initialize pac_f2c_attr_aligned_str to empty string and
    # it will remain as empty string if the following test fails.
    pac_f2c_attr_aligned_str=""
    for asize in 4 8 16 32 64 128 max ; do
        if test "$asize" != "max" ; then
            pac_attr_aligned_str="__attribute__((aligned($asize)))"
        else
            pac_attr_aligned_str="__attribute__((aligned))"
        fi
        AC_LANG_PUSH([C])
        #Compile the __attribute__ object file.
        AC_COMPILE_IFELSE([
            AC_LANG_SOURCE([
changequote(,)
struct mpif_cmblk_t_ { $MPI_FINT imembers[$cmblksize]; };
changequote([,])
typedef struct mpif_cmblk_t_ mpif_cmblk_t;
mpif_cmblk_t mpifcmbr $pac_attr_aligned_str = {0};

extern mpif_cmblk_t _CMPIFCMB  __attribute__ ((alias("mpifcmbr")));
extern mpif_cmblk_t   MPIFCMB  __attribute__ ((alias("mpifcmbr")));
extern mpif_cmblk_t   MPIFCMB_ __attribute__ ((alias("mpifcmbr")));
extern mpif_cmblk_t _Cmpifcmb  __attribute__ ((alias("mpifcmbr")));
extern mpif_cmblk_t   mpifcmb  __attribute__ ((alias("mpifcmbr")));
extern mpif_cmblk_t   mpifcmb_ __attribute__ ((alias("mpifcmbr")));
            ])
        ],[
            cp conftest.$ac_ext pac_conftest.c
            PAC_RUNLOG([mv conftest.$OBJEXT pac_conftest.$OBJEXT])
            saved_LIBS="$LIBS"
            LIBS="pac_conftest.$OBJEXT $LIBS"
            AC_LANG_PUSH([Fortran]) dnl AC_LANG_PUSH([Fortran 77])
            saved_ac_link="$ac_link"
            ac_link="`echo $saved_ac_link | sed -e 's|>.*$|> $pac_logfile 2>\&1|g'`"
            pac_logfile="pac_align1.log"
            rm -f $pac_logfile
            # Use conftest.f created in CONFTEST.
            AC_LINK_IFELSE([],[
                PAC_RUNLOG_IFELSE([diff -b pac_align0.log pac_align1.log],[
                    pac_attr_alignedn=yes
                ],[
                    pac_attr_alignedn=no
                    cat $pac_logfile >&AS_MESSAGE_LOG_FD
                    echo "failed C program was:" >&AS_MESSAGE_LOG_FD
                    cat pac_conftest.c >&AS_MESSAGE_LOG_FD
                    echo "failed Fortran program was:" >&AS_MESSAGE_LOG_FD
                    cat conftest.$ac_ext >&AS_MESSAGE_LOG_FD
                ])
            ],[
                pac_attr_alignedn=no
            ])
            # Restore everything in autoconf that has been overwritten
            ac_link="$saved_ac_link"
            # restore previously saved LIBS
            LIBS="$saved_LIBS"
            AC_LANG_POP([Fortran]) dnl AC_LANG_POP([Fortran 77])
            # remove previously generated object file and C file.
            rm -f pac_conftest.$OBJEXT pac_conftest.c
            rm -f $pac_logfile
            if test "$pac_attr_alignedn" = yes ; then
                ifelse([$3],[],[
                    pac_result_str="$asize"
                    pac_f2c_attr_aligned_str="$pac_attr_aligned_str"
                    break
                ],[
                    if test "$asize" != "max" -a "$asize" -lt "$3" ; then
                        if test "X$pac_result_str" = "X" ; then
                            pac_result_str="$asize"
                            pac_f2c_attr_aligned_str="$pac_attr_aligned_str"
                        fi
                        continue
                    else
                        pac_f2c_attr_aligned_str="$pac_attr_aligned_str"
                        if test "X$pac_result_str" != "X" ; then
                            pac_result_str="$pac_result_str, too small! reset to $asize"
                        else
                            pac_result_str="$asize"
                        fi
                        break
                    fi
                ])
            fi
        ], [
            pac_attr_alignedn=no
        ])
        AC_LANG_POP([C])
    done
    ifelse([$2],[],[],[$2="$pac_f2c_attr_aligned_str"])
else
    pac_result_str=""
    # Since the test fails, set pac_f2c_attr_aligned_str to empty string.
    pac_f2c_attr_aligned_str=""
fi
if test "X$pac_result_str" != "X" ; then
    AC_MSG_RESULT([$pac_result_str])
else
    AC_MSG_RESULT([unknown])
fi
rm -f pac_align0.log
])
dnl
