dnl
dnl This files contains additional macros for using autoconf to 
dnl build configure scripts.
dnl
dnl Almost all of this file is taken from the aclocal.m4 of MPICH
dnl
dnl Get the format of Fortran names.  Uses F77, FFLAGS, and sets WDEF.
dnl If the test fails, sets NOF77 to 1, HAVE_FORTRAN to 0.
dnl
dnl
define(PAC_GET_FORTNAMES,[
   rm -f confftest.f confftest.o
   cat > confftest.f <<EOF
       subroutine mpir_init_fop( a )
       integer a
       a = 1
       return
       end
EOF
   $F77 $FFLAGS -c confftest.f > /dev/null 2>&1
   if test ! -s confftest.o ; then
        print_error "Unable to test Fortran compiler"
        print_error "(compiling a test program failed to produce an "
        print_error "object file)."
	NOF77=1
   elif test -z "$FORTRANNAMES" ; then
    if test $arch_CRAY ; then
     # Cray doesn't accept -a ...
     nameform1=`strings confftest.o | grep mpir_init_fop_  | head -1`
     nameform2=`strings confftest.o | grep MPIR_INIT_FOP   | head -1`
     nameform3=`strings confftest.o | grep mpir_init_fop   | head -1`
     nameform4=`strings confftest.o | grep mpir_init_fop__ | head -1`
    else
     nameform1=`strings -a confftest.o | grep mpir_init_fop_  | head -1`
     nameform2=`strings -a confftest.o | grep MPIR_INIT_FOP   | head -1`
     nameform3=`strings -a confftest.o | grep mpir_init_fop   | head -1`
     nameform4=`strings -a confftest.o | grep mpir_init_fop__ | head -1`
    fi
    rm -f confftest.f confftest.o
    if test -n "$nameform4" ; then
	echo "Fortran externals are lower case and have two trailing underscores"
	FORTRANNAMES="FORTRANDOUBLEUNDERSCORE"
    elif test -n "$nameform1" ; then
        # We don't set this in CFLAGS; it is a default case
        echo "Fortran externals have a trailing underscore and are lowercase"
	FORTRANNAMES="FORTRANUNDERSCORE"
    elif test -n "$nameform2" ; then
	echo "Fortran externals are uppercase"     
	FORTRANNAMES="FORTRANCAPS" 
    elif test -n "$nameform3" ; then
	echo "Fortran externals are lower case"
	FORTRANNAMES="FORTRANNOUNDERSCORE"
    else
	print_error "Unable to determine the form of Fortran external names"
	print_error "Make sure that the compiler $F77 can be run on this system"
	print_error "Turning off Fortran (-nof77 being assumed)."
	NOF77=1
    fi
    fi
    if test -n "$FORTRANNAMES" ; then
        WDEF="-D$FORTRANNAMES"
    fi
    ])dnl
dnl
dnl
dnl
dnl PAC_GETWD(varname [, filename ] )
dnl
dnl This is from the aclocal.m4 of MPICH. 
dnl Set varname to current directory.  Use filename (relative to current
dnl directory) if provided to double check.
dnl
dnl Need a way to use "automounter fix" for this.
dnl
define(PAC_GETWD,[
$1=$PWD
if test "${$1}" != "" -a -d "${$1}" ; then 
    if test -r ${$1}/.foo$$ ; then
        /bin/rm -f ${$1}/.foo$$
        /bin/rm -f .foo$$
    fi
    if test -r ${$1}/.foo$$ -o -r .foo$$ ; then
        $1=
    else
        echo "test" > ${$1}/.foo$$
        if test ! -r .foo$$ ; then
            /bin/rm -f ${$1}/.foo$$
            $1=
        else
            /bin/rm -f ${$1}/.foo$$
        fi
    fi
fi
if test "${$1}" = "" ; then
    $1=`pwd | sed -e 's%/tmp_mnt/%/%g'`
fi
dnl
dnl First, test the PWD is sensible
ifelse($2,,,
if test ! -r ${$1}/$2 ; then
    dnl PWD must be messed up
    $1=`pwd`
    if test ! -r ${$1}/$2 ; then
        print_error "Cannot determine the root directory!" 
        exit 1
    fi
    $1=`pwd | sed -e 's%/tmp_mnt/%/%g'`
    if test ! -d ${$1} ; then 
        print_error "Warning: your default path uses the automounter; this may"
        print_error "cause some problems if you use other NFS-connected systems.
"
        $1=`pwd`
    fi
fi)
if test -z "${$1}" ; then
    $1=`pwd | sed -e 's%/tmp_mnt/%/%g'`
    if test ! -d ${$1} ; then 
        print_error "Warning: your default path uses the automounter; this may"
        print_error "cause some problems if you use other NFS-connected systems.
"
        $1=`pwd`
    fi
fi
])
dnl
dnl
dnl PAC_GET_TYPE_SIZE(typename,var_for_size)
dnl
dnl sets var_for_size to the size.  Ignores if the size cannot be determined
dnl (see aclocal.m4 in MPICH)
dnl
define(PAC_GET_TYPE_SIZE,
[Pac_name="$1"
 Pac_varname=`echo "$Pac_name" | sed -e 's/ /_/g' -e 's/\*/star/g'`
eval Pac_testval=\$"${Pac_varname}_len"
if test -z "$Pac_testval" ; then
   changequote(<<,>>)
   define(<<AC_TYPE_NAME>>,translit(CROSS_SIZEOF_$1,[a-z *],[A-Z_P]))dnl
   changequote([,])
   eval Pac_testval=\$"AC_TYPE_NAME"
fi
if test -n "$Pac_testval" ; then
    Pac_CV_NAME=$Pac_testval
else
AC_MSG_CHECKING([for size of $1])
dnl Check for existing size or for CROSS_SIZEOF_name
/bin/rm -f conftestval
AC_TEST_PROGRAM([#include <stdio.h>
main() { 
  FILE *f=fopen("conftestval","w");
  if (!f) exit(1);
  fprintf( f, "%d\n", sizeof($1));
  exit(0);
}],Pac_CV_NAME=`cat conftestval`,Pac_CV_NAME="")
/bin/rm -f conftestval
if test -n "$Pac_CV_NAME" -a "$Pac_CV_NAME" != 0 ; then
    AC_MSG_RESULT($Pac_CV_NAME)
    eval ${Pac_varname}_len=$Pac_CV_NAME
else
    AC_MSG_RESULT(unavailable)
fi
fi
$2=$Pac_CV_NAME
])dnl
dnl
dnl
dnl
define(PAC_INT_LT_POINTER,[
if test -z "$intsize" ; then
    PAC_GET_TYPE_SIZE(int,intsize)
fi
if test -z "$pointersize" ; then
    PAC_GET_TYPE_SIZE(void *,pointersize)
fi
AC_MSG_CHECKING([for int large enough for pointers])
if test -n "$pointersize" -a -n "$intsize" ; then
    if test $pointersize -le $intsize ; then
       AC_MSG_RESULT(yes)
    else
       AC_DEFINE(INT_LT_POINTER,,[Define if int smaller than pointer])
       AC_MSG_RESULT(no)
    fi
else
    AC_MSG_RESULT(cannot determine; assuming it is;)
    echo "use '-intsize' and '-ptrsize' to indicate otherwise"
fi
])dnl
dnl
dnl
dnl Check whether to use -n, \c, or newline-tab to separate
dnl checking messages from result messages.
dnl from MPICH
define(AC_PROG_ECHO_N,
ac_echo_n=yes
[if (echo "testing\c"; echo 1,2,3) | grep c >/dev/null; then
  if (echo -n testing; echo 1,2,3) | sed s/-n/xn/ | grep xn >/dev/null; then
    ac_n= ac_c='
' ac_t='	'
  else
    ac_n=-n ac_c= ac_t=
  fi
else
  ac_n= ac_c='\c' ac_t=
fi
ac_echo_test=`echo foo 1>&1`
if test -z "$ac_echo_test" ; then
     print_error "Your sh shell does not handle the output redirection"
     print_error "1>&1 correctly.  Configure will work around this problem,"
     print_error "but you should report the problem to your vendor."
fi
])dnl
dnl AC_MSG_CHECKING(FEATURE-DESCRIPTION)
define(AC_FD_MSG,1)dnl
define(AC_MSG_CHECKING,[dnl
if test -z "$ac_echo_n" ; then
AC_PROG_ECHO_N
fi
if test -z "$ac_echo_test" -a AC_FD_MSG = 1 ; then
echo $ac_n "checking $1""... $ac_c"
else
echo $ac_n "checking $1""... $ac_c" 1>&AC_FD_MSG
fi])dnl
dnl
dnl AC_MSG(msg)
dnl generates "msg..." (no newline)
define(AC_MSG,[dnl
if test -z "$ac_echo_n" ; then
AC_PROG_ECHO_N
fi
if test -z "$ac_echo_test" -a AC_FD_MSG = 1 ; then
echo $ac_n "$1""... $ac_c"
else
echo $ac_n "$1""... $ac_c" 1>&AC_FD_MSG
fi])dnl
dnl
dnl AC_CHECKING(FEATURE-DESCRIPTION)
define(AC_CHECKING,dnl
[echo "checking $1" 1>&AC_FD_MSG])dnl
dnl
dnl AC_MSG_RESULT(RESULT-DESCRIPTION)
define(AC_MSG_RESULT,dnl
if test -z "$ac_echo_test" -a AC_FD_MSG = 1 ; then
[echo "$ac_t""$1"]
else
[echo "$ac_t""$1" 1>&AC_FD_MSG]
fi)dnl
dnl
define(PAC_GET_SPECIAL_SYSTEM_INFO,[
#
if test -n "$arch_IRIX"; then
   AC_MSG_CHECKING(for IRIX OS version)
   dnl This block of code replaces a generic "IRIX" arch value with
   dnl  IRIX_<version>_<chip>
   dnl  For example
   dnl  IRIX_5_4400 (IRIX 5.x, using MIPS 4400)
   osversion=`uname -r | sed 's/\..*//'`
   dnl Note that we need to allow brackets here, so we briefly turn off 
   dnl the macro quotes
   changequote(,)dnl
   dnl Get the second field (looking for 6.1)
   osvminor=`uname -r | sed 's/[0-9]\.\([0-9]*\)\..*/\1/'`
   AC_MSG_RESULT($osversion)
   dnl Get SGI processor count by quick hack
   AC_MSG_CHECKING(for IRIX cpucount)
   cpucount=`hinv | grep '[0-9]* [0-9]* MHZ IP[0-9]* Proc' | cut -f 1 -d' '`
   if test "$cpucount" = "" ; then
     cpucount=`hinv | grep 'Processor [0-9]*:' | wc -l | sed -e 's/ //g'`
   fi
   changequote([,])dnl
   if test "$cpucount" = "" ; then
     print_error "Could not determine cpucount."
     print_error "Please send "
     hinv
     print_error "to romio-maint@mcs.anl.gov"
     exit 1
   fi
   AC_MSG_RESULT($cpucount)
   dnl
   AC_MSG_CHECKING(for IRIX cpumodel)
   dnl The tail -1 is necessary for multiple processor SGI boxes
   dnl We might use this to detect SGI multiprocessors and recommend
   dnl -comm=shared
   cputype=`hinv -t cpu | tail -1 | cut -f 3 -d' '`
   if test -z "$cputype" ; then
        print_error "Could not get cputype from hinv -t cpu command."
        print_error "Please send "
        hinv -t cpu 2>&1
        hinv -t cpu | cut -f 3 -d' ' 2>&1
        print_error "to romio-maint@mcs.anl.gov" 
        exit 1
   fi
   AC_MSG_RESULT($cputype)
   dnl echo "checking for osversion and cputype"
   dnl cputype may contain R4400, R2000A/R3000, or something else.  
   dnl We may eventually need to look at it.
   if test -z "$osversion" ; then
        print_error "Could not determine OS version.  Please send" 
        print_error " " 
        uname -a
        print_error "to romio-maint@mcs.anl.gov" 
        exit 1
   elif test $osversion = 4 ; then
        true
   elif test $osversion = 5 ; then
        true
   elif test $osversion = 6 ; then
        true
   else 
       print_error "Could not recognize the version of IRIX (got $osversion)"
       print_error "ROMIO knows about versions 4, 5 and 6; the version being"
       print_error "returned from uname -r is $osversion."
       print_error "Please send"
       uname -a 2>&1
       hinv 2>&1
       print_error "to romio-maint@mcs.anl.gov"
       exit 1
   fi
   AC_MSG_CHECKING(for cputype)
   OLD_ARCH=IRIX
   IRIXARCH="$ARCH_$osversion"
   dnl Now, handle the chip set
   changequote(,)dnl
   cputype=`echo $cputype | sed -e 's%.*/%%' -e 's/R//' | tr -d "[A-Z]"`
   changequote([,])dnl
   case $cputype in 
        3000) ;;
        4000) ;;
        4400) ;;
        4600) ;;
        5000) ;;
        8000) ;;
        10000);;
	12000);;
        *)
        print_error "Unexpected IRIX/MIPS chipset $cputype.  Please send the output"
        print_error " "
        uname -a 2>&1
        hinv 2>&1 
        print_error " " 
        print_error "to romio-maint@mcs.anl.gov" 
        print_error "ROMIO will continue and assume that the cputype is"
        print_error "compatible with a MIPS 4400 processor."
        print_error " " 
        cputype=4400
        ;;
   esac
   AC_MSG_RESULT($cputype)
   IRIXARCH="$IRIXARCH_$cputype"
   echo "IRIX-specific architecture is $IRIXARCH"
fi
])dnl
dnl
dnl
dnl On an SGI check whether to link 32 bit objects or 64 bit objects
dnl for the MPI-2 datatype accessor functions
dnl
define(PAC_CHECK_SGI_3264,[
AC_MSG_CHECKING(for 32-bit or 64-bit objects)
cat <<EOF >bittest.c
main()
{
  int i;
  i = 0;
}
EOF
$CC $CFLAGS -c bittest.c > /dev/null 2>&1
if test $MIPS = 4 ; then
    testlink='$CC $CFLAGS -o bittest bittest.o adio/sgi/mpi2/mips4.64/get_contents.o $MPI_LIB >/dev/null 2>&1'
    if eval $testlink ; then
       BITS=64
    else
        testlink='$CC $CFLAGS -o bittest bittest.o adio/sgi/mpi2/mips4.32/get_contents.o $MPI_LIB >/dev/null 2>&1'
        if eval $testlink ; then
           BITS=32
        else
            echo "Error: Can't link with either 32-bit or 64-bit"
            echo "Send email to romio-maint@mcs.anl.gov"
            exit 1
        fi
    fi
else
    testlink='$CC $CFLAGS -o bittest bittest.o adio/sgi/mpi2/mips3.64/get_contents.o $MPI_LIB >/dev/null 2>&1'
    if eval $testlink ; then
       BITS=64
    else
        testlink='$CC $CFLAGS -o bittest bittest.o adio/sgi/mpi2/mips3.32/get_contents.o $MPI_LIB >/dev/null 2>&1'
        if eval $testlink ; then
           BITS=32
        else
            echo "Error: Can't link with either 32-bit or 64-bit"
            echo "Send email to romio-maint@mcs.anl.gov"
            exit 1
        fi
    fi
fi
rm -f bittest*
AC_MSG_RESULT($BITS bit)
])
dnl
dnl
dnl
define(PAC_TEST_MPI,[
  AC_MSG_CHECKING(if a simple MPI program compiles and links)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
         MPI_Init(&argc,&argv);
         MPI_Finalize(); 
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test ! -x conftest ; then
      echo " "
      print_error "Unable to compile a simple MPI program"
      print_error "Use the -mpi, -mpiincdir, and -mpilib options to configure to specify the"
      print_error "MPI implementation, the include path for mpi.h, and the MPI library to link"
      rm -f conftest mpitest.c
      exit 1
  else
      rm -f conftest mpitest.c
  fi
AC_MSG_RESULT(yes)
])dnl
dnl
dnl
dnl
define(PAC_NEEDS_FINT,[
  AC_MSG_CHECKING(if MPI_Fint is defined in the MPI implementation)
  cat > mpitest1.c <<EOF
#include "mpi.h"
     main()
     {
         MPI_Fint i;
         i = 0;
     }
EOF
  rm -f mpitest1.o
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -c mpitest1.c > /dev/null 2>&1
  if test ! -s mpitest1.o ; then
      NEEDS_MPI_FINT="#define NEEDS_MPI_FINT"
      CFLAGS="$CFLAGS -DNEEDS_MPI_FINT"
      AC_MSG_RESULT(no)
      rm -f mpitest1.o mpitest1.c
  else
      NEEDS_MPI_FINT=""
      AC_MSG_RESULT(yes)
      rm -f mpitest1.o mpitest1.c
  fi
])dnl
dnl
dnl
dnl
define(PAC_LONG_64,[
if test -z "$longsize" ; then
    PAC_GET_TYPE_SIZE(long,longsize)
fi
if test -n "$longsize" ; then
   if test $longsize = 8 ; then
       AC_DEFINE(HAVE_LONG_64,,[Define if long is 64 bits])
   fi
else
   echo "assuming size of long is NOT 8 bytes; use '-longsize' to indicate otherwise"
fi
])dnl
dnl
dnl
define(PAC_MPI_LONG_LONG_INT,[
  AC_MSG_CHECKING(if MPI_LONG_LONG_INT is defined in mpi.h)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
         long long i;   
         MPI_Init(&argc,&argv);
         MPI_Send(&i, 1, MPI_LONG_LONG_INT, 0, 0, MPI_COMM_WORLD);
         MPI_Finalize(); 
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_MPI_LONG_LONG_INT,,[Define if mpi has long long it])
  else
      AC_MSG_RESULT(no)
  fi
  rm -f conftest mpitest.c
])dnl
dnl
dnl Check that the compile accepts ANSI prototypes. 
dnl PAC_CHECK_CC_PROTOTYPES()
dnl
define(PAC_CHECK_CC_PROTOTYPES,[
AC_MSG_CHECKING(that the compiler $CC accepts ANSI prototypes)
AC_COMPILE_CHECK(,[int f(double a){return 0;}],,eval "ac_cv_ccworks=yes",eval "ac_cv_ccworks=no")
AC_MSG_RESULT($ac_cv_ccworks)
if test $ac_cv_ccworks = "yes" ; then
   AC_DEFINE(HAVE_PROTOTYPES,,[Define if C compiler supports prototypes])
fi
])dnl
dnl
dnl
dnl PAC_TEST_LONG_LONG()
dnl
dnl tests if the compiler prints long long correctly and whether to use
dnl %ld or %lld. Called from within PAC_LONG_LONG_64.
dnl
define(PAC_TEST_LONG_LONG,
[AC_MSG_CHECKING([if the compiler prints long longs correctly with %lld])
rm -f conftestll
AC_TEST_PROGRAM([#include <stdio.h>
main() {
  long long i=8; 
  FILE *f=fopen("conftestll","w");
  if (!f) exit(1);
  fprintf( f, "%lld\n", i);
  exit(0);
}],Pac_CV_NAME=`cat conftestll`,Pac_CV_NAME="")
rm -f conftestll
if test "$Pac_CV_NAME" = 8 ; then
    AC_MSG_RESULT(yes)
    AC_DEFINE(HAVE_LONG_LONG_64,,[Define if have 64 bit long long])
    DEFINE_MPI_OFFSET="typedef long long MPI_Offset;"
    FORTRAN_MPI_OFFSET="integer*8"
    echo "defining MPI_Offset as long long in C and integer*8 in Fortran"
    LL="\%lld"
else
    AC_MSG_RESULT(no)
    AC_MSG_CHECKING([if the compiler prints long longs correctly with %ld])
    AC_TEST_PROGRAM([#include <stdio.h>
    main() {
      long long i=8; 
      FILE *f=fopen("conftestll","w");
      if (!f) exit(1);
      fprintf( f, "%ld\n", i);
      exit(0);
    }],Pac_CV_NAME=`cat conftestll`,Pac_CV_NAME="")
    rm -f conftestll
    if test "$Pac_CV_NAME" = 8 ; then
       AC_MSG_RESULT(yes)
       AC_DEFINE(HAVE_LONG_LONG_64,,[Define if long long is 64 bits])
       DEFINE_MPI_OFFSET="typedef long long MPI_Offset;"
       FORTRAN_MPI_OFFSET="integer*8"
       echo "defining MPI_Offset as long long in C and integer*8 in Fortran"
       LL="\%ld"
    else
       AC_MSG_RESULT(no!!)
       echo "the compiler doesn't print long longs correctly!"
       echo "defining MPI_Offset as long in C and integer in Fortran" 
       DEFINE_MPI_OFFSET="typedef long MPI_Offset;"
       FORTRAN_MPI_OFFSET="integer"
       LL="\%ld"
       MPI_OFFSET_KIND1="!"
       MPI_OFFSET_KIND2="!"
    fi
fi
])dnl
dnl
dnl
dnl PAC_LONG_LONG_64: check if there is a 64-bit long long
dnl
define(PAC_LONG_LONG_64,[
if test -n "$longlongsize" ; then
    if test "$longlongsize" = 8 ; then
       echo "defining MPI_Offset as long long in C and integer*8 in Fortran" 
       AC_DEFINE(HAVE_LONG_LONG_64,,[Define if long long is 64 bits])
       DEFINE_MPI_OFFSET="typedef long long MPI_Offset;"
       FORTRAN_MPI_OFFSET="integer*8"
       LL="\%lld"
    elif test "$longlongsize" = "int" ; then  # a hack to set MPI_Offset as int
       echo "defining MPI_Offset as int in C and integer in Fortran"
       DEFINE_MPI_OFFSET="typedef int MPI_Offset;"
       FORTRAN_MPI_OFFSET="integer"
       AC_DEFINE(MPI_OFFSET_IS_INT,,[Define if MPI_Offset is int])
       LL="\%d"
       MPI_OFFSET_KIND1="!"
       MPI_OFFSET_KIND2="!"
    else 
       echo "defining MPI_Offset as long in C and integer in Fortran" 
       DEFINE_MPI_OFFSET="typedef long MPI_Offset;"
       FORTRAN_MPI_OFFSET="integer"
       LL="\%ld"
       MPI_OFFSET_KIND1="!"
       MPI_OFFSET_KIND2="!"
    fi
else
   PAC_GET_TYPE_SIZE(long long, longlongsize)
   if test -n "$longlongsize" ; then
      if test "$longlongsize" = 8 ; then
         PAC_TEST_LONG_LONG()
      else
         echo "defining MPI_Offset as long in C and integer in Fortran" 
         DEFINE_MPI_OFFSET="typedef long MPI_Offset;"
         FORTRAN_MPI_OFFSET="integer"
         LL="\%ld"
         MPI_OFFSET_KIND1="!"
         MPI_OFFSET_KIND2="!"
      fi
   else 
dnl   check if longlong is not supported or only its size cannot be determined
dnl   because the program cannot be run.
      rm -f ltest.c
      cat > ltest.c <<EOF
        main()
        {
           long long i=8;
           return 0;
        }
EOF
      rm -f conftest
      $CC $USER_CFLAGS -o conftest ltest.c > /dev/null 2>&1
      if test -x conftest ; then
         echo "assuming size of long long is 8bytes; use '-longlongsize' to indicate otherwise"
         rm -f conftest ltest.c
         echo "defining MPI_Offset as long long in C and integer*8 in Fortran" 
         AC_DEFINE(HAVE_LONG_LONG_64,,[Define if long long is 64 bits])
         DEFINE_MPI_OFFSET="typedef long long MPI_Offset;"
         FORTRAN_MPI_OFFSET="integer*8"
         LL="\%lld"
      else 
         echo "assuming long long is not available; use '-longlongsize' to indicate otherwise"
         echo "defining MPI_Offset as long in C and integer in Fortran" 
         DEFINE_MPI_OFFSET="typedef long MPI_Offset;"
         FORTRAN_MPI_OFFSET="integer"
         LL="\%ld"
         MPI_OFFSET_KIND1="!"
         MPI_OFFSET_KIND2="!"
      fi
   fi
fi
])dnl
dnl
dnl
define(PAC_MPI_INFO,[
  AC_MSG_CHECKING(if MPI_Info functions are defined in the MPI implementation)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
         MPI_Info info;
         MPI_Init(&argc,&argv);
         MPI_Info_create(&info);
         MPI_Finalize(); 
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_MPI_INFO,,[Define if MPI_Info available])
      HAVE_MPI_INFO="#define HAVE_MPI_INFO"
      MPI_FINFO1="!"
      MPI_FINFO2="!"
      MPI_FINFO3="!"
      MPI_FINFO4="!"
  else
      AC_MSG_RESULT(no)
      BUILD_MPI_INFO=1
      MPI_FINFO1="      INTEGER MPI_MAX_INFO_KEY, MPI_MAX_INFO_VAL"
      MPI_FINFO2="      PARAMETER (MPI_MAX_INFO_KEY=255, MPI_MAX_INFO_VAL=1024)"
      MPI_FINFO3="      INTEGER MPI_INFO_NULL"
      MPI_FINFO4="      PARAMETER (MPI_INFO_NULL=0)"
  fi
  rm -f conftest mpitest.c
])dnl
dnl
dnl
define(PAC_MPI_DARRAY_SUBARRAY,[
  AC_MSG_CHECKING(if darray and subarray constructors are defined in the MPI implementation)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
         int i=MPI_DISTRIBUTE_CYCLIC;
         MPI_Datatype t;
         MPI_Init(&argc,&argv);
         MPI_Type_create_darray(i, i, i, &i, &i, &i, &i, i, MPI_INT, &t);
         MPI_Type_create_subarray(i, &i, &i, &i, i, MPI_INT, &t);
         MPI_Finalize(); 
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_MPI_DARRAY_SUBARRAY,,[Define if MPI Darray available])
      HAVE_MPI_DARRAY_SUBARRAY="#define HAVE_MPI_DARRAY_SUBARRAY"
      MPI_FARRAY1="!"
      MPI_FARRAY2="!"
      MPI_FARRAY3="!"
      MPI_FARRAY4="!"
      MPI_FARRAY5="!"
      MPI_FARRAY6="!"
      MPI_FARRAY7="!"
  else
      AC_MSG_RESULT(no)
      BUILD_MPI_ARRAY=1
      MPI_FARRAY1="      INTEGER MPI_ORDER_C, MPI_ORDER_FORTRAN"
      MPI_FARRAY2="      PARAMETER (MPI_ORDER_C=56, MPI_ORDER_FORTRAN=57)"
      MPI_FARRAY3="      INTEGER MPI_DISTRIBUTE_BLOCK, MPI_DISTRIBUTE_CYCLIC"
      MPI_FARRAY4="      INTEGER MPI_DISTRIBUTE_NONE, MPI_DISTRIBUTE_DFLT_DARG"
      MPI_FARRAY5="      PARAMETER (MPI_DISTRIBUTE_BLOCK=121, MPI_DISTRIBUTE_CYCLIC=122)"
      MPI_FARRAY6="      PARAMETER (MPI_DISTRIBUTE_NONE=123)"
      MPI_FARRAY7="      PARAMETER (MPI_DISTRIBUTE_DFLT_DARG=-49767)"
  fi
  rm -f conftest mpitest.c
])dnl
dnl
dnl
define(PAC_CHECK_MPI_SGI_INFO_NULL,[
  AC_MSG_CHECKING(if MPI_INFO_NULL is defined in mpi.h)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
	int i;
	i = MPI_INFO_NULL;
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
      AC_MSG_RESULT(yes)
      cp adio/sgi/mpi3.1/*.h include
  else
      AC_MSG_RESULT(no)
  fi
  rm -f conftest mpitest.c
])dnl
dnl
dnl
dnl
define(PAC_CHECK_MPIOF_H,[
  AC_MSG_CHECKING(if mpiof.h is included in mpif.h)
  rm -f mpitest.f
  cat > mpitest.f <<EOF
      program main
      implicit none
      include 'mpif.h'
      integer i
      i = MPI_MODE_RDWR
      stop
      end
EOF
  rm -f conftest
  $F77 $FFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.f $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
      AC_MSG_RESULT(yes)
      MPIOF_H_INCLUDED=1
  else
      AC_MSG_RESULT(no)
  fi
  rm -f conftest mpitest.f
])dnl
dnl
dnl
dnl check if pread64 is defined in IRIX. needed on IRIX 6.5
dnl
define(PAC_HAVE_PREAD64,[
  AC_MSG_CHECKING(if pread64 is defined)
  rm -f conftest.c
  cat > conftest.c <<EOF
#include <unistd.h>
     main()
     {
         int fd=0, buf=0, i=0;
         off64_t off=0;
         pread64(fd, &buf, i, off);
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -o conftest conftest.c > /dev/null 2>&1
  if test -x conftest ; then
      AC_MSG_RESULT(yes)
      AC_DEFINE(HAVE_PREAD64,,[Define if pread64 available])
  else
      AC_MSG_RESULT(no)
  fi
rm -f conftest conftest.c
])dnl
dnl
dnl
define(PAC_TEST_MPI_SGI_type_is_contig,[
  AC_MSG_CHECKING(if MPI_SGI_type_is_contig is defined)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
         MPI_Datatype type;
         int i;

         MPI_Init(&argc,&argv);
         i = MPI_SGI_type_is_contig(type);
         MPI_Finalize(); 
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
     AC_MSG_RESULT(yes)
  else
     AC_MSG_RESULT(no)
     AC_DEFINE(NO_MPI_SGI_type_is_contig,,[Define if no MPI type is contig])
  fi
  rm -f conftest mpitest.c
])dnl
dnl
dnl
dnl
define(PAC_TEST_MPI_COMBINERS,[
  AC_MSG_CHECKING(if MPI-2 combiners are defined in mpi.h)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
         int i;

         MPI_Init(&argc,&argv);
         i = MPI_COMBINER_STRUCT;
         MPI_Finalize(); 
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
     AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_MPI_COMBINERS,,[Define if MPI combiners available])
  else
     AC_MSG_RESULT(no)
  fi
  rm -f conftest mpitest.c
])dnl
dnl
dnl
dnl PAC_MPI_OFFSET_KIND()
dnl
dnl tries to determine the Fortran 90 kind parameter for 8-byte integers
dnl
define(PAC_MPI_OFFSET_KIND,
[AC_MSG_CHECKING([for Fortran 90 KIND parameter for 8-byte integers])
rm -f kind.f kind.o kind
cat <<EOF > kind.f
      program main
      integer i
      i = selected_int_kind(16)
      open(8, file="k.out", form="formatted")
      write (8,*) i
      close(8)
      stop
      end
EOF
if test -z "$F90" ; then
   F90=f90
fi
KINDVAL=""
if $F90 -o kind kind.f >/dev/null 2>&1 ; then
    ./kind >/dev/null 2>&1
    if test -s k.out ; then 
        KINDVAL=`cat k.out`
    fi
fi
rm -f kind k.out kind.f kind.o k.out
if test -n "$KINDVAL" -a "$KINDVAL" != "-1" ; then
   AC_MSG_RESULT($KINDVAL)
   MPI_OFFSET_KIND1="      INTEGER MPI_OFFSET_KIND"
   MPI_OFFSET_KIND2="      PARAMETER (MPI_OFFSET_KIND=$KINDVAL)"
else
    AC_MSG_RESULT(unavailable)
fi
])dnl
dnl
dnl
define(PAC_TEST_MPI_HAVE_OFFSET_KIND,[
  AC_MSG_CHECKING(if MPI_OFFSET_KIND is defined in mpif.h)
  rm -f mpitest.f
  cat > mpitest.f <<EOF
      program main
      implicit none
      include 'mpif.h'
      integer i
      i = MPI_OFFSET_KIND
      stop
      end
EOF
  rm -f conftest
  $F77 $FFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.f $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
     AC_MSG_RESULT(yes)
     MPI_OFFSET_KIND1="!"
     MPI_OFFSET_KIND2="!"
  else
     AC_MSG_RESULT(no)
  fi
  rm -f conftest mpitest.f
])dnl
dnl
dnl
dnl PAC_GET_XFS_MEMALIGN
dnl 
dnl
define(PAC_GET_XFS_MEMALIGN,
[AC_MSG_CHECKING([for memory alignment needed for direct I/O])
/bin/rm -f memalignval
/bin/rm -f /tmp/romio_tmp.bin
AC_TEST_PROGRAM([#include <stdio.h>
#include <unistd.h>
#include <fcntl.h>
#include <stdio.h>
main() { 
  struct dioattr st;
  int fd = open("/tmp/romio_tmp.bin", O_RDWR | O_CREAT, 0644);
  FILE *f=fopen("memalignval","w");
  if (fd == -1) exit(1);
  if (!f) exit(1);
  fcntl(fd, F_DIOINFO, &st);
  fprintf( f, "%u\n", st.d_mem);
  exit(0);
}],Pac_CV_NAME=`cat memalignval`,Pac_CV_NAME="")
/bin/rm -f memalignval
/bin/rm -f /tmp/romio_tmp.bin
if test -n "$Pac_CV_NAME" -a "$Pac_CV_NAME" != 0 ; then
    AC_MSG_RESULT($Pac_CV_NAME)
    CFLAGS="$CFLAGS -DXFS_MEMALIGN=$Pac_CV_NAME"
else
    AC_MSG_RESULT(unavailable, assuming 128)
    CFLAGS="$CFLAGS -DXFS_MEMALIGN=128"
fi
])dnl
dnl
dnl
dnl Look for a style of VPATH.  Known forms are
dnl VPATH = .:dir
dnl .PATH: . dir
dnl
dnl Defines VPATH or .PATH with . $(srcdir)
dnl Requires that vpath work with implicit targets
dnl NEED TO DO: Check that $< works on explicit targets.
dnl
define(PAC_MAKE_VPATH,[
AC_SUBST(VPATH)
AC_MSG_CHECKING(for virtual path format)
rm -rf conftest*
mkdir conftestdir
cat >conftestdir/a.c <<EOF
A sample file
EOF
cat > conftest <<EOF
all: a.o
VPATH=.:conftestdir
.c.o:
	@echo \$<
EOF
ac_out=`$MAKE -f conftest 2>&1 | grep 'conftestdir/a.c'`
if test -n "$ac_out" ; then 
    AC_MSG_RESULT(VPATH)
    VPATH='VPATH=.:$(srcdir)'
else
    rm -f conftest
    cat > conftest <<EOF
all: a.o
.PATH: . conftestdir
.c.o:
	@echo \$<
EOF
    ac_out=`$MAKE -f conftest 2>&1 | grep 'conftestdir/a.c'`
    if test -n "$ac_out" ; then 
        AC_MSG_RESULT(.PATH)
        VPATH='.PATH: . $(srcdir)'
    else
	AC_MSG_RESULT(neither VPATH nor .PATH works)
    fi
fi
rm -rf conftest*
])dnl
dnl
dnl
dnl There is a bug in AC_PREPARE that sets the srcdir incorrectly (it
dnl is correct in configure, but it puts an absolute path into config.status,
dnl which is a big problem for scripts like mpireconfig that are wrappers
dnl around config.status).  The bug is in not recognizing that ./ and .//
dnl are the same  directory as . (in fact, ./[/]* is the same).
dnl
define(PAC_FIXUP_SRCDIR,[
# Find the source files, if location was not specified.
if test "$srcdirdefaulted" = "yes" ; then
  srcdir=""
  # Try the directory containing this script, then `..'.
  prog=[$]0
changequote(,)dnl
  confdir=`echo $prog|sed 's%/[^/][^/]*$%%'`
  # Remove all trailing /'s 
  confdir=`echo $confdir|sed 's%[/*]$%%'`
changequote([,])dnl
  test "X$confdir" = "X$prog" && confdir=.
  srcdir=$confdir
  if test ! -r $srcdir/$unique_file; then
    srcdir=..
  fi
fi
if test ! -r $srcdir/$unique_file; then
  if test x$srcdirdefaulted = xyes; then
    echo "configure: Cannot find sources in \`${confdir}' or \`..'." 1>&2
  else
    echo "configure: Cannot find sources in \`${srcdir}'." 1>&2
  fi
  exit 1
fi
# Preserve a srcdir of `.' to avoid automounter screwups with pwd.
# (and preserve ./ and .//)
# But we can't avoid them for `..', to make subdirectories work.
case $srcdir in
  .|./|.//|/*|~*) ;;
  *) srcdir=`cd $srcdir; pwd` ;; # Make relative path absolute.
esac
])
dnl
dnl
dnl AC_TRY_LINK(INCLUDES, FUNCTION-BODY,
dnl             ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND])
define(AC_TRY_LINK,
if test -z "$ac_ext" ; then 
    ac_ext=c
fi
[cat > conftest.$ac_ext <<EOF
dnl This sometimes fails to find confdefs.h, for some reason.
dnl [#]line __oline__ "[$]0"
dnl [#]line __oline__ "configure"
#include "confdefs.h"
[$1]
int main() { return 0; }
int t() {
[$2]
; return 0; }
EOF
rm -f conftest.out
if test -z "$ac_link" ; then
ac_link='${CC-cc} -o conftest $CFLAGS $CPPFLAGS $LDFLAGS conftest.$ac_ext $LIBS >conftest.out 2>&1'
fi
if eval $ac_link; then
  ifelse([$3], , :, [rm -rf conftest*
  $3])
else
  if test -s conftest.out ; then cat conftest.out >> config.log ; fi
ifelse([$4], , , [rm -rf conftest*
  $4
])dnl
fi
rm -f conftest*]
)dnl
dnl
dnl
define(PAC_HAVE_MOUNT_NFS,[
  AC_MSG_CHECKING([if MOUNT_NFS is defined in the include files])
  rm -f conftest.c
  cat > conftest.c <<EOF
#include <sys/param.h>
#include <sys/mount.h>
     main()
     {
         int i=MOUNT_NFS;
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -o conftest conftest.c > /dev/null 2>&1
  if test -x conftest ; then
     AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_MOUNT_NFS,,[Define if MOUNT_NFS defined])
  else
     AC_MSG_RESULT(no)
  fi
  rm -f conftest conftest.c
])dnl
dnl
dnl
dnl PAC_MPI_OFFSET_KIND_4BYTE()
dnl
dnl tries to determine the Fortran 90 kind parameter for 4-byte integers
dnl
define(PAC_MPI_OFFSET_KIND_4BYTE,
[AC_MSG_CHECKING([for Fortran 90 KIND parameter for 4-byte integers])
rm -f kind.f kind.o kind
cat <<EOF > kind.f
      program main
      integer i
      i = selected_int_kind(8)
      open(8, file="k.out", form="formatted")
      write (8,*) i
      close(8)
      stop
      end
EOF
if test -z "$F90" ; then
   F90=f90
fi
KINDVAL=""
if $F90 -o kind kind.f >/dev/null 2>&1 ; then
    ./kind >/dev/null 2>&1
    if test -s k.out ; then 
        KINDVAL=`cat k.out`
    fi
fi
rm -f kind k.out kind.f kind.o
if test -n "$KINDVAL" -a "$KINDVAL" != "-1" ; then
   AC_MSG_RESULT($KINDVAL)
   MPI_OFFSET_KIND1="      INTEGER MPI_OFFSET_KIND"
   MPI_OFFSET_KIND2="      PARAMETER (MPI_OFFSET_KIND=$KINDVAL)"
else
    AC_MSG_RESULT(unavailable)
fi
])dnl
dnl
dnl
define(PAC_FUNC_STRERROR,[
  AC_MSG_CHECKING([for strerror()])
  rm -f conftest.c
  cat > conftest.c <<EOF
#include <string.h>
     main()
     {
        char *s = strerror(5);
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -o conftest conftest.c >> config.log 2>&1
  if test -x conftest ; then
     AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_STRERROR,,[Define if strerror available])
  else
     AC_MSG_RESULT(no)
     AC_MSG_CHECKING([for sys_errlist])
     rm -f conftest.c
changequote(,)
     cat > conftest.c <<EOF
#include <stdio.h>
        main()
        {
           extern char *sys_errlist[];
	   printf("%s\n", sys_errlist[34]);
        }
EOF
changequote([,])
     rm -f conftest
     $CC $USER_CFLAGS -o conftest conftest.c > config.log 2>&1
     if test -x conftest ; then
        AC_MSG_RESULT(yes)
        AC_DEFINE(HAVE_SYSERRLIST,,[Define if syserrlist available])
     else
        AC_MSG_RESULT(no)
     fi
  fi
  rm -f conftest conftest.c
])dnl
dnl
dnl
dnl
define(PAC_C_INLINE,[
AC_MSG_CHECKING([for inline])
if eval "test \"`echo '$''{'pac_cv_c_inline'+set}'`\" = set"; then
   AC_MSG_RESULT([(cached)])
else
  AC_COMPILE_CHECK(,[inline int a( int b ){return b+1;}],[int a;],
pac_cv_c_inline="yes",pac_cv_c_inline="no")
fi
AC_MSG_RESULT($pac_cv_c_inline)
if test "$pac_cv_c_inline" = "no" ; then
    AC_DEFINE(inline,,[Define if inline is not supported])
fi
])dnl
define(AC_MSG_WARN,[AC_MSG_RESULT([Warning: $1])])
dnl
dnl PAC_CHECK_HEADER(HEADER-FILE, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND],
dnl PRE-REQ-HEADERS )
dnl
dnl BUG: AIX 4.1 can't handle a \055 (octal for -) in a tr string (sometimes;
dnl it works from the shell but not within a file)
dnl I've removed that and hoped that no header will include a - in the
dnl name
dnl
dnl This can fail if the header needs OTHER headers for the compile
dnl to succeed.  Those headers should be specified in the "pre-req-headers"
dnl For example 
dnl PAC_CHECK_HEADER(sys/vfs.h,AC_DEFINE(HAVE_SYS_VFS_H),,
dnl                  [#include <sys/types.h>])
dnl
define(PAC_CHECK_HEADER,dnl
[dnl Do the transliteration at runtime so arg 1 can be a shell variable.
changequote(,)dnl
ac_safe=`echo "$1" | tr '[a-z]./' '[A-Z]__'`
changequote([,])dnl
AC_MSG_CHECKING([for $1])
dnl AC_CACHE_VAL(ac_cv_header_$ac_safe,[dnl
AC_COMPILE_CHECK(,[$4]
[#include <$1>],main();,eval "ac_cv_header_$ac_safe=yes",
  eval "ac_cv_header_$ac_safe=no")dnl])dnl
if eval "test \"`echo '$ac_cv_header_'$ac_safe`\" = yes"; then
  AC_MSG_RESULT(yes)
  ifelse([$2], , :, [$2])
else
  AC_MSG_RESULT(no)
ifelse([$3], , , [$3
])dnl
fi
])dnl
dnl
dnl PAC_CHECK_HEADERS(HEADER-FILE... [, ACTION-IF-FOUND [, ACTION-IF-NOT-FOUND]])
define(PAC_CHECK_HEADERS,[for ac_hdr in $1
do
PAC_CHECK_HEADER($ac_hdr,
[changequote(, )dnl
  ac_tr_hdr=HAVE_`echo $ac_hdr | tr '[a-z]./' '[A-Z]__'`
changequote([, ])dnl
  AC_DEFINE($ac_tr_hdr) $2], $3)dnl
done
])dnl
dnl
define(PAC_TEST_MPIR_STATUS_SET_BYTES,[
  AC_MSG_CHECKING(if MPIR_Status_set_bytes is defined)
  rm -f mpitest.c
  cat > mpitest.c <<EOF
#include "mpi.h"
     main(int argc, char **argv)
     {
     	 MPI_Status status;
         MPI_Datatype type;
	 int err;

         MPI_Init(&argc,&argv);
         MPIR_Status_set_bytes(status,type,err);
         MPI_Finalize(); 
     }
EOF
  rm -f conftest
  $CC $USER_CFLAGS -I$MPI_INCLUDE_DIR -o conftest mpitest.c $MPI_LIB > /dev/null 2>&1
  if test -x conftest ; then
     AC_MSG_RESULT(yes)
     AC_DEFINE(HAVE_STATUS_SET_BYTES,,[Define if status set bytes available])
  else
     AC_MSG_RESULT(no)
  fi
  rm -f conftest mpitest.c
])dnl
dnl
dnl
dnl

