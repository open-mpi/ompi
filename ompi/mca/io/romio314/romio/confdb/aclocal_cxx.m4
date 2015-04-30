dnl PAC_CXX_SEARCH_LIST - expands to a whitespace separated list of C++
dnl compilers for use with AC_PROG_CXX that is more suitable for HPC software
dnl packages
AC_DEFUN([PAC_CXX_SEARCH_LIST],[$CCC icpc pgCC xlC pathCC g++ clang++ c++ cc++ cxx CC cl])
dnl PAC_PROG_CXX - reprioritize the C++ compiler search order
dnl NOTE: this macro suffers from a basically intractable "expanded before it
dnl was required" problem when libtool is also used
AC_DEFUN([PAC_PROG_CXX],[
	PAC_PUSH_FLAG([CXXFLAGS])
        # This test uses the list from a recent PROG_CXX, but with the
        # addition of the Portland group, IBM, and Intel C++ compilers
        # (While the Intel icc compiler will compile C++ programs, it will
        # not *link* C++ object files unless there is at least one C++ source
        # file present on the command that performs the linking.  icpc is the
        # Intel C++ compiler that both compiles and links C++ programs)
	AC_PROG_CXX([PAC_CXX_SEARCH_LIST])
	PAC_POP_FLAG([CXXFLAGS])
])

dnl This is from crypt.to/autoconf-archive, slightly modified.
dnl It defines bool as int if it is not availalbe
dnl
AC_DEFUN([AX_CXX_BOOL],
[AC_CACHE_CHECK(whether the compiler recognizes bool as a built-in type,
ac_cv_cxx_bool,
[AC_LANG_SAVE
 AC_LANG_CPLUSPLUS
 AC_TRY_COMPILE([
int f(int  x){return 1;}
int f(char x){return 1;}
int f(bool x){return 1;}
],[bool b = true; return f(b);],
 ac_cv_cxx_bool=yes, ac_cv_cxx_bool=no)
 AC_LANG_RESTORE
])
if test "$ac_cv_cxx_bool" != yes; then
  AC_DEFINE(bool,int,[define if bool is a built-in type])
fi
])

dnl This is from crypt.to/autoconf-archive, slightly modified (name defined)
dnl
AC_DEFUN([AX_CXX_EXCEPTIONS],
[AC_CACHE_CHECK(whether the compiler supports exceptions,
ac_cv_cxx_exceptions,
[AC_LANG_SAVE
 AC_LANG_CPLUSPLUS
 AC_TRY_COMPILE(,[try { throw  1; } catch (int i) { return i; }],
 ac_cv_cxx_exceptions=yes, ac_cv_cxx_exceptions=no)
 AC_LANG_RESTORE
])
if test "$ac_cv_cxx_exceptions" = yes; then
  AC_DEFINE(HAVE_CXX_EXCEPTIONS,,[define if the compiler supports exceptions])
fi
])

dnl This is from crypt.to/autoconf-archive
dnl
AC_DEFUN([AX_CXX_NAMESPACES],
[AC_CACHE_CHECK(whether the compiler implements namespaces,
ac_cv_cxx_namespaces,
[AC_LANG_SAVE
 AC_LANG_CPLUSPLUS
 AC_TRY_COMPILE([namespace Outer { namespace Inner { int i = 0; }}],
                [using namespace Outer::Inner; return i;],
 ac_cv_cxx_namespaces=yes, ac_cv_cxx_namespaces=no)
 AC_LANG_RESTORE
])
if test "$ac_cv_cxx_namespaces" = yes; then
  AC_DEFINE(HAVE_NAMESPACES,,[define if the compiler implements namespaces])
fi
])

dnl Some compilers support namespaces but don't know about std
dnl
AC_DEFUN([AX_CXX_NAMESPACE_STD],
[AC_REQUIRE([AX_CXX_NAMESPACES])
AC_CACHE_CHECK(whether the compiler implements the namespace std,
ac_cv_cxx_namespace_std,
[ac_cv_cxx_namespace_std=no
if test "$ac_cv_cxx_namespaces" = yes ; then 
   AC_LANG_SAVE
   AC_LANG_CPLUSPLUS
   AC_TRY_COMPILE([
#include <iostream>
using namespace std;],
                [cout << "message\n";],
 ac_cv_cxx_namespace_std=yes, ac_cv_cxx_namespace_std=no)
   AC_LANG_RESTORE
fi
])
if test "$ac_cv_cxx_namespace_std" = yes; then
  AC_DEFINE(HAVE_NAMESPACE_STD,,[define if the compiler implements namespace std])
fi
])

dnl/*D
dnl PAC_CXX_CHECK_COMPILER_OPTION - Check that a C++ compiler option is
dnl accepted without warning messages
dnl
dnl Synopsis:
dnl PAC_CXX_CHECK_COMPILER_OPTION(optionname,action-if-ok,action-if-fail)
dnl
dnl Output Effects:
dnl
dnl If no actions are specified, a working value is added to 'CXXOPTIONS'
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
AC_DEFUN([PAC_CXX_CHECK_COMPILER_OPTION],[
AC_MSG_CHECKING([whether C++ compiler accepts option $1])
pac_opt="$1"
AC_LANG_PUSH([C++])
CXXFLAGS_orig="$CXXFLAGS"
CXXFLAGS_opt="$pac_opt $CXXFLAGS"
pac_result="unknown"

AC_LANG_CONFTEST([AC_LANG_PROGRAM()])
CXXFLAGS="$CXXFLAGS_orig"
rm -f pac_test1.log
PAC_LINK_IFELSE_LOG([pac_test1.log], [], [
    CXXFLAGS="$CXXFLAGS_opt"
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
    CXXFLAGS="$CXXFLAGS_orig"
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

        CXXFLAGS="$CXXFLAGS_opt"
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

dnl Restore CXXFLAGS before 2nd/3rd argument commands are executed,
dnl as 2nd/3rd argument command could be modifying CXXFLAGS.
CXXFLAGS="$CXXFLAGS_orig"
if test "$pac_result" = "yes" ; then
     ifelse([$2],[],[CXXOPTIONS="$CXXOPTIONS $1"],[$2])
else
     ifelse([$3],[],[:],[$3])
fi
AC_LANG_POP([C++])
])
