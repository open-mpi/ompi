dnl PAC_PROG_CXX - reprioritize the C++ compiler search order
AC_DEFUN([PAC_PROG_CXX],[
	PAC_PUSH_FLAG([CXXFLAGS])
	AC_PROG_CXX([g++ icpc pgCC xlC pathCC cl])
	PAC_POP_FLAG([CXXFLAGS])
])

dnl This is from crypt.to/autoconf-archive, slightly modified.
dnl It defines bool as int if it is not availalbe
dnl
AC_DEFUN([AC_CXX_BOOL],
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
AC_DEFUN([AC_CXX_EXCEPTIONS],
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
AC_DEFUN([AC_CXX_NAMESPACES],
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
AC_DEFUN([AC_CXX_NAMESPACE_STD],
[AC_REQUIRE([AC_CXX_NAMESPACES])
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
save_CXXFLAGS="$CXXFLAGS"
CXXFLAGS="$1 $CXXFLAGS"
rm -f conftest.out
echo 'int foo(void);int foo(void){return 0;}' > conftest2.cpp
echo 'int main(void);int main(void){return 0;}' > conftest.cpp
if ${CXX-g++} $save_CXXFLAGS $CPPFLAGS -o conftest conftest.cpp $LDFLAGS >conftest.bas 2>&1 ; then
   if ${CXX-g++} $CXXFLAGS $CPPFLAGS -o conftest conftest.cpp $LDFLAGS >conftest.out 2>&1 ; then
      if diff -b conftest.out conftest.bas >/dev/null 2>&1 ; then
         AC_MSG_RESULT(yes)
         AC_MSG_CHECKING([whether routines compiled with $1 can be linked with ones compiled without $1])       
         rm -f conftest.out
         rm -f conftest.bas
         if ${CXX-g++} -c $save_CXXFLAGS $CPPFLAGS conftest2.cpp >conftest2.out 2>&1 ; then
            if ${CXX-g++} $CXXFLAGS $CPPFLAGS -o conftest conftest2.o conftest.cpp $LDFLAGS >conftest.bas 2>&1 ; then
               if ${CXX-g++} $CXXFLAGS $CPPFLAGS -o conftest conftest2.o conftest.cpp $LDFLAGS >conftest.out 2>&1 ; then
                  if diff -b conftest.out conftest.bas >/dev/null 2>&1 ; then
	             AC_MSG_RESULT(yes)	  
		     CXXFLAGS="$save_CXXFLAGS"
                     ifelse($2,,CXXOPTIONS="$CXXOPTIONS $1",$2)
                  elif test -s conftest.out ; then
	             cat conftest.out >&AC_FD_CC
	             AC_MSG_RESULT(no)
                     CXXFLAGS="$save_CXXFLAGS"
	             $3
                  else
                     AC_MSG_RESULT(no)
                     CXXFLAGS="$save_CXXFLAGS"
	             $3
                  fi  
               else
	          if test -s conftest.out ; then
	             cat conftest.out >&AC_FD_CC
	          fi
                  AC_MSG_RESULT(no)
                  CXXFLAGS="$save_CXXFLAGS"
                  $3
               fi
	    else
               # Could not link with the option!
               AC_MSG_RESULT(no)
            fi
         else
            if test -s conftest2.out ; then
               cat conftest2.out >&AC_FD_CC
            fi
	    AC_MSG_RESULT(no)
            CXXFLAGS="$save_CXXFLAGS"
	    $3
         fi
      else
         cat conftest.out >&AC_FD_CC
         AC_MSG_RESULT(no)
         $3
         CXXFLAGS="$save_CXXFLAGS"         
      fi
   else
      AC_MSG_RESULT(no)
      $3
      if test -s conftest.out ; then cat conftest.out >&AC_FD_CC ; fi    
      CXXFLAGS="$save_CXXFLAGS"
   fi
else
    # Could not compile without the option!
    AC_MSG_RESULT(no)
fi
rm -f conftest*
])
