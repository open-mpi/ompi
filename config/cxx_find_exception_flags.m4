dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

define([LAM_CXX_FIND_EXCEPTION_FLAGS],[
#
# Arguments: none
#
# Dependencies: none
#
# Get the exception handling flags for the C++ compiler.  Leaves
# CXXFLAGS undisturbed.
# Provides --with-exflags command line argument for configure as well.
#
# Sets LAM_CXX_EXCEPTION_CXXFLAGS and LAM_CXX_EXCEPTION_LDFLAGS as
# appropriate.
# Must call AC_SUBST manually
#

# Command line flags

AC_ARG_WITH(exflags,
  AC_HELP_STRING([--with-exflags],
                 [Specify flags necessary to enable C++ exceptions]), 
  lam_force_exflags="$withval")

lam_CXXFLAGS_SAVE="$CXXFLAGS"
AC_MSG_CHECKING([for compiler exception flags])

# See which flags to use

if test "$lam_force_exflags" != ""; then

    # If the user supplied flags, use those

    lam_exflags="$lam_force_exflags"
elif test "$GXX" = "yes"; then

    # g++ has changed their flags a few times.  Sigh.

    CXXFLAGS="$CXXFLAGS -fexceptions"

    AC_LANG_SAVE
    AC_LANG_CPLUSPLUS
    AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[]], [[try { int i = 0; } catch(...) { int j = 2; }]]), lam_happy=1, lam_happy=0)

    if test "$lam_happy" = "1"; then
	lam_exflags="-fexceptions";
    else
	CXXFLAGS="$CXXFLAGS_SAVE -fhandle-exceptions"
	AC_COMPILE_IFELSE(AC_LANG_PROGRAM([[]], [[try { int i = 0; } catch(...) { int j = 2; }]]), lam_happy=1, lam_happy=0)
	if test "$lam_happy" = "1"; then
	    lam_exflags="-fhandle-exceptions";
	fi
    fi
    AC_LANG_RESTORE
elif test "`basename $CXX`" = "KCC"; then

    # KCC flags

    lam_exflags="--exceptions"
fi
CXXFLAGS="$lam_CXXFLAGS_SAVE"

# Save the result

LAM_CXX_EXCEPTIONS_CXXFLAGS="$lam_exflags"
LAM_CXX_EXCEPTIONS_LDFLAGS="$lam_exflags"
if test "$lam_exflags" = ""; then
    AC_MSG_RESULT([none necessary])
else
    AC_MSG_RESULT([$lam_exflags])
fi

# Clean up

unset lam_force_exflags lam_CXXFLAGS_SAVE lam_exflags lam_happy])dnl

