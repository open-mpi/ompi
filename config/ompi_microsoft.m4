dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2007 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

######################################################################
#
# OMPI_MICROSOFT_COMPILER
#
# Keep all the Windows checks in one place.
#
# USAGE:
#   OMPI_MICROSOFT_COMPILER()
#
######################################################################
AC_DEFUN([OMPI_MICROSOFT_COMPILER],[

    # If we are using  one of the Microsoft compilers check that we are
    # able to include windows.h. Most of the types that follow are defined
    # in this file. If we check for it here it will get included in the
    # default list of header files.
    if test "x$ompi_cv_c_compiler_vendor" = "xmicrosoft" ; then

        ompi_show_subtitle "Microsoft specific detection"

        #
        # These 2 libraries are a minimum ...
        SAVE_LDFLAGS="$LDFLAGS"
        LDFLAGS="$LDFLAGS Ws2_32.lib Advapi32.lib"

        AC_CHECK_HEADERS([windows.h winsock2.h wdm.h])

        # The atomic functions are defined in a very unuasual manner.
        # Some of them are intrinsic defined in windows.h others are
        # exported by kernel32.dll. If we force the usage of AC_TRY_RUN
        # here we will check for both in same time: compilation and run.

        AC_MSG_CHECKING(for working InterlockedCompareExchange)
        AC_TRY_RUN( [#include <windows.h>
                     int main() {
                     LONG dest = 1, exchange = 0, comperand = 1;
                     SetErrorMode(SEM_FAILCRITICALERRORS);
                     InterlockedCompareExchange( &dest, exchange, comperand );
                     return (int)dest;
                    }],
                    [AC_MSG_RESULT(yes)
                     ompi_windows_have_support_for_32_bits_atomic=1],
                    [AC_MSG_RESULT(no)
                     ompi_windows_have_support_for_32_bits_atomic=0])
        AC_DEFINE_UNQUOTED(HAVE_INTERLOCKEDCOMPAREEXCHANGE,
                       $ompi_windows_have_support_for_32_bits_atomic,
                       [Whether we support 32 bits atomic operations on Windows])

        AC_MSG_CHECKING(for working InterlockedCompareExchangeAcquire)
        AC_TRY_RUN( [#include <windows.h>
                 int main() {
                     LONG dest = 1, exchange = 0, comperand = 1;
                     SetErrorMode(SEM_FAILCRITICALERRORS);
                     InterlockedCompareExchangeAcquire( &dest, exchange, comperand );
                     return (int)dest;
                 }],
                    [AC_MSG_RESULT(yes)
                     ompi_windows_have_support_for_32_bits_atomic=1],
                    [AC_MSG_RESULT(no)
                     ompi_windows_have_support_for_32_bits_atomic=0])
        AC_DEFINE_UNQUOTED(HAVE_INTERLOCKEDCOMPAREEXCHANGEACQUIRE,
                       $ompi_windows_have_support_for_32_bits_atomic,
                       [Whether we support 32 bits atomic operations on Windows])

        AC_MSG_CHECKING(for working InterlockedCompareExchangeRelease)
        AC_TRY_RUN( [#include <windows.h>
                 int main() {
                     LONG dest = 1, exchange = 0, comperand = 1;
                     SetErrorMode(SEM_FAILCRITICALERRORS);
                     InterlockedCompareExchangeRelease( &dest, exchange, comperand );
                     return (int)dest;
                 }],
                    [AC_MSG_RESULT(yes)
                     ompi_windows_have_support_for_32_bits_atomic=1],
                    [AC_MSG_RESULT(no)
                     ompi_windows_have_support_for_32_bits_atomic=0])
        AC_DEFINE_UNQUOTED(HAVE_INTERLOCKEDCOMPAREEXCHANGERELEASE,
                       $ompi_windows_have_support_for_32_bits_atomic,
                       [Whether we support 32 bits atomic operations on Windows])

        AC_MSG_CHECKING(for working InterlockedCompareExchange64)
        AC_TRY_RUN( [#include <windows.h>
                 int main() {
                     LONGLONG dest = 1, exchange = 0, comperand = 1;
                     SetErrorMode(SEM_FAILCRITICALERRORS);
                     InterlockedCompareExchange64( &dest, exchange, comperand );
                     return (int)dest;
                 }],
                    [AC_MSG_RESULT(yes)
                     ompi_windows_have_support_for_64_bits_atomic=1],
                    [AC_MSG_RESULT(no)
                     ompi_windows_have_support_for_64_bits_atomic=0])
        AC_DEFINE_UNQUOTED(HAVE_INTERLOCKEDCOMPAREEXCHANGE64,
                       $ompi_windows_have_support_for_64_bits_atomic,
                       [Whether we support 64 bits atomic operations on Windows])

        ompi_show_title "Windows Type tests"

        AC_DEFINE([pid_t], [intptr_t], [Windows pid_t type is a pointer])
        AC_DEFINE_UNQUOTED([SIZEOF_PID_T], $ac_cv_sizeof_int,
                           [and here is it's size])

        LDFLAGS="$SAVE_LDFLAGS"
fi

])
