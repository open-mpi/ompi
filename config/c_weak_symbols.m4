dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl

define([LAM_C_WEAK_SYMBOLS],[
#
# Arguments: None
#
# Depdencies: None
#
# Check to see if the C compiler can handle weak symbols
#
# Sets LAM_C_WEAK_SYMBOLS=1 if the C compiler has support for weak
# symbols.
#

AC_MSG_CHECKING([for weak symbols])
AC_LINK_IFELSE(AC_LANG_SOURCE([[#pragma weak fake = real
extern int fake(int i);
int real(int i);
int real(int i) { return i; }
int main(int argc, char* argv[]) {
  return fake(3);
}]]), lam_happy=1, lam_happy=0)
if test "$lam_happy" = "1"; then
    AC_MSG_RESULT([yes])
    LAM_C_HAVE_WEAK_SYMBOLS=1
else
    AC_MSG_RESULT([no])
    LAM_C_HAVE_WEAK_SYMBOLS=0
fi

unset lam_happy])dnl
