dnl AM_IGNORE is an extension that tells (a patched) automake not to
dnl include the specified AC_SUBST variable in the Makefile.in that
dnl automake generates.  We don't use AC_DEFUN, since aclocal will 
dnl then complain that AM_IGNORE is a duplicate (if you are using the
dnl patched automake/aclocal).
m4_ifdef([AM_IGNORE],[],[m4_define([AM_IGNORE],[])])
