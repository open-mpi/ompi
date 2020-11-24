dnl This internal macro fails to work properly with OTHER internal macros.
dnl Basically, if the prologue is [], then no message should be generated.
dnl This macro is in autoconf 2.52
m4_define([AC_LANG_PROGRAM(Fortran 77)],
[m4_if([$1],[[[]]],,[m4_ifval([$1],
       [m4_warn([syntax], [$0: ignoring PROLOGUE: $1])])])dnl
      program main
$2
      end])


dnl/*D
dnl PAC_PROG_CHECK_INSTALL_WORKS - Check whether the install program in INSTALL
dnl works.
dnl
dnl Synopsis:
dnl PAC_PROG_CHECK_INSTALL_WORKS
dnl
dnl Output Effect:
dnl   Sets the variable 'INSTALL' to the value of 'ac_sh_install' if 
dnl   a file cannot be installed into a local directory with the 'INSTALL'
dnl   program
dnl
dnl Notes:
dnl   The 'AC_PROG_INSTALL' scripts tries to avoid broken versions of 
dnl   install by avoiding directories such as '/usr/sbin' where some 
dnl   systems are known to have bad versions of 'install'.  Unfortunately, 
dnl   this is exactly the sort of test-on-name instead of test-on-capability
dnl   that 'autoconf' is meant to eliminate.  The test in this script
dnl   is very simple but has been adequate for working around problems 
dnl   on Solaris, where the '/usr/sbin/install' program (known by 
dnl   autoconf to be bad because it is in /usr/sbin) is also reached by a 
dnl   soft link through /bin, which autoconf believes is good.
dnl
dnl   No variables are cached to ensure that we do not make a mistake in 
dnl   our choice of install program.
dnl
dnl   The Solaris configure requires the directory name to immediately
dnl   follow the '-c' argument, rather than the more common 
dnl.vb
dnl      args sourcefiles destination-dir
dnl.ve
dnl D*/
AC_DEFUN([PAC_PROG_CHECK_INSTALL_WORKS],[
if test -z "$INSTALL" ; then
    AC_MSG_RESULT([No install program available])
else
    # first make any "confdb/install-sh -c" into an absolute path
    # this is a hack, but it's still much cleaner than anything else I could
    # come up with (see tt#1007) [goodell@]
    AS_CASE(["$INSTALL"],
            [/*],[:],
            [*install-sh*],[INSTALL="$master_top_srcdir/$INSTALL"])

    # Check that this install really works
    rm -f conftest
    echo "Test file" > conftest
    if test ! -d .conftest ; then mkdir .conftest ; fi
    AC_MSG_CHECKING([whether install works])
    if $INSTALL conftest .conftest >/dev/null 2>&1 ; then
        installOk=yes
    else
        installOk=no
    fi
    rm -rf .conftest conftest
    AC_MSG_RESULT($installOk)
    if test "$installOk" = no ; then
        if test -n "$ac_install_sh" ; then
            INSTALL=$ac_install_sh
        else
	    AC_MSG_ERROR([Unable to find working install])
        fi
    fi
fi
])
