dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2005 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2010-2012 Cisco Systems, Inc.  All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CHECK_LINKER_FLAGS],[
#
# libtool has been created by this point
# Try to see if it will add any additional flags for dependant libraries
#

AC_MSG_CHECKING([for libtool-supplied linker flags])

libtool="$1"
extra_flags="$2"

# Get the directory where libtool lives

start="`pwd`"
d="`dirname $libtool`"
cd "$d"
libtool="`pwd`/libtool"
cd "$start"
unset d start

# Make a playground to work in

mkdir conftest.$$
cd conftest.$$

cat > foo.c <<EOF
int foo(void) { return 0; }
EOF

ompi_check_linker_flags_work() {
    OPAL_LOG_MSG([$cmd], [yes])
    eval $cmd >&5 2>&5
    if test -n "[$]1"; then
	output=`eval $cmd 2>/dev/null | head -n 1 | sed -e 's,^libtool: *,,' -e 's,^link: *,,'`
    fi
    status="$?"
    OPAL_LOG_MSG([\$? = $status], [yes])
    if test "$status" != "0"; then
	AC_MSG_RESULT([libtool error!])
	AC_MSG_ERROR([Cannot continue])
    fi
}

#
# First make a sample library with the current LDFLAGS and LIBS
#

cmd="$libtool --mode=compile --tag=CC $CC $CFLAGS -c -o foo.o foo.c"
ompi_check_linker_flags_work
cmd="$libtool --mode=link --tag=CC $CC $CFLAGS foo.lo $LDFLAGS $LIBS -o libfoo.la"
ompi_check_linker_flags_work

#
# Now fake linking to it and capture the output from libtool
#

cmd="$libtool --dry-run --mode=link --tag=CC $CC bar.lo libfoo.la -o bar $extra_flags"
ompi_check_linker_flags_work yes

# eat any extra whitespace in CC, as libtool will do the same
tmpCC=`echo $CC | sed -e 's/\//\\\\\//g'`
output=`echo $output | sed -e "s/^$tmpCC//"`
extra_ldflags=
for arg in $output ; do
    case "$arg" in
    *.libs/bar*) ;;
    bar*) ;;
    -I*) ;;
    -L*) ;;
    -R*) ;;
    -lfoo) ;;
    *.libs/libfoo.*) ;;
    -o) ;;
    *.so) ;;
    *.a) ;;
    *)
	extra_ldflags="$extra_ldflags $arg"
	;;
    esac
    shift
done

if test -n "$extra_ldflags"; then
    AC_MSG_RESULT([$extra_ldflags])
else
    AC_MSG_RESULT([no extra flags])
fi

#
# Now do something similar in order to capture the rpath flags: re-run
# the link, but give the libtool --rpath argument.  Then capture the
# difference between this output and the previous output.  Do this
# separately from the above tests to ensure that we don't accidentally
# remove -R if it's needed for rpath.
#

WRAPPER_RPATH_SUPPORT=disabled
AS_IF([test "x$enable_wrapper_rpath" = "xyes"],
      [AC_MSG_CHECKING([for libtool-supplied rpath arguments])
       no_rpath_output=$output

       cmd="$libtool --dry-run --mode=link --tag=CC $CC -rpath /ompi-bogus-test-dir bar.lo libfoo.la -o bar $extra_flags"
       ompi_check_linker_flags_work yes

       # eat any extra whitespace in CC, as libtool will do the same
       tmpCC=`echo $CC | sed -e 's/\//\\\\\//g'`
       output=`echo $output | sed -e "s/^$tmpCC//"`

       rpath_args=
       for rp in $output ; do
           found=0
           for nrp in $no_rpath_output ; do
               AS_IF([test "$rp" = "$nrp"], [found=1])
           done

           # If we didn't find it, then it must be an rpath argument.
           # Ensure to replace /ompi-bogus-test-dir with ${libdir} so
           # that the wrapper can do proper replacement, later.
           AS_IF([test "$found" = "0"], 
                 [rpath_args="$rpath_args `echo $rp | sed -e 's@/ompi-bogus-test-dir@\@{libdir}@'`"])
       done

       # If there were no flags necessary, then we really aren't doing
       # anything to enable rpath, so let's not claim that we are.
       AS_IF([test "`echo $rpath_args`" = ""],
             [rpath_args=
              enable_wrapper_rpath=no
              WRAPPER_RPATH_SUPPORT=unnecessary
              msg="no extra flags"],
             [wrapper_extra_ldflags="$wrapper_extra_ldflags $rpath_args"
              WRAPPER_RPATH_SUPPORT=rpath
              msg=$rpath_args])
       AC_MSG_RESULT([$msg])
      ])

# We don't need to be in the subdir any more
cd ..
rm -rf conftest.$$

AS_IF([test "x$enable_wrapper_rpath" = "xyes"],
      [
       # Now that we have the rpath flags, check to see if the linker
       # supports the DT_RUNPATH flags via --enable-new-dtags (a GNU
       # ld-specific option).  These flags are more social than
       # DT_RPATH -- they can be overridden by LD_LIBRARY_PATH (where
       # a regular DT_RPATH cannot).
       AC_MSG_CHECKING([if linker supports RUNPATH (vs. RPATH)])
       LDFLAGS_save=$LDFLAGS
       LDFLAGS="$LDFLAGS $rpath_args -Wl,--enable-new-dtags"
       AC_LANG_PUSH([C])
       AC_LINK_IFELSE([AC_LANG_PROGRAM([], [return 7;])],
                      [msg=yes
                       WRAPPER_RPATH_SUPPORT=runpath
                       wrapper_extra_ldflags="$wrapper_extra_ldflags -Wl,--enable-new-dtags"],
                      [msg=no])
       AC_LANG_POP([C])
       LDFLAGS=$LDFLAGS_save
       AC_MSG_RESULT([$msg])
      ])
])dnl
