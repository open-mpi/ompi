dnl -*- shell-script -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
dnl                         All rights reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
denl

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
    OMPI_LOG_MSG([$cmd], [yes])
    eval $cmd >&5 2>&5
    if test -n "[$]1"; then
	output="`eval $cmd 2>/dev/null`"
    fi
    status="$?"
    OMPI_LOG_MSG([\$? = $status], [yes])
    if test "$status" != "0"; then
	AC_MSG_RESULT([libtool error!])
	AC_MSG_ERROR([Cannot continue])
    fi
}

#
# First make a sample library with the current LDFLAGS and LIBS
#

cmd="$libtool --mode=compile $CC $CFLAGS foo.c -c -o foo.o"
ompi_check_linker_flags_work
cmd="$libtool --mode=link $CC $CFLAGS foo.lo $LDFLAGS $LIBS -o libfoo.la"
ompi_check_linker_flags_work

#
# Now fake linking to it and capture the output from libtool
#

cmd="$libtool --dry-run --mode=link $CC bar.lo libfoo.la -o bar $extra_flags"
ompi_check_linker_flags_work yes

eval "set $output"
extra_ldflags=
while test -n "[$]1"; do
    case "[$]1" in
    $CC) ;;
    bar*) ;;
    -I*) ;;
    -L*) ;;
    -R*) ;;
    -lfoo) ;;
    -o) ;;
    *.so) ;;
    *.a) ;;
    *)
	extra_ldflags="$extra_ldflags [$]1"
	;;
    esac
    shift
done

if test -n "$extra_ldflags"; then
    AC_MSG_RESULT([$extra_ldflags])
else
    AC_MSG_RESULT([no extra flags])
fi

cd ..
rm -rf conftest.$$])dnl
