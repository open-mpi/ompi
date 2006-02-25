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
dnl $COPYRIGHT$
dnl 
dnl Additional copyrights may follow
dnl 
dnl $HEADER$
dnl

AC_DEFUN([OMPI_CONFIGURE_SETUP],[

# Some helper script functions.  Unfortunately, we cannot use $1 kinds
# of arugments here because of the m4 substitution.  So we have to set
# special variable names before invoking the function.  :-\

ompi_show_title() {
  cat <<EOF

============================================================================
== ${1}
============================================================================
EOF
}


ompi_show_subtitle() {
  cat <<EOF

*** ${1}
EOF
}


ompi_show_subsubtitle() {
  cat <<EOF

+++ ${1}
EOF
}

ompi_show_subsubsubtitle() {
  cat <<EOF

--- ${1}
EOF
}

#
# Save some stats about this build
#

OMPI_CONFIGURE_USER="`whoami`"
OMPI_CONFIGURE_HOST="`hostname | head -n 1`"
OMPI_CONFIGURE_DATE="`date`"

#
# Save these details so that they can be used in ompi_info later
#
AC_SUBST(OMPI_CONFIGURE_USER)
AC_SUBST(OMPI_CONFIGURE_HOST)
AC_SUBST(OMPI_CONFIGURE_DATE)])dnl

dnl #######################################################################
dnl #######################################################################
dnl #######################################################################

AC_DEFUN([OMPI_BASIC_SETUP],[
#
# Save some stats about this build
#

OMPI_CONFIGURE_USER="`whoami`"
OMPI_CONFIGURE_HOST="`hostname | head -n 1`"
OMPI_CONFIGURE_DATE="`date`"

#
# Make automake clean emacs ~ files for "make clean"
#

CLEANFILES="*~ .\#*"
AC_SUBST(CLEANFILES)

#
# This is useful later (ompi_info, and therefore mpiexec)
#

AC_CANONICAL_HOST
AC_DEFINE_UNQUOTED(OMPI_ARCH, "$host", [OMPI architecture string])

#
# See if we can find an old installation of OMPI to overwrite
#

# Stupid autoconf 2.54 has a bug in AC_PREFIX_PROGRAM -- if ompi_clean
# is not found in the path and the user did not specify --prefix,
# we'll get a $prefix of "."

ompi_prefix_save="$prefix"
AC_PREFIX_PROGRAM(ompi_clean)
if test "$prefix" = "."; then
    prefix="$ompi_prefix_save"
fi
unset ompi_prefix_save

#
# Basic sanity checking; we can't install to a relative path
#

case "$prefix" in
  /*/bin)
    prefix="`dirname $prefix`"
    echo installing to directory \"$prefix\" 
    ;;
  /*) 
    echo installing to directory \"$prefix\" 
    ;;
  NONE)
    echo installing to directory \"$ac_default_prefix\" 
    ;;
  @<:@a-zA-Z@:>@:*)
    echo installing to directory \"$prefix\" 
    ;;
  *) 
    AC_MSG_ERROR(prefix \"$prefix\" must be an absolute directory path) 
    ;;
esac

# Allow the --enable-dist flag to be passed in

AC_ARG_ENABLE(dist, 
    AC_HELP_STRING([--enable-dist],
		   [guarantee that that the "dist" make target will be functional, although may not guarantee that any other make target will be functional.]),
    OMPI_WANT_DIST=yes, OMPI_WANT_DIST=no)

if test "$OMPI_WANT_DIST" = "yes"; then
    AC_MSG_WARN([Configuring in 'make dist' mode])
    AC_MSG_WARN([Most make targets may be non-functional!])
fi])dnl

dnl #######################################################################
dnl #######################################################################
dnl #######################################################################

AC_DEFUN([OMPI_LOG_MSG],[
# 1 is the message
# 2 is whether to put a prefix or not
if test -n "$2"; then
    echo "configure:__oline__: $1" >&5
else
    echo $1 >&5
fi])dnl

dnl #######################################################################
dnl #######################################################################
dnl #######################################################################

AC_DEFUN([OMPI_LOG_FILE],[
# 1 is the filename
if test -n "$1" -a -f "$1"; then
    cat $1 >&5
fi])dnl

dnl #######################################################################
dnl #######################################################################
dnl #######################################################################

AC_DEFUN([OMPI_LOG_COMMAND],[
# 1 is the command
# 2 is actions to do if success
# 3 is actions to do if fail
echo "configure:__oline__: $1" >&5
$1 1>&5 2>&1
ompi_status=$?
OMPI_LOG_MSG([\$? = $ompi_status], 1)
if test "$ompi_status" = "0"; then
    unset ompi_status
    $2
else
    unset ompi_status
    $3
fi])dnl

dnl #######################################################################
dnl #######################################################################
dnl #######################################################################

AC_DEFUN([OMPI_UNIQ],[
# 1 is the variable name to be uniq-ized
ompi_name=$1

# Go through each item in the variable and only keep the unique ones

ompi_count=0
for val in ${$1}; do
    ompi_done=0
    ompi_i=1
    ompi_found=0

    # Loop over every token we've seen so far

    ompi_done="`expr $ompi_i \> $ompi_count`"
    while test "$ompi_found" = "0" -a "$ompi_done" = "0"; do

	# Have we seen this token already?  Prefix the comparison with
	# "x" so that "-Lfoo" values won't be cause an error.

	ompi_eval="expr x$val = x\$ompi_array_$ompi_i"
	ompi_found=`eval $ompi_eval`

	# Check the ending condition

	ompi_done="`expr $ompi_i \>= $ompi_count`"

	# Increment the counter

	ompi_i="`expr $ompi_i + 1`"
    done

    # If we didn't find the token, add it to the "array"

    if test "$ompi_found" = "0"; then
	ompi_eval="ompi_array_$ompi_i=$val"
	eval $ompi_eval
	ompi_count="`expr $ompi_count + 1`"
    else
	ompi_i="`expr $ompi_i - 1`"
    fi
done

# Take all the items in the "array" and assemble them back into a
# single variable

ompi_i=1
ompi_done="`expr $ompi_i \> $ompi_count`"
ompi_newval=
while test "$ompi_done" = "0"; do
    ompi_eval="ompi_newval=\"$ompi_newval \$ompi_array_$ompi_i\""
    eval $ompi_eval

    ompi_eval="unset ompi_array_$ompi_i"
    eval $ompi_eval

    ompi_done="`expr $ompi_i \>= $ompi_count`"
    ompi_i="`expr $ompi_i + 1`"
done

# Done; do the assignment

ompi_newval="`echo $ompi_newval`"
ompi_eval="$ompi_name=\"$ompi_newval\""
eval $ompi_eval

# Clean up

unset ompi_name ompi_i ompi_done ompi_newval ompi_eval ompi_count])dnl

