dnl -*- shell-script -*-
dnl
dnl $HEADER$
dnl
dnl $Id: lam_functions.m4,v 1.2 2004/01/07 07:46:37 jsquyres Exp $
dnl

AC_DEFUN(LAM_CONFIGURE_SETUP,[

# Some helper script functions.  Unfortunately, we cannot use $1 kinds
# of arugments here because of the m4 substitution.  So we have to set
# special variable names before invoking the function.  :-\

lam_show_title() {
  cat <<EOF

============================================================================
== ${1}
============================================================================
EOF
}


lam_show_subtitle() {
  cat <<EOF

*** ${1}
EOF
}

#
# Save some stats about this build
#

LAM_CONFIGURE_USER="`whoami`"
LAM_CONFIGURE_HOST="`hostname | head -n 1`"
LAM_CONFIGURE_DATE="`date`"

#
# Make automake clean emacs ~ files for "make clean"
#

CLEANFILES="*~ .\#*"
AC_SUBST(CLEANFILES)])dnl

dnl #######################################################################

AC_DEFUN(LAM_UNIQ,[
# 1 is the variable name to be uniq-ized
lam_name=$1

# Go through each item in the variable and only keep the unique ones

lam_count=0
for val in ${$1}; do
    lam_done=0
    lam_i=1
    lam_found=0

    # Loop over every token we've seen so far

    lam_done="`expr $lam_i \> $lam_count`"
    while test "$lam_found" = "0" -a "$lam_done" = "0"; do

	# Have we seen this token already?  Prefix the comparison with
	# "x" so that "-Lfoo" values won't be cause an error.

	lam_eval="expr x$val = x\$lam_array_$lam_i"
	lam_found=`eval $lam_eval`

	# Check the ending condition

	lam_done="`expr $lam_i \>= $lam_count`"

	# Increment the counter

	lam_i="`expr $lam_i + 1`"
    done

    # If we didn't find the token, add it to the "array"

    if test "$lam_found" = "0"; then
	lam_eval="lam_array_$lam_i=$val"
	eval $lam_eval
	lam_count="`expr $lam_count + 1`"
    else
	lam_i="`expr $lam_i - 1`"
    fi
done

# Take all the items in the "array" and assemble them back into a
# single variable

lam_i=1
lam_done="`expr $lam_i \> $lam_count`"
lam_newval=
while test "$lam_done" = "0"; do
    lam_eval="lam_newval=\"$lam_newval \$lam_array_$lam_i\""
    eval $lam_eval

    lam_eval="unset lam_array_$lam_i"
    eval $lam_eval

    lam_done="`expr $lam_i \>= $lam_count`"
    lam_i="`expr $lam_i + 1`"
done

# Done; do the assignment

lam_newval="`echo $lam_newval`"
lam_eval="$lam_name=\"$lam_newval\""
eval $lam_eval

# Clean up

unset lam_name lam_i lam_done lam_newval lam_eval lam_count])dnl

