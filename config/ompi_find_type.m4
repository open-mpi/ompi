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
dnl

AC_DEFUN([OMPI_FIND_TYPE],[
# $1 = message ouptupt
# $2 = suffix of output variable to set
# $3 = list of type names to check
msg="$1"
target_name="$2"
types="$3"
abort_on_fail="$4"

# Announce
AC_MSG_CHECKING([for C type corresponding to $msg])

# Put a default answer in there
str="MPI_$target_name='not found'"
eval $str

# Get the size of the thing we're looking for
str="target_size=\$OMPI_SIZEOF_${target_name}"
eval $str

# Loop over all the types handed to us
real_type=
for type in $types; do
    if test -z "$real_type"; then

        # Convert the name handed to us to a variable name, and get
        # its size in $type_size

        type_varname="`echo $type | sed -e s/:/_/g`"
        str="type_size=\$ac_cv_sizeof_$type_varname"
        eval $str

        # If the size matches the target size, we're done

        if test "$target_size" != "" -a \
            "$type_size" = "$target_size"; then
            real_type="`echo $type | sed -e 's/:/ /'g`"
        fi
    fi
done

# Did we find it?
if test -z "$real_type"; then
    AC_MSG_RESULT([not found])
    AC_MSG_WARN([*** Did not find corresponding C type])
    if test "$abort_on_fail" != ""; then
        AC_MSG_ERROR([Cannot continue])
    fi
else
    str="MPI_${target_name}_TYPE=\$real_type"
    eval $str
    AC_MSG_RESULT([$real_type])
fi

unset types name done str real_type target_size type_size msg type_varname])
