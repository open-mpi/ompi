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
# $2 = C types to check
# $3 = abort on not found
# $4 = target size
# $5 = output variable name
oft_msg="$1"
oft_types="$2"
oft_abort_on_fail="$3"
oft_target_size="$4"
oft_target_name="$5"

# Announce
AC_MSG_CHECKING([for C type corresponding to $oft_msg])

# Loop over all the types handed to us
oft_real_type=
for oft_type in $oft_types; do
    if test -z "$oft_real_type"; then

        # Convert the name handed to us to a variable name, and get
        # its size in $oft_type_size

        oft_type_varname="`echo $oft_type | sed -e s/:/_/g`"
        oft_str="oft_type_size=\$ac_cv_sizeof_${oft_type_varname}"
        eval $oft_str

        # If the size matches the target size, we're done

        if test "$oft_target_size" != "" -a \
            "$oft_type_size" = "$oft_target_size"; then
            oft_real_type="`echo $oft_type | sed -e 's/:/ /'g`"
        fi
    fi
done

# Did we find it?
if test -z "$oft_real_type"; then
    AC_MSG_RESULT([not found])
    AC_MSG_WARN([*** Did not find corresponding C type])
    if test "$oft_abort_on_fail" != "no"; then
        AC_MSG_ERROR([Cannot continue])
    fi
else
    AC_MSG_RESULT([$oft_real_type])
fi

# Set the type in the output, even if it's empty (so that the caller
# knows if we found it or not)

oft_str="${oft_target_name}=\$oft_real_type"
eval $oft_str

unset oft_types oft_name oft_str oft_real_type oft_target_size oft_type_size oft_msg oft_type_varname])
