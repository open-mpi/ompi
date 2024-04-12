dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
dnl                         University Research and Technology
dnl                         Corporation.  All rights reserved.
dnl Copyright (c) 2004-2018 The University of Tennessee and The University
dnl                         of Tennessee Research Foundation.  All rights
dnl                         reserved.
dnl Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
dnl                         University of Stuttgart.  All rights reserved.
dnl Copyright (c) 2004-2005 The Regents of the University of California.
dnl                         All rights reserved.
dnl Copyright (c) 2007      Sun Microsystems, Inc.  All rights reserved.
dnl Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
dnl Copyright (c) 2009-2023 Cisco Systems, Inc.  All rights reserved.
dnl Copyright (c) 2014      Intel, Inc. All rights reserved.
dnl Copyright (c) 2015-2017 Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2021-2022 Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$


dnl OAC_UNIQ: Uniqify the string-seperated words in the input variable
dnl
dnl 1 -> variable name to be uniq-ized
AC_DEFUN([OAC_UNIQ],[
OAC_VAR_SCOPE_PUSH([oac_uniq_name oac_uniq_done oac_uniq_i oac_uniq_found oac_uniq_count oac_uniq_newval oac_uniq_val])

oac_uniq_name=$1

# Go through each item in the variable and only keep the unique ones
oac_uniq_count=0
for oac_uniq_val in ${$1}; do
    oac_uniq_done=0
    oac_uniq_i=1
    oac_uniq_found=0

    # Loop over every token we've seen so far
    oac_uniq_done="`expr ${oac_uniq_i} \> ${oac_uniq_count}`"
    while test ${oac_uniq_found} -eq 0 && test ${oac_uniq_done} -eq 0; do
        # Have we seen this token already?  Prefix the comparison with
        # "x" so that "-Lfoo" values won't be cause an error.
        oac_uniq_eval="expr x${oac_uniq_val} = x\${oac_uniq_array_$oac_uniq_i}"
        oac_uniq_found=`eval ${oac_uniq_eval}`

        # Check the ending condition
        oac_uniq_done="`expr ${oac_uniq_i} \>= ${oac_uniq_count}`"

        # Increment the counter
        oac_uniq_i="`expr ${oac_uniq_i} + 1`"
    done

    # If we didn't find the token, add it to the "array"
    if test ${oac_uniq_found} -eq 0; then
        oac_uniq_eval="oac_uniq_array_${oac_uniq}_i=${oac_uniq_val}"
        eval ${oac_uniq_eval}
        oac_uniq_count="`expr ${oac_uniq_count} + 1`"
    else
        oac_uniq_i="`expr ${oac_uniq_i} - 1`"
    fi
done

# Take all the items in the "array" and assemble them back into a
# single variable
oac_uniq_i=1
oac_uniq_done="`expr ${oac_uniq_i} \> ${oac_uniq_count}`"
oac_uniq_newval=
while test ${oac_uniq_done} -eq 0; do
    oac_uniq_eval="oac_uniq_newval=\"${oac_uniq_newval} \${oac_uniq_array_$oac_uniq_i}\""
    eval ${oac_uniq_eval}

    oac_uniq_eval="unset oac_uniq_array_${oac_uniq_i}"
    eval ${oac_uniq_eval}

    oac_uniq_done="`expr ${oac_uniq_i} \>= ${oac_uniq_count}`"
    oac_uniq_i="`expr ${oac_uniq_i} + 1`"
done

# Done; do the assignment

oac_uniq_newval="`echo ${oac_uniq_newval}`"
oac_uniq_eval="${oac_uniq_name}=\"${oac_uniq_newval}\""
eval ${oac_uniq_eval}

OAC_VAR_SCOPE_POP
])dnl


dnl OAC_APPEND: Append argument to list
dnl
dnl 1 -> variable name to append to
dnl 2 -> string to append
dnl
dnl Append the given argument ($2) to the variable name passed as $1.
dnl The list is assumed to be space separated, and $1 must be a string
dnl literal (ie, no indirection is supported).
AC_DEFUN([OAC_APPEND],
[OAC_ASSERT_LITERAL([$1])
AS_IF([test -z "${$1}"], [$1="$2"], [$1="${$1} $2"])
])dnl


dnl OAC_APPEND_UNIQ: Append argument to list if not already there
dnl
dnl 1 -> variable name to append to
dnl 2 -> string to append
dnl
dnl uniquely append arguments to a space separated list.  $1 is a
dnl string literal variable name into which the arguments are
dnl inserted.  $2 is a space separated list of arguments to add, each
dnl of which is individually unique-checked before insertion.
dnl
dnl This could probably be made more efficient :(.
AC_DEFUN([OAC_APPEND_UNIQ],
[OAC_ASSERT_LITERAL([$1])
OAC_VAR_SCOPE_PUSH([oac_list_arg oac_list_found oac_list_val])
for oac_list_arg in $2; do
    oac_list_found=0;
    for oac_list_val in ${$1}; do
        AS_IF([test "x${oac_list_val}" = "x${oac_list_arg}"],
              [oac_list_found=1
               break])
    done
    AS_IF([test "${oac_list_found}" = "0"],
          [OAC_APPEND([$1], [${oac_list_arg}])])
done
OAC_VAR_SCOPE_POP
])dnl


dnl OAC_FLAGS_APPEND_UNIQ: Uniquely append argument to list
dnl
dnl 1 -> variable name to append to
dnl 2 -> string to append
dnl
dnl Append new_argument to variable if:
dnl
dnl - the argument does not begin with -I, -L, or -l, or
dnl - the argument begins with -I, -L, or -l, and it's not already in variable
dnl
dnl This macro assumes a space separated list.
AC_DEFUN([OAC_FLAGS_APPEND_UNIQ],
[OAC_ASSERT_LITERAL([$1])
OAC_VAR_SCOPE_PUSH([oac_list_prefix oac_list_append oac_list_arg oac_list_val])
for oac_list_arg in $2; do
    oac_list_append=1
    AS_CASE([${oac_list_arg}],
            [-I*|-L*|-l*],
            [for oac_list_val in ${$1}; do
                 AS_IF([test "x${oal_list_val}" = "x${oac_list_arg}"],
                       [oac_list_append=0])
             done])
    AS_IF([test ${oac_list_append} -eq 1],
          [OAC_APPEND([$1], [$oac_list_arg])])
done
OAC_VAR_SCOPE_POP
])dnl


dnl OAC_FLAGS_PREPEND_UNIQ: Uniquely prepend argument to list
dnl
dnl 1 -> variable name to prepend to
dnl 2 -> string to append
dnl
dnl Prepend new_argument to variable if:
dnl
dnl - the argument does not begin with -I, -L, or -l, or
dnl - the argument begins with -I, -L, or -l, and it's not already in variable
dnl
dnl This macro assumes a space separated list.
AC_DEFUN([OAC_FLAGS_PREPEND_UNIQ],
[OAC_ASSERT_LITERAL([$1])
OAC_VAR_SCOPE_PUSH([oac_list_prefix oac_list_prepend oac_list_arg oac_list_val])
for oac_list_arg in $2; do
    oac_list_prepend=1
    AS_CASE([${oac_list_arg}],
            [-I*|-L*|-l*],
            [for oac_list_val in ${$1}; do
                 AS_IF([test "x${oal_list_val}" = "x${oac_list_arg}"],
                       [oac_list_prepend=0])
             done])
    AS_IF([test ${oac_list_prepend} -eq 1],
           [AS_IF([test -z "${$1}"], [$1="$2"], [$1="$2 ${$1}"])])
done
OAC_VAR_SCOPE_POP
])dnl


dnl OAC_FLAGS_APPEND_MOVE: Uniquely add libraries to list
dnl
dnl 1 -> variable name to append to
dnl 2 -> string to append
dnl
dnl add new_arguments to the end of variable.
dnl
dnl If an argument in new_arguments does not begin with -I, -L, or -l OR
dnl the argument begins with -I, -L, or -l and it is not already in
dnl variable, it is appended to variable.
dnl
dnl If an argument in new_argument begins with a -l and is already in
dnl variable, the existing occurrences of the argument are removed from
dnl variable and the argument is appended to variable.  This behavior
dnl is most useful in LIBS, where ordering matters and being rightmost
dnl is usually the right behavior.
dnl
dnl This macro assumes a space separated list.
AC_DEFUN([OAC_FLAGS_APPEND_MOVE],
[OAC_ASSERT_LITERAL([$1])
OAC_VAR_SCOPE_PUSH([oac_list_arg oac_list_append oac_list_val oac_list_tmp_variable])
for oac_list_arg in $2; do
    AS_CASE([${oac_list_arg}],
            [-I*|-L*],
            [oac_list_append=1
             for oac_list_val in ${$1} ; do
                 AS_IF([test "x${oac_list_val}" = "x${oac_list_arg}"],
                       [oac_list_append=0])
             done
             AS_IF([test ${oac_list_append} -eq 1],
                   [OAC_APPEND([$1], [${oac_list_arg}])])],
            [-l*],
            [oac_list_tmp_variable=
             for oac_list_val in ${$1}; do
                 AS_IF([test "x${oac_list_val}" != "x${oac_list_arg}"],
                       [OAC_APPEND([oac_list_tmp_variable], [${oac_list_val}])])
             done
             OAC_APPEND([oac_list_tmp_variable], [${oac_list_arg}])
             $1="${oac_list_tmp_variable}"],
            [OAC_APPEND([$1], [${oac_list_arg}])])
done
OAC_VAR_SCOPE_POP
])dnl
