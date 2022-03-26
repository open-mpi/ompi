dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2016-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2016      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OPAL_SUMMARY_ADD(section, topic, unused, result)
#
# queue a summary line in the given section of the form:
#   topic: result
#
# section:topic lines are only added once; first to add wins.
# The key for uniqification is a shell-variable-ified representation
# of section followed by an underscore followed by a
# shell-variable-ified representation of line.
#
# There are no restrictions on the contents of section and topic; they
# can be variable references (although the use case for this is
# dubious) and they can contain most ASCII characters (escape
# characters excluded).  Note that some care must be taken with the
# unique check and this liberal rule, as the unique check is after the
# string has been run through AS_TR_SH.  Basically, any character that
# is not legal in a shell variable name will be turned into an
# underscore.  So the strings "Section_foo" and "Section-foo" would be
# the same as far as the unique check is concerned.
#
# The input strings are evaluated during OPAL_SUMMARY_ADD, not during
# OPAL_SUMMARY_PRINT.  This seems to meet the principle of least
# astonishment.  A common pattern is to call
# OPAL_SUMMARY_ADD([Resource Type], [Component Name], [], [$results])
# and then unset $results to avoid namespace pollution.  This will
# work properly with the current behavior, but would result in odd
# errors if we delayed evaulation.
#
# As a historical note, the third argument has never been used in
# OPAL_SUMMARY_ADD and its meaning is unclear.  Preferred behavior is
# to leave it empty.
#
# As a final historical note, the initial version of SUMMARY_ADD was
# added with implementation of the callers having all of the section
# and topic headers double quoted.  This was never necessary, and
# certainly is not with the current implementation.  While harmless,
# it is not great practice to over-quote, so we recommend against
# doing so.
# -----------------------------------------------------------
AC_DEFUN([OPAL_SUMMARY_ADD],[
    OPAL_VAR_SCOPE_PUSH([opal_summary_line opal_summary_newline opal_summary_key])

    # The end quote on the next line is intentional!
    opal_summary_newline="
"
    opal_summary_line="$2: $4"
    opal_summary_key="AS_TR_SH([$1])_AS_TR_SH([$2])"

    # Use the section name variable as an indicator for whether or not
    # the section has already been created.
    AS_IF([AS_VAR_TEST_SET([opal_summary_section_]AS_TR_SH([$1])[_name])],
          [],
          [AS_VAR_SET([opal_summary_section_]AS_TR_SH([$1])[_name], ["$1"])
           OPAL_APPEND([opal_summary_sections], [AS_TR_SH([$1])])])

    # Use the summary key as indicator if the section:topic has already
    # been added to the results for the given section.
    AS_IF([AS_VAR_TEST_SET([${opal_summary_key}])],
          [],
          [AS_VAR_SET([${opal_summary_key}], [1])
           dnl this is a bit overcomplicated, but we are basically implementing
           dnl a poor mans AS_VAR_APPEND with the ability to specify a separator,
           dnl because we have a newline separator in the string.
           AS_IF([AS_VAR_TEST_SET([opal_summary_section_]AS_TR_SH([$1])[_value])],
                 [AS_VAR_APPEND([opal_summary_section_]AS_TR_SH([$1])[_value],
                                ["${opal_summary_newline}${opal_summary_line}"])],
                 [AS_VAR_SET([opal_summary_section_]AS_TR_SH([$1])[_value],
                             ["${opal_summary_line}"])])])

    OPAL_VAR_SCOPE_POP
])

AC_DEFUN([OPAL_SUMMARY_PRINT],[
    OPAL_VAR_SCOPE_PUSH([opal_summary_section opal_summary_section_name])
    cat <<EOF

Open MPI configuration:
-----------------------
Version: $OMPI_MAJOR_VERSION.$OMPI_MINOR_VERSION.$OMPI_RELEASE_VERSION$OMPI_GREEK_VERSION
EOF

    if test "$project_ompi_amc" = "true" ; then
        echo "Build MPI C bindings: yes"
    else
        echo "Build MPI C bindings: no"
    fi

    dnl Print out the bindings if we are building OMPI
    if test "$project_ompi_amc" = "true" ; then
        if test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_MPIFH_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h"
        elif test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_USEMPI_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h, use mpi"
        elif test $OMPI_BUILD_FORTRAN_BINDINGS = $OMPI_FORTRAN_USEMPIF08_BINDINGS ; then
            echo "Build MPI Fortran bindings: mpif.h, use mpi, use mpi_f08"
        else
            echo "Build MPI Fortran bindings: no"
        fi

        if test $WANT_MPI_JAVA_BINDINGS -eq 1 ; then
            echo "Build MPI Java bindings (experimental): yes"
        else
            echo "Build MPI Java bindings (experimental): no"
        fi
    fi

    if test "$project_oshmem_amc" = "true" ; then
        echo "Build Open SHMEM support: yes"
    elif test -z "$project_oshmem_amc" ; then
        echo "Build Open SHMEM support: no"
    else
        echo "Build Open SHMEM support: $project_oshmem_amc"
    fi

    if test $WANT_DEBUG = 0 ; then
        echo "Debug build: no"
    else
        echo "Debug build: yes"
    fi

    if test ! -z $with_platform ; then
        echo "Platform file: $with_platform"
    else
        echo "Platform file: (none)"
    fi

    echo

    for opal_summary_section in ${opal_summary_sections} ; do
        AS_VAR_COPY([opal_summary_section_name], [opal_summary_section_${opal_summary_section}_name])
        AS_VAR_COPY([opal_summary_section_value], [opal_summary_section_${opal_summary_section}_value])
        echo "${opal_summary_section_name}"
        echo "-----------------------"
        echo "${opal_summary_section_value}" | sort -f
        echo " "
    done

    if test $WANT_DEBUG = 1 ; then
        cat <<EOF
*****************************************************************************
 THIS IS A DEBUG BUILD!  DO NOT USE THIS BUILD FOR PERFORMANCE MEASUREMENTS!
*****************************************************************************

EOF
    fi

    OPAL_VAR_SCOPE_POP
])
