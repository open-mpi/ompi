dnl -*- autoconf -*-
dnl
dnl Copyright (c) 2016      Los Alamos National Security, LLC. All rights
dnl                         reserved.
dnl Copyright (c) 2016-2018 Cisco Systems, Inc.  All rights reserved
dnl Copyright (c) 2016      Research Organization for Information Science
dnl                         and Technology (RIST). All rights reserved.
dnl Copyright (c) 2022      Amazon.com, Inc. or its affiliates.  All Rights reserved.
dnl Copyright (c) 2022      IBM Corporation.  All rights reserved.
dnl Copyright (c) 2024      Nanook Consulting  All rights reserved.
dnl $COPYRIGHT$
dnl
dnl Additional copyrights may follow
dnl
dnl $HEADER$
dnl

# OAC_SUMMARY_ADD(section, topic, result)
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
# The input strings are evaluated during OAC_SUMMARY_ADD, not during
# OAC_SUMMARY_PRINT.  This seems to meet the principle of least
# astonishment.  A common pattern is to call
# OAC_SUMMARY_ADD([Resource Type], [Component Name], [$results])
# and then unset $results to avoid namespace pollution.  This will
# work properly with the current behavior, but would result in odd
# errors if we delayed evaluation.
# -----------------------------------------------------------
AC_DEFUN([OAC_SUMMARY_ADD],[
    OAC_VAR_SCOPE_PUSH([oac_summary_line oac_summary_newline oac_summary_key])

    # The end quote on the next line is intentional!
    oac_summary_newline="
"
    oac_summary_line="$2: $3"
    oac_summary_key="AS_TR_SH([$1])_AS_TR_SH([$2])"

    # Use the section name variable as an indicator for whether or not
    # the section has already been created.
    AS_IF([AS_VAR_TEST_SET([oac_summary_section_]AS_TR_SH([$1])[_name])],
          [],
          [AS_VAR_SET([oac_summary_section_]AS_TR_SH([$1])[_name], ["$1"])
           OAC_APPEND([oac_summary_sections], [AS_TR_SH([$1])])])

    # Use the summary key as indicator if the section:topic has already
    # been added to the results for the given section.
    AS_IF([AS_VAR_TEST_SET([${oac_summary_key}])],
          [],
          [AS_VAR_SET([${oac_summary_key}], [1])
           dnl this is a bit overcomplicated, but we are basically implementing
           dnl a poor mans AS_VAR_APPEND with the ability to specify a separator,
           dnl because we have a newline separator in the string.
           AS_IF([AS_VAR_TEST_SET([oac_summary_section_]AS_TR_SH([$1])[_value])],
                 [AS_VAR_APPEND([oac_summary_section_]AS_TR_SH([$1])[_value],
                                ["${oac_summary_newline}${oac_summary_line}"])],
                 [AS_VAR_SET([oac_summary_section_]AS_TR_SH([$1])[_value],
                             ["${oac_summary_line}"])])])

    OAC_VAR_SCOPE_POP
])

dnl $1 can be the following:
dnl
dnl "stderr" : emit the summary to stderr
dnl "stdout" : emit the summary to stdout
dnl blank : emit the summary to configure's default (i.e., AS_MESSAGE_FD)
dnl
dnl Other values will cause an m4_fatal error.
AC_DEFUN([OAC_SUMMARY_PRINT],[
    OAC_VAR_SCOPE_PUSH([oac_summary_section oac_summary_section_name])
    m4_define([oac_summary_print_fd],
              [m4_if([$1], [stderr], [2],
                     [$1], [stdout], [1],
                     [$1], [], [AS_MESSAGE_FD],
                     [m4_fatal([You must pass stdin, stderr, or nothing to $0])])
              ])

    for oac_summary_section in ${oac_summary_sections} ; do
        AS_VAR_COPY([oac_summary_section_name], [oac_summary_section_${oac_summary_section}_name])
        AS_VAR_COPY([oac_summary_section_value], [oac_summary_section_${oac_summary_section}_value])
        echo "${oac_summary_section_name}" >&oac_summary_print_fd
        echo "-----------------------" >&oac_summary_print_fd
        echo "${oac_summary_section_value}" | sort -f >&oac_summary_print_fd
        echo " " >&oac_summary_print_fd
    done

    m4_undefine([oac_summary_print_fd])
    OAC_VAR_SCOPE_POP
])
