#!/bin/sh
#
# Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
#                         University Research and Technology
#                         Corporation.  All rights reserved.
# Copyright (c) 2004-2005 The University of Tennessee and The University
#                         of Tennessee Research Foundation.  All rights
#                         reserved.
# Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
#                         University of Stuttgart.  All rights reserved.
# Copyright (c) 2004-2005 The Regents of the University of California.
#                         All rights reserved.
# Copyright (c) 2009      Oak Ridge National Labs.  All rights reserved.
#
# orte_show_help_replacement.sh Script to detect occurences of 
# #include "orte/util/show_help.h", where actually either
#  1. #include "opal/util/output.h"
#  2. #include "orte/mca/rml/rml_types.h"
# were required.
#
# Some grep/sed mojo may be of interest to others...
#

#
# Function adds into FILE a HEADER as the first #include
#
# Checks for #if  to do the right thing, but does not handle
# single-line or (even harder) multi-line comments
#
function add_header_first()
{
    file=$1                               # File to add header to
    header=$2                             # E.g. opal/util/output.h for #include "opal/util/output.h"
    line=`grep -n "#include " $file | cut -f1 -d':' | head -n1`

    # check if this is a header wrapped in #ifdef HAVE_LALALA_H, if so, add before #if or #ifdef
    prev_line=$(($line - 1))

    if [ $prev_line = -1 -o $prev_line = 0 ] ; then
      prev_line=1
    fi

    head -n $prev_line $file | tail -n1 | grep -q "#if" \
        && sed -i -e "${prev_line}s:#if.*:#include \"$header\"\n\n\0:" $file \
        || sed -i -e "${line}s:#include.*:#include \"$header\"\n\0:" $file
}


function add_header()
{
    file=$1                               # File to add header to
    header=$2                             # E.g. opal/util/output.h for #include "opal/util/output.h"
    after_header_pattern=$3               # Add after occurences of pattern, e.g. opal/util
    line=`grep -n "#include \"$after_header_pattern" $file | cut -f1 -d':' | head -n1`
    if [ $# -gt 3 -a "x$line" = "x" ] ; then
        after_header_pattern=$4           # If above pattern is not found, try more generic, e.g. opal/
        line=`grep -n "#include \"$after_header_pattern" $file | cut -f1 -d':' | head -n1`

        # If we have a final even more general pattern to search for...
        if [ $# -eq 5 -a "x$line" = "x" ] ; then
            after_header_pattern=$5       # If above pattern is not found, try even more generic, e.g. opal/
            line=`grep -n "#include \"$after_header_pattern" $file | cut -f1 -d':' | head -n1`
        fi
        # If still not found, go for plain '#include "'
        if [ "x$line" = "x" ] ; then
            echo Can neither find pattern $3 nor pattern $4 in file $file -- will include after the first include
            line=`grep -n "#include \"" $file | cut -f1 -d':' | head -n1`
            if [ "x$line" = "x" ] ; then
                echo REAL ERROR -- NO INCLUDES AT ALL. INCLUDE MANUALLY
                return
            fi
            # check if this is a header wrapped in #ifdef HAVE_LALALA_H, if so, add after endif
            next_line=$(($line + 1))
            head -n $next_line $file | tail -n1 | grep -q \#endif \
              && sed -i -e "${next_line}s:#endif.*:\0\n\n#include \"$header\":" $file \
              || sed -i -e "${line}s:#include.*:\0\n#include \"$header\":" $file
            return
        fi
    fi
    # check if this is a header wrapped in #ifdef HAVE_LALALA_H, if so, add after endif
    next_line=$(($line + 1))
    head -n $next_line $file | tail -n1 | grep -q \#endif \
        && sed -i -e "${next_line}s:#endif.*:\0\n\n#include \"$header\":" $file \
        || sed -i -e "${line}s:#include \"$after_header_pattern.*:\0\n#include \"$header\":" $file
}

function del_header()
{
    file=$1
    header=`echo $2 | sed 's/\//\\\\\//g'`
    line=`grep -n "#include \"$2" $file | cut -f1 -d':' | head -n1`

    if [ "x$line" = "x" ] ; then
        echo Can not find pattern $header file $file -- will not delete
        return
    fi

    # Remove the header including any characters at end of the line   MULTI_LINE COMMENTS...?
    sed -i -e "/#include \"$header\".*/d" $file
}

SEARCH_HEADER=show_help.h

# Search for all source files with show_help.h in it.
for i in `find . -type f '(' -name '*.[cChysSfF]' -o -iname '*.cc' -o -name '*.cpp' -o -name '*.[fF]77' -o -name '*.[fF]90' ')' | sort | xargs grep -n $SEARCH_HEADER | cut -f1 -d':' | sort | uniq` ; do
    # Now we do know that we have orte/util/show_help.h
    found_orte_show_help_h=1
    need_orte_show_help_h=0

    found_opal_util_output_h=0
    need_opal_util_output_h=0

    found_orte_mca_rml_rml_types_h=0
    need_orte_mca_rml_rml_types_h=0

    grep -q orte_show_help $i && need_orte_show_help_h=1

    grep -q opal\/util\/output.h $i && found_opal_util_output_h=1
    grep -q opal_output $i          && need_opal_util_output_h=1

    grep -q orte\/mca\/rml\/rml_types.h $i     && found_orte_mca_rml_rml_types_h=1
    grep -q -E '(orte_rml_tag_t|ORTE_RML_)' $i && need_orte_mca_rml_rml_types_h=1

    if [ $need_opal_util_output_h -eq 1 -a $found_opal_util_output_h -eq 0 ] ; then
        echo -e $i  \\t Found opal_output in file, but not include opal/util/output.h
        add_header $i   opal/util/output.h   opal/util/   opal/class/  opal/
    fi

    if [ $need_orte_mca_rml_rml_types_h -eq 1 -a $found_orte_mca_rml_rml_types_h -eq 0 ] ; then
        echo -e $i  \\t Found orte_rml_tag_t or ORTE_RML_ in file, but no include orte/mca/rml/rml_types.h
        add_header $i   orte/mca/rml/rml_types.h   orte/mca/rml/  orte/mca/   orte/
    fi

    if [ $need_orte_show_help_h -eq 0 ] ; then
        echo -e $i  \\t Found orte_rml_tag_t or ORTE_RML_ in file, but no include orte/mca/rml/rml_types.h
        del_header $i orte/util/show_help.h
    fi
done

