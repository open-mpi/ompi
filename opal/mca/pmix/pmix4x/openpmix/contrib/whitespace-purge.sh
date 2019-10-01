#!/bin/bash
#
# Copyright (c) 2015-2019 Intel, Inc.  All rights reserved.
# Copyright (c) 2015      Los Alamos National Security, LLC. All rights
#                         reserved
# Copyright (c) 2015 Cisco Systems, Inc.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

for file in $(git ls-files) ; do
    # check for the mime-type and do not follow symbolic links. this
    # will cause file to print application/x-symlink for the mime-type
    # allowing us to only have to check if the type is application to
    # skip sym links, pdfs, etc. If any other file types should be
    # skipped add the check here.
    type=$(file -b --mime-type -h $file)
    if test ${type::4} = "text" ; then
        # Eliminate whitespace at the end of lines
        perl -pi -e 's/\s*$/\n/' $file
    fi
done
