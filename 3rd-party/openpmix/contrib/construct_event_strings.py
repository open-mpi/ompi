#
# Copyright (c) 2020      Intel, Inc.  All rights reserved.
# Copyright (c) 2020-2022 Cisco Systems, Inc.  All rights reserved
# Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
# Copyright (c) 2022      Triad National Security, LLC. All rights reserved.
# $COPYRIGHT$
#
# Construct a dictionary for translating attributes to/from
# their defined name and their string representation - used
# by tools to interpret user input
#

from __future__ import print_function
import os
import os.path
import sys
from optparse import OptionParser, OptionGroup

index = 0


def harvest_constants(options, path, constants):
    global index
    # open the file
    try:
        inputfile = open(path, "r")
    except Exception as e:
        print("File {path} could not be opened: {e}"
              .format(path=path, e=e))
        return 1

    # read the file - these files aren't too large
    # so ingest the whole thing at one gulp
    try:
        lines = inputfile.readlines()
    except Exception as e:
        print("Error reading file {path}: {e}"
              .format(path=path, e=e))
        inputfile.close()
        return 1

    inputfile.close()  # we read everything, so done with the file

    firstline = True

    # find the start of the event codes
    n = 0
    found = False
    while n < len(lines):
        if "PMIX ERROR CONSTANTS" in lines[n]:
            found = True
            n = n + 1
            break;
        n = n + 1
    # error out if not found
    if not found:
        print("START OF EVENT CODES NOT FOUND")
        return 1

    # loop over the lines
    while n < len(lines):
        line = lines[n]
        # remove white space at front and back
        myline = line.strip()
        # remove comment lines
        if "/*" in myline or "*/" in myline or myline.startswith("*"):
            n = n + 1
            continue
        # if we have found the end of the event codes, we are done
        if "PMIX_EXTERNAL_ERR_BASE" in myline or "PMIX_ERR_SYS_BASE" in myline or "PMIX_INTERNAL_ERR_DONE" in myline:
            return 0
        # skip a well-known macro
        if "PMIX_SYSTEM_EVENT" in myline:
            n = n + 2
            continue
        # if the line starts with #define, then we want it
        if not myline.startswith("#define"):
            n = n + 1
            continue

        value = myline[8:]
        tokens = value.split()
        if not firstline:
            constants.write(",\n\n")
        firstline = False
        constants.write("    {.index = " + str(index) + ", .name = \"" + tokens[0] + "\", .code = " + str(tokens[1]) + "}")
        index = index + 1
        n = n + 1

    return 0


def _write_header(options, base_path, num_elements):
    contents = '''/*
 * This file is autogenerated by construct_event_strings.py.
 * Do not edit this file by hand.
 */

#include "src/include/pmix_config.h"
#include "src/include/pmix_globals.h"
#include "include/pmix_common.h"

#ifndef PMIX_EVENT_STRINGS_H
#define PMIX_EVENT_STRINGS_H

BEGIN_C_DECLS

PMIX_EXPORT extern const pmix_event_string_t pmix_event_strings[{ne}];

#define PMIX_EVENT_INDEX_BOUNDARY {nem1}

END_C_DECLS

#endif\n
'''.format(ne=num_elements, nem1=num_elements - 1)

    if options.dryrun:
        constants = sys.stdout
        outpath = None
    else:
        outpath = os.path.join(base_path, "pmix_event_strings.h")
        try:
            constants = open(outpath, "w+")
        except Exception as e:
            print("{outpath} CANNOT BE OPENED - EVENT STRINGS COULD NOT BE CONSTRUCTED: {e}"
                  .format(outpath=outpath, e=e))
            return 1
    constants.write(contents)
    constants.close()
    return 0


def main():
    parser = OptionParser("usage: %prog [options]")
    debugGroup = OptionGroup(parser, "Debug Options")
    debugGroup.add_option("--dryrun",
                          action="store_true", dest="dryrun", default=False,
                          help="Show output to screen")
    parser.add_option_group(debugGroup)

    (options, args) = parser.parse_args()

    # Find the top-level PMIx source tree dir.
    # Start with the location of this script, which we know is in
    # $top_srcdir/contrib.
    top_src_dir = os.path.dirname(sys.argv[0])
    top_src_dir = os.path.join(top_src_dir, "..")
    top_src_dir = os.path.abspath(top_src_dir)

    # Sanity check
    checkfile = os.path.join(top_src_dir, "VERSION")
    if not os.path.exists(checkfile):
        print("ERROR: Could not find top source directory for Open PMIx")
        return 1

    source_include_dir = os.path.join(top_src_dir, "include")
    util_include_dir = os.path.join(top_src_dir, "src", "util")

    # This script is invoked from src/include/Makefile.am, and
    # therefore the cwd will be $(builddir)/src/include.  Verify this
    # by checking for a file that we know should be in there.
    build_src_include_dir = os.getcwd()
    checkfile = os.path.join(build_src_include_dir, "pmix_config.h")
    if not os.path.exists(checkfile):
        print("ERROR: Could not find build directory for Open PMIx")
        return 1

    if options.dryrun:
        constants = sys.stdout
        outpath = None
    else:
        outpath = os.path.join(build_src_include_dir, "pmix_event_strings.c")
        try:
            constants = open(outpath, "w+")
        except Exception as e:
            print("{outpath} CANNOT BE OPENED - EVENT STRINGS COULD NOT BE CONSTRUCTED: {e}"
                  .format(outpath=outpath, e=e))
            return 1

    # write the source file
    constants.write("""/*
 * This file is autogenerated by construct_event_strings.py.
 * Do not edit this file by hand.
 */

#include "src/include/pmix_event_strings.h"

const pmix_event_string_t pmix_event_strings[] = {
""")

    # scan across the header files in the src directory
    # looking for events

    # pmix_common.h.in is in the src tree
    rc = harvest_constants(options,
                           os.path.join(source_include_dir, "pmix_common.h.in"),
                           constants)
    if 0 != rc:
        constants.close()
        if outpath:
            os.remove(outpath)
        print("HARVEST PMIX_COMMON FAILED - EVENT STRINGS COULD NOT BE CONSTRUCTED")
        return 1
    constants.write(",\n\n")

    # pmix_deprecated.h is in the source tree
    rc = harvest_constants(options,
                           os.path.join(source_include_dir, "pmix_deprecated.h"),
                           constants)
    if 0 != rc:
        constants.close()
        if outpath:
            os.remove(outpath)
        print("HARVEST PMIX_DEPRECATED FAILED - EVENT STRINGS COULD NOT BE CONSTRUCTED")
        return 1
    constants.write(",\n\n")

    # pmix_error.h is in the source tree
    rc = harvest_constants(options,
                           os.path.join(util_include_dir, "pmix_error.h"),
                           constants)
    if 0 != rc:
        constants.close()
        if outpath:
            os.remove(outpath)
        print("HARVEST PMIX_ERROR FAILED - EVENT STRINGS COULD NOT BE CONSTRUCTED")
        return 1

    # mark the end of the array
    constants.write(""",\n
    {.index = UINT32_MAX, .name = "", .code = -1}
};
""")
    constants.write("\n")
    constants.close()

    # write the header
    return _write_header(options, build_src_include_dir, index + 1)


if __name__ == '__main__':
    exit(main())
