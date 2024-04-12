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
        return 1, 0

    # read the file - these files aren't too large
    # so ingest the whole thing at one gulp
    try:
        lines = inputfile.readlines()
    except Exception as e:
        print("Error reading file {path}: {e}"
              .format(path=path, e=e))
        inputfile.close()
        return 1, 0

    inputfile.close()  # we read everything, so done with the file

    firstline = True
    preamble = "                               \""
    linesize = 53
    # loop over the lines
    for n in range(len(lines)):
        line = lines[n]
        # remove white space at front and back
        myline = line.strip()
        # remove comment lines
        if "/*" in myline or "*/" in myline or myline.startswith("*"):
            continue
        # if the line starts with #define, then we want it
        if myline.startswith("#define"):
            value = myline[8:]
            # skip some well-known unwanted values
            if value.startswith("PMIx"):
                continue
            if value.startswith("PMIX_HAVE_VISIB"):
                continue
            if value.startswith("PMIX_LAUNCHER_RNDZ_FILE"):
                continue
            if value.startswith("PMIX_LAUNCHER_RNDZ_URI"):
                continue
            if value.startswith("PMIX_KEEPALIVE_PIPE"):
                continue
            tokens = value.split()
            if len(tokens) >= 2:
                if tokens[1][0] == '"':
                    if not firstline:
                        constants.write(",\n\n")
                    firstline = False
                    constants.write("    {.index = " + str(index) + ", .name = \"" + tokens[0] + "\", .string = " + tokens[1])
                    index = index + 1
                    # only one attribute violates the one-word rule for type
                    if tokens[0] == "PMIX_EVENT_BASE":
                        dstart = 3
                        datatype = "PMIX_POINTER"
                    elif tokens[0] == "PMIX_ATTR_UNDEF":
                        constants.write(", .type = PMIX_POINTER,\n     .description = (char *[]){\"NONE\", NULL}}")
                        continue
                    else:
                        dstart = 4
                        if tokens[3] == "(bool)":
                            datatype = "PMIX_BOOL"
                        elif tokens[3] == "(char*)":
                            datatype = "PMIX_STRING"
                        elif tokens[3] == "(pmix_rank_t)":
                            datatype = "PMIX_PROC_RANK"
                        elif tokens[3] == "(uint32_t)":
                            datatype = "PMIX_UINT32"
                        elif tokens[3] == "(int32_t)":
                            datatype = "PMIX_INT32"
                        elif tokens[3] == "(uint64_t)":
                            datatype = "PMIX_UINT64"
                        elif tokens[3] == "(int)":
                            datatype = "PMIX_INT"
                        elif tokens[3] == "(uint16_t)":
                            datatype = "PMIX_UINT16"
                        elif tokens[3] == "(pmix_data_array_t)" or tokens[3] == "(pmix_data_array_t*)":
                            datatype = "PMIX_DATA_ARRAY"
                        elif tokens[3] == "(pmix_proc_t*)":
                            datatype = "PMIX_PROC"
                        elif tokens[3] == "(pmix_coord_t*)":
                            datatype = "PMIX_COORD"
                        elif tokens[3] == "(pmix_coord_view_t)":
                            datatype = "PMIX_UINT8"
                        elif tokens[3] == "(float)":
                            datatype = "PMIX_FLOAT"
                        elif tokens[3] == "(pmix_byte_object_t)":
                            datatype = "PMIX_BYTE_OBJECT"
                        elif tokens[3] == "(hwloc_topology_t)":
                            datatype = "PMIX_POINTER"
                        elif tokens[3] == "(size_t)":
                            datatype = "PMIX_SIZE"
                        elif tokens[3] == "(pmix_data_range_t)":
                            datatype = "PMIX_UINT8"
                        elif tokens[3] == "(pmix_persistence_t)":
                            datatype = "PMIX_UINT8"
                        elif tokens[3] == "(pmix_scope_t)":
                            datatype = "PMIX_UINT8"
                        elif tokens[3] == "(pmix_status_t)":
                            datatype = "PMIX_STATUS"
                        elif tokens[3] == "(void*)":
                            datatype = "PMIX_POINTER"
                        elif tokens[3] == "(TBD)":
                            datatype = "TBD"
                        elif tokens[3] == "(time_t)":
                            datatype = "PMIX_TIME"
                        elif tokens[3] == "(pmix_envar_t*)":
                            datatype = "PMIX_ENVAR"
                        elif tokens[3] == "(pid_t)":
                            datatype = "PMIX_PID"
                        elif tokens[3] == "(pmix_proc_state_t)":
                            datatype = "PMIX_PROC_STATE"
                        elif tokens[3] == "(pmix_link_state_t)":
                            datatype = "PMIX_LINK_STATE"
                        elif tokens[3] == "(pointer)":
                            datatype = "PMIX_POINTER"
                        elif tokens[3] == "(pmix_topology_t)" or tokens[3] == "(pmix_topology_t*)":
                            datatype = "PMIX_TOPO"
                        elif tokens[3] == "(pmix_cpuset_t)" or tokens[3] == "(pmix_cpuset_t*)":
                            datatype = "PMIX_PROC_CPUSET"
                        elif tokens[3] == "(pmix_geometry_t)" or tokens[3] == "(pmix_geometry_t*)":
                            datatype = "PMIX_GEOMETRY"
                        elif tokens[3] == "(pmix_device_distance_t)" or tokens[3] == "(pmix_device_distance_t*)":
                            datatype = "PMIX_DEVICE_DIST"
                        elif tokens[3] == "(pmix_endpoint_t)" or tokens[3] == "(pmix_endpoint_t*)":
                            datatype = "PMIX_ENDPOINT"
                        elif tokens[3] == "(pmix_device_type_t)":
                            datatype = "PMIX_DEVTYPE"
                        elif tokens[3] == "(varies)":
                            datatype = "PMIX_INT"
                        elif tokens[3] == "(pmix_storage_accessibility_t)":
                            datatype = "PMIX_UINT64"
                        elif tokens[3] == "(pmix_storage_medium_t)":
                            datatype = "PMIX_UINT64"
                        elif tokens[3] == "(pmix_storage_persistence_t)":
                            datatype = "PMIX_UINT64"
                        elif tokens[3] == "(pmix_storage_access_type_t)":
                            datatype = "PMIX_UINT16"
                        elif tokens[3] == "(double)":
                            datatype = "PMIX_DOUBLE"
                        else:
                            print("UNKNOWN TOKEN: {tok}".format(tok=tokens[3]))
                            return 1
                    constants.write(", .type = " + datatype + ",\n     .description = (char *[]){\"")
                    # the description consists of at least all remaining tokens
                    m = dstart + 1
                    desc = tokens[dstart].replace("\"", "\\\"")
                    if "DEPRECATED" in desc:
                        constants.write(desc + "\"")
                        constants.write(", NULL}}")
                        continue
                    firstout = True
                    while m < len(tokens):
                        tmp = tokens[m].replace("\"", "\\\"")
                        if (len(tmp) + len(desc) + 1) > linesize:
                            if firstout:
                                constants.write(desc + "\"")
                            else:
                                constants.write(",\n" + preamble + desc + "\"")
                            firstout = False
                            desc = tmp
                        else:
                            desc += " " + tmp
                        m += 1
                    # if the next line starts with '/', then it is a continuation
                    # of the description
                    line = lines[n+1]
                    # remove white space at front and back
                    myline = line.strip()
                    while len(myline) > 0 and myline[0] == '/':
                        # step over until we see the beginning of text
                        m = 1
                        while myline[m] == '/' or myline[m] == ' ':
                            m += 1
                        # the rest of the line is part of the description
                        k = m
                        tmp = myline[k]
                        if tmp == '"':
                            tmp = "\\\""
                        k += 1
                        while k < len(myline):
                            while k < len(myline) and ' ' != myline[k]:
                                if myline[k] == '"':
                                    tmp += "\\\""
                                else:
                                    tmp += myline[k]
                                k += 1
                            if (len(tmp) + len(desc) + 1) > linesize:
                                if firstout:
                                    constants.write(desc + "\"")
                                else:
                                    constants.write(",\n" + preamble + desc + "\"")
                                firstout = False
                                desc = tmp
                            else:
                                if len(desc) > 0:
                                    desc += " " + tmp
                                else:
                                    desc = tmp
                            tmp = ""
                            k += 1
                        n += 1
                        line = lines[n+1]
                        myline = line.strip()
                    if len(desc) > 0:
                        if firstout:
                            constants.write(desc + "\"")
                        else:
                            constants.write(",\n" + preamble + desc + "\"")
                    # finish it up by closing the definition
                    constants.write(", NULL}}")

    inputfile.close()
    return 0


def _write_header(options, base_path, num_elements):
    contents = '''/*
 * This file is autogenerated by construct_dictionary.py.
 * Do not edit this file by hand.
 */

#include "src/include/pmix_config.h"
#include "src/include/pmix_globals.h"
#include "include/pmix_common.h"

#ifndef PMIX_DICTIONARY_H
#define PMIX_DICTIONARY_H

BEGIN_C_DECLS

PMIX_EXPORT extern const pmix_regattr_input_t pmix_dictionary[{ne}];

#define PMIX_INDEX_BOUNDARY {ne}

END_C_DECLS

#endif\n
'''.format(ne=num_elements)

    if options.dryrun:
        constants = sys.stdout
        outpath = None
    else:
        outpath = os.path.join(base_path, "pmix_dictionary.h")
        try:
            constants = open(outpath, "w+")
        except Exception as e:
            print("{outpath} CANNOT BE OPENED - DICTIONARY COULD NOT BE CONSTRUCTED: {e}"
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
        outpath = os.path.join(build_src_include_dir, "pmix_dictionary.c")
        try:
            constants = open(outpath, "w+")
        except Exception as e:
            print("{outpath} CANNOT BE OPENED - DICTIONARY COULD NOT BE CONSTRUCTED: {e}"
                  .format(outpath=outpath, e=e))
            return 1

    # write the source file
    constants.write("""/*
 * This file is autogenerated by construct_dictionary.py.
 * Do not edit this file by hand.
 */

#include "src/include/pmix_dictionary.h"

const pmix_regattr_input_t pmix_dictionary[] = {
""")

    # scan across the header files in the src directory
    # looking for constants and typedefs

    # pmix_common.h.in is in the src tree
    rc = harvest_constants(options,
                           os.path.join(source_include_dir, "pmix_common.h.in"),
                           constants)
    if 0 != rc:
        constants.close()
        if outpath:
            os.remove(outpath)
        print("HARVEST PMIX_COMMON FAILED - DICTIONARY COULD NOT BE CONSTRUCTED")
        return 1
    constants.write(",\n")

    # pmix_deprecated.h is in the source tree
    rc = harvest_constants(options,
                           os.path.join(source_include_dir, "pmix_deprecated.h"),
                           constants)
    if 0 != rc:
        constants.close()
        if outpath:
            os.remove(outpath)
        print("HARVEST PMIX_DEPRECATED FAILED - DICTIONARY COULD NOT BE CONSTRUCTED")
        return 1

    # mark the end of the array
    constants.write(""",\n
    {.index = UINT32_MAX, .name = "", .string = "", .type = PMIX_POINTER, .description = (char *[]){"NONE", NULL}}
};
""")
    constants.write("\n")
    constants.close()

    # write the header
    return _write_header(options, build_src_include_dir, index + 1)


if __name__ == '__main__':
    exit(main())
