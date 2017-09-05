#!/usr/bin/env python

# Copyright (c) 2017      IBM Corporation. All rights reserved.
# $COPYRIGHT$
#

import glob, os, re, shutil

projects= {'opal' : ["$(top_builddir)/opal/lib@OPAL_LIB_PREFIX@open-pal.la"],
           'orte' : ["$(top_builddir)/orte/lib@ORTE_LIB_PREFIX@open-rte.la"],
           'ompi' : ["$(top_builddir)/ompi/lib@OMPI_LIBMPI_NAME@.la"],
           'oshmem' : ["$(top_builddir)/oshmem/liboshmem.la"],
          }

no_anchor_file = []
missing_files = []
skipped_files = []
partly_files = []
updated_files = []

#
# Check of all of the libadd fields are accounted for in the LIBADD
# Return a list indicating which are missing (positional)
#
def check_libadd(content, libadd_field, project):
    global projects

    libadd_list = projects[project]
    libadd_missing = [True] * len(libadd_list)

    on_libadd = False
    for line in content:
        # First libadd line
        if re.search( r"^\s*"+libadd_field, line):
            # If line continuation, then keep searching after this point
            if line[-2] == '\\':
                on_libadd = True

            for idx, lib in enumerate(libadd_list):
                if True == libadd_missing[idx]:
                    if 0 <= line.find(lib):
                        libadd_missing[idx] = False

        # Line continuation
        elif True == on_libadd:
            for idx, lib in enumerate(libadd_list):
                if True == libadd_missing[idx]:
                    if 0 <= line.find(lib):
                        libadd_missing[idx] = False

            # No more line continuations, so stop processing
            if line[-2] != '\\':
                on_libadd = False
                break

    return libadd_missing

#
# Update all of the Makefile.am's with the proper LIBADD additions
#
def update_makefile_ams():
    global projects
    global no_anchor_file
    global missing_files
    global skipped_files
    global partly_files
    global updated_files

    for project, libadd_list in projects.items():
        libadd_str = " \\\n\t".join(libadd_list)

        print("="*40)
        print("Project: "+project)
        print("LIBADD:\n"+libadd_str)
        print("="*40)

        #
        # Walk the directory structure
        #
        for root, dirs, files in os.walk(project+"/mca"):
            parts = root.split("/")
            if len(parts) != 4:
                continue
            if parts[-1] == ".libs" or parts[-1] == ".deps" or parts[-1] == "base":
                continue
            if parts[2] == "common":
                continue

            print("Processing: "+root)

            #
            # Find Makefile.am
            #
            make_filename = os.path.join(root, "Makefile.am")
            if False == os.path.isfile( make_filename ):
                missing_files.append("Missing: "+make_filename)
                print("  ---> Error: "+make_filename+" is not present in this directory")
                continue

            #
            # Stearching for: mca_FRAMEWORK_COMPONENT_la_{LIBADD|LDFLAGS}
            # First scan file to see if it has an LIBADD / LDFLAGS
            #
            libadd_field  = "mca_"+parts[2]+"_"+parts[3]+"_la_LIBADD"
            ldflags_field = "mca_"+parts[2]+"_"+parts[3]+"_la_LDFLAGS"
            has_ldflags = False
            has_libadd  = False

            r_fd = open(make_filename, 'r')
            orig_content = r_fd.readlines()
            r_fd.close()
            libadd_missing = []

            for line in orig_content:
                if re.search( r"^\s*"+ldflags_field, line):
                    has_ldflags = True
                elif re.search( r"^\s*"+libadd_field, line):
                    has_libadd = True

            if True == has_libadd:
                libadd_missing = check_libadd(orig_content, libadd_field, project)

            #
            # Sanity Check: Was there an anchor field.
            # If not skip, we might need to manually update or it might be a
            # static component.
            #
            if False == has_ldflags and False == has_libadd:
                no_anchor_file.append("No anchor ("+ldflags_field+"): "+make_filename)
                print("  ---> Error: Makefile.am does not contain necessary anchor")
                continue

            #
            # Sanity Check: This file does not need to be updated.
            #
            if True == has_libadd and all(False == v for v in libadd_missing):
                skipped_files.append("Skip: "+make_filename)
                print("       Skip: Already updated Makefile.am")
                continue

            #
            # Now go though and create a new version of the Makefile.am
            #
            r_fd = open(make_filename, 'r')
            w_fd = open(make_filename+".mod", 'w')

            num_libadds=0
            for line in r_fd:
                # LDFLAGS anchor
                if re.search( r"^\s*"+ldflags_field, line):
                    w_fd.write(line)
                    # If there is no LIBADD, then put it after the LDFLAGS
                    if False == has_libadd:
                        w_fd.write(libadd_field+" = "+libadd_str+"\n")
                # Existing LIBADD field to extend
                elif 0 == num_libadds and re.search( r"^\s*"+libadd_field, line):
                    parts = line.partition("=")
                    num_libadds += 1

                    if parts[0][-1] == '+':
                        w_fd.write(libadd_field+" += ")
                    else:
                        w_fd.write(libadd_field+" = ")

                    # If all libs are missing, then add the full string
                    # Otherwise only add the missing items
                    if all(True == v for v in libadd_missing):
                        w_fd.write(libadd_str)
                        # Only add a continuation if there is something to continue
                        if 0 != len(parts[2].strip()):
                            w_fd.write(" \\")
                        w_fd.write("\n")
                    else:
                        partly_files.append("Partly updated: "+make_filename)
                        for idx, lib in enumerate(libadd_list):
                            if True == libadd_missing[idx]:
                                w_fd.write(lib+" \\\n")

                    # Original content (unless it's just a line continuation)
                    if 0 != len(parts[2].strip()) and parts[2].strip() != "\\":
                        w_fd.write("\t"+parts[2].lstrip())

                # Non matching line, just echo
                else:
                    w_fd.write(line)

            r_fd.close()
            w_fd.close()

            #
            # Replace the original with the updated version
            #
            shutil.move(make_filename+".mod", make_filename)
            updated_files.append(make_filename)


if __name__ == "__main__":

    update_makefile_ams()

    print("")

    print("="*40);
    print("{:>3} : Files skipped".format(len(skipped_files)))
    print("="*40);

    print("="*40);
    print("{:>3} : Files updated, but had some libs already in place.".format(len(partly_files)))
    print("="*40);
    for fn in partly_files:
        print(fn)

    print("="*40);
    print("{:>3} : Files fully updated".format(len(updated_files)))
    print("="*40);
    for fn in updated_files:
        print(fn)

    print("="*40);
    print("{:>3} : Missing Makefile.am".format(len(missing_files)))
    print("="*40);
    for err in missing_files:
        print(err)

    print("="*40);
    print("{:>3} : Missing Anchor for parsing (might be static-only components)".format(len(no_anchor_file)))
    print("="*40);
    for err in no_anchor_file:
        print(err)

