#!/usr/bin/env python3
#
# Copyright (c) 2022      Nanook Consulting. All rights reserved

import os, os.path, sys, shutil, signal
from optparse import OptionParser, OptionGroup

takeconst = False
takeapis = False
takedtypes = False
takemacros = False

def signal_handler(signal, frame):
    print("Ctrl-C received")
    sys.exit(0)

def harvest_constants(options, flag, src, strconsts, errconsts,
                      nconsts, typedefs, apis, macros):
    global takeconst, takeapis, takedtypes, takemacros

    if flag:
        path = os.path.join(options.src, src)
    else:
        path = os.path.join(options.tgt, src)

    # open the file
    try:
        inputfile = open(path, "r", encoding="utf-8")
    except:
        path = os.path.join(options.includedir, src)
        try:
            inputfile = open(path, "r", encoding="utf-8")
        except:
            print("File", path, "could not be opened")
            return -1
    # read the file - these files aren't too large
    # so ingest the whole thing at one gulp
    lines = inputfile.readlines()
    # cache the string constants, numeric, and typedef constants
    # in separate lists
    strconstlen = 0
    nconstlen = 0
    # loop over the lines
    for n in range(len(lines)):
        line = lines[n]
        # remove white space at front and back
        myline = line.strip()
        # remove comment lines
        if "/*" in myline or "*/" in myline or myline.startswith("*"):
            n += 1
            continue
        # if the line starts with #define, then we want it
        if (takeconst or takemacros) and myline.startswith("#define"):
            value = myline[8:]
             # skip some well-known unwanted values
            if value.startswith("PMIx"):
                continue
            if value.startswith("PMIX_HAVE_VISIB"):
                continue
            if "__attribute__" in value:
                continue
            if takeconst:
                tokens = value.split()
                if len(tokens) >= 2:
                    if tokens[1][0] == '"':
                        strconsts.append([tokens[0], tokens[1]])
                        if len(tokens[0]) > strconstlen:
                            strconstlen = len(tokens[0])
                    elif "PMIX_ERR_" in value or tokens[1].startswith("-"):
                        # numerical constant that looks just like a
                        # string constant - i.e., PMIX_ERR_FOO...1
                        # we output them in a separate section, but
                        # consider them string constants
                        uniq = True
                        for a in errconsts:
                            if tokens[0] == a[0]:
                                uniq = False
                                break
                        if uniq:
                            errconsts.append([tokens[0], tokens[1]])
                            if len(tokens[0]) > strconstlen:
                                strconstlen = len(tokens[0])
                    elif tokens[1].isdigit() or tokens[1].startswith("UINT") or tokens[1].startswith("0x"):
                        uniq = True
                        for a in nconsts:
                            if tokens[0] == a[0]:
                                uniq = False
                                break
                        if uniq:
                            nconsts.append([tokens[0], tokens[1]])
                            if len(tokens[0]) > nconstlen:
                                nconstlen = len(tokens[0])
            if takemacros:
                tokens = value.split()
                if len(tokens) >= 2:
                    if value.startswith("PMIX_") and \
                        "//" not in value and (value[-1] == '\\' or value.endswith(")")):
                        # find the closing paren as we don't want the rest
                        idx = value.find(")") + 1
                        macros.append(value[:idx])
                        # if the line ends in continuation, then 
                        # we have to take lines until we hit the end of the macro
                        if value[-1] == '\\':
                            n += 1
                            value = lines[n].strip()
                            while n < len(lines) and value[-1] == '\\':
                                n += 1
                                value = lines[n].strip()

        elif takeapis and myline.startswith("PMIX_EXPORT"):
            value = myline[12:].strip()
            # this is the name of an API - these
            # are frequently multi-line, so collect
            # all of them
            if ";" in value:
                value = value[:-1]
                newapi = [value]
                uniq = True
                for a in apis:
                    if newapi == a:
                        uniq = False
                        break
                if uniq:
                    apis.append(newapi)
            else:
                newapi = [value]
                apirunning = True
                while apirunning:
                    n += 1
                    value = lines[n].strip()
                    if ";" in value:
                        apirunning = False
                        value = value[:-1]
                    newapi.append(value)
                uniq = True
                for a in apis:
                    if newapi == a:
                        uniq = False
                        break
                if uniq:
                    apis.append(newapi)
        elif takedtypes and myline.startswith("typedef"):
            # there are three types of typedef's in PMIx:
            #
            # 1. simple one-line typedef - e.g., "typedef int foo"
            #
            # 2. multi-line struct types
            #
            # 3. function definitions - by PMIx convention, these
            #    always have an "fn_t" in the name
            #
            # 4. enum definitions
            #
            # start with the fourth option by looking for "enum"
            if "enum" in myline:
                # each line after this one should contain a name
                # so we just assign a value sequentially.
                counter = 0
                n += 1
                value = lines[n].strip()
                if ',' in value:
                    value = value[:-1]
                while '}' not in value and n < len(lines):
                    tokens[0] = value
                    tokens[1] = str(counter)
                    counter += 1
                    nconsts.append([tokens[0], tokens[1]])
                    if len(tokens[0]) > nconstlen:
                        nconstlen = len(tokens[0])
                    n += 1
                    value = lines[n].strip()
                    if ',' in value:
                        value = value[:-1]
                # the termination line contains the type name
                # for this enum - declare it as integer here
                value = "typedef int " + value[2:]
                uniq = True
                for a in typedefs:
                    if value == a:
                        uniq = False
                        break
                if uniq:
                    typedefs.append([value])
            # now address the first option - detectable by
            # having a semi-colon at the end of the line and
            # no "fn_t" or "cbfunc_t" in it
            elif ";" in myline and not "fn_t" in myline and not "cbfunc_t" in myline:
                value = myline[:-1]
                # check for pre-declaration statements of form
                # typedef struct foo foo
                ck = value.split()
                if len(ck) == 4 and ck[1] == "struct" and ck[2] == ck[3]:
                    n += 1
                    continue
                else:
                    uniq = True
                    for a in typedefs:
                        if value == a:
                            uniq = False
                            break
                    if uniq:
                        typedefs.append([value])
            # now check the third option by looking for
            # "fn_t" or "cbfunc_t" in it
            elif "fn_t" in myline or "cbfunc_t" in myline:
                if ";" in myline:
                    # this is a one-line function definition
                    value = myline[:-1]
                    uniq = True
                    for a in typedefs:
                        if value == a:
                            uniq = False
                            break
                    if uniq:
                        typedefs.append([value])
                else:
                    # this is a multi-line function definition
                    newdef = [myline]
                    defrunning = True
                    while defrunning:
                        n += 1
                        value = lines[n].strip()
                        if ";" in value:
                            defrunning = False
                            value = value[:-1]
                        newdef.append(value)
                    if takeapis:
                        uniq = True
                        for a in apis:
                            if value == a:
                                uniq = False
                                break
                        if uniq:
                            apis.append(newdef)
            # must be a multi-line struct type definition
            # we capture these in the typedef list
            # so we output their type definition
            else:
                # check for pre-declaration statements of form
                # typedef struct foo foo
                value = myline
                ck = value.split()
                if len(ck) == 4 and ck[1] == "struct" and ck[2] == ck[3]:
                    n += 1
                    continue
                else:
                    newdef = []
                    n += 1
                    value = lines[n].strip()
                    nbrk = 1
                    while nbrk > 0:
                        # avoid comments
                        if "/*" in value or "*/" in value or value.startswith("*"):
                            n += 1
                            value = lines[n].strip()
                            continue
                        if "}" in value:
                            nbrk -= 1
                            if nbrk > 0:
                                n += 1
                                value = lines[n].strip()
                            continue
                        if  "union" in value:
                            # we have to create another definition that contains
                            # just the union and then add that back into the
                            # current typedef
                            uniondef = []
                            n += 1
                            value = lines[n].strip()
                            while "}" not in value:
                                # terminate at the semi-colon
                                idx = value.rfind(';')
                                value = "    " + value[:idx]
                                # add it to the union def
                                uniondef.append(value)
                                n += 1
                                value = lines[n].strip()
                            # terminate at the semi-colon
                            idx = value.rfind(';')
                            value = value[:idx]
                            # extract the name of the union
                            idx = value.rfind(' ')
                            value = value[idx+1:]
                            # save this name
                            lowname = value
                            # capitalize the first letter
                            value = value.title()
                            # create the union definition - the output
                            # routine will prepend a 'c'
                            myvalue = "def union " + value + ":"
                            uniondef.insert(0, myvalue)
                            typedefs.append(uniondef)
                            # add the union to the struct
                            value = "    " + value + " " + lowname
                            newdef.append(value)
                            nbrk -= 1
                            n += 1
                            value = lines[n].strip()
                            continue
                        # terminate at the semi-colon
                        idx = value.rfind(';')
                        value = "    " + value[:idx]
                        newdef.append(value)
                        n += 1
                        value = lines[n].strip()
                    # we need to extract the type name
                    value = "typedef struct " + value[2:]
                    value = value[:-1] + ":"
                    newdef.insert(0, value)
                    uniq = True
                    for a in typedefs:
                        if newdef == a:
                            uniq = False
                            break
                    if uniq:
                        typedefs.append(newdef)
    return 0

def main():
    global takeconst, takeapis, takedtypes, takemacros
    signal.signal(signal.SIGINT, signal_handler)

    parser = OptionParser("usage: %prog [options]")
    debugGroup = OptionGroup(parser, "Debug Options")
    debugGroup.add_option("--debug",
                          action="store_true", dest="debug", default=False,
                          help="Output lots of debug messages while processing")
    debugGroup.add_option("--dryrun",
                          action="store_true", dest="dryrun", default=False,
                          help="Show commands, but do not execute them")
    parser.add_option_group(debugGroup)

    execGroup = OptionGroup(parser, "Execution Options")
    execGroup.add_option("--src", dest="src",
                         help="The directory where the source PMIx header files will be found")
    execGroup.add_option("--tgt", dest="tgt",
                         help="The directory where the target PMIx header files will be found")
    execGroup.add_option("--constants",
                         action="store_true", dest="constants", default=False,
                         help="Check constants")
    execGroup.add_option("--apis",
                         action="store_true", dest="apis", default=False,
                         help="Check APIs")
    execGroup.add_option("--datatypes",
                         action="store_true", dest="datatypes", default=False,
                         help="Check datatypes")
    execGroup.add_option("--macros",
                         action="store_true", dest="macros", default=False,
                         help="Check macros")
    parser.add_option_group(execGroup)

    (options, args) = parser.parse_args()

    if not options.src:
        print("Must specify source directory")
        sys.exit(1)
    # see if the source directory exists
    if not os.path.exists(options.src):
        print("SOURCE directory",options.src,"does not exist")
        sys.exit(1)

    if not options.tgt:
        print("Must specify target directory")
        sys.exit(1)
    # see if the include directory directory exists
    if not os.path.exists(options.tgt):
        print("Target directory",options.includedir,"does not exist")
        sys.exit(1)

    if not options.constants and not options.apis and not options.datatypes and not options.macros:
        takeconst = True
        takeapis = True
        takedtypes = True
        takemacros = True

    if options.constants:
        takeconst = True
    if options.apis:
        takeapis = True
    if options.datatypes:
        takedtypes = True
    if options.macros:
        takemacros = True

    if options.dryrun or options.debug:
        debug = True
    else:
        debug = False

    # scan across the header files in the src directory
    # looking for constants and typedefs
    srcconsts = []
    srcerr = []
    srcnconsts = []
    srctypes = []
    srcapis = []
    srcmacros = []

    if harvest_constants(options, True, "pmix_common.h.in", srcconsts,
                         srcerr, srcnconsts, srctypes, srcapis, srcmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, True, "pmix.h", srcconsts,
                         srcerr, srcnconsts, srctypes, srcapis, srcmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, True, "pmix_server.h", srcconsts,
                         srcerr, srcnconsts, srctypes, srcapis, srcmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, True, "pmix_tool.h", srcconsts,
                         srcerr, srcnconsts, srctypes, srcapis, srcmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, True, "pmix_deprecated.h", srcconsts,
                         srcerr, srcnconsts, srctypes, srcapis, srcmacros) != 0:
        sys.exit(1)
    

    # do the same for the tgt directory
    tgtconsts = []
    tgterr = []
    tgtnconsts = []
    tgttypes = []
    tgtapis = []
    tgtmacros = []

    if harvest_constants(options, False, "pmix_common.h.in", tgtconsts,
                         tgterr, tgtnconsts, tgttypes, tgtapis, tgtmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, False, "pmix.h", tgtconsts,
                         tgterr, tgtnconsts, tgttypes, tgtapis, tgtmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, False, "pmix_server.h", tgtconsts,
                         tgterr, tgtnconsts, tgttypes, tgtapis, tgtmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, False, "pmix_tool.h", tgtconsts,
                         tgterr, tgtnconsts, tgttypes, tgtapis, tgtmacros) != 0:
        sys.exit(1)

    if harvest_constants(options, False, "pmix_deprecated.h", tgtconsts,
                         tgterr, tgtnconsts, tgttypes, tgtapis, tgtmacros) != 0:
        sys.exit(1)


    added = []
    redefined = []
    removed = []

    # cross check the constants
    for s in srcconsts:
        match = False
        for t in tgtconsts:
            if s == t:
                match = True
                break
            elif s[0] == t[0]:
                match = True
                redefined.append(["Attributes", s])
                redefined.append(["Attributes", t])
                break;
        if not match:
            added.append(["Attributes", s])
    # check the reverse
    for t in tgtconsts:
        match = False
        for s in srcconsts:
            if s == t:
                match = True
                break
        if not match:
            removed.append(["Attributes", t])

    # cross check the error codes
    for s in srcerr:
        match = False
        for t in tgterr:
            if s == t:
                match = True
                break
            elif s[0] == t[0]:
                match = True
                redefined.append(["Error constants", s])
                redefined.append(["Error constants", t])
                break;
        if not match:
            added.append(["Error constants", s])
    # check the reverse
    for t in tgterr:
        match = False
        for s in srcerr:
            if s == t:
                match = True
                break
        if not match:
            removed.append(["Error constants", t])

    # cross check the numerical constants
    for s in srcnconsts:
        match = False
        for t in tgtnconsts:
            if s == t:
                match = True
                break
            elif s[0] == t[0]:
                match = True
                redefined.append(["Numerical constants", s])
                redefined.append(["Numerical constants", t])
                break;
        if not match:
            added.append(["Numerical constants", s])
    # check the reverse
    for t in tgtnconsts:
        match = False
        for s in srcnconsts:
            if s == t:
                match = True
                break
        if not match:
            removed.append(["Numerical constants", t])

    # cross check the data types
    for s in srctypes:
        match = False
        for t in tgttypes:
            if s == t:
                match = True
                break
            elif "pmix_server_module_t" in s[0] and "pmix_server_module_t" in t[0]:
                match = True
                # we have to check the server module fields individually
                submatch = False
                for sm in s[1:]:
                    smt = sm.split()
                    for tm in t[1:]:
                        tmt = tm.split()
                        if tmt == smt:
                            submatch = True
                            break
                    if not submatch:
                        redefined.append(["Server module functions", s[0]])
            elif s[0] == t[0]:
                match = True
                redefined.append(["Types", s])
                redefined.append(["Types", t])
                break;
        if not match:
            added.append(["Types", s])
    # check the reverse
    for t in tgttypes:
        # we already dealt with the server module special case
        if "pmix_server_module_t" in t[0]:
            continue
        match = False
        for s in srctypes:
            if s == t:
                match = True
                break
            elif "pmix_server_module_t" in s[0]:
                match = True
                continue
        if not match:
            removed.append(["Types", t])

    # cross check the APIs
    for s in srcapis:
        match = False
        for t in tgtapis:
            if s == t:
                match = True
                break
            elif s[0] == t[0]:
                match = True
                redefined.append(["APIs", s])
                redefined.append(["APIs", t])
                break;
        if not match:
            added.append(["APIs", s])
    # check the reverse
    for t in tgtapis:
        match = False
        for s in srcapis:
            if s == t:
                match = True
                break
        if not match:
            removed.append(["APIs", t])

    # cross check the macros
    for s in srcmacros:
        match = False
        for t in tgtmacros:
            if s == t:
                match = True
                break
        if not match:
            added.append(["Macros", s])
    # check the reverse
    for t in tgtmacros:
        match = False
        for s in srcmacros:
            if s == t:
                match = True
                break
        if not match:
            removed.append(["Macros", t])

    # output the results
    if 0 < len(added):
        print("The following items have been ADDED to the library")
        header = ""
        for i in added:
            if header != i[0]:
                print("\n", i[0].upper())
                header = i[0]
            if "APIS" == i[0].upper():
                loc = 0
                for a in i[1]:
                    if 0 == loc:
                        loc = a.find("(") + 1
                        if "fn_t" in a:
                            loc = a.find("(", loc) + 1
                        print("\t" + a)
                    else:
                        outstr = "\t" + a.rjust(len(a) + loc)
                        print(outstr)
            elif "MACROS" == i[0].upper():
                print("\t" + i[1][0])
            else:
                outstr = "\t" + ' = '.join(i[1])
                print(outstr)
    else:
        print("No items have been ADDED to the library")

    if 0 < len(redefined):
        print("\n\nThe following items have been MODIFIED in the library")
        header = ""
        i = 0
        while i < len(redefined):
            if header != redefined[i][0]:
                print("\n", redefined[i][0].upper())
                header = redefined[i][0]
            if "APIS" == redefined[i][0].upper():
                loc = 0
                for a in redefined[i][1]:
                    if 0 == loc:
                        loc = a.find("(") + 1
                        print("\t" + a)
                    else:
                        outstr = "\t" + a.rjust(len(a) + loc)
                        print(outstr)
                i += 1
                loc = 0
                for a in redefined[i][1]:
                    if 0 == loc:
                        loc = a.find("(") + 1
                        print("\t" + a)
                    else:
                        outstr = "\t" + a.rjust(len(a) + loc)
                        print(outstr)
                i += 1
            elif "MACROS" == redefined[i][0].upper():
                 print("\t" + redefined[i][1])
                 i += 1
                 print("\t" + redefined[i][1])
                 i += 1
            else:
                outstr = "\t" + ' = '.join(redefined[i][1])
                print(outstr)
                i += 1
                outstr = "\t" + ' = '.join(redefined[i][1])
                print(outstr)
                i += 1
    else:
        print("\n\nNo items have been MODIFIED in the library")

    if 0 < len(removed):
        print("\n\nThe following items have been REMOVED from the library")
        header = ""
        for i in removed:
            if header != i[0]:
                print("\n", i[0].upper())
                header = i[0]
            if "APIS" == header.upper():
                loc = 0
                for a in i[1]:
                    if 0 == loc:
                        loc = a.find("(") + 1
                        print("\t" + a)
                    else:
                        outstr = "\t" + a.rjust(len(a) + loc)
                        print(outstr)
            elif "MACROS" == i[0].upper():
                 print("\t" + i[1])
            else:
                outstr = "\t" + ' = '.join(i[1])
                print(outstr)
    else:
        print("\n\nNo items have been REMOVED from the library")

if __name__ == '__main__':
    main()

