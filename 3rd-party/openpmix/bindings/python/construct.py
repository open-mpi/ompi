#!/usr/bin/env python3
#
# Copyright (c) 2022      Nanook Consulting. All rights reserved

import os, os.path, sys, shutil, signal
from optparse import OptionParser, OptionGroup

takeconst = False
takeapis = False
takedtypes = False

def signal_handler(signal, frame):
    print("Ctrl-C received")
    sys.exit(0)

def harvest_constants(options, src, constants, definitions):
    global takeconst, takeapis, takedtypes

    path = os.path.join(options.src, src)
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
    strconsts = []
    strconstlen = 0
    errconsts = []
    nconsts = []
    nconstlen = 0
    typedefs = []
    apis = []
    # loop over the lines
    for n in range(len(lines)):
        line = lines[n]
        # remove white space at front and back
        myline = line.strip()
        # remove comment lines
        if "/*" in myline or "*/" in myline or myline.startswith("*"):
            if "DUPLICATES" in myline:
                break
            n += 1
            continue
        # if the line starts with #define, then we want it
        if takeconst and myline.startswith("#define"):
            value = myline[8:]
            # skip some well-known unwanted values
            if value.startswith("PMIx"):
                continue
            if value.startswith("PMIX_HAVE_VISIB"):
                continue
            tokens = value.split()
            if len(tokens) >= 2:
                if tokens[1][0] == '"':
                    strconsts.append(tokens[0])
                    if len(tokens[0]) > strconstlen:
                        strconstlen = len(tokens[0])
                elif "PMIX_ERR_" in value or tokens[1].startswith("-"):
                    # numerical constant that looks just like a
                    # string constant - i.e., PMIX_ERR_FOO...1
                    # we output them in a separate section, but
                    # consider them string constants
                    errconsts.append(tokens[0])
                    if len(tokens[0]) > strconstlen:
                        strconstlen = len(tokens[0])
                elif tokens[1].isdigit() or tokens[1].startswith("UINT") or tokens[1].startswith("0x"):
                    # values that were defined as UINT32_MAX need to be converted
                    # to hex values as Python otherwise gets confused
                    if tokens[1] == "UINT32_MAX":
                        tokens[1] = "0xffffffff"
                    elif "UINT32_MAX-1" in tokens[1]:
                        tokens[1] = "0xfffffffe"
                    elif "UINT32_MAX-2" in tokens[1]:
                        tokens[1] = "0xfffffffd"
                    elif "UINT32_MAX-3" in tokens[1]:
                        tokens[1] = "0xfffffffc"
                    elif "UINT8_MAX" in tokens[1]:
                        tokens[1] = "0xff"
                    nconsts.append([tokens[0], tokens[1]])
                    if len(tokens[0]) > nconstlen:
                        nconstlen = len(tokens[0])
        elif takeapis and myline.startswith("PMIX_EXPORT"):
            value = myline[12:].strip()
            # this is the name of an API - these
            # are frequently multi-line, so collect
            # all of them
            if ";" in value:
                value = value[:-1]
                # check for bool type - must be converted to bint
                value = value.replace("bool ", "bint ")
                # a single-line API might have a "void" arg
                # Python doesn't accept "void" as an arg, so
                # we have to "snip" it out
                start = value.find("(") + 1
                end = value.find(")")
                snip = value[start:end]
                if snip == "void":
                    value = value[0:start] + value[end:]
                newapi = [value]
                apis.append(newapi)
            else:
                value = value.replace("bool ", "bint ")
                newapi = [value]
                apirunning = True
                while apirunning:
                    n += 1
                    value = lines[n].strip()
                    # check for bool type - must be converted to bint
                    value = value.replace("bool ", "bint ")
                    if ";" in value:
                        apirunning = False
                        value = value[:-1]
                    newapi.append(value)
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
                typedefs.append([value])
            # now address the first option - detectable by
            # having a semi-colon at the end of the line and
            # no "fn_t" or "cbfunc_t" in it
            elif ";" in myline and not "fn_t" in myline and not "cbfunc_t" in myline:
                value = myline[:-1]
                # check for bool type - must be converted to bint
                if "bool" in value:
                    value.replace("bool ", "bint ")
                # check for pre-declaration statements of form
                # typedef struct foo foo
                ck = value.split()
                if len(ck) == 4 and ck[1] == "struct" and ck[2] == ck[3]:
                    n += 1
                    continue
                else:
                    # check for a typedef that includes a named value
                    # of either PMIX_MAX_NSLEN or PMIX_MAX_KEYLEN
                    if "PMIX_MAX_NSLEN+1" in value:
                        value = value.replace("PMIX_MAX_NSLEN+1", str(256))
                    elif "PMIX_MAX_NSLEN" in value:
                        value = value.replace("PMIX_MAX_NSLEN", str(255))
                    elif "PMIX_MAX_KEYLEN+1" in value:
                        value = value.replace("PMIX_MAX_KEYLEN+1", str(512))
                    elif "PMIX_MAX_KEYLEN" in value:
                        value = value.replace("PMIX_MAX_KEYLEN", str(511))
                    typedefs.append([value])
            # now check the third option by looking for
            # "fn_t" or "cbfunc_t" in it
            elif "fn_t" in myline or "cbfunc_t" in myline:
                if ";" in myline:
                    # this is a one-line function definition
                    value = myline[:-1]
                    # check for bool type - must be converted to bint
                    if "bool" in value:
                        value.replace("bool ", "bint ")
                    typedefs.append([value])
                else:
                    # this is a multi-line function definition
                    # check for bool type - must be converted to bint
                    if "bool" in myline:
                        myline.replace("bool ", "bint ")
                    newdef = [myline]
                    defrunning = True
                    while defrunning:
                        n += 1
                        value = lines[n].strip()
                        # check for bool type - must be converted to bint
                        if "bool" in value:
                            value.replace("bool ", "bint ")
                        if ";" in value:
                            defrunning = False
                            value = value[:-1]
                        newdef.append(value)
                    typedefs.append(newdef)
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
                                # check for bool type - must be converted to bint
                                if "bool" in value:
                                    value = value.replace("bool ", "bint ")
                                elif "struct timeval " in value:
                                    value = value.replace("struct timeval", "timeval")
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
                        # check for bool type - must be converted to bint
                        if "bool" in value:
                            value = value.replace("bool ", "bint ")
                        elif "struct timeval " in value:
                            value = value.replace("struct timeval", "timeval")
                        # we don't want any dimensions - if given, convert
                        # to the corresponding value
                        idx = value.find('[')
                        if idx >= 0:
                            # find the end of the dimension
                            dim = value.rfind(']')
                            dimstr = value[idx+1:dim]
                            # have to do this manually
                            if "MAX_KEYLEN" in dimstr:
                                value = value[:idx] + "[512]" + value[dim+1:]
                            elif "MAX_NSLEN" in dimstr:
                                value = value[:idx] + "[256]" + value[dim+1:]
                            else:
                                print("BAD DIMENSION " + dimstr)
                                exit(1)
                        newdef.append(value)
                        n += 1
                        value = lines[n].strip()
                    # we need to extract the type name
                    value = "typedef struct " + value[2:]
                    value = value[:-1] + ":"
                    newdef.insert(0, value)
                    typedefs.append(newdef)
    # only write the data sources once per file
    defsrc = False
    constsrc = False

    # start by pretty-printing the string constants
    # prepended with an underscore to avoid conflicts
    # with the Python version of the name
    if takeconst and len(strconsts) > 0:
        if not defsrc:
            definitions.write("cdef extern from \"" + src + "\":\n")
            defsrc = True
        if not constsrc:
            constants.write("# " + src + "\n")
            constsrc = True
        definitions.write("\n    # STRING CONSTANTS\n")
        for const in strconsts:
            defname = "_" + const
            definitions.write("    cdef const char* " + defname)
            for i in range (4 + strconstlen - len(const)):
                definitions.write(" ")
            definitions.write("\"" + const + "\"\n")
            # now output it into the constants file
            constants.write(const)
            for i in range (4 + strconstlen - len(const)):
                constants.write(" ")
            constants.write("= " + defname + "\n")
        # add some space
        definitions.write("\n")
        constants.write("\n")

    if takeconst and len(errconsts) > 0:
        if not defsrc:
            definitions.write("cdef extern from \"" + src + "\":\n")
            defsrc = True
        if not constsrc:
            constants.write("# " + src + "\n")
            constsrc = True
        definitions.write("\n    # ERROR CONSTANTS\n")
        for const in errconsts:
            defname = "_" + const
            definitions.write("    cdef int " + defname)
            for i in range (4 + strconstlen - len(const)):
                definitions.write(" ")
            definitions.write("\"" + const + "\"\n")
            # now output it into the constants file
            constants.write(const)
            for i in range (4 + strconstlen - len(const)):
                constants.write(" ")
            constants.write("= " + defname + "\n")
        # add some space
        definitions.write("\n")
        constants.write("\n")

    if takeconst and len(nconsts) > 0:
        if not constsrc:
            constants.write("# " + src + "\n")
            constsrc = True
        # pretty-print the numeric constants
        for num in nconsts:
            constants.write(num[0])
            for i in range (4 + strconstlen - len(num[0])):
                constants.write(" ")
            constants.write("= " + num[1] + "\n")
        # add some space
        constants.write("\n")

    # pretty-print any typedefs
    if takedtypes and len(typedefs) > 0:
        if not defsrc:
            definitions.write("cdef extern from \"" + src + "\":\n")
            defsrc = True
        definitions.write("\n    # TYPEDEFS\n")
        for t in typedefs:
            definitions.write("    c" + t[0] + "\n")
            if len(t) > 1:
                # find the 2nd opening paren
                idx = t[0].find("(") + 1
                idx = t[0].find("(", idx) + 2
                for n in range(1, len(t)):
                    definitions.write("    ")
                    for m in range(idx):
                        definitions.write(" ")
                    definitions.write(t[n] + "\n")
            definitions.write("\n")
        # add some space
        definitions.write("\n")

    # pretty-print any APIs
    if takeapis and len(apis) > 0:
        if not defsrc:
            definitions.write("cdef extern from \"" + src + "\":\n")
            defsrc = True
        definitions.write("\n    # APIS\n")
        for api in apis:
            fill = 5 + api[0].find("(")
            fstring = "    {}" + ("\n" + " " * fill + "{}") * (len(api)-1) + " nogil\n\n"
            definitions.write(fstring.format(*api))
    return 0

def main():
    global takeconst, takeapis, takedtypes
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
                         help="The directory where the PMIx header files will be found")
    execGroup.add_option("--include-dir", dest="includedir",
                         help="The directory where the generated PMIx header files will be found")
    execGroup.add_option("--constants",
                         action="store_true", dest="constants", default=False,
                         help="Translate constants")
    execGroup.add_option("--apis",
                         action="store_true", dest="apis", default=False,
                         help="Translate APIs")
    execGroup.add_option("--datatypes",
                         action="store_true", dest="datatypes", default=False,
                         help="Translate datatypes")
    parser.add_option_group(execGroup)

    (options, args) = parser.parse_args()

    if not options.constants and not options.apis and not options.datatypes:
        takeconst = True
        takeapis = True
        takedtypes = True

    if options.constants:
        takeconst = True
    if options.apis:
        takeapis = True
    if options.datatypes:
        takedtypes = True

    if options.dryrun or options.debug:
        debug = True
    else:
        debug = False

    if options.src:
        # see if the source directory exists
        if not os.path.exists(options.src):
            print("SOURCE directory",options.src,"does not exist")
            sys.exit(1)

    if options.includedir:
        # see if the include directory directory exists
        if not os.path.exists(options.includedir):
            print("Include directory",options.includedir,"does not exist")
            sys.exit(1)

    if options.dryrun:
        constants = sys.stdout
        definitions = sys.stdout
    else:
        # open the .pxd file for the definitions
        # if the output file exists, remove it
        if os.path.exists("pmix_constants.pxd"):
            if debug:
                print("Remove pmix_constants.pxd")
            if not options.dryrun:
                os.remove("pmix_constants.pxd")
        elif debug:
            print("File pmix_constants.pxd not found")
        definitions = open("pmix_constants.pxd", "w+")
        # add the necessary imports
        definitions.write("from posix.types cimport *\n")
        definitions.write("from posix.time cimport *\n")
        definitions.write("from libc.stdint cimport *\n\n")

        # open the .pxi file for the Python-level constants
        if os.path.exists("pmix_constants.pxi"):
            if debug:
                print("Remove pmix_constants.pxi")
            if not options.dryrun:
                os.remove("pmix_constants.pxi")
        elif debug:
            print("File pmix_constants.pxi not found")
        constants = open("pmix_constants.pxi", "w+")
        # add the necessary import and provide a little space for neatness
        constants.write("from pmix_constants cimport *\n\n")

    # scan across the header files in the src directory
    # looking for constants and typedefs
    # add some space
    if harvest_constants(options, "pmix_common.h", constants, definitions) != 0:
        sys.exit(1)
    definitions.write("\n\n")
    constants.write("\n\n")
    harvest_constants(options, "pmix.h", constants, definitions)
    # add some space
    definitions.write("\n\n")
    constants.write("\n\n")
    harvest_constants(options, "pmix_server.h", constants, definitions)
    # add some space
    definitions.write("\n\n")
    constants.write("\n\n")
    harvest_constants(options, "pmix_tool.h", constants, definitions)
    # add some space
    definitions.write("\n\n")
    constants.write("\n\n")
    harvest_constants(options, "pmix_deprecated.h", constants, definitions)
    # close the files to ensure all output is written
    constants.close()
    definitions.close()
    
if __name__ == '__main__':
    main()

