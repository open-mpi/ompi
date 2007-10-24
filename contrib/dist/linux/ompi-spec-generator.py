#!/usr/bin/env python
#
# generator for Open MPI
import sys
import os
import optparse
import ConfigParser

        
######################################################################
# global stuff
######################################################################
configext   = ".package"   # the name of the configurations files
configfiles = []           # list with all found config files
params      = 0            # contains the cmd line options
packages    = {}           # directory for packages 
options     = ["name", "type", "license", "summary", "files", "version", "description", "group", "vendor", "requires"]
shell_cmds  = {}
compilers   = { "default" : {"compiler":"default",
                             "cc": " ",
                             "cxx":" ",
                             "f77":" ",
                             "fc":" "},
                "gcc" :     {"compiler":"gcc",
                             "cc":"gcc",
                             "cxx":"c++",
                             "f77":"f77",
                             "fc":"gfortran"},
                "icc"  :    {"compiler":"icc",
                             "cc":"icc",
                             "cxx":"icpc",
                             "f77":"ifort",
                             "fc":"ifort"}
                }


copyright_template = """#
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
# Copyright (c) 2006      Cisco Systems, Inc.  All rights reserved.
# $COPYRIGHT$
#
# Additional copyrights may follow
#
# $HEADER$
#

# don't stop with an error if we don't pack all files at once
%define _unpackaged_files_terminate_build  0

"""

build_macro_template = "%%{!?build_%(name)s: %%define build_%(name)s %(value)i}\n"

build_command_template = '''
BUILD_PACKAGE=%(default)s
for entry in %(files)s; do
    for file in  $RPM_BUILD_ROOT/$entry; do
        if [ -e $file ] ; then
            BUILD_PACKAGE=1
        fi
    done
done
if [ $BUILD_PACKAGE == 1 ] ; then 
eval export OMPI_PACKAGE_VERSION=`%(version)s`
rpmbuild %(mode)s --define \'_topdir %%_topdir\'  --define \'build_%(name)s 1\' --define \'build_default 0\' --define \"ompi_package_version $OMPI_PACKAGE_VERSION\" %%{ompi_specfile}
fi
'''

global_template = """
#
# global Open MPI stuff
#
%%define ompi_name %(name)s
%%define ompi_name_prefix %(name_prefix)s
%%define ompi_version %(version)s
%%{!?ompi_package_version:%%define ompi_package_version default}
%%define ompi_extra_version %(extra_version)s
%%define ompi_release 1
%%define ompi_prefix  %(prefix)s
%%define ompi_build_root %%{_tmppath}/%%{ompi_name}-%%{ompi_version}-%%{ompi_release}-root
%%define ompi_source %%{ompi_name_prefix}%%{ompi_name}-%%{ompi_version}.tar.gz
%%define ompi_url %(url)s
%%define ompi_specfile %%{_topdir}/SPECS/%(output)s
%%define ompi_configure_params %(configure_params)s
%%define ompi_compile_root %%{ompi_name_prefix}%%{ompi_name}-%%{ompi_version}


#
# fix configure 
#
%%define _prefix %%{ompi_prefix}
%%define _sysconfdir %%{_prefix}/etc
%%define _libdir %%{_prefix}/lib
%%define _includedir %%{_prefix}/include
%%define _mandir %%{_prefix}/man
%%define _pkgdatadir %%{_prefix}/share/%%{ompi_name}

"""

compiler_template = """
#
# compiler settings
#
%%define ompi_compiler %(compiler)s
%%define ompi_cc  \"%(cc)s\"
%%define ompi_cxx \"%(cxx)s\"
%%define ompi_f77 \"%(f77)s\"
%%define ompi_fc  \"%(fc)s\"

"""

build_template = """
######################################################################
#
# Build section
#
######################################################################
%%if %%{build_build}
Summary: Configure and build the Open MPI tree
Name: %%{ompi_name_prefix}%%{ompi_name}
Version: %%{ompi_version}
Release: %%{ompi_release}
License: BSD
Group: Others
URL: %%{ompi_url}
Source0: %%{ompi_source}
BuildRoot: %%{ompi_build_root}

%%description
This part build and install the Open MPI source tree.

%%prep
%%setup -q

%%build
OMPI_CONFIGURE_FLAGS=\"%%{ompi_configure_params}\"
if [ \"%%{ompi_compiler}\" != \"default\" ]; then
OMPI_CONFIGURE_FLAGS=\"$OMPI_CONFIGURE_FLAGS CC=%%{ompi_cc} CXX=%%{ompi_cxx} F77=%%{ompi_f77} FC=%%{ompi_fc}\"
fi

%%configure $OMPI_CONFIGURE_FLAGS
make -j4

%%install

%%clean

%%files 
%%defattr(-,root,root,-)

%%endif 

"""


install_template = """
######################################################################
#
# Install section
#
######################################################################
%%if %%{build_install}
Summary: Install a already compiled tree 
Name: %%{ompi_name_prefix}%%{ompi_name}
Version: %%{ompi_version}
Release: %%{ompi_release}
License: BSD
Group: Others
URL: %%{ompi_url}
Source0: %%{ompi_source}
BuildRoot: %%{ompi_build_root}

%%description
This part build and install the Open MPI source tree.

%%prep

%%build


%%install
cd %%{ompi_compile_root}
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install


#
# create a module file on request
#
if [ %(modulefile_condition)s ] ; then 
%%{__mkdir_p} $RPM_BUILD_ROOT/%(modulefile_path)s/%%{ompi_name}/
cat <<EOF >$RPM_BUILD_ROOT/%(modulefile_path)s/%%{ompi_name}/%%{ompi_version}
#%%Module

# NOTE: This is an automatically-generated file!  (generated by the
# Open MPI RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

proc ModulesHelp { } {
   puts stderr \"This module adds Open MPI v%(ompi_version)s to various paths\"
}

module-whatis   \"Sets up Open MPI v%(ompi_version)s in your enviornment\"

append-path PATH \"%%{_prefix}/bin/\"
append-path LD_LIBRARY_PATH %%{_libdir}
append-path MANPATH %%{_mandir}
EOF
fi


#
# profile.d files
#
if [ %(profile_condition)s ] ; then
%%{__mkdir_p} $RPM_BUILD_ROOT/etc/profile.d/
cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%%{ompi_name}-%%{ompi_version}.sh
# NOTE: This is an automatically-generated file!  (generated by the
# Open MPI RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

CHANGED=0
if test -z \"`echo $PATH | grep %%{_prefix}/bin`\"; then
    PATH=\${PATH}:%%{_prefix}/bin/
    CHANGED=1
fi
if test -z \"`echo $LD_LIBRARY_PATH | grep %%{_libdir}`\"; then
    LD_LIBRARY_PATH=\${LD_LIBRARY_PATH}:%%{_libdir}
    CHANGED=1
fi
if test -z \"`echo $MANPATH | grep %%{_mandir}`\"; then
    MANPATH=\${MANPATH}:%%{_mandir}
    CHANGED=1
fi
if test \"$CHANGED\" = \"1\"; then
    export PATH LD_LIBRARY_PATH MANPATH
fi
EOF

cat <<EOF > $RPM_BUILD_ROOT/etc/profile.d/%%{ompi_name}-%%{ompi_version}s.csh
# NOTE: This is an automatically-generated file!  (generated by the
# Open MPI RPM).  Any changes made here will be lost a) if the RPM is
# uninstalled, or b) if the RPM is upgraded or uninstalled.

if (\"`echo $PATH | grep %%{_prefix}/bin`\") then
    setenv PATH \${PATH}:%%{_prefix}/bin/
endif
if (\"$?LD_LIBRARY_PATH\") then
    if (\"`echo $LD_LIBRARY_PATH | grep %%{_libdir}`\") then
        setenv LD_LIBRARY_PATH \${LD_LIBRARY_PATH}:%%{_libdir}
    endif
endif
if (\"$?MANPATH\") then
    if (\"`echo $MANPATH | grep %%{_mandir}`\") then
        setenv MANPATH \${MANPATH}:%%{_mandir}
    endif
endif
EOF
fi


%%clean

%%files 
%%defattr(-,root,root,-)

%%endif 

"""

package_template = """
######################################################################
#
# %(name)s package section
#
######################################################################
%%if %%{build_%(name)s}
Summary: %(summary)s
Name: %%{ompi_name_prefix}%%{ompi_name}-%(type)s-%(name)s
Version: %%{ompi_version}%(name)s_%%{ompi_package_version}%%{ompi_extra_version}
Release: %%{ompi_release}
License: %(license)s
Group: %(group)s
URL: %%{ompi_url}
Source0: %%{ompi_source}
BuildRoot: %%{ompi_build_root}
Requires: %(requires)s

%%description
%(description)s

%%prep

%%build

%%install
rm -f %(name)s.files
rm -f %(name)s.files.tmp
for i in %(installed_files)s; do
for file in $RPM_BUILD_ROOT/$i ; do
echo $file | sed "s#$RPM_BUILD_ROOT/##g" >> %(name)s.files.tmp
done
done
sort -u %(name)s.files.tmp > %(name)s.files
rm -f %(name)s.files.tmp

%%clean
for i in `cat %(name)s.files`; do
rm -f $RPM_BUILD_ROOT/$i
done
rm %(name)s.files

%%files -f %(name)s.files
%%defattr(-,root,root,-)

%%endif

"""

default_template = """
######################################################################
#
# default  
#
######################################################################
%%if %%{build_default}
Summary: Open MPI 
Name: %%{ompi_name_prefix}%%{ompi_name}
Version: %%{ompi_version}%%{ompi_extra_version}
Release: %%{ompi_release}
License: %%{ompi_license}
Group: Development/Library
URL: %%{ompi_url}
Source0: %%{ompi_source}
BuildRoot: %%{ompi_build_root}

%%description
Open MPI is a project combining technologies and resources from
several other projects (FT-MPI, LA-MPI, LAM/MPI, and PACX-MPI) in
order to build the best MPI library available.

This RPM contains all the tools necessary to compile, link, and run
Open MPI jobs. Additional this RPM also contains modules for communicating
via shared memory and TCP networks. Components for other transports (e.g.
Myrinet, Infiniband, ...) are provided in separate RPMs.


%%prep

%%build
%(build_cmds)s

%%install

%%clean
rm -rf $RPM_BUILD_ROOT

%%files
%%defattr(-,root,root,-)
%%{ompi_prefix}

%%endif

"""

changelog = """
#############################################################################
#
# Changelog
#
#############################################################################
%changelog
* Tue Jun 27 2006 Sven Stork <stork@hlrs.de>
- switch to specfile generator

* Wed Apr 26 2006 Jeff Squyres <jsquyres@cisco.com>
- Revamp files listings to ensure that rpm -e will remove directories
  if rpm -i created them.
- Simplify options for making modulefiles and profile.d scripts.
- Add oscar define.
- Ensure to remove the previous installation root during prep.
- Cleanup the modulefile specification and installation; also ensure
  that the profile.d scripts get installed if selected.
- Ensure to list sysconfdir in the files list if it's outside of the
  prefix.

* Wed Mar 30 2006 Jeff Squyres <jsquyres@cisco.com>
- Lots of bit rot updates
- Reorganize and rename the subpackages
- Add / formalize a variety of rpmbuild --define options
- Comment out the docs subpackage for the moment (until we have some
  documentation -- coming in v1.1!)

* Wed May 03 2005 Jeff Squyres <jsquyres@open-mpi.org>
- Added some defines for LANL defaults
- Added more defines for granulatirty of installation location for
  modulefile
- Differentiate between installing in /opt and whether we want to
  install environment script files
- Filled in files for man and mca-general subpackages

* Thu Apr 07 2005 Greg Kurtzer <GMKurtzer@lbl.gov>
- Added opt building
- Added profile.d/modulefile logic and creation
- Minor cleanups

* Fri Apr 01 2005 Greg Kurtzer <GMKurtzer@lbl.gov>
- Added comments
- Split package into subpackages
- Cleaned things up a bit
- Sold the code to Microsoft, and now I am retiring. Thanks guys!

* Wed Mar 23 2005 Mezzanine <mezzanine@kainx.org>
- Specfile auto-generated by Mezzanine



"""


######################################################################
#
# class to represent a package
#
######################################################################
class Package:
    def __init__(self, name):
        self.options = {}
        for option in options:
            if option == "name":
                self.options[option] = name
            elif option == "files":
                self.options[option] = []
            elif option == "version":
                self.options[option] = "/bin/echo unknown"
            elif option == "group":
                self.options[option] = "Development/Library"
            elif option == "license":
                self.options[option] = "BSD"
            elif option == "vendor":
                self.options[option] = "Open MPI"
            elif option == "requires":
                name_prefix = ""
                if ( params.ompi_name_prefix != "%{nil}" ):
                    name_prefix = params.ompi_name_prefix
                self.options[option] = name_prefix + params.ompi_name + " >= " + params.ompi_version
            else:
                self.options[option] = None            

    def getOption(self, option):
        if option in self.options.keys():
            return self.options[option]
        else:
            return None        

    def setOption(self, option, value):
        if ( option == "files" ):
            # append file list
            self.options["files"] += value.split()
        elif ( option == "requires"):
            self.options["requires"] += ", " + value
        else:
            # replace value
            self.options[option] = value

    def getOptions(self):
        return self.options

    def Validate(self):
        if ( self.options["files"] == [] ):
            error("Package %(package)s does not contain files\n" % {"package":self.options["name"]})
            sys.exit(-1)

    def Dump(self, prefix=""):
        global options
        for option in options:
            print "%(prefix)s %(name)-15s : %(value)s" % {"prefix":prefix, "name":option, "value":self.options[option]}


######################################################################
#
# return the specified package object. If not such an object exist a
# new object will be created and stored in the global packages list
#
######################################################################
def get_package(name):
    global packages
    if not (name in packages.keys()):
        packages[name] = Package(name)
    return packages[name]
    

######################################################################
#
# verbose output 
#
######################################################################
def verbose(msg):
    if (params.verbose or params.debug):
        print msg


######################################################################
#
# debug output 
#
######################################################################
def debug(msg):
    if (params.debug):
        print msg


######################################################################
#
# error output 
#
######################################################################
def error(msg):
    print "+++ ERROR : " + msg


######################################################################
#
# error output 
#
######################################################################
def  get_compiler(name):
    if name in compilers.keys():
        return compilers[name]
    else:
        error("Failed to find comiler : "+name);
        sys.exit(-1)


######################################################################
#
# interactive shell
#
######################################################################

def shell_help(cmd):
    for item in shell_cmds.values():
        print "%(cmd)-10s - %(help)s" % {"cmd":item["name"], "help":item["help"]}
    return True

def shell_quit(cmd):
    return False

def shell_list(cmd):
    return True

def shell_list(cmd):
    for package in packages.keys():
        print package
    return True

def shell_show(cmd):
    if len(cmd) < 2:
        print "Usage : show PACKAGE\n"
    else:
        if cmd[1] in packages.keys():
            package = packages[cmd[1]]
            package.Dump()
        else:
            print "Invalid package : " + cmd[1]
    return True

def shell_drop(cmd):
    if len(cmd) < 2:
        print "Usage : drop PACKAGE"
    else:
        if cmd[1] in packages.keys():
            packages.pop(cmd[1])
        else:
            print "Package not found : " + cmd[1]
    return True

def shell_write(cmd):
    write_specfile(packages.values())
    return True

def register_shell_cmd(name, help, function):
    shell_cmds[name] = {"name":name, "help":help, "function":function}

def shell():
    loop = True
    register_shell_cmd("help", "Display list of available commands.", shell_help)
    register_shell_cmd("quit", "Quit the interactive shell", shell_quit)
    register_shell_cmd("list", "List all found packages", shell_list)
    register_shell_cmd("show", "Display one specific package", shell_show)
    register_shell_cmd("drop", "Remove the specified package from the list", shell_drop)
    register_shell_cmd("write", "Write the specfile", shell_write)
    print "ompi-spec-generator interactive shell"
    print "Type 'help' to get more information about available commands."
    while loop:
        try:
            cmd = raw_input("ompi-spec-generator> ")
            cmd = cmd.split()
            if 0 == len(cmd):
                continue
            if ( cmd[0] in shell_cmds.keys() ):
                shell_cmd = shell_cmds[cmd[0]]
                shell_cmd_func = shell_cmd["function"]
                loop = shell_cmd_func(cmd)
            else:
                print "Invalid command"
        except Exception:
            print "\n"
            continue



######################################################################
#
# interactive shell
#
######################################################################
def write_specfile(build_packages):
    global params
    # create output file
    print "--> Create output file"
    verbose("     Open outout file : %(filename)s" % {"filename":params.output})
    specfile = open(params.output, 'w')
    verbose("     Write copyright header")
    specfile.write(copyright_template)

    verbose("     Create build macros")
    specfile.write("# macros to select which part of the specfile should be active\n")
    for package in build_packages:
        specfile.write(build_macro_template % {"name":package.getOption("name"), "value":0})
    specfile.write(build_macro_template % {"name":"build", "value":0})
    specfile.write(build_macro_template % {"name":"install", "value":0})
    specfile.write(build_macro_template % {"name":"default", "value":1})

    verbose("     Write global macro section")
    global_params = {"name":params.ompi_name, "prefix" : params.ompi_prefix, "url":params.ompi_url, "version":params.ompi_version, "output":params.output, "extra_version":params.ompi_extra_version, "configure_params":params.ompi_configure_params, "name_prefix":params.ompi_name_prefix}
    specfile.write(global_template % global_params)

    verbose("     Write compiler section")
    specfile.write(compiler_template % get_compiler(params.ompi_compiler))

    verbose("     Create package sections")
    build_cmds = ""
    if params.no_build:
        build_cmds += build_command_template % {"files":"/no_build", "default":"0", "name":"build", "mode":"-bc", "version":"/bin/echo unknown"}
    else:
        build_cmds += build_command_template % {"files":"/no_build", "default":"1", "name":"build", "mode":"-bc", "version":"/bin/echo unknown"}
    if params.no_install:
        build_cmds += build_command_template % {"files":"/no_install", "default":"0", "name":"install", "mode":"-bi", "version":"/bin/echo unknown"}
    else:
        build_cmds += build_command_template % {"files":"/no_install", "default":"1", "name":"install", "mode":"-bi", "version":"/bin/echo unknown"}
    for package in build_packages:
        verbose("      Create section for " + package.getOption("name"));
        package_params = package.getOptions()
        package_params["installed_files"] = ""
        for f in package_params["files"]:
            package_params["installed_files"] += "\"" + f + "\" "
        specfile.write(package_template % package_params);
        # create build command
        build_cmds += build_command_template % {"files":package_params["installed_files"], "default":"0", "name":package_params["name"], "mode":"-bb", "version":package_params["version"]}
        
    verbose("     Create build section")
    specfile.write(build_template % {"ompi_prefix":params.ompi_prefix})

    verbose("     Create install section")
    inst_params = {}
    inst_params["ompi_name"] = params.ompi_name
    inst_params["ompi_version"] = params.ompi_version
    if params.ompi_modulefile_path == None:
        inst_params["modulefile_condition"] = "1 == 0"
        inst_params["modulefile_path"] = "nirwana"
    else:
        inst_params["modulefile_condition"] = "0 == 0"
        inst_params["modulefile_path"] =  params.ompi_modulefile_path
    if params.ompi_profile_files:
        inst_params["profile_condition"] = "0 == 0"
    else:
        inst_params["profile_condition"] = "1 == 0"
    specfile.write(install_template % inst_params)
    
    verbose("     Create default section")
    default_params = { "build_cmds": build_cmds, "version":params.ompi_version}
    specfile.write(default_template % default_params)
    
    verbose("     Write changelog")
    specfile.write(changelog)

    verbose("     Close outputfile")
    specfile.close()



######################################################################
#
# main function
#
######################################################################
def main():
    # parse comand line parameters
    global params
    param_parser = optparse.OptionParser()
    param_parser.add_option("-r", "--root", action="store", dest="root", default="../../../",help="Specify the top root directory of the Open MPI Sources.")
    param_parser.add_option("-v", "--verbose", action="store_true", dest="verbose", default=False, help="Dis/Enable verbose output.")
    param_parser.add_option("-d", "--debug", action="store_true", dest="debug", default=False, help="Dis/Enable debug output.")
    param_parser.add_option("-p", "--packages", action="store", dest="packages", default=None, help="Comma separated list of packages to build.")
    param_parser.add_option("-o", "--output", action="store", dest="output", default=None, help="Specify the filename of the outputfile.")
    param_parser.add_option("-i", "--interactive", action="store_true", dest="interactive", default=False, help="Enter interactive shell after all config files have been parsed.")
    param_parser.add_option("--no-build", action="store_true", dest="no_build", default=False, help="Disable the build section.")
    param_parser.add_option("--no-install", action="store_true", dest="no_install", default=False, help="Disable the installation.")
    param_parser.add_option("--ompi-name", action="store", dest="ompi_name", default="openmpi", help="Specify the name.")
    param_parser.add_option("--ompi-prefix", action="store", dest="ompi_prefix", default="/opt/openmpi", help="Specify the installation prefix.")
    param_parser.add_option("--ompi-url", action="store", dest="ompi_url", default="http://www.open-mpi.org", help="Specify the tarball to use.")
    param_parser.add_option("--ompi-version", action="store", dest="ompi_version", default="1.1", help="Specify the version to use.")
    param_parser.add_option("--ompi-extra-version", action="store", dest="ompi_extra_version", default="%{nil}", help="Specify extra version to indicate special builds.")
    param_parser.add_option("--ompi-name-prefix", action="store", dest="ompi_name_prefix", default="%{nil}", help="Specify and name prefix for the RPMS.")
    param_parser.add_option("--ompi-configure-params", action="store", dest="ompi_configure_params", default="%{nil}", help="Specify extra version to indicate special builds.")
    param_parser.add_option("--ompi-compiler", action="store", dest="ompi_compiler", default="default", help="Specify the compiler to use.(default: let configure decide)")
    param_parser.add_option("--ompi-modulefile-path", action="store", dest="ompi_modulefile_path", default=None, help="Sets the path of the modulefile directory. If this parameter is omitted no modulefile will be generated.")
    param_parser.add_option("--ompi-profile-file", action="store_true", dest="ompi_profile_files", default=False, help="Add file to /etc/profile.d.")
    (params, args) = param_parser.parse_args()
    if ( params.root == None ):
        error("You must specify a the top root directory (option -r).")
        sys.exit(-1)
    # fix parameters
    params.root = os.path.abspath(params.root)
    if ( params.ompi_prefix == "/opt/openmpi" ):
        params.ompi_prefix = params.ompi_prefix + "/" + params.ompi_version

    print "--> Using root " + params.root
    if params.output == None:
        params.output = params.ompi_name_prefix + params.ompi_name + "-" + params.ompi_version + ".spec"

    # find config files
    print "--> Search for configuration files"
    for root, dirs, files in os.walk(params.root):
        for f in files:
            if configext != os.path.splitext(f)[1]:
                continue
            cf = root + "/" + f
            configfiles.append(cf)
            verbose("     Found : " + cf)

    # parse config files
    print "--> Parse config files"
    for file in configfiles:
        verbose("     Parse " + file)
        # parse configfile
        config = ConfigParser.ConfigParser()
        if not (file in  config.read(file)):
            error("Failed to parse " + file )
            sys.exit(-1)
        for section in config.sections():
            verbose("      Found package information : " + section)
            package = get_package(section)
            for option in config.options(section):
                debug("        Found " + option + "= " + config.get(section, option))
                # allow only predefined sections
                if not(option in options):
                    error("Parse error, found invalid option")
                    error("File    : " + file)
                    error("Package : " + section)
                    error("Option  : " + option)
                    sys.exit(-1)
                # add data into the package object
                package.setOption(option, config.get(section, option))

    # shell or not shell that's the question
    if params.interactive:
        shell()
        return
                    
    # filter packages
    print "--> Select packages"
    build_packages = []
    # filter packages 
    if params.packages != None:
        verbose("     Apply user profided packages list : " + params.packages);
        user_packages = params.packages.split(',')
        for name in packages.keys():
            if name in user_packages:
                build_packages.append(packages[name])
            else:
                verbose("      Remove package : " + name);
    else:
        # if nothing is specified than use all found packages
        build_packages = packages.values()

    # do sanity check on the components
    print "--> Sanity check packages"
    for package in build_packages:
        verbose("     Check package " + package.getOption("name") )
        if params.debug:
            package.Dump("     ")
        package.Validate()

    # write output file
    write_specfile(build_packages)

    # done
    print "--> Finished."
    

if ("__main__" == __name__):
    main()
