//
// $HEADER$
//
/** @file **/

#include "ompi_config.h"

#include <iostream>
#include <string>
#include <utility>
#include <list>

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/param.h>
#include <errno.h>

#include "runtime/runtime.h"
#include "util/output.h"
#include "util/cmd_line.h"
#include "communicator/communicator.h"
#include "mca/base/base.h"
#include "tools/ompi_info/ompi_info.h"


/*
 * This will ultimately end up being the manual for ompi_info
 *
 * ompi_info - Display configuration information about OMPI/MPI
 * SYNTAX
 * ompi_info [-arch] [-config] [-help|-h] [-param <type> <module>]
 * [-parsable|-pretty] [-path <item>] [-version <item> <scope>]
 * OPTIONS
 * -all          Show all configuration information
 * -arch         Show architecture that OMPI was configured for
 * -config       Show some information about OMPI configuration
 * -help         Show help message
 * -param        Show MCA parameter(s) for a given MCA type and module
 * -parsable     Show output in an easily parsable format
 * -path <item>  Print a given configuration path
 * -pretty       Show output in a prettyprint format (default)
 * -version      Print some or part of a version number of a given item
 * DESCRIPTION
 * The  ompi_info  command  is  used  to display information about a OMPI/MPI
 * installation.  Particularly with the MCA run-time module selection sys-
 * tem, the ompi_info command can be useful to scripts and resource managers
 * to determine the capabilities of the installed OMPI/MPI in order to pass
 * run-time parameters to MPI programs.
 * Output  can be displayed in a "pretty" format (i.e., suitable for human
 * reading) and also in a parsable format (i.e., suitable for easy parsing
 * by  scripts or other automated mechanisms).  There are no other OMPI API
 * functions to retrieve this data (in any language); the ompi_info  command
 * is  the  best  mechanism  to obtain any configuration information about
 * OMPI/MPI.  The parsable output was designed such that  common  utilities
 * such  as grep, awk, cut, and sed can easily be used to extract relevant
 * information.
 * Running ompi_info with no arguments will display a subset  of  configura-
 * tion  parameters  in  the  "pretty"  format  (see the EXAMPLES section,
 * below).  Several command line options are available  to  limit  exactly
 * which  information  is displayed.  These options, when used in conjunc-
 * tion with the parsable output, can provide  automated  mechanisms  spe-
 * cific information about the capabilities of OMPI/MPI.
 * GENERAL PARAMETERS
 * The  -pretty  and -parsable switches are used to select whether to dis-
 * play the output in "pretty" or machine-parsable  format,  respectively.
 * If neither is specified, -pretty is the default.
 * The -arch switch will display the architecture that OMPI/MPI was config-
 * ured and compiled on.
 * The -config switch will display  a  set  of  configuration  information
 * about  the  MPI  capabilities  of OMPI/MPI, such as whether there are C,
 * C++, and Fortran MPI bindings available, whether there is MPI profiling
 * support  for  C,  C++, and Fortran, whether ROMIO support is available,
 * whether IMPI support is available, whether debugging support is  avail-
 * able  (mostly  for OMPI/MPI maintainers), and whether OMPI/MPI is "purify
 * clean" (meaning that it is suitable for use with memory checking debug-
 * gers).   Most of these are options to the OMPI/MPI configure script, and
 * are configure/compile-time selections that cannot be changed  once  OMPI
 * has been installed.  While there is no fine-grained control to individ-
 * ually request each of these pieces of  information,  using  -config  in
 * conjunction  with  -parsable  and  commands such as grep can return any
 * individual piece of information.
 * PARAM PARAMETERS
 * The -param switch can be used to  show  available  MCA  parameters  and
 * their  default  values.   The  type and module arguments can be used to
 * specify a particular MCA type and/or module, or use the special keyword
 * "all" to indicate all available MCA types/modules (respectively).
 * Available MCA types are:
 * all         Show all MCA types
 * base        Intrinsic OMPI/MPI parameters
 * boot        Boot modules (e.g., ompi_boot)
 * coll        MPI collectives
 * cr          Checkpoint / restart
 * RPI         MPI point-to-point.
 * The  names  of  the modules that are available are dependant upon which
 * modules are available for any given type.   See  EXAMPLES,  below,  for
 * example usage.
 * PATH PARAMETERS
 * The -path switch returns various paths that were compiled into OMPI/MPI.
 * These were all decided when OMPI was configured, and cannot  be  changed
 * at run-time.  However, knowing the location of these directories can be
 * useful in order to find OMPI data files, binaries, include  files,  etc.
 * The  -path  switch  takes  a parameter: item.  Pomcable values for item
 * are:
 * prefix      Display the prefix directory for OMPI/MPI
 * bindir      Display the directory where the  OMPI/MPI  executables  were
 * installed
 * libdir      Display  the  directory  where  the  OMPI/MPI libraries were
 * installed
 * incdir      Display the directory where the OMPI/MPI include files  were
 * installed
 * pkglibdir   Display  the  directory where the OMPI/MPI dynamic libraries
 * were installed
 * sysconfdir  Display the directory where the OMPI/MPI help and configura-
 * tion files were installed
 * Note  that although OMPI's GNU configure script defaults to certain val-
 * ues for all of these directories based on the prefix (e.g.,  bindir  is
 * typically  $prefix/bin),  they  can  all  be  overriden by command line
 * switches to configure, and should therefore never be assumed.  Use ompi-
 * info to determine what values were selected at configure time.
 * VERSION PARAMETERS
 * Since each MCA module in OMPI/MPI is an independant entity in itself, it
 * may have an entirely different  version  number  than  OMPI/MPI  itself.
 * Indeed,  each  MCA module has three version numbers: the version of the
 * base MCA API that it supports, the version of the  component  type  API
 * it  it  supports,  and  its own version number.  Most users will only
 * care about the last one (the module's own version number).
 * The -path switch takes two parameters: item and scope.
 * The item can be the main OMPI version itself, any of the MCA types, or a
 * specific  MCA  module.   There  are currently four kinds of MCA modules
 * that can be queried: boot, coll, pml, and cr.  Hence, the version  num-
 * bers that can be obtained from the -version switch are:
 * ompi         The version of OMPI/MPI
 * boot        The three versions of each boot MCA module
 * boot:name   The three versions of a specific boot MCA module
 * coll        The three versions of each coll MCA module
 * coll:name   The three versions of a specific coll MCA module
 * pml         The three versions of each pml MCA module
 * pml:name    The three versions of a specific pml MCA module
 * cr          The three versions of each cr MCA module
 * cr:name     The three versions of a specific cr MCA module
 * The  scope  argument  describes what part of the version number to dis-
 * play.  This allows either the full version number to be  displayed,  or
 * any  specific individual component of the version number.  Valid values
 * for scope are:
 * full        Display the full version number (i.e., all components).   A
 * sequence  of  rules  are  used  to  run  all the components
 * together into a single string.  Generally: major and  minor
 * are  always  displayed,  but other components are only dis-
 * played if they are not zero.
 * major       Display the major version number
 * minor       Display the minor version number
 * release     Display the release version number
 * alpha       Display the alpha version number.  In the  full  scope,  if
 * nonzero, this number will be preceeded by "a".
 * beta        Display  the  beta  version  number.  In the full scope, if
 * nonzero, this number will be preceeded by "b".
 * cvs         Display whether OMPI was installed from a CVS checkout.   In
 * pretty  mode,  this  will  be  the string "cvs" if true, or
 * blank if false.  In parsable mode, this will be 1 if  true,
 * 0 if false.
 * EXAMPLES
 * ompi_info
 * With  no  parameters, ompi_info displays a default set of information
 * about the OMPI/MPI installation.  This information includes:
 * -   Version of OMPI/MPI
 * -   Installation prefix directory
 * -   Architecture that OMPI/MPI is installed for
 * -   User who configured OMPI/MPI
 * -   Time/datestamp when OMPI/MPI was configured
 * -   Host that OMPI/MPI was configured on
 * -   Whether MPI bindings are provided for C, C++, Fortran
 * -   Whether MPI profiling is available for C, C++, Fortran
 * -   Whether ROMIO support is included
 * -   Whether IMPI support is included
 * -   Whether debug support is included (mainly for OMPI/MPI maintainers)
 * -   Whether OMPI/MPI is "purify  clean"  (suitable  for  memory-checking
 * debuggers)
 * -   List  all  boot,  coll, and pml MCA modules that are available, and
 * their corresponding versions
 * ompi_info -parsable
 * Display the same default set of information but in a  machine-read-
 * able format.
 * ompi_info -all
 * Display all information that is available to ompi_info.
 * ompi_info -param all all
 * Show  all  MCA  parameters (and their corresponding default values)
 * for all available MCA types and modules.
 * ompi_info -param pml all
 * Show all MCA parameters (and their  corresponding  default  values)
 * for all pml MCA modules.
 * ompi_info -param pml tcp
 * Show  all  MCA  parameters (and their corresponding default values)
 * for the tcp pml MCA module.
 * ompi_info -param pml tcp -parsable
 * Show all MCA parameters (and their  corresponding  default  values)
 * for the tcp pml MCA module in a machine-readable format.
 * ompi_info -path bindir -path sysconfdir -parsable
 * Display the directories where the OMPI/MPI executables and help/con-
 * figuration files were installed in a machine-readable format.
 * ompi_info -version ompi full -parsable
 * Display the full version of OMPI/MPI in a machine-readable format.
 * ompi_info -version pml:tcp full
 * Show the full version of the TCP RPI MCA module.
 * SEE ALSO
 * mpirun(1), ompi_boot(1), ompi_wipe(1)
 */

using namespace std;
using namespace ompi_info;


//
// Public variables
//

bool ompi_info::pretty = true;
ompi_cmd_line_t *ompi_info::cmd_line = NULL;

const string ompi_info::type_all = "all";
const string ompi_info::type_ompi = "ompi";
const string ompi_info::type_base = "base";
ompi_info::type_vector_t ompi_info::mca_types;

int main(int argc, char *argv[])
{
  int ret = 0;
  bool acted = false;
  bool want_all = false;

  // Start OMPI process

  if (OMPI_SUCCESS != ompi_init(argc, argv)) {
    return -1;
  }

  // Initialize the argv parsing handle

  cmd_line = ompi_cmd_line_create();
  if (NULL == cmd_line) {
    ret = errno;
#if 0
    show_help(NULL, "lib-call-fail", "ompi_cmd_line_create", NULL);
#endif
    exit(ret);
  }
  ompi_cmd_line_make_opt(cmd_line, 'v', "version", 2, 
                        "Show version of OMPI/MPI or a module");
  ompi_cmd_line_make_opt(cmd_line, '\0', "param", 2, 
                        "Show MCA parameters");
  ompi_cmd_line_make_opt(cmd_line, '\0', "path", 1, 
                        "Show paths that OMPI/MPI was configured with");
  ompi_cmd_line_make_opt(cmd_line, '\0', "arch", 0, 
                        "Show architecture OMPI/MPI was compiled on");
  ompi_cmd_line_make_opt(cmd_line, 'c', "config", 0, 
                        "Show configuration options");
  ompi_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                        "Show this help message");
  ompi_cmd_line_make_opt(cmd_line, '\0', "pretty", 0, 
                        "Display output in 'prettyprint' format (default)");
  ompi_cmd_line_make_opt(cmd_line, '\0', "parsable", 0, 
                        "Display output in parsable format");
  ompi_cmd_line_make_opt(cmd_line, '\0', "hostname", 0, 
                        "Show the hostname that OMPI/MPI was configured "
                        "and built on");
  ompi_cmd_line_make_opt(cmd_line, 'a', "all", 0, 
                        "Show all configuration options and MCA parameters");

  // Call some useless functions in order to guarantee to link in some
  // global variables.  Only check the return value so that the
  // compiler doesn't optimize out the useless function.

  if (OMPI_SUCCESS != ompi_comm_link_function()) {
    // Stop .. or I'll say stop again!
    ++ret;
  } else {
    --ret;
  }

  // Get MCA parameters, if any */
  
  mca_base_open();
  mca_base_cmd_line_setup(cmd_line);

  // Do the parsing

  if (OMPI_SUCCESS != ompi_cmd_line_parse(cmd_line, false, argc, argv) ||
      ompi_cmd_line_is_taken(cmd_line, "help") || 
      ompi_cmd_line_is_taken(cmd_line, "h")) {
#if 1
    printf("...showing ompi_info help message...\n");
#else
    show_help("ompi_info", "usage", NULL);
#endif
    exit(1);
  }

  mca_base_cmd_line_process_args(cmd_line);

  ompi_info::mca_types.push_back("base");

  ompi_info::mca_types.push_back("pcm");
  ompi_info::mca_types.push_back("oob");
  ompi_info::mca_types.push_back("registry");

  ompi_info::mca_types.push_back("coll");
  ompi_info::mca_types.push_back("io");
  ompi_info::mca_types.push_back("one");
  ompi_info::mca_types.push_back("pml");
  ompi_info::mca_types.push_back("ptl");
  ompi_info::mca_types.push_back("topo");

  // Execute the desired action(s)

  if (ompi_cmd_line_is_taken(cmd_line, "pretty")) {
    ompi_info::pretty = true;
  } else if (ompi_cmd_line_is_taken(cmd_line, "parsable")) {
    ompi_info::pretty = false;
  }

  want_all = ompi_cmd_line_is_taken(cmd_line, "all");
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "version")) {
    do_version(want_all, cmd_line);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "path")) {
    do_path(want_all, cmd_line);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "arch")) {
    do_arch(cmd_line);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "config")) {
    do_config(true);
    acted = true;
  }
  if (want_all || ompi_cmd_line_is_taken(cmd_line, "param")) {
    do_params();
    acted = true;
  }

  // If no command line args are specified, show default set

  if (!acted) {
    ompi_info::show_ompi_version(ver_full);
    ompi_info::show_path(path_prefix, OMPI_PREFIX);
    ompi_info::do_arch(cmd_line);
    ompi_info::do_config(false);
    ompi_info::open_modules();
    ompi_info::show_module_version("oob", module_all, ver_full, type_all);
    ompi_info::show_module_version("pcm", module_all, ver_full, type_all);
    ompi_info::show_module_version("registry", module_all, ver_full, type_all);
    ompi_info::show_module_version("coll", module_all, ver_full, type_all);
    ompi_info::show_module_version("pml", module_all, ver_full, type_all);
    ompi_info::show_module_version("ptl", module_all, ver_full, type_all);
  }

  // All done

  ompi_info::close_modules();
  ompi_cmd_line_free(cmd_line);
  mca_base_close();
  ompi_finalize();
  return 0;
}
