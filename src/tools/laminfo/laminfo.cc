//
// $HEADER$
//
/** @file **/

#include "lam_config.h"

#include <iostream>
#include <string>
#include <utility>
#include <list>

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/param.h>

#include "lam/runtime/runtime.h"
#include "lam/util/output.h"
#include "lam/util/cmd_line.h"
#include "mca/lam/base/base.h"
#include "tools/laminfo/laminfo.h"


/*
 * This will ultimately end up being the manual for laminfo
 *
 * laminfo - Display configuration information about LAM/MPI
 * SYNTAX
 * laminfo [-arch] [-config] [-help|-h] [-param <type> <module>]
 * [-parsable|-pretty] [-path <item>] [-version <item> <scope>]
 * OPTIONS
 * -all          Show all configuration information
 * -arch         Show architecture that LAM was configured for
 * -config       Show some information about LAM configuration
 * -help         Show help message
 * -param        Show MCA parameter(s) for a given MCA type and module
 * -parsable     Show output in an easily parsable format
 * -path <item>  Print a given configuration path
 * -pretty       Show output in a prettyprint format (default)
 * -version      Print some or part of a version number of a given item
 * DESCRIPTION
 * The  laminfo  command  is  used  to display information about a LAM/MPI
 * installation.  Particularly with the MCA run-time module selection sys-
 * tem, the laminfo command can be useful to scripts and resource managers
 * to determine the capabilities of the installed LAM/MPI in order to pass
 * run-time parameters to MPI programs.
 * Output  can be displayed in a "pretty" format (i.e., suitable for human
 * reading) and also in a parsable format (i.e., suitable for easy parsing
 * by  scripts or other automated mechanisms).  There are no other LAM API
 * functions to retrieve this data (in any language); the laminfo  command
 * is  the  best  mechanism  to obtain any configuration information about
 * LAM/MPI.  The parsable output was designed such that  common  utilities
 * such  as grep, awk, cut, and sed can easily be used to extract relevant
 * information.
 * Running laminfo with no arguments will display a subset  of  configura-
 * tion  parameters  in  the  "pretty"  format  (see the EXAMPLES section,
 * below).  Several command line options are available  to  limit  exactly
 * which  information  is displayed.  These options, when used in conjunc-
 * tion with the parsable output, can provide  automated  mechanisms  spe-
 * cific information about the capabilities of LAM/MPI.
 * GENERAL PARAMETERS
 * The  -pretty  and -parsable switches are used to select whether to dis-
 * play the output in "pretty" or machine-parsable  format,  respectively.
 * If neither is specified, -pretty is the default.
 * The -arch switch will display the architecture that LAM/MPI was config-
 * ured and compiled on.
 * The -config switch will display  a  set  of  configuration  information
 * about  the  MPI  capabilities  of LAM/MPI, such as whether there are C,
 * C++, and Fortran MPI bindings available, whether there is MPI profiling
 * support  for  C,  C++, and Fortran, whether ROMIO support is available,
 * whether IMPI support is available, whether debugging support is  avail-
 * able  (mostly  for LAM/MPI maintainers), and whether LAM/MPI is "purify
 * clean" (meaning that it is suitable for use with memory checking debug-
 * gers).   Most of these are options to the LAM/MPI configure script, and
 * are configure/compile-time selections that cannot be changed  once  LAM
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
 * base        Intrinsic LAM/MPI parameters
 * boot        Boot modules (e.g., lamboot)
 * coll        MPI collectives
 * cr          Checkpoint / restart
 * RPI         MPI point-to-point.
 * The  names  of  the modules that are available are dependant upon which
 * modules are available for any given type.   See  EXAMPLES,  below,  for
 * example usage.
 * PATH PARAMETERS
 * The -path switch returns various paths that were compiled into LAM/MPI.
 * These were all decided when LAM was configured, and cannot  be  changed
 * at run-time.  However, knowing the location of these directories can be
 * useful in order to find LAM data files, binaries, include  files,  etc.
 * The  -path  switch  takes  a parameter: item.  Pomcable values for item
 * are:
 * prefix      Display the prefix directory for LAM/MPI
 * bindir      Display the directory where the  LAM/MPI  executables  were
 * installed
 * libdir      Display  the  directory  where  the  LAM/MPI libraries were
 * installed
 * incdir      Display the directory where the LAM/MPI include files  were
 * installed
 * pkglibdir   Display  the  directory where the LAM/MPI dynamic libraries
 * were installed
 * sysconfdir  Display the directory where the LAM/MPI help and configura-
 * tion files were installed
 * Note  that although LAM's GNU configure script defaults to certain val-
 * ues for all of these directories based on the prefix (e.g.,  bindir  is
 * typically  $prefix/bin),  they  can  all  be  overriden by command line
 * switches to configure, and should therefore never be assumed.  Use lam-
 * info to determine what values were selected at configure time.
 * VERSION PARAMETERS
 * Since each MCA module in LAM/MPI is an independant entity in itself, it
 * may have an entirely different  version  number  than  LAM/MPI  itself.
 * Indeed,  each  MCA module has three version numbers: the version of the
 * base MCA API that it supports, the version of the  component  type  API
 * it  it  supports,  and  its own version number.  Most users will only
 * care about the last one (the module's own version number).
 * The -path switch takes two parameters: item and scope.
 * The item can be the main LAM version itself, any of the MCA types, or a
 * specific  MCA  module.   There  are currently four kinds of MCA modules
 * that can be queried: boot, coll, pml, and cr.  Hence, the version  num-
 * bers that can be obtained from the -version switch are:
 * lam         The version of LAM/MPI
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
 * cvs         Display whether LAM was installed from a CVS checkout.   In
 * pretty  mode,  this  will  be  the string "cvs" if true, or
 * blank if false.  In parsable mode, this will be 1 if  true,
 * 0 if false.
 * EXAMPLES
 * laminfo
 * With  no  parameters, laminfo displays a default set of information
 * about the LAM/MPI installation.  This information includes:
 * -   Version of LAM/MPI
 * -   Installation prefix directory
 * -   Architecture that LAM/MPI is installed for
 * -   User who configured LAM/MPI
 * -   Time/datestamp when LAM/MPI was configured
 * -   Host that LAM/MPI was configured on
 * -   Whether MPI bindings are provided for C, C++, Fortran
 * -   Whether MPI profiling is available for C, C++, Fortran
 * -   Whether ROMIO support is included
 * -   Whether IMPI support is included
 * -   Whether debug support is included (mainly for LAM/MPI maintainers)
 * -   Whether LAM/MPI is "purify  clean"  (suitable  for  memory-checking
 * debuggers)
 * -   List  all  boot,  coll, and pml MCA modules that are available, and
 * their corresponding versions
 * laminfo -parsable
 * Display the same default set of information but in a  machine-read-
 * able format.
 * laminfo -all
 * Display all information that is available to laminfo.
 * laminfo -param all all
 * Show  all  MCA  parameters (and their corresponding default values)
 * for all available MCA types and modules.
 * laminfo -param pml all
 * Show all MCA parameters (and their  corresponding  default  values)
 * for all pml MCA modules.
 * laminfo -param pml tcp
 * Show  all  MCA  parameters (and their corresponding default values)
 * for the tcp pml MCA module.
 * laminfo -param pml tcp -parsable
 * Show all MCA parameters (and their  corresponding  default  values)
 * for the tcp pml MCA module in a machine-readable format.
 * laminfo -path bindir -path sysconfdir -parsable
 * Display the directories where the LAM/MPI executables and help/con-
 * figuration files were installed in a machine-readable format.
 * laminfo -version lam full -parsable
 * Display the full version of LAM/MPI in a machine-readable format.
 * laminfo -version pml:tcp full
 * Show the full version of the TCP RPI MCA module.
 * SEE ALSO
 * mpirun(1), lamboot(1), lamwipe(1)
 */

using namespace std;
using namespace laminfo;


//
// Public variables
//

bool laminfo::pretty = true;
lam_cmd_line_t *laminfo::cmd_line = NULL;

const string laminfo::type_all = "all";
const string laminfo::type_lam = "lam";
const string laminfo::type_base = "base";
laminfo::type_vector_t laminfo::mca_types;


int main(int argc, char *argv[])
{
  int ret;
  bool acted = false;
  bool want_all = false;

  // Start LAM process

  if (LAM_SUCCESS != lam_init(argc, argv))
    return -1;

  // Initialize the argv parsing handle

  cmd_line = lam_cmd_line_create();
  if (NULL == cmd_line) {
    ret = errno;
#if 0
    show_help(NULL, "lib-call-fail", "lam_cmd_line_create", NULL);
#endif
    exit(ret);
  }
  lam_cmd_line_make_opt(cmd_line, 'v', "version", 2, 
                        "Show version of LAM/MPI or a module");
  lam_cmd_line_make_opt(cmd_line, '\0', "param", 2, 
                        "Show MCA parameters");
  lam_cmd_line_make_opt(cmd_line, '\0', "path", 1, 
                        "Show paths that LAM/MPI was configured with");
  lam_cmd_line_make_opt(cmd_line, '\0', "arch", 0, 
                        "Show architecture LAM/MPI was compiled on");
  lam_cmd_line_make_opt(cmd_line, 'c', "config", 0, 
                        "Show configuration options");
  lam_cmd_line_make_opt(cmd_line, 'h', "help", 0, 
                        "Show this help message");
  lam_cmd_line_make_opt(cmd_line, '\0', "pretty", 0, 
                        "Display output in 'prettyprint' format (default)");
  lam_cmd_line_make_opt(cmd_line, '\0', "parsable", 0, 
                        "Display output in parsable format");
  lam_cmd_line_make_opt(cmd_line, '\0', "hostname", 0, 
                        "Show the hostname that LAM/MPI was configured "
                        "and built on");
  lam_cmd_line_make_opt(cmd_line, 'a', "all", 0, 
                        "Show all configuration options and MCA parameters");

  // Get MCA parameters, if any */
  
  mca_base_open();
  mca_base_cmd_line_setup(cmd_line);

  // Do the parsing

  if (LAM_SUCCESS != lam_cmd_line_parse(cmd_line, false, argc, argv) ||
      lam_cmd_line_is_taken(cmd_line, "help") || 
      lam_cmd_line_is_taken(cmd_line, "h")) {
#if 1
    printf("...showing laminfo help message...\n");
#else
    show_help("laminfo", "usage", NULL);
#endif
    exit(1);
  }

  mca_base_cmd_line_process_args(cmd_line);

  laminfo::mca_types.push_back("base");

  laminfo::mca_types.push_back("pcm");
  laminfo::mca_types.push_back("oob");
  laminfo::mca_types.push_back("registry");

  laminfo::mca_types.push_back("coll");
  laminfo::mca_types.push_back("io");
  laminfo::mca_types.push_back("one");
  laminfo::mca_types.push_back("pml");
  laminfo::mca_types.push_back("ptl");
  laminfo::mca_types.push_back("topo");

  // Execute the desired action(s)

  if (lam_cmd_line_is_taken(cmd_line, "pretty")) {
    laminfo::pretty = true;
  } else if (lam_cmd_line_is_taken(cmd_line, "parsable")) {
    laminfo::pretty = false;
  }

  want_all = lam_cmd_line_is_taken(cmd_line, "all");
  if (want_all || lam_cmd_line_is_taken(cmd_line, "version")) {
    do_version(want_all, cmd_line);
    acted = true;
  }
  if (want_all || lam_cmd_line_is_taken(cmd_line, "path")) {
    do_path(want_all, cmd_line);
    acted = true;
  }
  if (want_all || lam_cmd_line_is_taken(cmd_line, "arch")) {
    do_arch(cmd_line);
    acted = true;
  }
  if (want_all || lam_cmd_line_is_taken(cmd_line, "config")) {
    do_config(true);
    acted = true;
  }
  if (want_all || lam_cmd_line_is_taken(cmd_line, "param")) {
    do_params();
    acted = true;
  }

  // If no command line args are specified, show default set

  if (!acted) {
    laminfo::show_lam_version(ver_full);
    laminfo::show_path(path_prefix, LAM_PREFIX);
    laminfo::do_arch(cmd_line);
    laminfo::do_config(false);
    laminfo::open_modules();
    laminfo::show_module_version("oob", module_all, ver_full, type_all);
    laminfo::show_module_version("pcm", module_all, ver_full, type_all);
    laminfo::show_module_version("registry", module_all, ver_full, type_all);
    laminfo::show_module_version("coll", module_all, ver_full, type_all);
    laminfo::show_module_version("pml", module_all, ver_full, type_all);
    laminfo::show_module_version("ptl", module_all, ver_full, type_all);
  }

  // All done

  laminfo::close_modules();
  lam_cmd_line_free(cmd_line);
  mca_base_close();
  lam_finalize();
  return 0;
}
