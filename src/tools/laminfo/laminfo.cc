//
// $HEADER$
//
/** @file **/
#include "lam_config.h"

#include <iostream>
#include <string>
#include <utility>

#include <stdio.h>
#include <ctype.h>
#include <unistd.h>
#include <netdb.h>
#include <sys/param.h>

#include "lam/util/cmd_line.h"
#include "lam/util/few.h"
#include "lam/util/argv.h"
#include "mca/mca.h"
#include "mca/lam/base/base.h"
#include "mca/mpi/mpi.h"
#include "mca/mpi/coll/base/base.h"
#include "mca/mpi/pml/pml.h"

#define LAM_max(a,b) (((a) > (b)) ? (a) : (b)) 
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
 * -param        Show SSI parameter(s) for a given SSI type and module
 * -parsable     Show output in an easily parsable format
 * -path <item>  Print a given configuration path
 * -pretty       Show output in a prettyprint format (default)
 * -version      Print some or part of a version number of a given item
 * DESCRIPTION
 * The  laminfo  command  is  used  to display information about a LAM/MPI
 * installation.  Particularly with the SSI run-time module selection sys-
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
 * The -param switch can be used to  show  available  SSI  parameters  and
 * their  default  values.   The  type and module arguments can be used to
 * specify a particular SSI type and/or module, or use the special keyword
 * "all" to indicate all available SSI types/modules (respectively).
 * Available SSI types are:
 * all         Show all SSI types
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
 * The  -path  switch  takes  a parameter: item.  Possible values for item
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
 * Since each SSI module in LAM/MPI is an independant entity in itself, it
 * may have an entirely different  version  number  than  LAM/MPI  itself.
 * Indeed,  each  SSI module has three version numbers: the version of the
 * base SSI API that it supports, the version of the  component  type  API
 * it  it  supports,  and  its own version number.  Most users will only
 * care about the last one (the module's own version number).
 * The -path switch takes two parameters: item and scope.
 * The item can be the main LAM version itself, any of the SSI types, or a
 * specific  SSI  module.   There  are currently four kinds of SSI modules
 * that can be queried: boot, coll, pml, and cr.  Hence, the version  num-
 * bers that can be obtained from the -version switch are:
 * lam         The version of LAM/MPI
 * boot        The three versions of each boot SSI module
 * boot:name   The three versions of a specific boot SSI module
 * coll        The three versions of each coll SSI module
 * coll:name   The three versions of a specific coll SSI module
 * pml         The three versions of each pml SSI module
 * pml:name    The three versions of a specific pml SSI module
 * cr          The three versions of each cr SSI module
 * cr:name     The three versions of a specific cr SSI module
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
 * -   List  all  boot,  coll, and pml SSI modules that are available, and
 * their corresponding versions
 * laminfo -parsable
 * Display the same default set of information but in a  machine-read-
 * able format.
 * laminfo -all
 * Display all information that is available to laminfo.
 * laminfo -param all all
 * Show  all  SSI  parameters (and their corresponding default values)
 * for all available SSI types and modules.
 * laminfo -param pml all
 * Show all SSI parameters (and their  corresponding  default  values)
 * for all pml SSI modules.
 * laminfo -param pml tcp
 * Show  all  SSI  parameters (and their corresponding default values)
 * for the tcp pml SSI module.
 * laminfo -param pml tcp -parsable
 * Show all SSI parameters (and their  corresponding  default  values)
 * for the tcp pml SSI module in a machine-readable format.
 * laminfo -path bindir -path sysconfdir -parsable
 * Display the directories where the LAM/MPI executables and help/con-
 * figuration files were installed in a machine-readable format.
 * laminfo -version lam full -parsable
 * Display the full version of LAM/MPI in a machine-readable format.
 * laminfo -version pml:tcp full
 * Show the full version of the TCP RPI SSI module.
 * SEE ALSO
 * mpirun(1), lamboot(1), lamwipe(1)
 */

using namespace std;

/*
 * Local functions. All local functions in LAM are made static
 * so that there is no namespace clashes. However, all public 
 * functions have to start with lam_<component>_<module>_<name>
 * or mca_<component>_<module>)<name> so that the namespace 
 * clashes are prevented
 */
static void do_version(bool want_all, lam_cmd_line_t *cmd_line);
static void show_lam_version(const string& scope);
static void show_boot_version(const mca_base_module_t **boot_modules, 
				  const string& which, const string& scope,
				  const string& ver_type);
static void show_coll_version(const mca_base_module_t **coll_modules, 
				  const string& which, const string& scope,
				  const string& ver_type);
static void show_pml_version(const mca_base_module_t **pml_modules, 
				 const string& which, const string& scope,
				 const string& ver_type);
static void show_cr_version(const mca_base_module_t **cr_modules, 
                             const string& which, const string& scope,
                             const string& ver_type);
static void show_mca_version(const mca_base_module_t *ls, const string& type,
				 const string& scope, const string& ver_type);
static string make_version_str(const string& scope,
				   int major, int minor, int release, 
				   int alpha, int beta, int cvs);

static void do_params(bool want_all);
static void show_mca_params(const string& type, const string& module, 
                            const string& param);

static void do_path(bool want_all, lam_cmd_line_t *cmd_line);
static void show_path(const string& type, const string& value);

static void do_arch(lam_cmd_line_t *cmd_line);
static void do_config(bool want_all);

static void out(const string& pretty_message, const string& plain_message, 
                const string &value);
static void out(const string& pretty_message, const string &plain_message, 
                int value);

static void open_modules();
static void close_modules();

/*
 * Local variables. Same policy which applies to local functions applies 
 * to local variables
 */

static bool opened_modules = false;
static bool pretty = true;
static int centerpoint = 20;
static int screen_width = 78;

static string ver_full = "full";
static string ver_major = "major";
static string ver_minor = "minor";
static string ver_release = "release";
static string ver_alpha = "alpha";
static string ver_beta = "beta";
static string ver_cvs = "cvs";

static string type_lam = "lam";
static string type_all = "all";
static string type_base = "base";
static string type_boot = "boot";
static string type_coll = "coll";
static string type_pml = "pml";
static string type_cr = "cr";

static string module_all = "all";
static string param_all = "all";

static string ver_mca = "mca";
static string ver_type = "type";
static string ver_module = "module";

static string path_prefix = "prefix";
static string path_bindir = "bindir";
static string path_libdir = "libdir";
static string path_incdir = "incdir";
static string path_pkglibdir = "pkglibdir";
static string path_sysconfdir = "sysconfdir";

static lam_cmd_line_t *cmd_line = NULL;


int
main(int argc, char *argv[])
{
  int ret;
  bool acted = false;
  bool want_all = false;

  // Initialize the argv parsing handle

  cmd_line = lam_cmd_line_create();
  if (NULL == cmd_line) {
    ret = errno;
#if 0
    show_help(NULL, "lib-call-fail", "lam_cmd_line_create", NULL);
#endif
    exit(ret);
  }
#if 0
  // Anju: 
  // This function has not been ported as yet
  ao_setflags(cmd_line, AOPRESERVE_ARGV);
#endif
  // Anju:
  // The int flags options has been replaced by const char* string. Also, the 
  // mutex which could be optionally passed has been removed as one of the 
  // parameters to this call. Check with Jeff about it.
  //
  lam_cmd_line_set_opt1(cmd_line, "h", 0, NULL);

  // Actions
  // Anju:
  // lam_cmd_line_set_opt and lam_cmd_line_set_opt1  paramters are slightly 
  // different. I have passed NULL values whenever prototypes are different.
  //
  lam_cmd_line_set_opt(cmd_line, "version", NULL, 2, NULL);//params are diffferent
  lam_cmd_line_set_opt(cmd_line, "param", NULL, 2, NULL);
  lam_cmd_line_set_opt(cmd_line, "path", NULL, 1, NULL);
  lam_cmd_line_set_opt(cmd_line, "arch", NULL, 0, NULL);
  lam_cmd_line_set_opt(cmd_line, "config", NULL, 0, NULL);
  lam_cmd_line_set_opt(cmd_line, "help", NULL, 0, NULL);
  lam_cmd_line_set_opt(cmd_line, "pretty", NULL, 0, NULL);
  lam_cmd_line_set_opt(cmd_line, "parsable", NULL, 0, NULL);
  lam_cmd_line_set_opt(cmd_line, "hostname", NULL, 0, NULL);
  lam_cmd_line_set_opt(cmd_line, "all", NULL, 0, NULL);

  // Get SSI parameters, if any */
#if 0
  // Anju: 
  // JMS has too fix this. The semantics of the function are different
  
  mca_base_open(cmd_line);
  mca_base_ao_setup(cmd_line);
#endif 

  // Do the parsing

  if (lam_cmd_line_parse(cmd_line, &argc, argv) ||
      lam_cmd_line_is_taken(cmd_line, "help") || lam_cmd_line_is_taken(cmd_line, "h")) {
#if 0	  
    show_help("laminfo", "usage", NULL);
#endif
	// Anju:
	// EUSAGE is defined in share/terror.h in trillium tree
	// exit (EUSAGE);
    exit(3);
  }
#if 0
  mca_base_ao_process_args(cmd_line);
#endif

  // Execute the desired action(s)

  if (lam_cmd_line_is_taken(cmd_line, "pretty"))
    pretty = true;
  else if (lam_cmd_line_is_taken(cmd_line, "parsable"))
    pretty = false;

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
    do_params(want_all);
    acted = true;
  }

  // If no command line args are specified, show [a concise] everything

  if (!acted) {
    // Anju:
    // For now, I am blocking this entire chunk out as none of them actually
    // work.
#if 0
    show_lam_version(ver_full);
    show_path(path_prefix, LAM_PREFIX);
#endif
    do_arch(cmd_line);
    do_config(false);
#if 0
    open_modules();
    show_boot_version(mca_boot_modules, type_all, ver_full, ver_module);
    show_coll_version(mca_coll_modules, type_all, ver_full, ver_module);
    show_pml_version(mca_pml_modules, type_all, ver_full, ver_module);
    show_cr_version(mca_crmpi_modules, type_all, ver_full, ver_module);
#endif
  }

  // All done
#if 0
  close_modules();
#endif
  lam_cmd_line_free(cmd_line);
  return 0;
}

//
// do_version
//
// Determines the version information related to the lam modules
// being used.
// Accepts: 
//	- want_all: True if all modules' info is required.
//	- cmd_line: The constructed command line argument
//

static void
do_version(bool want_all, lam_cmd_line_t *cmd_line)
{
  int i, count;
  string arg1, scope, module;

  open_modules();

  if (want_all) {
#if 0
    show_lam_version(ver_full);
    show_boot_version(mca_boot_modules, type_all, ver_full, type_all);
    show_coll_version(mca_coll_modules, type_all, ver_full, type_all);
    show_pml_version(mca_pml_modules, type_all, ver_full, type_all);
    show_cr_version(mca_crmpi_modules, type_all, ver_full, type_all);
#endif
  } else {
    count = lam_cmd_line_get_ninsts(cmd_line, "version");
    for (i = 0; i < count; ++i) {
      arg1 = lam_cmd_line_get_param(cmd_line, "version", i, 0);
      scope = lam_cmd_line_get_param(cmd_line, "version", i, 1);

      // Version of LAM/MPI
    
      if (type_lam == arg1)
        show_lam_version(scope);

      // Version of all boot modules or a specific boot module

      else if (type_boot == arg1) {
#if 0
        show_boot_version(mca_boot_modules, type_all, scope, type_all);
#endif
      } else if (arg1.substr(0, type_boot.length() + 1) == (type_boot + ":")) {
#if 0
        show_boot_version(mca_boot_modules, 
                          arg1.substr(type_boot.length() + 1), scope, 
                          type_all);
#endif
      } 

      // Version of all coll modules or a specific coll module

      else if (type_coll == arg1) {
#if 0
        show_coll_version(mca_coll_modules, type_all, scope, type_all);
#endif
      } else if (arg1.substr(0, type_coll.length() + 1) == (type_coll + ":")) {
#if 0
        show_coll_version(mca_coll_modules, 
                          arg1.substr(type_coll.length() + 1), scope, 
                          type_all);
#endif
      } 

      // Version of all pml modules or a specific pml module

      else if (type_pml == arg1) {
#if 0
        show_pml_version(mca_pml_modules, type_all, scope, type_all);
#endif
      } else if (arg1.substr(0, type_pml.length() + 1) == (type_pml + ":")) {
#if 0
        show_pml_version(mca_pml_modules, 
                         arg1.substr(type_pml.length() + 1), scope, type_all);
#endif
      } 

      // Version of all cr modules or a specific cr module
      // Note: we only list the compiled crmpi modules. But the list of crmpi 
      // modules available should be the same as this list.
      
      // commented out because there is not cr module as of now.
      else if (type_cr == arg1) {
#if 0
        show_cr_version(mca_crmpi_modules, type_all, scope, type_all);
#endif
      } else if (arg1.substr(0, type_cr.length() + 1) == (type_cr + ":")) {
#if 0
        show_cr_version(mca_crmpi_modules,
                        arg1.substr(type_cr.length() + 1), scope, type_all);
#endif
      }
      // Fall through
      
      else {
#if 0
        show_help("laminfo", "usage");
#endif
        exit(1);
      }
    }
  }
}

static void
show_lam_version(const string& scope)
{
  out("LAM/MPI", "version:" + type_lam, 
      make_version_str(scope, 
                       LAM_MAJOR_VERSION, LAM_MINOR_VERSION, 
                       LAM_RELEASE_VERSION, 
                       LAM_ALPHA_VERSION, LAM_BETA_VERSION, LAM_CVS_VERSION));
}


static void 
show_boot_version(const mca_base_module_t **boot_modules, 
		  const string& which, const string& scope, 
		  const string& ver_type)
{
  int i;
  const mca_base_module_t *ls;
  bool want_all_modules = (type_all == which);

  for (i = 0; NULL != boot_modules[i]; ++i) {
    ls = boot_modules[i];
    if (!want_all_modules && which != ls->mca_module_name)
      continue;

    show_mca_version(ls, type_boot, scope, ver_type);
  }
}


static void 
show_coll_version(const mca_base_module_t **coll_modules, 
		  const string& which, const string& scope, 
		  const string& ver_type)
{
  int i;
  const mca_base_module_t *ls;
  bool want_all_modules = (type_all == which);

  for (i = 0; NULL != coll_modules[i]; ++i) {
    ls = coll_modules[i];
    if (!want_all_modules && which != ls->mca_module_name)
      continue;

    show_mca_version(ls, type_coll, scope, ver_type);
  }
}


static void 
show_pml_version(const mca_base_module_t **pml_modules, 
		  const string& which, const string& scope, 
		  const string& ver_type)
{
  int i;
  const mca_base_module_t *ls;
  bool want_all_modules = (type_all == which);

  for (i = 0; NULL != pml_modules[i]; ++i) {
    ls = pml_modules[i];
    if (!want_all_modules && which != ls->mca_module_name)
      continue;

    show_mca_version(ls, type_pml, scope, ver_type);
  }
}


static void 
show_cr_version(const mca_base_module_t **cr_modules, 
                  const string& which, const string& scope, 
                  const string& ver_type)
{
  int i;
  const mca_base_module_t *ls;
  bool want_all_modules = (which == type_all);

  for (i = 0; cr_modules[i] != NULL; ++i) {
    ls = cr_modules[i];
    if (!want_all_modules && which != ls->mca_module_name)
      continue;

    show_mca_version(ls, type_cr, scope, ver_type);
  }
}


static void 
show_mca_version(const mca_base_module_t *ls, const string &type,
		  const string& scope, 
		  const string& ver_type)
{
  bool printed;
  bool want_mca = (type_all == ver_type || ver_type == ver_mca);
  bool want_type = (type_all == ver_type || ver_type == ver_type);
  bool want_module = (type_all == ver_type || ver_type == ver_module);
  string message, content;
  string mca_version;
  string api_version;
  string module_version;
  string empty;

  mca_version = make_version_str(scope, ls->mca_major_version,
                                 ls->mca_minor_version,
                                 ls->mca_release_version, 0, 0, 0);
  api_version = make_version_str(scope, ls->mca_type_major_version,
                                 ls->mca_type_minor_version,
                                 ls->mca_type_release_version, 0, 0, 0);
  module_version = make_version_str(scope, ls->mca_module_major_version,
                                    ls->mca_module_minor_version,
                                    ls->mca_module_release_version, 0, 0, 0);

  if (pretty) {
    message = "SSI " + type;
    printed = false;

    content = ls->mca_module_name + string(" (");
    if (want_mca) {
      content += "SSI v" + mca_version;
      printed = true;
    }
    if (want_type) {
      if (printed)
        content += ", ";
      content += "API v" + api_version;
      printed = true;
    }
    if (want_module) {
      if (printed)
        content += ", ";
      content += "Module v" + module_version;
      printed = true;
    }
    out(message, empty, content + ")");
  } else {
    message = "mca:" + type + ":" + ls->mca_module_name + ":version";
    if (want_mca)
      out(empty, message, "mca:" + mca_version);
    if (want_type)
      out(empty, message, "api:" + api_version);
    if (want_module)
      out(empty, message, "module:" + module_version);
  }
}


static string 
make_version_str(const string& scope,
		 int major, int minor, int release, int alpha, int beta, 
                 int cvs)
{
  string str;
  char temp[BUFSIZ];

  temp[BUFSIZ - 1] = '\0';
  if (scope == ver_full) {
    snprintf(temp, BUFSIZ - 1, "%d.%d", major, minor);
    str = temp;
    if (release > 0) {
      snprintf(temp, BUFSIZ - 1, ".%d", release);
      str += temp;
    }
    if (alpha > 0) {
      snprintf(temp, BUFSIZ - 1, "a%d", alpha);
      str += temp;
    }
    else if (beta > 0) {
      snprintf(temp, BUFSIZ - 1, "b%d", beta);
      str += temp;
    }
    if (cvs > 0) {
      str += "cvs";
      if (cvs > 1) {
        snprintf(temp, BUFSIZ - 1, "%d", cvs);
        str += temp;
      }
    }
  } else if (scope == ver_major)
    snprintf(temp, BUFSIZ - 1, "%d", major);
  else if (scope == ver_minor)
    snprintf(temp, BUFSIZ - 1, "%d", minor);
  else if (scope == ver_release)
    snprintf(temp, BUFSIZ - 1, "%d", release);
  else if (scope == ver_alpha)
    snprintf(temp, BUFSIZ - 1, "%d", alpha);
  else if (scope == ver_beta)
    snprintf(temp, BUFSIZ - 1, "%d", beta);
  else if (scope == ver_cvs)
    snprintf(temp, BUFSIZ - 1, "%d", cvs);
  else {
#if 0
    show_help("laminfo", "usage");
#endif
    exit(1);
  }

  if (str.empty())
    str = temp;

  return str;
}


static void
do_params(bool want_all)
{
  int i, count;
  string type, module;

  if (want_all) {
    show_mca_params(type_all, module_all, param_all);
  } else {
    count = lam_cmd_line_get_ninsts(cmd_line, "param");
    for (i = 0; i < count; ++i) {
      type = lam_cmd_line_get_param(cmd_line, "param", i, 0);
      module = lam_cmd_line_get_param(cmd_line, "param", i, 1);

      if (type_all != type &&
          type_base != type &&
          type_boot != type &&
          type_coll != type &&
          type_cr != type &&
          type_pml != type) {
#if 0
        show_help("laminfo", "usage");
#endif
        exit(1);
      }

      show_mca_params(type, module, param_all);
    }
  }
}


static void 
show_mca_params(const string& type, const string& module, const string& param)
{
#if 0
  // Anju:
  // This datatype has not been incorporated as yet. Yet to 
  // decide how this will come into picture.
  lam_mca_base_param_t *array;
  int i, size;
  char *default_value_string, temp[BUFSIZ];
  string message, content;

  // Ensure that we've opened the modules (so that they can register
  // their parameters).

  open_modules();
  // Anju:
  // Since this whole function is built on the premise of 
  // "lam_ssi_base_param_t" and this enum (see lam_ssi.h) 
  // is not supported, it is best to not do anything right 
  // now about this function
  if (mca_base_params == NULL)
    return;

  size = lam_arr_size(mca_base_params);
  array = (lam_ssi_base_param_t*) lam_arr_get(mca_base_params);

  for (i = 0; i < size; ++i) {
    if (type == type_all || type == array[i].lsbp_type_name) {
      if (module == module_all || 
          (array[i].lsbp_module_name != NULL &&
           module == array[i].lsbp_module_name)) {
        if (param == param_all || param == array[i].lsbp_param_name) {

          // Make a string for the default value

          temp[0] = '\0';
          if (array[i].lsbp_type == LAM_SSI_BASE_PARAM_TYPE_STRING) {
            if (array[i].lsbp_default_value.stringval != NULL)
              default_value_string = array[i].lsbp_default_value.stringval;
            else
              default_value_string = temp;
          } else {
            default_value_string = temp;
            snprintf(default_value_string, BUFSIZ, "%d", 
                     array[i].lsbp_default_value.intval);
          }
          content = default_value_string;

          // Build up the strings to output.

          if (pretty) {
            message = "SSI ";
            message += array[i].lsbp_type_name;

            // Put in the real, full name (which may be different than
            // the categorization).

            content = (array[i].lsbp_env_var_name != NULL) ?
              "parameter \"" : "information \"";
            content += array[i].lsbp_full_name;
            content += (array[i].lsbp_env_var_name != NULL) ?
              "\" (default value: " : "\" (value: ";

            if (strlen(default_value_string) == 0)
              content += "<none>)";
            else {
              content += "\"";
              content += default_value_string;
              content += "\")";
            }

            out(message, message, content);
          } else {
            message = "ssi:";
            message += array[i].lsbp_type_name;
            message += ":";

            if (array[i].lsbp_module_name != NULL) {
              message += array[i].lsbp_module_name;
            } else {
              message += "base";
            }
            message += (array[i].lsbp_env_var_name != NULL) ?
              ":param:" : ":info:";

            // Put in the real, full name (which may be different than
            // the categorization).

            message += array[i].lsbp_full_name;

            content = default_value_string;

            out(message, message, content);
          }
        }
      }
    }
  }
#endif // Nullifying the function
}


static void
do_path(bool want_all, lam_cmd_line_t *cmd_line)
{
  int i, count;
  string scope;

  if (want_all) {
    show_path(path_prefix, LAM_PREFIX);
    show_path(path_bindir, LAM_BINDIR);
    show_path(path_libdir, LAM_LIBDIR);
    show_path(path_incdir, LAM_INCDIR);
    show_path(path_pkglibdir, LAM_PKGLIBDIR);
    show_path(path_sysconfdir, LAM_SYSCONFDIR);
  } else {
    count = lam_cmd_line_get_ninsts(cmd_line, "path");
    for (i = 0; i < count; ++i) {
      scope = lam_cmd_line_get_param(cmd_line, "path", i, 0);

      if (path_prefix == scope)
        show_path(path_prefix, LAM_PREFIX);
      else if (path_bindir == scope)
        show_path(path_bindir, LAM_BINDIR);
      else if (path_libdir == scope)
        show_path(path_libdir, LAM_LIBDIR);
      else if (path_incdir == scope)
        show_path(path_incdir, LAM_INCDIR);
      else if (path_pkglibdir == scope)
        show_path(path_pkglibdir, LAM_PKGLIBDIR);
      else if (path_sysconfdir == scope)
        show_path(path_sysconfdir, LAM_SYSCONFDIR);
      else {
#if 0
        show_help("laminfo", "usage");
#endif
        exit(1);
      }
    }
  }
}


static void 
show_path(const string& type, const string& value)
{
  string pretty(type);
  pretty[0] &= toupper(pretty[0]);
  out(pretty, "path:" + type, value);
}


static void
do_arch(lam_cmd_line_t *cmd_line)
{
  string prefix;
  char hostname[MAXHOSTNAMELEN];

  if (lam_cmd_line_is_taken(cmd_line, "hostname")) {
    gethostname(hostname, MAXHOSTNAMELEN);
    prefix = hostname + string(":");
  }
  out("Architecture", prefix + "arch", LAM_ARCH);
}

//
// do_config
// Accepts:
//	- want_all: boolean flag; TRUE -> display all options
//				  FALSE -> display selected options
//
// This function displays all the options with which the current
// installation of lam was configured. There are many options here 
// that are carried forward from LAM-7 and are not mca parameters 
// in LAM-10. I have to dig through the invalid options and replace
// them with LAM-10 options.
//
static void 
do_config(bool want_all)
{
  const string f77(LAM_ENABLE_MPI_F77 ? "yes" : "no");
  const string f90(LAM_ENABLE_MPI_F90 ? "yes" : "no");
  const string sthreads(LAM_HAVE_SOLARIS_THREADS ? "yes" : "no");
  const string pthreads(LAM_HAVE_POSIX_THREADS ? "yes" : "no");
#if 0
  // Anju: 
  // These are the options that will definately be added along the
  // line. So, I am compiling them out instead of deleting them
  const string romio(LAM_WANT_ROMIO ? "yes" : "no");
  const string impi(LAM_WANT_IMPI ? "yes" : "no");
  const string purify(LAM_DISINFECT ? "yes" : "no");
#endif
  const string debug(LAM_ENABLE_DEBUG ? "yes" : "no");
  const string cprofiling(LAM_WANT_MPI_PROFILING ? "yes" : "no");
  const string cxxprofiling(LAM_WANT_MPI_PROFILING ? "yes" : "no");
  const string f77profiling((LAM_WANT_MPI_PROFILING && LAM_ENABLE_MPI_F77) ?
                          "yes" : "no");
  const string f90profiling((LAM_WANT_MPI_PROFILING && LAM_ENABLE_MPI_F90) ?
                          "yes" : "no");
  const string cxxexceptions(LAM_HAVE_CXX_EXCEPTION_SUPPORT ? "yes" : "no");
  cout << "I am here";
  out("Configured by", "config:user", LAM_CONFIGURE_USER);
  out("Configured on", "config:timestamp", LAM_CONFIGURE_DATE);
  out("Configure host", "config:host", LAM_CONFIGURE_HOST);

  out("C bindings", "bindings:c", "yes");
  out("C++ bindings", "bindings:cxx", "yes");
  out("Fortran77 bindings", "bindings:f77", f77);
  out("Fortran90 bindings", "bindings:f90", f90);

  out("C compiler", "compiler:c:command", LAM_CC);

  if (want_all) {
    out("C char size", "compiler:c:sizeof:char", sizeof(char));
    out("C bool size", "compiler:c:sizeof:bool", sizeof(bool));
    out("C short size", "compiler:c:sizeof:short", sizeof(short));
    out("C int size", "compiler:c:sizeof:int", sizeof(int));
    out("C long size", "compiler:c:sizeof:long", sizeof(long));
    out("C float size", "compiler:c:sizeof:float", sizeof(float));
    out("C double size", "compiler:c:sizeof:double", sizeof(double));
    out("C pointer size", "compiler:c:sizeof:pointer", sizeof(void *));
#if 0
    // Anju:
    // Not yet gotten to this part in LAM-10.
    //
    out("C char align", "compiler:c:align:char", LAM_ALIGNMENT_CHAR);
    out("C bool align", "compiler:c:align:bool", LAM_ALIGNMENT_CXX_BOOL);
    out("C int align", "compiler:c:align:int", LAM_ALIGNMENT_INT);
    out("C float align", "compiler:c:align:float", LAM_ALIGNMENT_FLOAT);
    out("C double align", "compiler:c:align:double", 
        LAM_ALIGNMENT_DOUBLE);
#endif
  }

  out("C++ compiler", "compiler:cxx:command", LAM_CXX);

  out("Fortran77 compiler", "compiler:f77:command", LAM_F77);
  out("Fortran90 compiler", "compiler:f90:command", LAM_F90);
  if (want_all) {
#if 0
    // Anju:
    // Not yet gotten to this part in LAM-10.
    //
    out("Fort integer size", "compiler:fortran:sizeof:integer", 
        LAM_SIZEOF_FORTRAN_INT);
    out("Fort real size", "compiler:fortran:sizeof:real", 
        LAM_SIZEOF_FORTRAN_REAL);
    out("Fort dbl prec size", 
        "compiler:fortran:sizeof:double_precision", LAM_SIZEOF_FORTRAN_REAL);
    out("Fort cplx size", "compiler:fortran:sizeof:complex", 
        LAM_SIZEOF_FORTRAN_REAL);
    out("Fort dbl cplx size",
        "compiler:fortran:sizeof:double_complex", 
        LAM_SIZEOF_FORTRAN_REAL);

    out("Fort integer align", "compiler:fortran:align:integer", 
        LAM_ALIGNMENT_FORTRAN_INT);
    out("Fort real align", "compiler:fortran:align:real", 
        LAM_ALIGNMENT_FORTRAN_REAL);
    out("Fort dbl prec align", 
        "compiler:fortran:align:double_precision", LAM_ALIGNMENT_FORTRAN_REAL);
    out("Fort cplx align", "compiler:fortran:align:complex", 
        LAM_ALIGNMENT_FORTRAN_REAL);
    out("Fort dbl cplx align",
        "compiler:fortran:align:double_complex", 
        LAM_ALIGNMENT_FORTRAN_REAL);
#endif
  }

  out("C profiling", "option:profiling:c", cprofiling);
  out("C++ profiling", "option:profiling:cxx", cxxprofiling);
  out("Fortran77 profiling", "option:profiling:f77", f77profiling);
  out("Fortran90 profiling", "option:profiling:f90", f90profiling);

  out("C++ exceptions", "option:cxx_exceptions", cxxexceptions);
  out("Posix thread support", "option:threads", pthreads);
  out("Solaris thread support", "option:threads", sthreads);
#if 0
  // Anju:
  // Someday even this should be presented as information
  out("ROMIO support", "option:romio", romio);
  out("IMPI support", "option:impi", impi);
  out("Purify clean", "option:purify", purify);
#endif
  out("Debug support", "option:debug", debug);
}

// 
// out
// 
// Prints the passed parameters in a pretty or parsable format.
// Does nothing special other than that. There are no changes 
// w.r.t LAM-10 here.
//
static void
out(const string& pretty_message, const string &plain_message,
    const string& value)
{
  if (pretty) {
    string::size_type pos, max_value_width;
    string spaces(LAM_max(centerpoint - pretty_message.length(), 0), ' ');
    string v = value;
    string filler;

    max_value_width = screen_width - spaces.length() -
      pretty_message.length() - 2;
    filler = spaces + pretty_message + ": ";

    while (1) {
      if (v.length() < max_value_width) {
        cout << filler << v << endl;
        break;
      } else {
        string spaces(centerpoint + 2, ' ');

        // Work backwards to find the first space before
        // max_value_width

        pos = v.rfind(' ', max_value_width);
        if (string::npos == pos) {

          // No space found < max_value_width.  Look for the first
          // space after max_value_width.

          pos = v.find(' ', max_value_width);

        if (string::npos == pos) {
            // There's just no spaces.  So just print it and be done.

            cout << filler << v << endl;
            break;
          } else {
            cout << filler << v.substr(0, pos) << endl;
            v = v.substr(pos + 1);
          }
        } else {
          cout << filler << v.substr(0, pos) << endl;
          v = v.substr(pos + 1);
        }

        // Reset for the next iteration

        filler = spaces;
      }
    }
  } else {
    cout << plain_message << ":" << value << endl;
  }
}


static void
out(const string& pretty_message, const string &plain_message, int value)
{
  if (pretty) {
    string spaces(LAM_max(centerpoint - pretty_message.length(), 0), ' ');
    cout << spaces << pretty_message << ": " << value << endl;
  } else {
    cout << plain_message << ":" << value << endl;
  }
}


//
// Open all SSI modules so that they can register their SSI
// parameters.  Take a shotgun approach here and indiscriminately open
// all modules -- don't be selective.  To this end, we need to clear
// out the environment of all LAM_MPI_mca_<type> variables to ensure
// that the open algorithms don't try to only open one module.
//
static void
open_modules()
{
  if (opened_modules)
    return;

  // Clear out the environment

  if (NULL != getenv("LAM_MPI_mca_boot"))
    putenv("LAM_MPI_mca_boot=");
  if (NULL != getenv("LAM_MPI_mca_coll"))
    putenv("LAM_MPI_mca_coll=");
  if (NULL != getenv("LAM_MPI_mca_cr"))
    putenv("LAM_MPI_ssi_cr=");
  if (NULL != getenv("LAM_MPI_mca_pml"))
    putenv("LAM_MPI_mca_pml=");

  // Open all modules
#if 0
  mca_base_open(cmd_line);
  mca_pml_base_open(cmd_line);
  mca_coll_base_open(cmd_line);
  mca_boot_open(cmd_line);
  mca_crmpi_base_open(cmd_line);
#endif

  // All done

  opened_modules = true;
}


static void
close_modules()
{
  if (opened_modules) {
#if 0
    mca_crmpi_base_close();
    mca_coll_base_close();
    mca_pml_base_close();
    mca_boot_close();
    mca_base_close();
#endif
  }
}
