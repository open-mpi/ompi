//
// $HEADER$
//

#include "lam_config.h"

#include <iostream>
#include <string>

#include <stdio.h>

#include "mca/base/base.h"
#include "tools/laminfo/laminfo.h"

using namespace std;
using namespace laminfo;


//
// Public variables
//

const string laminfo::ver_full = "full";
const string laminfo::ver_major = "major";
const string laminfo::ver_minor = "minor";
const string laminfo::ver_release = "release";
const string laminfo::ver_alpha = "alpha";
const string laminfo::ver_beta = "beta";
const string laminfo::ver_cvs = "cvs";

//
// Private variables
//

static const string ver_all = "all";
static const string ver_mca = "mca";
static const string ver_type = "type";
static const string ver_module = "module";


//
// Private functions
//

static void show_mca_version(const mca_base_module_t *module,
                             const string& scope, const string& ver_type);
static string make_version_str(const string& scope,
                               int major, int minor, int release, int alpha, 
                               int beta, int cvs);

//
// do_version
//
// Determines the version information related to the lam modules
// being used.
// Accepts: 
//	- want_all: True if all modules' info is required.
//	- cmd_line: The constructed command line argument
//
void laminfo::do_version(bool want_all, lam_cmd_line_t *cmd_line)
{
  unsigned int count;
  laminfo::type_vector_t::size_type i;
  string arg1, scope, type, module;
  string::size_type pos;

  open_modules();

  if (want_all) {
    show_lam_version(ver_full);
    for (i = 0; i < mca_types.size(); ++i) {
      show_module_version(mca_types[i], module_all, ver_full, type_all);
    }
  } else {
    count = lam_cmd_line_get_ninsts(cmd_line, "version");
    for (i = 0; i < count; ++i) {
      arg1 = lam_cmd_line_get_param(cmd_line, "version", i, 0);
      scope = lam_cmd_line_get_param(cmd_line, "version", i, 1);

      // Version of LAM/MPI
 
      if (type_lam == arg1) {
        show_lam_version(scope);
      } 

      // Specific type and module

      else if (string::npos != (pos = arg1.find(':'))) {
        type = arg1.substr(0, pos - 1);
        module = arg1.substr(pos);

        show_module_version(type, module, scope, ver_all);
      }

      // All modules of a specific type

      else {
        show_module_version(arg1, module_all, scope, ver_all);
      }
    }
  }
}


//
// Show the version of LAM/MPI
//
void laminfo::show_lam_version(const string& scope)
{
  out("LAM/MPI", "version:" + type_lam, 
      make_version_str(scope, 
                       LAM_MAJOR_VERSION, LAM_MINOR_VERSION, 
                       LAM_RELEASE_VERSION, 
                       LAM_ALPHA_VERSION, LAM_BETA_VERSION, LAM_CVS_VERSION));
}


//
// Show all the modules of a specific type/module combo (module may be
// a wildcard)
//
void laminfo::show_module_version(const string& type_name, 
                                  const string& module_name,
                                  const string& scope, const string& ver_type)
{
  laminfo::type_vector_t::size_type i;
  bool want_all_modules = (type_all == module_name);
  bool found;
  lam_list_item *item;
  mca_base_module_list_item_t *mli;
  const mca_base_module_t *module;
  lam_list_t *modules;

  // Check to see if the type is valid

  for (found = false, i = 0; i < mca_types.size(); ++i) {
    if (mca_types[i] == type_name) {
      found = true;
      break;
    }
  }

  if (!found) {
#if 0
    show_help("laminfo", "usage");
#endif
    exit(1);
  }

  // Now that we have a valid type, find the right module list

  modules = module_map[type_name];
  if (NULL != modules) {
    for (item = lam_list_get_first(modules);
         lam_list_get_end(modules) != item;
         item = lam_list_get_next(item)) {
      mli = (mca_base_module_list_item_t *) item;
      module = mli->mli_module;
      if (want_all_modules || module->mca_module_name == module_name) {
        show_mca_version(module, scope, ver_type);
      }
    }
  }
}


//
// Given a module, display its relevant version(s)
//
static void show_mca_version(const mca_base_module_t* module,
                             const string& scope, const string& ver_type)
{
  bool printed;
  bool want_mca = (ver_all == ver_type || ver_type == ver_mca);
  bool want_type = (ver_all == ver_type || ver_type == ver_type);
  bool want_module = (ver_all == ver_type || ver_type == ver_module);
  string message, content;
  string mca_version;
  string api_version;
  string module_version;
  string empty;

  mca_version = make_version_str(scope, module->mca_major_version,
                                 module->mca_minor_version,
                                 module->mca_release_version, 0, 0, 0);
  api_version = make_version_str(scope, module->mca_type_major_version,
                                 module->mca_type_minor_version,
                                 module->mca_type_release_version, 0, 0, 0);
  module_version = make_version_str(scope, module->mca_module_major_version,
                                    module->mca_module_minor_version,
                                    module->mca_module_release_version, 
                                    0, 0, 0);

  if (pretty) {
    message = "MCA ";
    message += module->mca_type_name;
    printed = false;

    content = module->mca_module_name + string(" (");
    if (want_mca) {
      content += "MCA v" + mca_version;
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
    message = "mca:";
    message += module->mca_type_name;
    message += ":";
    message += module->mca_module_name;
    message += ":version";
    if (want_mca)
      out(empty, message, "mca:" + mca_version);
    if (want_type)
      out(empty, message, "api:" + api_version);
    if (want_module)
      out(empty, message, "module:" + module_version);
  }
}


static string make_version_str(const string& scope,
                               int major, int minor, int release, int alpha, 
                               int beta, int cvs)
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
