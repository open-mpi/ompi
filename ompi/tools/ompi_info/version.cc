//
// Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2006 The University of Tennessee and The University
//                         of Tennessee Research Foundation.  All rights
//                         reserved.
// Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
//                         University of Stuttgart.  All rights reserved.
// Copyright (c) 2004-2005 The Regents of the University of California.
//                         All rights reserved.
// $COPYRIGHT$
// 
// Additional copyrights may follow
// 
// $HEADER$
//

#include "ompi_config.h"

#include <iostream>
#include <string>

#include <stdio.h>

#include "opal/version.h"
#include "orte/version.h"
#include "ompi/version.h"
#include "opal/mca/base/base.h"
#include "ompi/tools/ompi_info/ompi_info.h"
#include "opal/util/printf.h"

using namespace std;
using namespace ompi_info;


//
// Public variables
//

const string ompi_info::ver_full = "full";
const string ompi_info::ver_major = "major";
const string ompi_info::ver_minor = "minor";
const string ompi_info::ver_release = "release";
const string ompi_info::ver_greek = "greek";
const string ompi_info::ver_svn = "svn";

//
// Private variables
//

static const string ver_all = "all";
static const string ver_mca = "mca";
static const string ver_type = "type";
static const string ver_component = "component";


//
// Private functions
//

static void show_mca_version(const mca_base_component_t *component,
                             const string& scope, const string& ver_type);
static string make_version_str(const string& scope,
                               int major, int minor, int release,
                               const string& greek, 
                               bool want_svn, const string& svn);

//
// do_version
//
// Determines the version information related to the ompi components
// being used.
// Accepts: 
//	- want_all: True if all components' info is required.
//	- cmd_line: The constructed command line argument
//
void ompi_info::do_version(bool want_all, opal_cmd_line_t *cmd_line)
{
  unsigned int count;
  ompi_info::type_vector_t::size_type i;
  string arg1, scope, type, component;
  string::size_type pos;

  open_components();

  if (want_all) {
    show_ompi_version(ver_full);
    for (i = 0; i < mca_types.size(); ++i) {
      show_component_version(mca_types[i], component_all, ver_full, type_all);
    }
  } else {
    count = opal_cmd_line_get_ninsts(cmd_line, "version");
    for (i = 0; i < count; ++i) {
      arg1 = opal_cmd_line_get_param(cmd_line, "version", i, 0);
      scope = opal_cmd_line_get_param(cmd_line, "version", i, 1);

      // Version of Open MPI
 
      if (type_ompi == arg1) {
        show_ompi_version(scope);
      } 

      // Specific type and component

      else if (string::npos != (pos = arg1.find(':'))) {
        type = arg1.substr(0, pos - 1);
        component = arg1.substr(pos);

        show_component_version(type, component, scope, ver_all);
      }

      // All components of a specific type

      else {
        show_component_version(arg1, component_all, scope, ver_all);
      }
    }
  }
}


//
// Show the version of Open MPI
//
void ompi_info::show_ompi_version(const string& scope)
{
  out("Open MPI", type_ompi + ":version:full",
      make_version_str(scope, 
                       OMPI_MAJOR_VERSION, OMPI_MINOR_VERSION, 
                       OMPI_RELEASE_VERSION, 
                       OMPI_GREEK_VERSION,
                       OMPI_WANT_SVN, OMPI_SVN_R));
  out("Open MPI SVN revision", type_ompi + ":version:svn",
      OMPI_SVN_R);

  out("Open RTE", type_orte + ":version:full",
      make_version_str(scope, 
                       ORTE_MAJOR_VERSION, ORTE_MINOR_VERSION, 
                       ORTE_RELEASE_VERSION, 
                       ORTE_GREEK_VERSION,
                       ORTE_WANT_SVN, ORTE_SVN_R));
  out("Open RTE SVN revision", type_orte + ":version:svn",
      ORTE_SVN_R);

  out("OPAL", type_opal + ":version:full",
      make_version_str(scope, 
                       OPAL_MAJOR_VERSION, OPAL_MINOR_VERSION, 
                       OPAL_RELEASE_VERSION, 
                       OPAL_GREEK_VERSION,
                       OPAL_WANT_SVN, OPAL_SVN_R));
  out("OPAL SVN revision", type_opal + ":version:svn",
      OPAL_SVN_R);
}


//
// Show all the components of a specific type/component combo (component may be
// a wildcard)
//
void ompi_info::show_component_version(const string& type_name, 
                                  const string& component_name,
                                  const string& scope, const string& ver_type)
{
  ompi_info::type_vector_t::size_type i;
  bool want_all_components = (type_all == component_name);
  bool found;
  opal_list_item_t *item;
  mca_base_component_list_item_t *cli;
  const mca_base_component_t *component;
  opal_list_t *components;

  // Check to see if the type is valid

  for (found = false, i = 0; i < mca_types.size(); ++i) {
    if (mca_types[i] == type_name) {
      found = true;
      break;
    }
  }

  if (!found) {
#if 0
    show_help("ompi_info", "usage");
#endif
    exit(1);
  }

  // Now that we have a valid type, find the right component list

  components = component_map[type_name];
  if (NULL != components) {
      if (opal_list_get_size(components) > 0){
          for (item = opal_list_get_first(components);
               opal_list_get_end(components) != item;
               item = opal_list_get_next(item)) {
              cli = (mca_base_component_list_item_t *) item;
              component = cli->cli_component;
              if (want_all_components || 
                  component->mca_component_name == component_name) {
                  show_mca_version(component, scope, ver_type);
              }
          }
      }
  }
}


//
// Given a component, display its relevant version(s)
//
static void show_mca_version(const mca_base_component_t* component,
                             const string& scope, const string& ver_type)
{
  bool printed;
  bool want_mca = (ver_all == ver_type || ver_type == ver_mca);
  bool want_type = (ver_all == ver_type || ver_type == ver_type);
  bool want_component = (ver_all == ver_type || ver_type == ver_component);
  string message, content;
  string mca_version;
  string api_version;
  string component_version;
  string empty;

  mca_version = make_version_str(scope, component->mca_major_version,
                                 component->mca_minor_version,
                                 component->mca_release_version, "",
                                 false, "");
  api_version = make_version_str(scope, component->mca_type_major_version,
                                 component->mca_type_minor_version,
                                 component->mca_type_release_version, "",
                                 false, "");
  component_version = make_version_str(scope, component->mca_component_major_version,
                                    component->mca_component_minor_version,
                                    component->mca_component_release_version, 
                                    "", false, "");

  if (pretty) {
    message = "MCA ";
    message += component->mca_type_name;
    printed = false;

    content = component->mca_component_name + string(" (");
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
    if (want_component) {
      if (printed)
        content += ", ";
      content += "Component v" + component_version;
      printed = true;
    }
    out(message, empty, content + ")");
  } else {
    message = "mca:";
    message += component->mca_type_name;
    message += ":";
    message += component->mca_component_name;
    message += ":version";
    if (want_mca)
      out(empty, message, "mca:" + mca_version);
    if (want_type)
      out(empty, message, "api:" + api_version);
    if (want_component)
      out(empty, message, "component:" + component_version);
  }
}


static string make_version_str(const string& scope,
                               int major, int minor, int release,
                               const string &greek, 
                               bool want_svn, const string& svn)
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
    if (!greek.empty()) {
        str += greek;
    }
    if (want_svn && !svn.empty()) {
      str += svn;
    }
  } else if (scope == ver_major) {
    snprintf(temp, BUFSIZ - 1, "%d", major);
  } else if (scope == ver_minor) {
    snprintf(temp, BUFSIZ - 1, "%d", minor);
  } else if (scope == ver_release) {
    snprintf(temp, BUFSIZ - 1, "%d", release);
  } else if (scope == ver_greek) {
    str = greek;
  } else if (scope == ver_svn) {
    str = svn;
  } else {
#if 0
    show_help("ompi_info", "usage");
#endif
    exit(1);
  }

  if (str.empty()) {
    str = temp;
  }

  return str;
}
