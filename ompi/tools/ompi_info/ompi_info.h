//
// Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
//                         University Research and Technology
//                         Corporation.  All rights reserved.
// Copyright (c) 2004-2005 The University of Tennessee and The University
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

#ifndef OMPI_INFO_H
#define OMPI_INFO_H

#include <string>
#include <vector>
#include <map>

#include "opal/class/opal_list.h"
#include "opal/util/cmd_line.h"
#include "opal/mca/mca.h"


namespace ompi_info {

  //
  // Globals
  //

  typedef std::vector<std::string> type_vector_t;

  extern bool pretty;
  extern opal_cmd_line_t *cmd_line;

  extern const std::string type_all;
  extern const std::string type_ompi;
  extern const std::string type_orte;
  extern const std::string type_opal;
  extern const std::string type_base;
  extern type_vector_t mca_types;

  //
  // Version-related strings and functions
  //

  extern const std::string ver_full;
  extern const std::string ver_major;
  extern const std::string ver_minor;
  extern const std::string ver_release;
  extern const std::string ver_greek;
  extern const std::string ver_svn;

  void do_version(bool want_all, opal_cmd_line_t *cmd_line);
  void show_ompi_version(const std::string& scope);
  void show_component_version(const std::string& type_name, 
                           const std::string& component_name,
                           const std::string& scope, 
                           const std::string& ver_type);

  //
  // Parameter/configuration-related functions
  //

  extern std::string component_all;
  extern std::string param_all;

  extern std::string path_prefix;
  extern std::string path_bindir;
  extern std::string path_libdir;
  extern std::string path_incdir;
  extern std::string path_pkglibdir;
  extern std::string path_sysconfdir;

  void do_params(bool want_all, bool want_internal);
  void show_mca_params(const std::string& type, const std::string& component, 
                       bool want_internal);

  void do_path(bool want_all, opal_cmd_line_t *cmd_line);
  void show_path(const std::string& type, const std::string& value);

  void do_arch(opal_cmd_line_t *cmd_line);
  void do_config(bool want_all);

  //
  // Output-related functions
  //
  void out(const std::string& pretty_message, 
           const std::string &plain_message, 
           int value);
  void out(const std::string& pretty_message, 
           const std::string &plain_message,
           const std::string& value);

  //
  // Component-related functions
  //

  typedef std::map<std::string, opal_list_t *> component_map_t;

  extern component_map_t component_map;

  void open_components();
  void close_components();

}

#endif /* OMPI_INFO_H */
