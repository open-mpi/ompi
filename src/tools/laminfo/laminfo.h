//
// $HEADER$
//

#ifndef LAMINFO_H
#define LAMINFO_H

#include <string>
#include <vector>
#include <map>

#include "lfc/lam_list.h"
#include "util/cmd_line.h"
#include "mca/mca.h"


namespace laminfo {

  //
  // Globals
  //

  typedef std::vector<std::string> type_vector_t;

  extern bool pretty;
  extern lam_cmd_line_t *cmd_line;

  extern const std::string type_all;
  extern const std::string type_lam;
  extern const std::string type_base;
  extern type_vector_t mca_types;

  //
  // Version-related strings and functions
  //

  extern const std::string ver_full;
  extern const std::string ver_major;
  extern const std::string ver_minor;
  extern const std::string ver_release;
  extern const std::string ver_alpha;
  extern const std::string ver_beta;
  extern const std::string ver_cvs;

  void do_version(bool want_all, lam_cmd_line_t *cmd_line);
  void show_lam_version(const std::string& scope);
  void show_module_version(const std::string& type_name, 
                           const std::string& module_name,
                           const std::string& scope, 
                           const std::string& ver_type);

  //
  // Parameter/configuration-related functions
  //

  extern std::string module_all;
  extern std::string param_all;

  extern std::string path_prefix;
  extern std::string path_bindir;
  extern std::string path_libdir;
  extern std::string path_incdir;
  extern std::string path_pkglibdir;
  extern std::string path_sysconfdir;

  void do_params();
  void show_mca_params(const std::string& type, const std::string& module, 
                       const std::string& param);

  void do_path(bool want_all, lam_cmd_line_t *cmd_line);
  void show_path(const std::string& type, const std::string& value);

  void do_arch(lam_cmd_line_t *cmd_line);
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
  // Module-related functions
  //

  typedef std::map<std::string, lam_list_t *> module_map_t;

  extern module_map_t module_map;

  void open_modules();
  void close_modules();

}

#endif /* LAMINFO_H */
