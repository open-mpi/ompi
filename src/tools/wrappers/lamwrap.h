//
// $HEADER$
//
/** @file **/
#ifndef LAM_WRAP_H
#define LAM_WRAP_H 1

#include "lam_config.h"

#include <string>
#include <vector>

//
// Commonly used type
//

typedef std::vector<std::string> lam_sv_t;


//
// Global variables
//

extern bool fl_libs;
extern bool fl_building;
extern bool fl_profile;
extern bool fl_cpp;

extern bool showme_cmd;
extern bool showme_compile;
extern bool showme_link;


//
// Functions in the helper LAM wrapper compiler library
//

// This is the granddaddy that will invoke the rest

int
lam_wrap_engine(int argc, char* argv[],
		const lam_sv_t& env_vars, const std::string& default_compiler,
		bool want_cxx_libs, bool want_f77_includes,
		const std::string& extra_args);

// Parse the command line and get some environment information

void lam_wrap_parse_args(int argc, char* argv[], bool& want_flags);

// Build up various argument lists

void lam_wrap_get_compiler(const lam_sv_t& env_list,
			   const std::string& default_comp, lam_sv_t& out);
void lam_wrap_build_user_args(int argc, char* argv[], lam_sv_t& user_args);
void lam_wrap_build_cflags(bool want_f77_includes, lam_sv_t& cflags);
void lam_wrap_build_ldflags(lam_sv_t& ldflags);
void lam_wrap_build_libs(bool want_cxx_libs, lam_sv_t& libs);
void lam_wrap_build_extra_flags(const std::string& extra_string, 
				lam_sv_t& extra_flags);

// Print or execute the file list of arguments

void lam_wrap_print_sv(const lam_sv_t& sv);
int lam_wrap_exec_sv(const lam_sv_t& sv);

// Various helper functions

void lam_wrap_strip_white(std::string & str);
bool lam_wrap_split(const std::string& str, char c, 
		    lam_sv_t& out);
void lam_wrap_split_append_sv(const std::string& str, lam_sv_t& out);
void lam_wrap_append_sv(const lam_sv_t& in, lam_sv_t& out);
bool lam_wrap_check_file(const std::string& dir, const std::string& file);

#endif // LAM_WRAP_H
