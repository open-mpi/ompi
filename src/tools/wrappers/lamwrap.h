//
// $HEADER$
//
/** @file **/
#ifndef OMPI_WRAP_H
#define OMPI_WRAP_H 1

#include "ompi_config.h"

#include <string>
#include <vector>

//
// Commonly used type
//

typedef std::vector<std::string> ompi_sv_t;


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
// Functions in the helper OMPI wrapper compiler library
//

// This is the granddaddy that will invoke the rest

int
ompi_wrap_engine(int argc, char* argv[],
		const ompi_sv_t& env_vars, const std::string& default_compiler,
		bool want_cxx_libs, bool want_f77_includes,
		const std::string& extra_args);

// Parse the command line and get some environment information

void ompi_wrap_parse_args(int argc, char* argv[], bool& want_flags);

// Build up various argument lists

void ompi_wrap_get_compiler(const ompi_sv_t& env_list,
			   const std::string& default_comp, ompi_sv_t& out);
void ompi_wrap_build_user_args(int argc, char* argv[], ompi_sv_t& user_args);
void ompi_wrap_build_cflags(bool want_f77_includes, ompi_sv_t& cflags);
void ompi_wrap_build_ldflags(ompi_sv_t& ldflags);
void ompi_wrap_build_libs(bool want_cxx_libs, ompi_sv_t& libs);
void ompi_wrap_build_extra_flags(const std::string& extra_string, 
				ompi_sv_t& extra_flags);

// Print or execute the file list of arguments

void ompi_wrap_print_sv(const ompi_sv_t& sv);
int ompi_wrap_exec_sv(const ompi_sv_t& sv);

// Various helper functions

void ompi_wrap_strip_white(std::string & str);
bool ompi_wrap_split(const std::string& str, char c, 
		    ompi_sv_t& out);
void ompi_wrap_split_append_sv(const std::string& str, ompi_sv_t& out);
void ompi_wrap_append_sv(const ompi_sv_t& in, ompi_sv_t& out);
bool ompi_wrap_check_file(const std::string& dir, const std::string& file);

#endif // OMPI_WRAP_H
