/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2012, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_FILTER_H_
#define _VT_FILTER_H_

#include "config.h"

#if defined(HAVE_OMP) && HAVE_OMP
  // disable OpenMP under the following circumstances:

  // on NEC SX platforms (causes "internal compiler error")
# if defined(_SX)
#   undef HAVE_OMP

  // on MacOS X using GCC < v4.5
  // causes "undefined reference to ___builtin_expect()"
  // (induced by assert()'s within OpenMP-parallel regions)
# elif (defined(__APPLE__) && defined(__MACH__) && defined(__GNUC__) && \
       (__GNUC__ < 4 || (__GNUC__ == 4 &&  __GNUC_MINOR__ < 5)))
#   undef HAVE_OMP

  // using Open64 < v4.2.4 (causes "internal compiler error")
# elif defined(__OPEN64__)
#   if !defined(__OPENCC__) || !defined(__OPENCC_MINOR__) || !defined(__OPENCC_PATCHLEVEL__)
      // unknown compiler version; disable OpenMP to be on the safe side
#     undef HAVE_OMP
#   else
      // __OPENCC_PATCHLEVEL__ can be empty; redefine it to 0
#     if !(__OPENCC_PATCHLEVEL__ + 0)
#       undef __OPENCC_PATCHLEVEL__
#       define __OPENCC_PATCHLEVEL__ 0
#     endif
      // disable OpenMP, if compiler version is less than 4.2.4
#     if __OPENCC__ < 4 || (__OPENCC__ == 4 && (__OPENCC_MINOR__ < 2 || (__OPENCC_MINOR__ == 2 && __OPENCC_PATCHLEVEL__ < 4)))
#       undef HAVE_OMP
#     endif
#   endif
# endif // __OPEN64__
#endif // HAVE_OMP
#if defined(HAVE_OMP) && HAVE_OMP
# include <omp.h>
#endif // HAVE_OMP

#include "vt_inttypes.h"

#include <string>
#include <vector>

#include <assert.h>

#ifdef VT_MPI
# include "vt_defs.h" // to get VT_MPI_INT
# define MASTER if( MyRank == 0 )
# define SLAVE  if( MyRank != 0 )
#else // VT_MPI
# define MASTER
# define SLAVE
#endif // VT_MPI

// typedef for filter modes
//
typedef enum
{
  MODE_GEN, // generate a filter file
  MODE_FILT // filter a trace using an already existing filter file
} FilterModeT;

// data structure for program parameters
//
struct ParamsS
{
  ParamsS()
  : mode(default_mode), input_trcfile(""), verbose_level(0),
    show_progress(false), show_usage(false), show_version(false),
    g_output_filtfile(""), g_incl_file(""), g_excl_file(""),
    g_call_limit(g_default_call_limit), g_reduce_ratio(0),
    g_print_stats(false), g_incl_callees(false),
    f_output_trcfile(""), f_input_filtfile(""),
    f_max_output_streams(f_default_max_output_streams),
    f_max_file_handles(f_default_max_file_handles),
    f_compress_level(f_default_compress_level) {}

  // defaults
  //
  static const FilterModeT default_mode                 = MODE_FILT;
  static const uint32_t    g_default_call_limit         = 0;
  static const uint32_t    f_default_max_output_streams = 0;
  static const uint32_t    f_default_max_file_handles   = 256;
  static const uint32_t    f_default_compress_level     = 4;

  // command line parameters
  //

  // general
  //
  FilterModeT              mode;
  std::string              input_trcfile;
  uint32_t                 verbose_level;
  bool                     show_progress;
  bool                     show_usage;
  bool                     show_version;

  // generate
  //
  std::string              g_output_filtfile;
  std::string              g_incl_file;
  std::string              g_excl_file;
  std::vector<std::string> g_incl_funcs;
  std::vector<std::string> g_excl_funcs;
  uint32_t                 g_call_limit;
  uint32_t                 g_reduce_ratio;
  bool                     g_print_stats;
  bool                     g_incl_callees;

  // filter
  //
  std::string              f_output_trcfile;
  std::string              f_input_filtfile;
  uint32_t                 f_max_output_streams;
  uint32_t                 f_max_file_handles;
  uint32_t                 f_compress_level;

};

// print verbose message
extern void VPrint( uint8_t level, const char * fmt, ... );

// print verbose message in a parallel region
extern void PVPrint( uint8_t level, const char * fmt, ... );

// global variables
//

// name of program's executable
extern std::string ExeName;

// program parameters
extern ParamsS           Params;

#ifdef VT_MPI
  // number of MPI-ranks
  extern VT_MPI_INT      NumRanks;

  // MPI-rank of calling process
  extern VT_MPI_INT      MyRank;
#endif // VT_MPI

#endif // _VT_FILTER_H_
