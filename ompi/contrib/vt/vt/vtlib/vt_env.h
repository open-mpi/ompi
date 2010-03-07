/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2008, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich, Juelich Supercomputing
 *                          Centre, Federal Republic of Germany
 *
 * See the file COPYING in the package base directory for details
 **/

#ifndef _VT_ENV_H
#define _VT_ENV_H

#ifdef __cplusplus
#   define EXTERN extern "C" 
#else
#   define EXTERN extern 
#endif

#include <stdio.h>

EXTERN char*  vt_env_apppath(void);
EXTERN char*  vt_env_dyn_blacklist(void);
EXTERN char*  vt_env_dyn_shlibs(void);
EXTERN char*  vt_env_gdir(void);
EXTERN char*  vt_env_ldir(void);
EXTERN char*  vt_env_fprefix(void);
EXTERN size_t vt_env_bsize(void);
EXTERN int    vt_env_mode(void);
EXTERN int    vt_env_stat_intv(void);
EXTERN int    vt_env_stat_show(void);
EXTERN int    vt_env_is_verbose(void);
EXTERN int    vt_env_debug(void);
EXTERN int    vt_env_do_unify(void);
EXTERN int    vt_env_do_clean(void);
EXTERN int    vt_env_memtrace(void);
EXTERN int    vt_env_iotrace(void);
EXTERN int    vt_env_mpitrace(void);
EXTERN char*  vt_env_metrics(void);
EXTERN char*  vt_env_metrics_spec(void);
EXTERN int    vt_env_max_flushes(void);
EXTERN int    vt_env_max_threads(void);
EXTERN char*  vt_env_nm(void);
EXTERN char*  vt_env_nmfile(void);
EXTERN int    vt_env_compression(void);
EXTERN char*  vt_env_filter_spec(void);
EXTERN char*  vt_env_groups_spec(void);

#endif



















