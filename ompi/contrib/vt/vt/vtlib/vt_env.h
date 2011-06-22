/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2011, ZIH, TU Dresden, Federal Republic of Germany
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
EXTERN char*  vt_env_dyn_shlibs(void);
EXTERN int    vt_env_dyn_ignore_nodbg(void);
EXTERN char*  vt_env_gnu_nm(void);
EXTERN char*  vt_env_gnu_nmfile(void);
EXTERN char*  vt_env_gdir(void);
EXTERN char*  vt_env_ldir(void);
EXTERN int    vt_env_gdir_check(void);
EXTERN int    vt_env_ldir_check(void);
EXTERN char*  vt_env_fprefix(void);
EXTERN int    vt_env_funique(void);
EXTERN size_t vt_env_bsize(void);
EXTERN size_t vt_env_copy_bsize(void);
EXTERN int    vt_env_pthread_reuse(void);
EXTERN int    vt_env_mode(void);
EXTERN int    vt_env_stat_intv(void);
EXTERN int    vt_env_stat_props(void);
EXTERN int    vt_env_stat_msg_dtls(void);
EXTERN int    vt_env_stat_collop_dtls(void);
EXTERN int    vt_env_verbose(void);
EXTERN int    vt_env_debug(void);
EXTERN int    vt_env_do_unify(void);
EXTERN int    vt_env_do_clean(void);
EXTERN int    vt_env_memtrace(void);
EXTERN int    vt_env_memtrace_marker(void);
EXTERN int    vt_env_cpuidtrace(void);
EXTERN int    vt_env_iotrace(void);
EXTERN char*  vt_env_iolibpathname(void);
EXTERN int    vt_env_libctrace(void);
EXTERN int    vt_env_omptrace(void);
EXTERN int    vt_env_mpitrace(void);
EXTERN int    vt_env_mpicheck(void);
EXTERN int    vt_env_mpicheck_errexit(void);
EXTERN int    vt_env_max_mpi_comms(void);
EXTERN int    vt_env_max_mpi_wins(void);
EXTERN char*  vt_env_rusage(void);
EXTERN int    vt_env_rusage_intv(void);
EXTERN char*  vt_env_metrics(void);
EXTERN char*  vt_env_metrics_sep(void);
EXTERN char*  vt_env_metrics_spec(void);
EXTERN int    vt_env_sync_flush(void);
EXTERN int    vt_env_sync_flush_level(void);
EXTERN int    vt_env_max_stack_depth(void);
EXTERN int    vt_env_max_flushes(void);
EXTERN int    vt_env_max_threads(void);
EXTERN int    vt_env_compression(void);
EXTERN int    vt_env_java_native(void);
EXTERN int    vt_env_java_synthetic(void);
EXTERN int    vt_env_java_group_classes(void);
EXTERN char*  vt_env_java_filter_spec(void);
EXTERN char*  vt_env_filter_spec(void);
EXTERN char*  vt_env_groups_spec(void);
EXTERN int    vt_env_etimesync(void);
EXTERN int    vt_env_etimesync_intv(void);
EXTERN int    vt_env_cudarttrace(void);
EXTERN int    vt_env_cudatrace_idle(void);
EXTERN size_t vt_env_cudatrace_bsize(void);
EXTERN int    vt_env_cudatrace_kernel(void);
EXTERN int    vt_env_cudatrace_memcpyasync(void);
EXTERN int    vt_env_cudatrace_sync(void);
EXTERN int    vt_env_cudatrace_gpumem(void);
EXTERN int    vt_env_cudatrace_error(void);
EXTERN char*  vt_env_cupti_metrics(void);
EXTERN int    vt_env_cupti_sampling(void);
EXTERN int    vt_env_gputrace_debug(void);

#endif /* _VT_ENV_H */



















