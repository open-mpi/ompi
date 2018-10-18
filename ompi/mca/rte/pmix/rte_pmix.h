/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2018 Intel, Inc. All rights reserved.
 * Copyright (c) 2014      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2014-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2015      Intel, Inc. All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 * When this component is used, this file is included in the rest of
 * the OPAL/OMPI code base via ompi/mca/rte/rte.h.  As such,
 * this header represents the public interface to this static component.
 */

#ifndef MCA_OMPI_RTE_PMIX_H
#define MCA_OMPI_RTE_PMIX_H

#include "ompi_config.h"
#include "ompi/constants.h"

#include <stdint.h>

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif

struct opal_proc_t;

#include "opal/threads/threads.h"
#include "opal/util/proc.h"
#include "opal/mca/hwloc/hwloc-internal.h"
#include "opal/mca/pmix/pmix.h"

struct ompi_proc_t;
struct ompi_communicator_t;

BEGIN_C_DECLS

/* Process name objects and operations */
typedef opal_process_name_t ompi_process_name_t;
typedef uint32_t ompi_jobid_t;
typedef uint32_t ompi_vpid_t;

/* some local storage */
OMPI_DECLSPEC extern opal_process_name_t pmix_name_wildcard;
OMPI_DECLSPEC extern hwloc_cpuset_t ompi_proc_applied_binding;

#define OMPI_PROC_MY_NAME (&pmix_process_info.my_name)
#define OMPI_NAME_WILDCARD  (&pmix_name_wildcard)

typedef uint8_t ompi_rte_cmp_bitmask_t;
#define OMPI_RTE_CMP_NONE   0x00
#define OMPI_RTE_CMP_JOBID  0x02
#define OMPI_RTE_CMP_VPID   0x04
#define OMPI_RTE_CMP_ALL    0x0f
#define OMPI_RTE_CMP_WILD   0x10

OMPI_DECLSPEC char* ompi_pmix_print_name(const ompi_process_name_t *name);

#define OMPI_NAME_PRINT(a) ompi_pmix_print_name(a)
OMPI_DECLSPEC int ompi_rte_compare_name_fields(ompi_rte_cmp_bitmask_t mask,
                                               const opal_process_name_t* name1,
                                               const opal_process_name_t* name2);
OMPI_DECLSPEC int ompi_rte_convert_string_to_process_name(opal_process_name_t *name,
                                                          const char* name_string);
OMPI_DECLSPEC int ompi_rte_convert_process_name_to_string(char** name_string,
                                                          const opal_process_name_t *name);

#define OMPI_LOCAL_JOBID(n) \
    ( (n) & 0x0000ffff)
#define OMPI_JOB_FAMILY(n)  \
    (((n) >> 16) & 0x0000ffff)
#define OMPI_CONSTRUCT_LOCAL_JOBID(local, job) \
    ( ((local) & 0xffff0000) | ((job) & 0x0000ffff) )
#define OMPI_CONSTRUCT_JOB_FAMILY(n) \
    ( ((n) << 16) & 0xffff0000)

#define OMPI_CONSTRUCT_JOBID(family, local) \
    OMPI_CONSTRUCT_LOCAL_JOBID(OMPI_CONSTRUCT_JOB_FAMILY(family), local)

/* This is the DSS tag to serialize a proc name */
#define OMPI_NAME OPAL_NAME
#define OMPI_PROCESS_NAME_HTON OPAL_PROCESS_NAME_HTON
#define OMPI_PROCESS_NAME_NTOH OPAL_PROCESS_NAME_NTOH

#if OPAL_ENABLE_DEBUG
static inline opal_process_name_t * OMPI_CAST_RTE_NAME(opal_process_name_t * name) {
    return (opal_process_name_t *)name;
}
#else
#define OMPI_CAST_RTE_NAME(a) ((opal_process_name_t*)(a))
#endif

/* Process info struct and values */
typedef uint16_t ompi_node_rank_t;
typedef uint16_t ompi_local_rank_t;
#define OMPI_NODE_RANK_INVALID UINT16_MAX
#define OMPI_LOCAL_RANK_INVALID UINT16_MAX

typedef struct {
    opal_process_name_t my_name;
    char *my_hnp_uri;
    char *nodename;
    pid_t pid;
    char *job_session_dir;
    char *proc_session_dir;
    uint16_t my_local_rank;
    uint16_t my_node_rank;
    int32_t num_local_peers;
    uint32_t num_procs;
    uint32_t app_num;
} pmix_process_info_t;
OMPI_DECLSPEC extern pmix_process_info_t pmix_process_info;
#define ompi_process_info pmix_process_info

OMPI_DECLSPEC extern bool pmix_proc_is_bound;
#define ompi_rte_proc_is_bound pmix_proc_is_bound

/* Error handling objects and operations */
OMPI_DECLSPEC void __opal_attribute_noreturn__
  ompi_rte_abort(int error_code, char *fmt, ...);
OMPI_DECLSPEC void ompi_rte_abort_peers(opal_process_name_t *procs,
                                        int32_t num_procs,
                                        int error_code);
#define OMPI_ERROR_LOG OPAL_ERROR_LOG

/* Init and finalize operations */
OMPI_DECLSPEC int ompi_rte_init(int *argc, char ***argv);
OMPI_DECLSPEC int ompi_rte_finalize(void);
OMPI_DECLSPEC void ompi_rte_wait_for_debugger(void);

/* check dynamics support */
OMPI_DECLSPEC bool ompi_rte_connect_accept_support(const char *port);

END_C_DECLS

#endif /* MCA_OMPI_RTE_PMIX_H */
