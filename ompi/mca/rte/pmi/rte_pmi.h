/*
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#ifndef MCA_OMPI_RTE_PMI_H
#define MCA_OMPI_RTE_PMI_H

#include "opal/dss/dss_types.h"
#include "opal/class/opal_pointer_array.h"

BEGIN_C_DECLS

/* Process name objects and operations */
typedef uint32_t ompi_jobid_t;
typedef uint32_t ompi_vpid_t;
struct ompi_process_name_t {
    ompi_jobid_t jobid;
    ompi_vpid_t vpid;
};
typedef struct ompi_process_name_t ompi_process_name_t;

#define OMPI_PROCESS_NAME_HTON(n)       \
do {                                    \
    n.jobid = htonl(n.jobid);           \
    n.vpid = htonl(n.vpid);             \
} while (0)

#define OMPI_PROCESS_NAME_NTOH(n)       \
do {                                    \
    n.jobid = ntohl(n.jobid);           \
    n.vpid = ntohl(n.vpid);             \
} while (0)

typedef int ompi_rte_cmp_bitmask_t;

OMPI_DECLSPEC extern ompi_process_name_t ompi_rte_my_process_name;
#define OMPI_PROC_MY_NAME (&ompi_rte_my_process_name)

OMPI_DECLSPEC char* ompi_rte_print_process_name(const ompi_process_name_t *name);
#define OMPI_NAME_PRINT(a) ompi_rte_print_process_name(a)

OMPI_DECLSPEC int ompi_rte_compare_name_fields(ompi_rte_cmp_bitmask_t fields,
                                               const ompi_process_name_t *name1,
                                               const ompi_process_name_t *name2);
#define OMPI_RTE_CMP_JOBID  0x01
#define OMPI_RTE_CMP_VPID   0x02
#define OMPI_RTE_CMP_ALL    0x03

OMPI_DECLSPEC extern ompi_process_name_t ompi_rte_wildcard_process_name;
#define OMPI_NAME_WILDCARD (&ompi_rte_wildcard_process_name)

OMPI_DECLSPEC uint64_t ompi_rte_hash_name(const ompi_process_name_t *name);

#define OMPI_NAME (OPAL_DSS_ID_DYNAMIC + 25)

/* Collective objects and operations */
struct ompi_rte_collective_t {
    opal_object_t super;
    int id;
    bool active;
};
typedef struct ompi_rte_collective_t ompi_rte_collective_t;
OBJ_CLASS_DECLARATION(ompi_rte_collective_t);

OMPI_DECLSPEC int ompi_rte_modex(ompi_rte_collective_t *coll);
OMPI_DECLSPEC int ompi_rte_barrier(ompi_rte_collective_t *coll);

/* Process info struct and values */
typedef int ompi_node_rank_t;
typedef int ompi_local_rank_t;
struct ompi_process_info_t {
    int app_num;
    pid_t pid;
    ompi_vpid_t num_procs;
    ompi_local_rank_t my_local_rank;
    ompi_node_rank_t my_node_rank;
    ompi_node_rank_t num_local_peers;
    char *my_hnp_uri;
    int peer_modex;
    int peer_init_barrier;
    int peer_fini_barrier;
    char *job_session_dir;
    char *proc_session_dir;
    char nodename[100]; /* BWB: FIX ME: This really needs to be a rational constant */
};
typedef struct ompi_process_info_t ompi_process_info_t;
#define OMPI_LOCAL_RANK_INVALID (-1)
#define OMPI_NODE_RANK_INVALID (-1)


OMPI_DECLSPEC extern ompi_process_info_t ompi_process_info;
OMPI_DECLSPEC extern bool ompi_rte_proc_is_bound;

/* Error handling objects and operations */
OMPI_DECLSPEC void ompi_rte_abort(int error_code, char *fmt, ...);
OMPI_DECLSPEC int ompi_rte_abort_peers(ompi_process_name_t *procs, size_t nprocs, int status);
OMPI_DECLSPEC int ompi_rte_error_log(const char *file, int line, 
                                     const char *func, int ret);
#define OMPI_ERROR_LOG(ret) ompi_rte_error_log(__FILE__, __LINE__, __func__, ret)
void ompi_rte_set_fault_callback(void (*)(opal_pointer_array_t*));

/* Init and finalize objects and operations */
OMPI_DECLSPEC int ompi_rte_init(int *argc, char ***argv);
OMPI_DECLSPEC int ompi_rte_finalize(void);
OMPI_DECLSPEC void ompi_rte_wait_for_debugger(void);

/* Database operations */
struct opal_buffer_t;

OMPI_DECLSPEC int ompi_rte_db_store(const ompi_process_name_t *proc,
                                    const char *key,
                                    const void *data,
                                    opal_data_type_t type);
OMPI_DECLSPEC int ompi_rte_db_fetch(const ompi_process_name_t *proc,
                                    const char *key,
                                    void **data,
                                    opal_data_type_t type);
OMPI_DECLSPEC int ompi_rte_db_fetch_pointer(const ompi_process_name_t *proc,
                                            const char *key,
                                            void **data,
                                            opal_data_type_t type);
#define OMPI_DB_HOSTNAME     "ompi.hostname"
#define OMPI_DB_LOCALITY     "ompi.locality"

/* Communications */
typedef int ompi_rml_tag_t;

OMPI_DECLSPEC int ompi_rte_send_buffer(const ompi_process_name_t *peer,
                                       struct opal_buffer_t *buffer,
                                       ompi_rml_tag_t tag,
                                       int flags);
OMPI_DECLSPEC int ompi_rte_send_buffer_nb(const ompi_process_name_t *peer,
                                          struct opal_buffer_t *buffer,
                                          ompi_rml_tag_t tag,
                                          int flags,
                                          void (*cbfunc)(int, ompi_process_name_t*,
                                                         opal_buffer_t*, ompi_rml_tag_t,
                                                         void*),
                                          void *cbdata);
OMPI_DECLSPEC int ompi_rte_recv_buffer(const ompi_process_name_t *peer,
                                       struct opal_buffer_t *buf,
                                       ompi_rml_tag_t tag,
                                       int flags);
OMPI_DECLSPEC int ompi_rte_recv_buffer_nb(const ompi_process_name_t *peer,
                                          ompi_rml_tag_t tag,
                                          int flags,
                                          void (*cbfunc)(int, ompi_process_name_t*,
                                                         opal_buffer_t*, ompi_rml_tag_t,
                                                         void*),
                                          void *cbdata);
OMPI_DECLSPEC int ompi_rte_recv_cancel(const ompi_process_name_t *peer,
                                       ompi_rml_tag_t tag);
OMPI_DECLSPEC int ompi_rte_parse_uris(const char* contact_info,
                                      ompi_process_name_t *peer,
                                      char ***uris);

/* Communication tags */
/* carry over the INVALID def */
#define OMPI_RML_TAG_INVALID -1
/* define a starting point to avoid conflicts */
#define OMPI_RML_TAG_BASE    0

#define OMPI_RML_PERSISTENT  0

/* BWB: FIX ME: THis is not the right way to do this... */
#define ORTE_ERR_NO_MATCH_YET OMPI_ERROR

END_C_DECLS

#endif /* MCA_OMPI_RTE_PMI_H */
