/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 *
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_GDS_HASH_H
#define PMIX_GDS_HASH_H

#include "src/include/pmix_config.h"

#include "src/class/pmix_list.h"
#include "src/include/pmix_globals.h"
#include "src/util/pmix_argv.h"
#include "src/util/pmix_error.h"
#include "src/util/pmix_hash.h"
#include "src/util/pmix_name_fns.h"
#include "src/util/pmix_output.h"

#include "src/mca/gds/gds.h"

BEGIN_C_DECLS

typedef struct {
    pmix_gds_base_component_t super;
    pmix_list_t mysessions;
    pmix_list_t myjobs;
} pmix_gds_hash_component_t;

/* the component must be visible data for the linker to find it */
PMIX_EXPORT extern pmix_gds_hash_component_t pmix_mca_gds_hash_component;
extern pmix_gds_base_module_t pmix_hash_module;

/* Define a bitmask to track what information may not have
 * been provided but is computable from other info */
#define PMIX_HASH_PROC_DATA 0x00000001
#define PMIX_HASH_JOB_SIZE  0x00000002
#define PMIX_HASH_MAX_PROCS 0x00000004
#define PMIX_HASH_NUM_NODES 0x00000008
#define PMIX_HASH_PROC_MAP  0x00000010
#define PMIX_HASH_NODE_MAP  0x00000020

/* struct definitions */
typedef struct {
    pmix_list_item_t super;
    uint32_t session;
    pmix_list_t sessioninfo;
    pmix_list_t nodeinfo;
} pmix_session_t;
PMIX_CLASS_DECLARATION(pmix_session_t);

typedef struct {
    pmix_list_item_t super;
    char *ns;
    pmix_namespace_t *nptr;
    pmix_hash_table_t internal;
    pmix_hash_table_t remote;
    pmix_hash_table_t local;
    bool gdata_added;
    pmix_list_t jobinfo;
    pmix_list_t apps;
    pmix_list_t nodeinfo;
    pmix_session_t *session;
} pmix_job_t;
PMIX_CLASS_DECLARATION(pmix_job_t);

typedef struct {
    pmix_list_item_t super;
    uint32_t appnum;
    pmix_list_t appinfo;
    pmix_list_t nodeinfo;
    pmix_job_t *job;
} pmix_apptrkr_t;
PMIX_CLASS_DECLARATION(pmix_apptrkr_t);

typedef struct {
    pmix_list_item_t super;
    uint32_t nodeid;
    char *hostname;
    char **aliases;
    pmix_list_t info;
} pmix_nodeinfo_t;
PMIX_CLASS_DECLARATION(pmix_nodeinfo_t);

extern pmix_status_t pmix_gds_hash_process_node_array(pmix_value_t *val, pmix_list_t *tgt);

extern pmix_status_t pmix_gds_hash_process_app_array(pmix_value_t *val, pmix_job_t *trk);

extern pmix_status_t pmix_gds_hash_process_job_array(pmix_info_t *info, pmix_job_t *trk,
                                                     uint32_t *flags, char ***procs, char ***nodes);

extern pmix_status_t pmix_gds_hash_process_session_array(pmix_value_t *val, pmix_job_t *trk);

extern pmix_job_t *pmix_gds_hash_get_tracker(const pmix_nspace_t nspace, bool create);

extern pmix_session_t* pmix_gds_hash_check_session(pmix_job_t *trk,
                                                   uint32_t sid,
                                                   bool create);
extern pmix_status_t pmix_gds_hash_xfer_sessioninfo(pmix_session_t *sptr,
                                                    pmix_job_t *trk,
                                                    const char *key,
                                                    pmix_list_t *kvs);

extern bool pmix_gds_hash_check_hostname(char *h1, char *h2);

extern bool pmix_gds_hash_check_node(pmix_nodeinfo_t *n1, pmix_nodeinfo_t *n2);

extern pmix_nodeinfo_t* pmix_gds_hash_check_nodename(pmix_list_t *nodes, char *hostname);

extern pmix_status_t pmix_gds_hash_store_map(pmix_job_t *trk, char **nodes, char **ppn,
                                             uint32_t flags);

extern pmix_status_t pmix_gds_hash_fetch(const pmix_proc_t *proc, pmix_scope_t scope, bool copy,
                                         const char *key, pmix_info_t qualifiers[], size_t nqual,
                                         pmix_list_t *kvs);

extern pmix_status_t pmix_gds_hash_fetch_sessioninfo(const char *key,
                                                     pmix_job_t *trk,
                                                     pmix_info_t *info, size_t ninfo,
                                                     pmix_list_t *kvs);

extern pmix_status_t pmix_gds_hash_fetch_nodeinfo(const char *key, pmix_job_t *trk,
                                                  pmix_list_t *tgt, pmix_info_t *info, size_t ninfo,
                                                  pmix_list_t *kvs);

extern pmix_status_t pmix_gds_hash_fetch_appinfo(const char *key, pmix_job_t *trk, pmix_list_t *tgt,
                                                 pmix_info_t *info, size_t ninfo, pmix_list_t *kvs);

extern pmix_status_t pmix_gds_hash_store(const pmix_proc_t *proc, pmix_scope_t scope,
                                         pmix_kval_t *kv);

extern pmix_status_t pmix_gds_hash_store_qualified(pmix_hash_table_t *ht,
                                                   pmix_rank_t rank,
                                                   pmix_value_t *value);

extern pmix_status_t pmix_gds_hash_fetch_arrays(struct pmix_peer_t *pr, pmix_buffer_t *reply);

END_C_DECLS

#endif
