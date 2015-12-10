/*
 * Copyright (c) 2014-2015 Intel, Inc.  All rights reserved.
 * Copyright (c) 2014-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_PMIX_EXTERNAL_H
#define MCA_PMIX_EXTERNAL_H

#include "opal_config.h"

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/pmix.h"
#include "pmix_server.h"
#include "pmix_server.h"
#include "pmix/pmix_common.h"

BEGIN_C_DECLS

typedef struct {
  opal_pmix_base_component_t super;
  opal_list_t jobids;
  bool native_launch;
} mca_pmix_external_component_t;

OPAL_DECLSPEC extern mca_pmix_external_component_t mca_pmix_external_component;

OPAL_DECLSPEC extern const opal_pmix_base_module_t opal_pmix_external_module;

/****  INTERNAL OBJECTS  ****/
typedef struct {
    opal_list_item_t super;
    opal_jobid_t jobid;
    char nspace[PMIX_MAX_NSLEN + 1];
} opal_pmix1_jobid_trkr_t;
OBJ_CLASS_DECLARATION(opal_pmix1_jobid_trkr_t);

typedef struct {
    opal_object_t super;
    pmix_proc_t p;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_proc_t *error_procs;
    size_t nerror_procs;
    pmix_info_t *info;
    size_t ninfo;
    pmix_app_t *apps;
    size_t sz;
    opal_pmix_op_cbfunc_t opcbfunc;
    opal_pmix_modex_cbfunc_t mdxcbfunc;
    opal_pmix_value_cbfunc_t valcbfunc;
    opal_pmix_lookup_cbfunc_t lkcbfunc;
    opal_pmix_spawn_cbfunc_t spcbfunc;
    void *cbdata;
} pmix1_opcaddy_t;
OBJ_CLASS_DECLARATION(pmix1_opcaddy_t);

typedef struct {
    opal_object_t super;
    opal_list_t procs;
    opal_list_t info;
    opal_list_t apps;
    pmix_op_cbfunc_t opcbfunc;
    pmix_dmodex_response_fn_t dmdxfunc;
    pmix_modex_cbfunc_t mdxcbfunc;
    pmix_lookup_cbfunc_t lkupcbfunc;
    pmix_spawn_cbfunc_t spwncbfunc;
    void *cbdata;
    opal_pmix_release_cbfunc_t odmdxfunc;
    void *ocbdata;
} pmix1_opalcaddy_t;
OBJ_CLASS_DECLARATION(pmix1_opalcaddy_t);


/****  CLIENT FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC int pmix1_client_init(void);
OPAL_MODULE_DECLSPEC int pmix1_client_finalize(void);
OPAL_MODULE_DECLSPEC int pmix1_initialized(void);
OPAL_MODULE_DECLSPEC int pmix1_abort(int flag, const char *msg,
                                     opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix1_commit(void);
OPAL_MODULE_DECLSPEC int pmix1_fence(opal_list_t *procs, int collect_data);
OPAL_MODULE_DECLSPEC int pmix1_fencenb(opal_list_t *procs, int collect_data,
                                       opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_put(opal_pmix_scope_t scope,
                                     opal_value_t *val);
OPAL_MODULE_DECLSPEC int pmix1_get(const opal_process_name_t *proc, const char *key,
                                   opal_list_t *info, opal_value_t **val);
OPAL_MODULE_DECLSPEC int pmix1_getnb(const opal_process_name_t *proc, const char *key,
                                     opal_list_t *info,
                                     opal_pmix_value_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_publish(opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix1_publishnb(opal_list_t *info,
                                         opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_lookup(opal_list_t *data, opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix1_lookupnb(char **keys, opal_list_t *info,
                                        opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_unpublish(char **keys, opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix1_unpublishnb(char **keys, opal_list_t *info,
                                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_spawn(opal_list_t *job_info, opal_list_t *apps, opal_jobid_t *jobid);
OPAL_MODULE_DECLSPEC int pmix1_spawnnb(opal_list_t *job_info, opal_list_t *apps,
                                       opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_connect(opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix1_connectnb(opal_list_t *procs,
                                         opal_pmix_op_cbfunc_t cbfunc,
                                         void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_disconnect(opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix1_disconnectnb(opal_list_t *procs,
                                            opal_pmix_op_cbfunc_t cbfunc,
                                            void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_resolve_peers(const char *nodename, opal_jobid_t jobid,
                                             opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix1_resolve_nodes(opal_jobid_t jobid, char **nodelist);

/****  COMMON FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC int pmix1_store_local(const opal_process_name_t *proc,
                                             opal_value_t *val);

/****  SERVER SOUTHBOUND FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC int pmix1_server_init(opal_pmix_server_module_t *module,
                                           opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix1_server_finalize(void);
OPAL_MODULE_DECLSPEC int pmix1_server_gen_regex(const char *input, char **regex);
OPAL_MODULE_DECLSPEC int pmix1_server_gen_ppn(const char *input, char **ppn);
OPAL_MODULE_DECLSPEC int pmix1_server_register_nspace(opal_jobid_t jobid,
                                                      int nlocalprocs,
                                                      opal_list_t *info,
                                                      opal_pmix_op_cbfunc_t cbfunc,
                                                      void *cbdata);
OPAL_MODULE_DECLSPEC void pmix1_server_deregister_nspace(opal_jobid_t jobid);
OPAL_MODULE_DECLSPEC int pmix1_server_register_client(const opal_process_name_t *proc,
                                                      uid_t uid, gid_t gid,
                                                      void *server_object,
                                                      opal_pmix_op_cbfunc_t cbfunc,
                                                      void *cbdata);
OPAL_MODULE_DECLSPEC void pmix1_server_deregister_client(const opal_process_name_t *proc);
OPAL_MODULE_DECLSPEC int pmix1_server_setup_fork(const opal_process_name_t *proc, char ***env);
OPAL_MODULE_DECLSPEC int pmix1_server_dmodex(const opal_process_name_t *proc,
                                             opal_pmix_modex_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix1_server_notify_error(int status,
                                                   opal_list_t *procs,
                                                   opal_list_t *error_procs,
                                                   opal_list_t *info,
                                                   opal_pmix_op_cbfunc_t cbfunc, void *cbdata);


/****  COMPONENT UTILITY FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC pmix_status_t pmix1_convert_opalrc(int rc);
OPAL_MODULE_DECLSPEC int pmix1_convert_rc(pmix_status_t rc);
OPAL_MODULE_DECLSPEC void pmix1_value_load(pmix_value_t *v,
                                           opal_value_t *kv);
OPAL_MODULE_DECLSPEC int pmix1_value_unload(opal_value_t *kv,
                                            const pmix_value_t *v);

END_C_DECLS

#endif /* MCA_PMIX_EXTERNAL_H */
