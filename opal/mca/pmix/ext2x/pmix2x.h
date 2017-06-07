/*
 * Copyright (c) 2014-2017 Intel, Inc. All rights reserved.
 * Copyright (c) 2014-2015 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MCA_PMIX_PMIX2X_H
#define MCA_PMIX_PMIX2X_H

#include "opal_config.h"

#ifdef HAVE_SYS_SOCKET_H
#include <sys/socket.h>
#endif
#ifdef HAVE_SYS_UN_H
#include <sys/un.h>
#endif

#include "opal/class/opal_list.h"
#include "opal/mca/mca.h"
#include "opal/mca/event/event.h"
#include "opal/util/proc.h"

#include "opal/mca/pmix/pmix.h"
#include "pmix_server.h"
#include "pmix_common.h"

BEGIN_C_DECLS

typedef struct {
  opal_pmix_base_component_t super;
  opal_list_t jobids;
  bool native_launch;
  size_t evindex;
  opal_list_t single_events;
  opal_list_t multi_events;
  opal_list_t default_events;
  int cache_size;
  opal_list_t cache;
} mca_pmix_ext2x_component_t;

OPAL_DECLSPEC extern mca_pmix_ext2x_component_t mca_pmix_ext2x_component;

OPAL_DECLSPEC extern const opal_pmix_base_module_t opal_pmix_ext2x_module;

/****  INTERNAL OBJECTS  ****/
typedef struct {
    opal_list_item_t super;
    opal_jobid_t jobid;
    char nspace[PMIX_MAX_NSLEN + 1];
} opal_pmix2x_jobid_trkr_t;
OBJ_CLASS_DECLARATION(opal_pmix2x_jobid_trkr_t);

typedef struct {
    opal_list_item_t super;
    size_t index;
    int code;
    opal_pmix_notification_fn_t handler;
} opal_pmix2x_single_event_t;
OBJ_CLASS_DECLARATION(opal_pmix2x_single_event_t);

typedef struct {
    opal_list_item_t super;
    size_t index;
    int *codes;
    size_t ncodes;
    opal_pmix_notification_fn_t handler;
} opal_pmix2x_multi_event_t;
OBJ_CLASS_DECLARATION(opal_pmix2x_multi_event_t);

typedef struct {
    opal_list_item_t super;
    size_t index;
    opal_pmix_notification_fn_t handler;
} opal_pmix2x_default_event_t;
OBJ_CLASS_DECLARATION(opal_pmix2x_default_event_t);

typedef struct {
    opal_list_item_t super;
    int status;
    bool nondefault;
    opal_process_name_t source;
    pmix_data_range_t range;
    opal_list_t *info;
    opal_list_t results;
    opal_pmix2x_single_event_t *sing;
    opal_pmix2x_multi_event_t *multi;
    opal_pmix2x_default_event_t *def;
    opal_pmix_op_cbfunc_t final_cbfunc;
    void *final_cbdata;
} opal_pmix2x_event_chain_t;
OBJ_CLASS_DECLARATION(opal_pmix2x_event_chain_t);

typedef struct {
    opal_object_t super;
    pmix_status_t status;
    pmix_proc_t p;
    pmix_proc_t *procs;
    size_t nprocs;
    pmix_proc_t *error_procs;
    size_t nerror_procs;
    pmix_info_t *info;
    size_t ninfo;
    pmix_app_t *apps;
    size_t sz;
    volatile bool active;
    opal_pmix_op_cbfunc_t opcbfunc;
    opal_pmix_modex_cbfunc_t mdxcbfunc;
    opal_pmix_value_cbfunc_t valcbfunc;
    opal_pmix_lookup_cbfunc_t lkcbfunc;
    opal_pmix_spawn_cbfunc_t spcbfunc;
    void *cbdata;
} pmix2x_opcaddy_t;
OBJ_CLASS_DECLARATION(pmix2x_opcaddy_t);

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
    pmix_info_cbfunc_t infocbfunc;
    pmix_tool_connection_cbfunc_t toolcbfunc;
    void *cbdata;
    opal_pmix_release_cbfunc_t odmdxfunc;
    void *ocbdata;
} pmix2x_opalcaddy_t;
OBJ_CLASS_DECLARATION(pmix2x_opalcaddy_t);

typedef struct {
    opal_object_t super;
    opal_event_t ev;
    volatile bool active;
    size_t id;
    int status;
    opal_process_name_t pname;
    opal_jobid_t jobid;
    const opal_process_name_t *source;
    opal_pmix_data_range_t range;
    bool nondefault;
    size_t handler;
    opal_list_t *event_codes;
    opal_list_t *info;
    opal_pmix_notification_fn_t evhandler;
    opal_pmix_evhandler_reg_cbfunc_t cbfunc;
    opal_pmix_op_cbfunc_t opcbfunc;
    void *cbdata;
} pmix2x_threadshift_t;
OBJ_CLASS_DECLARATION(pmix2x_threadshift_t);

#define OPAL_PMIX_OPCD_THREADSHIFT(i, s, sr, if, nif, fn, cb, cd)   \
    do {                                                    \
        pmix2x_opalcaddy_t *_cd;                            \
        _cd = OBJ_NEW(pmix2x_opalcaddy_t);                  \
        _cd->id = (i);                                      \
        _cd->status = (s);                                  \
        _cd->source = (sr);                                 \
        _cd->info = (i);                                    \
        _cd->evcbfunc = (cb);                               \
        _cd->cbdata = (cd);                                 \
        event_assign(&((_cd)->ev), opal_pmix_base.evbase,   \
                     -1, EV_WRITE, (fn), (_cd));            \
        event_active(&((_cd)->ev), EV_WRITE, 1);            \
    } while(0)

#define OPAL_PMIX_OP_THREADSHIFT(e, fn, cb, cd)             \
    do {                                                    \
        pmix2x_threadshift_t *_cd;                          \
        _cd = OBJ_NEW(pmix2x_threadshift_t);                \
        _cd->handler = (e);                                 \
        _cd->opcbfunc = (cb);                               \
        _cd->cbdata = (cd);                                 \
        event_assign(&((_cd)->ev), opal_pmix_base.evbase,   \
                     -1, EV_WRITE, (fn), (_cd));            \
        event_active(&((_cd)->ev), EV_WRITE, 1);            \
    } while(0)

#define OPAL_PMIX_THREADSHIFT(e, i, eh, fn, cb, cd)         \
    do {                                                    \
        pmix2x_threadshift_t *_cd;                          \
        _cd = OBJ_NEW(pmix2x_threadshift_t);                \
        _cd->event_codes = (e);                             \
        _cd->info = (i);                                    \
        _cd->evhandler = (eh);                              \
        _cd->cbfunc = (cb);                                 \
        _cd->cbdata = (cd);                                 \
        event_assign(&((_cd)->ev), opal_pmix_base.evbase,   \
                     -1, EV_WRITE, (fn), (_cd));            \
        event_active(&((_cd)->ev), EV_WRITE, 1);            \
    } while(0)

#define OPAL_PMIX_NOTIFY_THREADSHIFT(s, sr, r, i, fn, cb, cd)   \
    do {                                                        \
        pmix2x_threadshift_t *_cd;                              \
        _cd = OBJ_NEW(pmix2x_threadshift_t);                    \
        _cd->status = (s);                                      \
        _cd->source = (sr);                                     \
        _cd->range = (r);                                       \
        _cd->info = (i);                                        \
        _cd->opcbfunc = (cb);                                   \
        _cd->cbdata = (cd);                                     \
        event_assign(&((_cd)->ev), opal_pmix_base.evbase,       \
                     -1, EV_WRITE, (fn), (_cd));                \
        event_active(&((_cd)->ev), EV_WRITE, 1);                \
    } while(0)

/****  CLIENT FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC int pmix2x_client_init(opal_list_t *ilist);
OPAL_MODULE_DECLSPEC int pmix2x_client_finalize(void);
OPAL_MODULE_DECLSPEC int pmix2x_initialized(void);
OPAL_MODULE_DECLSPEC int pmix2x_abort(int flag, const char *msg,
                                     opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix2x_commit(void);
OPAL_MODULE_DECLSPEC int pmix2x_fence(opal_list_t *procs, int collect_data);
OPAL_MODULE_DECLSPEC int pmix2x_fencenb(opal_list_t *procs, int collect_data,
                                       opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_put(opal_pmix_scope_t scope,
                                     opal_value_t *val);
OPAL_MODULE_DECLSPEC int pmix2x_get(const opal_process_name_t *proc, const char *key,
                                   opal_list_t *info, opal_value_t **val);
OPAL_MODULE_DECLSPEC int pmix2x_getnb(const opal_process_name_t *proc, const char *key,
                                     opal_list_t *info,
                                     opal_pmix_value_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_publish(opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix2x_publishnb(opal_list_t *info,
                                         opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_lookup(opal_list_t *data, opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix2x_lookupnb(char **keys, opal_list_t *info,
                                        opal_pmix_lookup_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_unpublish(char **keys, opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix2x_unpublishnb(char **keys, opal_list_t *info,
                                           opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_spawn(opal_list_t *job_info, opal_list_t *apps, opal_jobid_t *jobid);
OPAL_MODULE_DECLSPEC int pmix2x_spawnnb(opal_list_t *job_info, opal_list_t *apps,
                                       opal_pmix_spawn_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_connect(opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix2x_connectnb(opal_list_t *procs,
                                         opal_pmix_op_cbfunc_t cbfunc,
                                         void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_disconnect(opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix2x_disconnectnb(opal_list_t *procs,
                                            opal_pmix_op_cbfunc_t cbfunc,
                                            void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_resolve_peers(const char *nodename, opal_jobid_t jobid,
                                             opal_list_t *procs);
OPAL_MODULE_DECLSPEC int pmix2x_resolve_nodes(opal_jobid_t jobid, char **nodelist);

/****  COMMON FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC int pmix2x_store_local(const opal_process_name_t *proc,
                                             opal_value_t *val);

/****  SERVER SOUTHBOUND FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC int pmix2x_server_init(opal_pmix_server_module_t *module,
                                           opal_list_t *info);
OPAL_MODULE_DECLSPEC int pmix2x_server_finalize(void);
OPAL_MODULE_DECLSPEC int pmix2x_server_gen_regex(const char *input, char **regex);
OPAL_MODULE_DECLSPEC int pmix2x_server_gen_ppn(const char *input, char **ppn);
OPAL_MODULE_DECLSPEC int pmix2x_server_register_nspace(opal_jobid_t jobid,
                                                      int nlocalprocs,
                                                      opal_list_t *info,
                                                      opal_pmix_op_cbfunc_t cbfunc,
                                                      void *cbdata);
OPAL_MODULE_DECLSPEC void pmix2x_server_deregister_nspace(opal_jobid_t jobid,
                                                          opal_pmix_op_cbfunc_t cbfunc,
                                                          void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_server_register_client(const opal_process_name_t *proc,
                                                      uid_t uid, gid_t gid,
                                                      void *server_object,
                                                      opal_pmix_op_cbfunc_t cbfunc,
                                                      void *cbdata);
OPAL_MODULE_DECLSPEC void pmix2x_server_deregister_client(const opal_process_name_t *proc,
                                                          opal_pmix_op_cbfunc_t cbfunc,
                                                          void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_server_setup_fork(const opal_process_name_t *proc, char ***env);
OPAL_MODULE_DECLSPEC int pmix2x_server_dmodex(const opal_process_name_t *proc,
                                             opal_pmix_modex_cbfunc_t cbfunc, void *cbdata);
OPAL_MODULE_DECLSPEC int pmix2x_server_notify_event(int status,
                                                    const opal_process_name_t *source,
                                                    opal_list_t *info,
                                                    opal_pmix_op_cbfunc_t cbfunc, void *cbdata);


/****  COMPONENT UTILITY FUNCTIONS  ****/
OPAL_MODULE_DECLSPEC void pmix2x_event_hdlr(size_t evhdlr_registration_id,
                                            pmix_status_t status, const pmix_proc_t *source,
                                            pmix_info_t info[], size_t ninfo,
                                            pmix_info_t results[], size_t nresults,
                                            pmix_event_notification_cbfunc_fn_t cbfunc,
                                            void *cbdata);
OPAL_MODULE_DECLSPEC pmix_status_t pmix2x_convert_opalrc(int rc);
OPAL_MODULE_DECLSPEC int pmix2x_convert_rc(pmix_status_t rc);

OPAL_MODULE_DECLSPEC opal_vpid_t pmix2x_convert_rank(int rank);
OPAL_MODULE_DECLSPEC pmix_rank_t pmix2x_convert_opalrank(opal_vpid_t vpid);

OPAL_MODULE_DECLSPEC opal_pmix_scope_t pmix2x_convert_scope(pmix_scope_t scope);
OPAL_MODULE_DECLSPEC pmix_scope_t pmix2x_convert_opalscope(opal_pmix_scope_t scope);

OPAL_MODULE_DECLSPEC pmix_data_range_t pmix2x_convert_opalrange(opal_pmix_data_range_t range);
OPAL_MODULE_DECLSPEC opal_pmix_data_range_t pmix2x_convert_range(pmix_data_range_t range);

OPAL_MODULE_DECLSPEC opal_pmix_persistence_t pmix2x_convert_persist(pmix_persistence_t scope);
OPAL_MODULE_DECLSPEC pmix_persistence_t pmix2x_convert_opalpersist(opal_pmix_persistence_t scope);

OPAL_MODULE_DECLSPEC void pmix2x_value_load(pmix_value_t *v,
                                            opal_value_t *kv);
OPAL_MODULE_DECLSPEC int pmix2x_value_unload(opal_value_t *kv,
                                            const pmix_value_t *v);

END_C_DECLS

#endif /* MCA_PMIX_EXTERNAL_H */
