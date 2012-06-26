/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "orca_config.h"

#include "orca/constants.h"
#include "orca/include/rte_orca.h"
#include "opal/mca/base/mca_base_param.h"

#include "orca/mca/stems/stems.h"
#include "orca/mca/stems/base/base.h"

#include "orte/include/orte/constants.h"
#include "orte/runtime/runtime.h"
#include "orte/util/name_fns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "orte/mca/ess/ess.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/grpcomm/grpcomm.h"
#if ORCA_WITH_FULL_ORTE_SUPPORT
#include "orte/runtime/orte_cr.h"
#endif /* ORCA_WITH_FULL_ORTE_SUPPORT */
#include "orte/util/show_help.h"

#include "stems_orte.h"


/* Callabck translation operations */
struct stems_orte_translate_callback_t {
    opal_object_t super;

    orca_oob_buffer_callback_fn_t p_cbfunc;
    void *p_cbdata;
    bool  p_persistent;
};
typedef struct stems_orte_translate_callback_t stems_orte_translate_callback_t;

/* OBJ_CLASS_DECLARATION(stems_orte_translate_callback_t);*/
static void stems_orte_translate_callback_construct(stems_orte_translate_callback_t *tcb);
static void stems_orte_translate_callback_destruct( stems_orte_translate_callback_t *tcb);

OBJ_CLASS_INSTANCE(stems_orte_translate_callback_t,
                   opal_object_t,
                   stems_orte_translate_callback_construct,
                   stems_orte_translate_callback_destruct);

static void stems_orte_translate_callback(int status,
                                          struct orte_process_name_t* peer,
                                          struct opal_buffer_t* buffer,
                                          orte_rml_tag_t tag,
                                          void* cbdata);
    
static stems_orte_translate_callback_t *
stems_orte_translate_register(orca_oob_buffer_callback_fn_t cbfunc,
                              void *cbdata,
                              bool persistent);


/*******************************************
 * Initialization / Finalization
 *******************************************/
int orca_stems_orte_module_init(int *pargc, char *** pargv)
{
    int ret;

    /*
     * Setup ORTE - note that we are an MPI process
     */
    if(ORTE_SUCCESS != (ret = orte_init(NULL, NULL, ORTE_PROC_MPI)) ) {
        opal_output(0, "Error: orte_init() failed (%d)", ret);
        return ret;
    }

    /*
     * Make sure we point to the process_info object in ORTE
     */
    orca_process_info = (orca_proc_info_t*)malloc(sizeof(orca_proc_info_t));
    orca_process_info->my_name.jobid    = orte_process_info.my_name.jobid;
    orca_process_info->my_name.vpid     = orte_process_info.my_name.vpid;
    orca_process_info->app_num          = orte_process_info.app_num;
    orca_process_info->num_procs        = orte_process_info.num_procs;
    orca_process_info->nodename         = strdup(orte_process_info.nodename);
    orca_process_info->pid              = orte_process_info.pid;
    orca_process_info->job_session_dir  = strdup(orte_process_info.job_session_dir);
    orca_process_info->proc_session_dir = strdup(orte_process_info.proc_session_dir);

    return ORCA_SUCCESS;
}

int orca_stems_orte_module_finalize(void)
{
    int ret;

    /*
     * Finalize ORTE
     */
    if(ORTE_SUCCESS != (ret = orte_finalize()) ) {
        return ret;
    }

    /*
     * Disconnect from the process_info object in ORTE
     */
    if( NULL != orca_process_info ) {
        free(orca_process_info->nodename);
        free(orca_process_info->job_session_dir);
        free(orca_process_info);
        orca_process_info = NULL;
    }

    return ORCA_SUCCESS;
}


/*******************************************
 * Process Name Functions
 *******************************************/
char* orca_stems_orte_name_print(const orca_process_name_t *name) {
    return orte_util_print_name_args((orte_process_name_t*)name);
}

int orca_stems_orte_name_to_string(char **output, const orca_process_name_t* name) {
    return orte_util_convert_process_name_to_string(output, (orte_process_name_t*)name);
}

int orca_stems_orte_name_from_string(orca_process_name_t* name, char *output) {
    return orte_util_convert_string_to_process_name((orte_process_name_t*)name, output);
}

uint64_t orca_stems_orte_name_hash(const orca_process_name_t* name) {
    return orte_util_hash_name((orte_process_name_t*)name);
}

uint64_t orca_stems_orte_name_compare(orca_name_cmp_bitmask_t fields,
                                           const orca_process_name_t* name1,
                                           const orca_process_name_t* name2) {
    return orte_util_compare_name_fields((orte_ns_cmp_bitmask_t)fields,
                                         (orte_process_name_t*)name1,
                                         (orte_process_name_t*)name2);
}


/*******************************************
 * Process Information Functions
 *******************************************/
bool orca_stems_orte_info_proc_is_bound(void) {
    return orte_proc_is_bound;
}

hwloc_cpuset_t orca_stems_orte_info_proc_get_applied_binding(void) {
    return orte_proc_applied_binding;
}


/*******************************************
 * General Information Functions
 *******************************************/
bool orca_stems_orte_info_create_session_dirs(void) {
    return orte_create_session_dirs;
}

bool orca_stems_orte_info_in_parallel_debugger(void) {
    return orte_in_parallel_debugger;
}

bool orca_stems_orte_info_standalone_operation(void) {
    return orte_standalone_operation;
}

bool orca_stems_orte_info_cr_continue_like_restart(void) {
    return orte_cr_continue_like_restart;
}

char * orca_stems_orte_info_job_identifier(void) {
    return orte_job_ident;
}


/*******************************************
 * Out-Of-Band Communication
 *******************************************/
int orca_stems_orte_oob_send_buffer(struct orca_process_name_t* peer,
                                         struct opal_buffer_t* buffer,
                                         orca_oob_tag_t tag,
                                         int flags) {
    return orte_rml.send_buffer((orte_process_name_t*)peer,
                                buffer, tag, flags);
}

int orca_stems_orte_oob_send_buffer_nb(struct orca_process_name_t* peer,
                                            struct opal_buffer_t* buffer,
                                            orca_oob_tag_t tag,
                                            int flags,
                                            orca_oob_buffer_callback_fn_t cbfunc,
                                            void* cbdata) {
    stems_orte_translate_callback_t *tcb = NULL;

    tcb = stems_orte_translate_register(cbfunc, cbdata,
                                        (flags & ORCA_OOB_PERSISTENT ? true : false) );

    return orte_rml.send_buffer_nb((orte_process_name_t*)peer,
                                   buffer, tag, flags,
                                   stems_orte_translate_callback,
                                   tcb);
}

int orca_stems_orte_oob_recv_buffer(struct orca_process_name_t* peer,
                                         struct opal_buffer_t *buffer,
                                         orca_oob_tag_t tag,
                                         int flags) {
    return orte_rml.recv_buffer((orte_process_name_t*)peer,
                                buffer, tag, flags);
}

int orca_stems_orte_oob_recv_buffer_nb(struct orca_process_name_t* peer,
                                            orca_oob_tag_t tag,
                                            int flags,
                                            orca_oob_buffer_callback_fn_t cbfunc,
                                            void* cbdata) {
    stems_orte_translate_callback_t *tcb = NULL;

    tcb = stems_orte_translate_register(cbfunc, cbdata,
                                        (flags & ORCA_OOB_PERSISTENT ? true : false) );

    return orte_rml.recv_buffer_nb((orte_process_name_t*)peer,
                                   tag, flags,
                                   stems_orte_translate_callback,
                                   tcb);
}

int orca_stems_orte_oob_recv_cancel(orca_process_name_t* peer,
                                         orca_oob_tag_t tag) {
    return orte_rml.recv_cancel((orte_process_name_t*)peer,
                                tag);
}

int orca_stems_orte_oob_parse_uris(const char* uri,
                                        orca_process_name_t* peer,
                                        char ***uris )
{
    orte_process_name_t loc_name;
    int ret;

    loc_name.jobid = orca_process_info_get_jobid(peer);
    loc_name.vpid  = orca_process_info_get_vpid(peer);

    ret = orte_rml_base_parse_uris(uri, &loc_name, uris);

    orca_process_info_set_jobid(peer, (orca_jobid_t)(loc_name.jobid));
    orca_process_info_set_vpid(peer, (orca_vpid_t)(loc_name.vpid));

    return ret;
}

/*******************************************
 * Remote Process Information
 *******************************************/
opal_hwloc_locality_t orca_stems_orte_proc_get_locality(orca_process_name_t* name) {
    return orte_ess.proc_get_locality((orte_process_name_t*)name);
}

char * orca_stems_orte_proc_get_hostname(orca_process_name_t* name) {
    return orte_ess.proc_get_hostname((orte_process_name_t*)name);
}

orca_node_rank_t orca_stems_orte_proc_get_node_rank(const orca_process_name_t* name) {
    return orte_ess.get_node_rank((orte_process_name_t*)name);
}


/*******************************************
 * Error Manager
 *******************************************/
void orca_stems_orte_error_log(int error_code, char *filename, int line) {
    orte_errmgr.log(error_code, filename, line);
}

orca_error_mgr_fault_callback_t *
orca_stems_orte_error_set_callback(orca_error_mgr_fault_callback_t *cbfunc) {
    if( NULL != orte_errmgr.set_fault_callback ) {
        return orte_errmgr.set_fault_callback(cbfunc);
    } else {
        return NULL;
    }
}

void orca_stems_orte_error_abort(int error_code, char *fmt, va_list arglist) {
    orte_errmgr.vabort(error_code, fmt, arglist);
}

int orca_stems_orte_error_abort_peers(orca_process_name_t *procs,
                                           orca_std_cntr_t num_procs) {
    return orte_errmgr.abort_peers((orte_process_name_t*)procs,
                                   num_procs);
}


/*******************************************
 * Collectives
 *******************************************/
int orca_stems_orte_coll_modex(void) {
    int ret;
    orte_grpcomm_collective_t *coll = NULL;

    coll = OBJ_NEW(orte_grpcomm_collective_t);
    coll->id = orte_process_info.peer_modex;
    if (ORTE_SUCCESS != (ret = orte_grpcomm.modex(coll))) {
        OBJ_RELEASE(coll);
        return ret;
    }

    /* wait for modex to complete. */
    while (coll->active) {
        opal_progress();  /* block in progress pending events */
    }
    OBJ_RELEASE(coll);

    return ORCA_SUCCESS;
}

int orca_stems_orte_coll_barrier(orca_coll_type_t type) {
    int ret;
    orte_grpcomm_collective_t *coll = NULL;

    coll = OBJ_NEW(orte_grpcomm_collective_t);

    if( ORCA_COLL_TYPE_BARRIER_INIT == type ) {
        coll->id = orte_process_info.peer_init_barrier;
    }
    else if( ORCA_COLL_TYPE_BARRIER_FINALIZE == type ) {
        coll->id = orte_process_info.peer_fini_barrier;
    }
    else if( ORCA_COLL_TYPE_BARRIER_CR == type ) {
        /* TODO: Double check this */
        coll->id = -1;
    }
    else {
        return ORCA_ERROR;
    }

    if (ORTE_SUCCESS != (ret = orte_grpcomm.barrier(coll))) {
        OBJ_RELEASE(coll);
        return ret;
    }

    /* wait for barrier to complete */
    while (coll->active) {
        opal_progress();  /* block in progress pending events */
    }
    OBJ_RELEASE(coll);

    return ORCA_SUCCESS;
}

int orca_stems_orte_coll_set_attribute(const char* attr_name,
                                            const void *buffer,
                                            size_t size) {
    return orte_grpcomm.set_proc_attr(attr_name, buffer, size);
}

int orca_stems_orte_coll_get_attribute(orca_process_name_t *name,
                                            const char* attr_name,
                                            void **buffer,
                                            size_t *size) {
    orte_process_name_t loc_name;
    int ret;

    loc_name.jobid = orca_process_info_get_jobid(name);
    loc_name.vpid  = orca_process_info_get_vpid(name);

    ret = orte_grpcomm.get_proc_attr(&loc_name,
                                     attr_name, buffer, size);

    orca_process_info_set_jobid(name, (orca_jobid_t)(loc_name.jobid));
    orca_process_info_set_vpid(name, (orca_vpid_t)(loc_name.vpid));

    return ret;
}


/*******************************************
 * Notifier / Show Help
 *******************************************/
int orca_stems_orte_notifier_show_help(const char *filename,
                                            const char *topic, 
                                            bool want_error_header,
                                            va_list arglist) {
    return orte_show_helpv(filename, topic, want_error_header, arglist);
}

bool orca_stems_orte_notifier_show_help_avail(void) {
    return orte_show_help_is_available();
}

bool orca_stems_orte_notifier_show_help_want_aggregate(void) {
    return orte_help_want_aggregate;
}

/*******************************************
 * Callabck translation operations
 *******************************************/
static void stems_orte_translate_callback_construct(stems_orte_translate_callback_t *tcb)
{
    tcb->p_cbdata = NULL;
    tcb->p_persistent = false;
    return;
}

static void stems_orte_translate_callback_destruct(stems_orte_translate_callback_t *tcb)
{
    tcb->p_cbdata = NULL;
    tcb->p_persistent = false;
    return;
}

static void stems_orte_translate_callback(int status,
                                          struct orte_process_name_t* peer,
                                          struct opal_buffer_t* buffer,
                                          orte_rml_tag_t tag,
                                          void* cbdata)
{
    stems_orte_translate_callback_t *tcb = (stems_orte_translate_callback_t*)cbdata;

    /* Call the callback */
    tcb->p_cbfunc(status,
                  (orca_process_name_t*)peer,
                  buffer,
                  (orca_oob_tag_t)tag,
                  tcb->p_cbdata);

    /* If ! persistent : remove entry in table */
    if( !(tcb->p_persistent) ) {
        OBJ_DESTRUCT(tcb);
    }
}

static stems_orte_translate_callback_t *
stems_orte_translate_register(orca_oob_buffer_callback_fn_t cbfunc,
                              void *cbdata,
                              bool persistent)
{
    stems_orte_translate_callback_t *tcb;

    tcb = OBJ_NEW(stems_orte_translate_callback_t);
    tcb->p_cbfunc = cbfunc;
    tcb->p_cbdata = cbdata;
    tcb->p_persistent = persistent;

    return tcb;
}
