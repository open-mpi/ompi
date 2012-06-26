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

#include "stems_none.h"

int orca_stems_none_module_init(int *pargc, char *** pargv)
{
    
    orca_process_info = (orca_proc_info_t*)malloc(sizeof(orca_proc_info_t));

    return ORCA_SUCCESS;
}

int orca_stems_none_module_finalize(void)
{
    if( NULL != orca_process_info ) {
        free(orca_process_info);
        orca_process_info = NULL;
    }

    return ORCA_SUCCESS;
}


/*******************************************
 * Process Name Functions
 *******************************************/
char* orca_stems_none_name_print(const orca_process_name_t *name) {
    return NULL;
}

int orca_stems_none_name_to_string(char **output, const orca_process_name_t* name) {
    return ORCA_SUCCESS;
}

int orca_stems_none_name_from_string(orca_process_name_t* name, char *output) {
    return ORCA_SUCCESS;
}

uint64_t orca_stems_none_name_hash(const orca_process_name_t* name) {
    return 0;
}

uint64_t orca_stems_none_name_compare(orca_name_cmp_bitmask_t fields,
                                           const orca_process_name_t* name1,
                                           const orca_process_name_t* name2) {
    /*
     * OPAL_EQUAL
     * OPAL_VALUE1_GREATER
     * OPAL_VALUE2_GREATER
     */
    return ORCA_SUCCESS;
}


/*******************************************
 * Proces Information Functions
 *******************************************/
bool orca_stems_none_info_proc_is_bound(void) {
    return false;
}

hwloc_cpuset_t orca_stems_none_info_proc_get_applied_binding(void) {
    return NULL;
}


/*******************************************
 * General Information Functions
 *******************************************/
bool orca_stems_none_info_create_session_dirs(void) {
    return false;
}

bool orca_stems_none_info_in_parallel_debugger(void) {
    return false;
}

bool orca_stems_none_info_standalone_operation(void) {
    return false;
}

bool orca_stems_none_info_cr_continue_like_restart(void) {
    return false;
}

char * orca_stems_none_info_job_identifier(void) {
    return NULL;
}


/*******************************************
 * Out-Of-Band Communication
 *******************************************/
int orca_stems_none_oob_send_buffer(struct orca_process_name_t* peer,
                                         struct opal_buffer_t* buffer,
                                         orca_oob_tag_t tag,
                                         int flags) {
    return ORCA_SUCCESS;
}

int orca_stems_none_oob_send_buffer_nb(struct orca_process_name_t* peer,
                                            struct opal_buffer_t* buffer,
                                            orca_oob_tag_t tag,
                                            int flags,
                                            orca_oob_buffer_callback_fn_t cbfunc,
                                            void* cbdata) {
    return ORCA_SUCCESS;
}

int orca_stems_none_oob_recv_buffer(struct orca_process_name_t* peer,
                                         struct opal_buffer_t *buffer,
                                         orca_oob_tag_t tag,
                                         int flags) {
    return ORCA_SUCCESS;
}

int orca_stems_none_oob_recv_buffer_nb(struct orca_process_name_t* peer,
                                            orca_oob_tag_t tag,
                                            int flags,
                                            orca_oob_buffer_callback_fn_t cbfunc,
                                            void* cbdata) {
    return ORCA_SUCCESS;
}

int orca_stems_none_oob_recv_cancel(orca_process_name_t* peer,
                                         orca_oob_tag_t tag) {
    return ORCA_SUCCESS;
}

int orca_stems_none_oob_parse_uris(const char* uri,
                                        orca_process_name_t* peer,
                                        char ***uris ) {
    return ORCA_SUCCESS;
}


/*******************************************
 * Remote Process Information
 *******************************************/
opal_hwloc_locality_t orca_stems_none_proc_get_locality(orca_process_name_t* name) {
    return (OPAL_PROC_ON_CU | OPAL_PROC_ON_CLUSTER);
}

char * orca_stems_none_proc_get_hostname(orca_process_name_t* name) {
    return NULL;
}

orca_node_rank_t orca_stems_none_proc_get_node_rank(const orca_process_name_t* name) {
    return ORCA_NODE_RANK_INVALID;
}


/*******************************************
 * Error Manager
 *******************************************/
void orca_stems_none_error_log(int error_code, char *filename, int line) {
    return;
}

orca_error_mgr_fault_callback_t * orca_stems_none_error_set_callback(orca_error_mgr_fault_callback_t *cbfunc) {
    return ORCA_SUCCESS;
}

void orca_stems_none_error_abort(int error_code, char *fmt, va_list arglist) {
    return;
}

int orca_stems_none_error_abort_peers(orca_process_name_t *procs,
                                           orca_std_cntr_t num_procs) {
    return ORCA_SUCCESS;
}


/*******************************************
 * Collectives
 *******************************************/
int orca_stems_none_coll_modex(void) {
    return ORCA_SUCCESS;
}

int orca_stems_none_coll_barrier(orca_coll_type_t type) {
    return ORCA_SUCCESS;
}

int orca_stems_none_coll_set_attribute(const char* attr_name,
                                            const void *buffer,
                                            size_t size) {
    return ORCA_SUCCESS;
}

int orca_stems_none_coll_get_attribute(orca_process_name_t *name,
                                            const char* attr_name,
                                            void **buffer,
                                            size_t *size) {
    return ORCA_SUCCESS;
}


/*******************************************
 * Notifier / Show Help
 *******************************************/
int orca_stems_none_notifier_show_help(const char *filename,
                                            const char *topic, 
                                            bool want_error_header,
                                            va_list arglist)
{
    return orca_stems_base_notifier_show_help(filename,
                                                   topic,
                                                   want_error_header,
                                                   arglist);
}

bool orca_stems_none_notifier_show_help_avail(void) {
    return false;
}

bool orca_stems_none_notifier_show_help_want_aggregate(void) {
    return false;
}
