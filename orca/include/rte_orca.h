/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#ifndef ORCA_RTE_H
#define ORCA_RTE_H

#include "orca_config.h"

#include "opal/mca/hwloc/hwloc.h"

#include "orca/include/orca/types.h"
#include "orca/include/orca/constants.h"

#include "orca/util/error_strings.h"
#include "orca/mca/stems/stems.h"

BEGIN_C_DECLS


/*******************************************
 * Global Variables
 *******************************************/
/*
 * Indicate if the orca is currently initialized
 */
ORCA_DECLSPEC extern bool orca_initialized;


/*******************************************
 * Initialize / Finalize / Register
 *******************************************/
/*
 * Initialize ORCA
 */
ORCA_DECLSPEC int orca_init(int *pargc, char *** pargv);

/*
 * Register MCA Parameters
 */
ORCA_DECLSPEC int orca_register_params(void);

/*
 * Finalize ORCA
 */
ORCA_DECLSPEC int orca_finalize(void);


/*******************************************
 * Process Name Functions
 *******************************************/
/*
 *
 */
ORCA_DECLSPEC extern orca_process_name_t orca_name_wildcard;
#define ORCA_NAME_WILDCARD      (&orca_name_wildcard)

/*
 *
 */
ORCA_DECLSPEC extern orca_process_name_t orca_name_invalid;
#define ORCA_NAME_INVALID       (&orca_name_invalid)

/*
 * Convert name to a string.
 * Useful for debugging output
 */
#define ORCA_NAME_PRINT(n) orca_util_print_name_args(n)

/*
 *
 */
static inline char* orca_util_print_name_args(const orca_process_name_t *name) {
    return orca_stems.name_print(name);
}

/*
 *
 */
static inline int orca_process_info_get_name_as_string(char **output,
                                                            const orca_process_name_t* name) {
    return orca_stems.name_to_string(output, name);
}

/*
 *
 */
static inline int orca_process_info_convert_string_to_name(orca_process_name_t* name,
                                                                char *output) {
    return orca_stems.name_from_string(name, output);
}

/*
 *
 */
static inline uint64_t orca_process_info_hash_name(const orca_process_name_t* name) {
    return orca_stems.name_hash(name);
}

/* 
 * Return one of the following:
 * OPAL_EQUAL
 * OPAL_VALUE1_GREATER
 * OPAL_VALUE2_GREATER
 */
static inline int orca_process_name_compare(orca_name_cmp_bitmask_t fields,
                                                 const orca_process_name_t* name1,
                                                 const orca_process_name_t* name2) {
    return orca_stems.name_compare(fields, name1, name2);
}


/*******************************************
 * Process Information (Accessors)
 *******************************************/
/*
 *
 */
ORCA_DECLSPEC extern orca_proc_info_t *orca_process_info;

/*
 *
 */
#define ORCA_PROC_MY_NAME (&(orca_process_info->my_name))

/*
 *
 */
static inline orca_vpid_t orca_process_info_get_num_procs(void) {
    return orca_process_info->num_procs;
}

/*
 *
 */
static inline orca_app_idx_t orca_process_info_get_app_num(void) {
    return orca_process_info->app_num;
}

/*
 *
 */
static inline char * orca_process_info_get_nodename(void) {
    return orca_process_info->nodename;
}

/*
 *
 */
static inline pid_t orca_process_info_get_pid(void) {
    return orca_process_info->pid;
}

/*
 *
 */
static inline bool orca_process_info_create_session_dirs(void) {
    return orca_stems.info_create_session_dirs();
}

/*
 *
 */
static inline char * orca_process_info_get_job_session_dir(void) {
    return orca_process_info->job_session_dir;
}

/*
 *
 */
static inline char * orca_process_info_get_process_session_dir(void) {
    return orca_process_info->proc_session_dir;
}

/*
 * Binding
 */
static inline bool orca_process_info_is_bound(void) {
    return orca_stems.info_proc_is_bound();
}

/*
 * Binding
 */
static inline hwloc_cpuset_t orca_process_info_get_applied_binding(void) {
    return orca_stems.info_proc_get_applied_binding();
}



/*******************************************
 * General Information Information
 *******************************************/
/*
 * Debugger
 */
static inline bool orca_info_in_parallel_debugger(void) {
    return orca_stems.info_in_parallel_debugger();
}

/*
 * Debugger
 */
static inline bool orca_info_standalone_operation(void) {
    return orca_stems.info_standalone_operation();
}

/*
 * Checkpoint/Restart
 */
static inline bool orca_info_cr_continue_like_restart(void) {
    return orca_stems.info_cr_continue_like_restart();
}

/*
 * ???
 */
static inline char * orca_info_job_identifier(void) {
    return orca_stems.info_job_identifier();
}


/*******************************************
 * Out-Of-Band Communication
 *******************************************/
/*
 *
 */
static inline int orca_oob_send_buffer(struct orca_process_name_t* peer,
                                            struct opal_buffer_t* buffer,
                                            orca_oob_tag_t tag,
                                            int flags) {
    return orca_stems.oob_send_buffer(peer, buffer, tag, flags);
}

/*
 *
 */
static inline int orca_oob_send_buffer_nb(struct orca_process_name_t* peer,
                                               struct opal_buffer_t* buffer,
                                               orca_oob_tag_t tag,
                                               int flags,
                                               orca_oob_buffer_callback_fn_t cbfunc,
                                               void* cbdata) {
    return orca_stems.oob_send_buffer_nb(peer, buffer, tag, flags, cbfunc, cbdata);
}

/*
 *
 */
static inline int orca_oob_recv_buffer(struct orca_process_name_t* peer,
                                            struct opal_buffer_t *buffer,
                                            orca_oob_tag_t tag,
                                            int flags) {
    return orca_stems.oob_recv_buffer(peer, buffer, tag, flags);
}

/*
 *
 */
static inline int orca_oob_recv_buffer_nb(struct orca_process_name_t* peer,
                                               orca_oob_tag_t tag,
                                               int flags,
                                               orca_oob_buffer_callback_fn_t cbfunc,
                                               void* cbdata) {
    return orca_stems.oob_recv_buffer_nb(peer, tag, flags, cbfunc, cbdata);
}

/*
 *
 */
static inline int orca_oob_recv_cancel(orca_process_name_t* peer,
                                            orca_oob_tag_t tag) {
    return orca_stems.oob_recv_cancel(peer, tag);
}

/*
 *
 */
static inline int orca_oob_parse_uris(const char* uri,
                                           orca_process_name_t* peer,
                                           char ***uris ) {
    return orca_stems.oob_parse_uris(uri, peer, uris);
}

/*******************************************
 * Remote Process Information
 *******************************************/
/*
 *
 */
static inline opal_hwloc_locality_t orca_process_get_locality(orca_process_name_t* name) {
    return orca_stems.proc_get_locality(name);
}

/*
 *
 */
static inline char * orca_process_get_hostname(orca_process_name_t* name) {
    return orca_stems.proc_get_hostname(name);
}

/*
 *
 */
static inline orca_node_rank_t orca_node_info_get_rank(const orca_process_name_t* name) {
    return orca_stems.proc_get_node_rank(name);
}


/*******************************************
 * Error Manager
 *******************************************/
/*
 *
 */
#define ORCA_ERROR_LOG(rc) \
    orca_error_mgr_log(rc, __FILE__, __LINE__);

/*
 *
 */
static inline void orca_error_mgr_log(int error_code,
                                           char *filename,
                                           int line) {
    orca_stems.error_log(error_code, filename, line);
}

/*
 *
 */
static inline orca_error_mgr_fault_callback_t *
orca_error_mgr_set_callback(orca_error_mgr_fault_callback_t *cbfunc) {
    return orca_stems.error_set_callback(cbfunc);
}

/*
 *
 */
static inline void orca_error_mgr_abort(int error_code,
                                             char *fmt, ...) {
    va_list arglist;
    va_start(arglist, fmt);
    orca_stems.error_abort(error_code, fmt, arglist);
    va_end(arglist);
}

/*
 *
 */
static inline int orca_error_mgr_abort_peers(orca_process_name_t *procs,
                                                  orca_std_cntr_t num_procs) {
    return orca_stems.error_abort_peers(procs, num_procs);
}


/*******************************************
 * Collectives
 *******************************************/
/*
 *
 */
static inline int orca_coll_modex(void) {
    return orca_stems.coll_modex();
}

/*
 *
 */
static inline int orca_coll_barrier(orca_coll_type_t type) {
    return orca_stems.coll_barrier(type);
}

/*
 *
 */
static inline int orca_coll_set_process_attribute(const char* attr_name, 
                                                       const void *buffer,
                                                       size_t size) {
    return orca_stems.coll_set_attr(attr_name, buffer, size);
}

/*
 *
 */
static inline int orca_coll_get_process_attribute(orca_process_name_t *name,
                                                       const char* attr_name,
                                                       void **buffer,
                                                       size_t *size) {
    return orca_stems.coll_get_attr(name, attr_name, buffer, size);
}


/*******************************************
 * Notifier / Show Help
 *******************************************/
/*
 *
 */
static inline int orca_show_help(const char *filename,
                                      const char *topic,
                                      bool want_error_header, ...) {
    va_list arglist;
    int rc;
    va_start(arglist, want_error_header);
    rc = orca_stems.notifier_show_help(filename, topic, want_error_header, arglist);
    va_end(arglist);
    return rc;
}

/*
 *
 */
static inline bool orca_show_help_is_available(void) {
    return orca_stems.notifier_show_help_is_available();
}

/*
 *
 */
static inline bool orca_show_help_want_aggregate(void) {
    return orca_stems.notifier_show_help_want_aggregate();
}

/*******************************************
 * ...
 *******************************************/

END_C_DECLS

#endif /* ORCA_RTE_H */
