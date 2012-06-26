/*
 * Copyright (c) 2012      Oak Ridge National Labs.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/**
 * @file
 *
 */

#ifndef MCA_STEMS_H
#define MCA_STEMS_H

#include "orca_config.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/base.h"
#include "opal/class/opal_object.h"
#include "opal/class/opal_pointer_array.h"
#include "opal/mca/hwloc/hwloc.h"

#include "orca/include/orca/types.h"
#include "orca/include/orca/constants.h"

BEGIN_C_DECLS


/**
 * Module initialization function.
 */
typedef int (*orca_stems_base_module_init_fn_t)
    (int *pargc, char *** pargv);

/**
 * Module finalization function.
 */
typedef int (*orca_stems_base_module_finalize_fn_t)
     (void);


/*******************************************
 * Process Name Functions
 *******************************************/
typedef char* (*orca_stems_base_module_name_print_fn_t)
    (const orca_process_name_t *name);

typedef int (*orca_stems_base_module_name_to_string_fn_t)
    (char **output,
     const orca_process_name_t* name);

typedef int (*orca_stems_base_module_name_from_string_fn_t)
    (orca_process_name_t* name,
     char *output);

typedef uint64_t (*orca_stems_base_module_name_hash_fn_t)
    (const orca_process_name_t* name);

typedef uint64_t (*orca_stems_base_module_name_compare_fn_t)
    (orca_name_cmp_bitmask_t fields,
     const orca_process_name_t* name1,
     const orca_process_name_t* name2);


/*******************************************
 * Process Information
 *******************************************/
typedef bool (*orca_stems_base_module_info_proc_is_bound_fn_t)
    (void);

typedef hwloc_cpuset_t (*orca_stems_base_module_info_proc_get_applied_binding_fn_t)
    (void);


/*******************************************
 * General Information Functions
 *******************************************/
typedef bool (*orca_stems_base_module_info_create_session_dirs_fn_t)
    (void);

typedef bool (*orca_stems_base_module_info_in_parallel_debugger_fn_t)
    (void);

typedef bool (*orca_stems_base_module_info_standalone_operation_fn_t)
    (void);

typedef bool (*orca_stems_base_module_info_cr_continue_like_restart_fn_t)
    (void);

typedef char * (*orca_stems_base_module_info_job_identifier_fn_t)
    (void);

/*******************************************
 * Out-Of-Band Communication
 *******************************************/
typedef void (*orca_oob_buffer_callback_fn_t)
    (int status,
     struct orca_process_name_t* peer,
     struct opal_buffer_t* buffer,
     orca_oob_tag_t tag,
     void* cbdata);

typedef int (*orca_stems_base_module_oob_send_buffer_fn_t)
    (struct orca_process_name_t* peer,
     struct opal_buffer_t* buffer,
     orca_oob_tag_t tag,
     int flags);

typedef int (*orca_stems_base_module_oob_send_buffer_nb_fn_t)
    (struct orca_process_name_t* peer,
     struct opal_buffer_t* buffer,
     orca_oob_tag_t tag,
     int flags,
     orca_oob_buffer_callback_fn_t cbfunc,
     void* cbdata);

typedef int (*orca_stems_base_module_oob_recv_buffer_fn_t)
    (struct orca_process_name_t* peer,
     struct opal_buffer_t *buffer,
     orca_oob_tag_t tag,
     int flags);

typedef int (*orca_stems_base_module_oob_recv_buffer_nb_fn_t)
    (struct orca_process_name_t* peer,
     orca_oob_tag_t tag,
     int flags,
     orca_oob_buffer_callback_fn_t cbfunc,
     void* cbdata);

typedef int (*orca_stems_base_module_oob_recv_cancel_fn_t)
    (orca_process_name_t* peer,
     orca_oob_tag_t tag);

typedef int (*orca_stems_base_module_oob_parse_uris_fn_t)
    (const char* uri,
     orca_process_name_t* peer,
     char ***uris );


/*******************************************
 * Remote Process Information
 *******************************************/
typedef opal_hwloc_locality_t (*orca_stems_base_module_proc_get_locality_fn_t)
    (orca_process_name_t* name);

typedef char * (*orca_stems_base_module_proc_get_hostname_fn_t)
    (orca_process_name_t* name);

typedef orca_node_rank_t (*orca_stems_base_module_proc_get_node_rank_fn_t)
    (const orca_process_name_t* name);


/*******************************************
 * Error Manager
 *******************************************/
typedef void (orca_error_mgr_fault_callback_t)
    (opal_pointer_array_t *procs);

typedef void (*orca_stems_base_module_error_log_fn_t)
    (int error_code, char *filename, int line);

typedef orca_error_mgr_fault_callback_t * (*orca_stems_base_module_error_set_callback_fn_t)
    (orca_error_mgr_fault_callback_t *cbfunc);

typedef void (*orca_stems_base_module_error_abort_fn_t)
    (int error_code,
     char *fmt,
     va_list arglist);

typedef int (*orca_stems_base_module_error_abort_peers_fn_t)
    (orca_process_name_t *procs,
     orca_std_cntr_t num_procs);

/*******************************************
 * Collectives
 *******************************************/
typedef int (*orca_stems_base_module_coll_modex_fn_t)
    (void);

typedef int (*orca_stems_base_module_coll_barrier_fn_t)
    (orca_coll_type_t type);

typedef int (*orca_stems_base_module_coll_set_attribute_fn_t)
    (const char* attr_name,
     const void *buffer,
     size_t size);

typedef int (*orca_stems_base_module_coll_get_attribute_fn_t)
    (orca_process_name_t *name,
     const char* attr_name,
     void **buffer,
     size_t *size);

/*******************************************
 * Notifier / Show Help
 *******************************************/
typedef int (*orca_stems_base_module_notifier_show_help_fn_t)
    (const char *filename,
     const char *topic, 
     bool want_error_header,
     va_list arglist);

typedef bool (*orca_stems_base_module_notifier_show_help_avail_fn_t)
    (void);

typedef bool (*orca_stems_base_module_notifier_show_help_want_aggregate_fn_t)
    (void);


/**
 * Structure for STEMS components.
 */
struct orca_stems_base_component_2_0_0_t {
    /** MCA base component */
    mca_base_component_t base_version;
    /** MCA base data */
    mca_base_component_data_t base_data;

    /** Verbosity Level */
    int verbose;
    /** Output Handle for opal_output */
    int output_handle;
    /** Default Priority */
    int priority;
};
typedef struct orca_stems_base_component_2_0_0_t orca_stems_base_component_2_0_0_t;
typedef struct orca_stems_base_component_2_0_0_t orca_stems_base_component_t;


/**
 * Structure for STEMS modules
 */
struct orca_stems_base_module_1_0_0_t {
    /** Initialization Function */
    orca_stems_base_module_init_fn_t           stems_init;
    /** Finalization Function */
    orca_stems_base_module_finalize_fn_t       stems_finalize;

    /** Process Name Functions */
    orca_stems_base_module_name_print_fn_t         name_print;
    orca_stems_base_module_name_to_string_fn_t     name_to_string;
    orca_stems_base_module_name_from_string_fn_t   name_from_string;
    orca_stems_base_module_name_hash_fn_t          name_hash;
    orca_stems_base_module_name_compare_fn_t       name_compare;

    /** Process Information */
    orca_stems_base_module_info_proc_is_bound_fn_t            info_proc_is_bound;
    orca_stems_base_module_info_proc_get_applied_binding_fn_t info_proc_get_applied_binding;

    /** General Information Functions */
    orca_stems_base_module_info_create_session_dirs_fn_t      info_create_session_dirs;
    orca_stems_base_module_info_in_parallel_debugger_fn_t     info_in_parallel_debugger;
    orca_stems_base_module_info_standalone_operation_fn_t     info_standalone_operation;
    orca_stems_base_module_info_cr_continue_like_restart_fn_t info_cr_continue_like_restart;
    orca_stems_base_module_info_job_identifier_fn_t           info_job_identifier;

    /** Out-Of-Band Communication */
    orca_stems_base_module_oob_send_buffer_fn_t    oob_send_buffer;
    orca_stems_base_module_oob_send_buffer_nb_fn_t oob_send_buffer_nb;
    orca_stems_base_module_oob_recv_buffer_fn_t    oob_recv_buffer;
    orca_stems_base_module_oob_recv_buffer_nb_fn_t oob_recv_buffer_nb;
    orca_stems_base_module_oob_recv_cancel_fn_t    oob_recv_cancel;
    orca_stems_base_module_oob_parse_uris_fn_t     oob_parse_uris;

    /** Remote Process Information */
    orca_stems_base_module_proc_get_locality_fn_t   proc_get_locality;
    orca_stems_base_module_proc_get_hostname_fn_t   proc_get_hostname;
    orca_stems_base_module_proc_get_node_rank_fn_t  proc_get_node_rank;

    /** Error Manager */
    orca_stems_base_module_error_log_fn_t          error_log;
    orca_stems_base_module_error_set_callback_fn_t error_set_callback;
    orca_stems_base_module_error_abort_fn_t        error_abort;
    orca_stems_base_module_error_abort_peers_fn_t  error_abort_peers;

    /** Collectives */
    orca_stems_base_module_coll_modex_fn_t         coll_modex;
    orca_stems_base_module_coll_barrier_fn_t       coll_barrier;
    orca_stems_base_module_coll_set_attribute_fn_t coll_set_attr;
    orca_stems_base_module_coll_get_attribute_fn_t coll_get_attr;

    /** Notifier / Show Help */
    orca_stems_base_module_notifier_show_help_fn_t       notifier_show_help;
    orca_stems_base_module_notifier_show_help_avail_fn_t notifier_show_help_is_available;
    orca_stems_base_module_notifier_show_help_want_aggregate_fn_t notifier_show_help_want_aggregate;
};
typedef struct orca_stems_base_module_1_0_0_t orca_stems_base_module_1_0_0_t;
typedef struct orca_stems_base_module_1_0_0_t orca_stems_base_module_t;

ORCA_DECLSPEC extern orca_stems_base_module_t orca_stems;


/**
 * Macro for use in components that are of type STEMS
 */
#define ORCA_STEMS_BASE_VERSION_1_0_0 \
    MCA_BASE_VERSION_2_0_0, \
    "stems", 1, 0, 0

END_C_DECLS

#endif /* ORCA_STEMS_H */

