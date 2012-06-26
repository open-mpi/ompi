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
 * ORTE STEMS component
 *
 * Simple, braindead implementation.
 */

#ifndef MCA_STEMS_ORTE_EXPORT_H
#define MCA_STEMS_ORTE_EXPORT_H

#include "orca_config.h"


#include "opal/mca/mca.h"
#include "orca/mca/stems/stems.h"

BEGIN_C_DECLS

/*
 * Local Component structures
 */
struct orca_stems_orte_component_t {
    orca_stems_base_component_t super;  /** Base STEMS component */
};
typedef struct orca_stems_orte_component_t orca_stems_orte_component_t;
ORCA_MODULE_DECLSPEC extern orca_stems_orte_component_t mca_stems_orte_component;

int orca_stems_orte_component_query(mca_base_module_t **module, int *priority);

/*
 * Module functions
 */
int orca_stems_orte_module_init(int *pargc, char *** pargv);
int orca_stems_orte_module_finalize(void);

/*
 * Process Name Functions
 */
char* orca_stems_orte_name_print(const orca_process_name_t *name);
int orca_stems_orte_name_to_string(char **output,
                                        const orca_process_name_t* name);
int orca_stems_orte_name_from_string(orca_process_name_t* name,
                                          char *output);
uint64_t orca_stems_orte_name_hash(const orca_process_name_t* name);
uint64_t orca_stems_orte_name_compare(orca_name_cmp_bitmask_t fields,
                                           const orca_process_name_t* name1,
                                           const orca_process_name_t* name2);

/*
 * Proces Information Functions
 */
bool orca_stems_orte_info_proc_is_bound(void);
hwloc_cpuset_t orca_stems_orte_info_proc_get_applied_binding(void);

/*
 * General Information Functions
 */
bool orca_stems_orte_info_create_session_dirs(void);
bool orca_stems_orte_info_in_parallel_debugger(void);
bool orca_stems_orte_info_standalone_operation(void);
bool orca_stems_orte_info_cr_continue_like_restart(void);
char * orca_stems_orte_info_job_identifier(void);

/*
 * Out-Of-Band Communication
 */
int orca_stems_orte_oob_send_buffer(struct orca_process_name_t* peer,
                                         struct opal_buffer_t* buffer,
                                         orca_oob_tag_t tag,
                                         int flags);
int orca_stems_orte_oob_send_buffer_nb(struct orca_process_name_t* peer,
                                            struct opal_buffer_t* buffer,
                                            orca_oob_tag_t tag,
                                            int flags,
                                            orca_oob_buffer_callback_fn_t cbfunc,
                                            void* cbdata);
int orca_stems_orte_oob_recv_buffer(struct orca_process_name_t* peer,
                                         struct opal_buffer_t *buffer,
                                         orca_oob_tag_t tag,
                                         int flags);
int orca_stems_orte_oob_recv_buffer_nb(struct orca_process_name_t* peer,
                                            orca_oob_tag_t tag,
                                            int flags,
                                            orca_oob_buffer_callback_fn_t cbfunc,
                                            void* cbdata);
int orca_stems_orte_oob_recv_cancel(orca_process_name_t* peer,
                                         orca_oob_tag_t tag);

int orca_stems_orte_oob_parse_uris(const char* uri,
                                        orca_process_name_t* peer,
                                        char ***uris );

/*
 * Remote Process Information
 */
opal_hwloc_locality_t orca_stems_orte_proc_get_locality(orca_process_name_t* name);
char * orca_stems_orte_proc_get_hostname(orca_process_name_t* name);
orca_node_rank_t orca_stems_orte_proc_get_node_rank(const orca_process_name_t* name);

/*
 * Error Manager
 */
void orca_stems_orte_error_log(int error_code,
                                    char *filename,
                                    int line);
orca_error_mgr_fault_callback_t * orca_stems_orte_error_set_callback(orca_error_mgr_fault_callback_t *cbfunc);
void orca_stems_orte_error_abort(int error_code,
                                      char *fmt,
                                      va_list arglist);
int orca_stems_orte_error_abort_peers(orca_process_name_t *procs,
                                           orca_std_cntr_t num_procs);

/* 
 * Collectives
 */
int orca_stems_orte_coll_modex(void);
int orca_stems_orte_coll_barrier(orca_coll_type_t type);
int orca_stems_orte_coll_set_attribute(const char* attr_name,
                                            const void *buffer,
                                            size_t size);
int orca_stems_orte_coll_get_attribute(orca_process_name_t *name,
                                            const char* attr_name,
                                            void **buffer,
                                            size_t *size);

/*
 * Notifier / Show Help
 */
int orca_stems_orte_notifier_show_help(const char *filename,
                                            const char *topic, 
                                            bool want_error_header,
                                            va_list arglist);
bool orca_stems_orte_notifier_show_help_avail(void);
bool orca_stems_orte_notifier_show_help_want_aggregate(void);


END_C_DECLS

#endif /* MCA_STEMS_ORTE_EXPORT_H */
