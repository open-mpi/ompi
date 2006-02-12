/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */
/** @file 
 */

#ifndef ORTE_GPR_REPLICA_COMM_H
#define ORTE_GPR_REPLICA_COMM_H


#include "orte_config.h"

#include <time.h>

#include "orte/class/orte_pointer_array.h"

#include "opal/threads/mutex.h"
#include "opal/threads/condition.h"

#include "orte/mca/ns/ns_types.h"
#include "orte/mca/rml/rml_types.h"

#include "orte/mca/gpr/replica/gpr_replica.h"
#include "orte/mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "orte/mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "orte/mca/gpr/replica/gpr_replica.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * GPR Replica Communications Interfaces
 */

/*
 * Proxy msg receiver
 */
void orte_gpr_replica_recv(int status, orte_process_name_t* sender,
                           orte_buffer_t *buffer, orte_rml_tag_t tag, void* cbdata);

/*
 * Remote notification transmitter
 */

/*
 * Local notification transmitter
 */

/*
 * Command buffer processor
 */
int orte_gpr_replica_process_command_buffer(orte_buffer_t *input_buffer,
                            orte_process_name_t *sender,
                            orte_buffer_t **output_buffer);

/*
 * Messaging functions
 */
int orte_gpr_replica_remote_notify(orte_process_name_t *recipient, 
                            orte_gpr_notify_message_t *msg);

/*
 * define the local functions for processing commands
 */
int orte_gpr_replica_recv_compound_cmd(orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_delete_segment_cmd(orte_buffer_t *input_buffer,
                                             orte_buffer_t *output_buffer);
                                             
int orte_gpr_replica_recv_put_cmd(orte_buffer_t *input_buffer,
                                  orte_buffer_t *output_buffer);
                                  
int orte_gpr_replica_recv_get_cmd(orte_buffer_t *input_buffer,
                                  orte_buffer_t *answer);
                                  
int orte_gpr_replica_recv_get_conditional_cmd(orte_buffer_t *input_buffer,
                                  orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_delete_entries_cmd(orte_buffer_t *input_buffer,
                                             orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_index_cmd(orte_buffer_t *input_buffer,
                                    orte_buffer_t *answer);

int orte_gpr_replica_recv_subscribe_cmd(orte_process_name_t* sender,
                                        orte_buffer_t *input_buffer,
                                        orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_unsubscribe_cmd(orte_process_name_t* sender,
                                          orte_buffer_t *input_buffer,
                                          orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_cancel_trigger_cmd(orte_process_name_t* sender,
                                             orte_buffer_t *input_buffer,
                                             orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_dump_all_cmd(orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_segments_cmd(orte_buffer_t *input_buffer, orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_triggers_cmd(orte_buffer_t *input_buffer,
                                            orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_subscriptions_cmd(orte_buffer_t *input_buffer,
                                                 orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_a_trigger_cmd(orte_buffer_t *input_buffer,
                                            orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_a_subscription_cmd(orte_buffer_t *input_buffer,
                                            orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_callbacks_cmd(orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_segment_size_cmd(orte_buffer_t *input_buffer, orte_buffer_t *answer);

int orte_gpr_replica_recv_get_startup_msg_cmd(orte_buffer_t *input_buffer,
                                              orte_buffer_t *answer);

int orte_gpr_replica_recv_cleanup_job_cmd(orte_buffer_t *input_buffer,
                                          orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_cleanup_proc_cmd(orte_buffer_t *input_buffer,
                                           orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_increment_value_cmd(orte_buffer_t *input_buffer,
                                              orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_decrement_value_cmd(orte_buffer_t *input_buffer,
                                              orte_buffer_t *output_buffer);
 
#if defined(c_plusplus) || defined(__cplusplus)
}
#endif

#endif
