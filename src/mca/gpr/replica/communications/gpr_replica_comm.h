/* -*- C -*-
 * 
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
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

#include "class/orte_pointer_array.h"

#include "threads/mutex.h"
#include "threads/condition.h"

#include "mca/ns/ns_types.h"
#include "mca/rml/rml_types.h"

#include "mca/gpr/replica/gpr_replica.h"
#include "mca/gpr/replica/functional_layer/gpr_replica_fn.h"
#include "mca/gpr/replica/transition_layer/gpr_replica_tl.h"

#include "mca/gpr/replica/gpr_replica.h"

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
int orte_gpr_replica_remote_notify(orte_process_name_t *recipient, int recipient_tag,
                 orte_gpr_notify_message_t *message);

/*
 * define the local functions for processing commands
 */
int orte_gpr_replica_recv_delete_segment_cmd(orte_buffer_t *input_buffer,
                                             orte_buffer_t *output_buffer);
                                             
int orte_gpr_replica_recv_put_cmd(orte_buffer_t *input_buffer,
                                  orte_buffer_t *output_buffer);
                                  
int orte_gpr_replica_recv_get_cmd(orte_buffer_t *input_buffer,
                                  orte_buffer_t *answer);
                                  
int orte_gpr_replica_recv_delete_entries_cmd(orte_buffer_t *input_buffer,
                                             orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_index_cmd(orte_buffer_t *input_buffer,
                                    orte_buffer_t *answer);

int orte_gpr_replica_recv_subscribe_cmd(orte_process_name_t* sender,
                                        orte_buffer_t *input_buffer,
                                        orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_unsubscribe_cmd(orte_buffer_t *input_buffer,
                                          orte_buffer_t *output_buffer);

int orte_gpr_replica_recv_dump_all_cmd(orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_segments_cmd(orte_buffer_t *answer);

int orte_gpr_replica_recv_dump_triggers_cmd(orte_buffer_t *answer);

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
 

#endif
