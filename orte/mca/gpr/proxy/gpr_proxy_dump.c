/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
/** @file:
 *
 * The Open MPI general purpose registry - implementation.
 *
 */

/*
 * includes
 */

#include "orte_config.h"

#include <stdio.h>
#include <stdarg.h>
#include <string.h>
#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_LIBGEN_H
#include <libgen.h>
#endif

#include "include/orte_constants.h"

#include "dps/dps.h"
#include "mca/errmgr/errmgr.h"

#include "mca/oob/oob_types.h"
#include "mca/rml/rml.h"

#include "gpr_proxy.h"

int orte_gpr_proxy_dump_all(int output_id)
{
    orte_gpr_cmd_flag_t command;
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc;
    size_t n;
    
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
	    return orte_gpr_base_pack_dump_all(orte_gpr_proxy_globals.compound_cmd);
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
	    return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_all(cmd))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
	    return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
	    return ORTE_ERR_COMM_FAILURE;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
	    return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(answer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
	if (ORTE_GPR_DUMP_ALL_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
	    return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_proxy_dump_segments(int output_id)
{
    orte_gpr_cmd_flag_t command;
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc;
    size_t n;
    
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        return orte_gpr_base_pack_dump_segments(orte_gpr_proxy_globals.compound_cmd);
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_segments(cmd))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
       return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(answer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
   if (ORTE_GPR_DUMP_SEGMENTS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_proxy_dump_triggers(int output_id)
{
    orte_gpr_cmd_flag_t command;
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc;
    size_t n;
    
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        return orte_gpr_base_pack_dump_triggers(orte_gpr_proxy_globals.compound_cmd);
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_triggers(cmd))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
       return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(answer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
   if (ORTE_GPR_DUMP_TRIGGERS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_proxy_dump_subscriptions(int output_id)
{
    orte_gpr_cmd_flag_t command;
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc;
    size_t n;
    
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        return orte_gpr_base_pack_dump_subscriptions(orte_gpr_proxy_globals.compound_cmd);
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_subscriptions(cmd))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
       return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(answer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
   if (ORTE_GPR_DUMP_SUBSCRIPTIONS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_proxy_dump_callbacks(int output_id)
{
    orte_gpr_cmd_flag_t command;
    orte_buffer_t *cmd;
    orte_buffer_t *answer;
    int rc;
    size_t n;
    
    if (orte_gpr_proxy_globals.compound_cmd_mode) {
        return orte_gpr_base_pack_dump_callbacks(orte_gpr_proxy_globals.compound_cmd);
    }

    cmd = OBJ_NEW(orte_buffer_t);
    if (NULL == cmd) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_pack_dump_callbacks(cmd))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(cmd);
       return rc;
    }

    if (0 > orte_rml.send_buffer(orte_process_info.gpr_replica, cmd, ORTE_RML_TAG_GPR, 0)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    
    if (0 > orte_rml.recv_buffer(orte_process_info.gpr_replica, answer, ORTE_RML_TAG_GPR)) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
       return ORTE_ERR_COMM_FAILURE;
    }

    n = 1;
    if (ORTE_SUCCESS != (rc = orte_dps.unpack(answer, &command, &n, ORTE_GPR_CMD))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
        return rc;
    }
    
   if (ORTE_GPR_DUMP_CALLBACKS_CMD != command) {
        ORTE_ERROR_LOG(ORTE_ERR_COMM_FAILURE);
        OBJ_RELEASE(answer);
        return ORTE_ERR_COMM_FAILURE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_proxy_dump_notify_msg(orte_gpr_notify_message_t *msg, int output_id)
{
    orte_buffer_t *answer;
    int rc;
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_dump_notify_msg(answer, msg))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
       return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}


int orte_gpr_proxy_dump_notify_data(orte_gpr_notify_data_t *data, int output_id)
{
    orte_buffer_t *answer;
    int rc;
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_dump_notify_data(answer, data))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
       return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}

int orte_gpr_proxy_dump_value(orte_gpr_value_t *value, int output_id)
{
    orte_buffer_t *answer;
    int rc;
    
    answer = OBJ_NEW(orte_buffer_t);
    if (NULL == answer) { /* got a problem */
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
     return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr_base_dump_value(answer, value))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(answer);
       return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr_base_print_dump(answer, output_id))) {
        ORTE_ERROR_LOG(rc);
    }
    
    OBJ_RELEASE(answer);
    return rc;
}
