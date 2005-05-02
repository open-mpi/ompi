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

#include "include/orte_constants.h"
#include "include/orte_types.h"
#include "dps/dps.h"
#include "util/output.h"

#include "mca/gpr/base/base.h"

static void orte_gpr_base_dump_data(orte_buffer_t *buffer, orte_gpr_notify_data_t *data);

static void orte_gpr_base_dump_load_string(orte_buffer_t *buffer, char **tmp);

int orte_gpr_base_dump_notify_msg(orte_buffer_t *buffer,
                                  orte_gpr_notify_message_t *msg)
{
    char *tmp_out;
    size_t i;
    
    asprintf(&tmp_out, "\nDUMP OF NOTIFY MESSAGE STRUCTURE");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    if (NULL == msg) {
        asprintf(&tmp_out, "NULL msg pointer");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        return ORTE_SUCCESS;
    }
    
    asprintf(&tmp_out, ORTE_SIZE_T_PRINTF
                       " Notify data structures in message going to trigger "
                       ORTE_SIZE_T_PRINTF,
                       msg->cnt, msg->idtag);
    orte_gpr_base_dump_load_string(buffer, &tmp_out);
    
    if (0 < msg->cnt && NULL != msg->data) {
        for (i=0; i < msg->cnt; i++) {
            asprintf(&tmp_out, "\nDump of data structure " ORTE_SIZE_T_PRINTF, i);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            orte_gpr_base_dump_data(buffer, msg->data[i]);
        }
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_dump_notify_data(orte_buffer_t *buffer,
                                   orte_gpr_notify_data_t *data)
{
    char *tmp_out;

    asprintf(&tmp_out, "\nDUMP OF NOTIFY DATA STRUCTURE");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);

    if (NULL == data) {
        asprintf(&tmp_out, "NULL data pointer");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        return ORTE_SUCCESS;
    }
    
    orte_gpr_base_dump_data(buffer, data);
    return ORTE_SUCCESS;
}

static void orte_gpr_base_dump_data(orte_buffer_t *buffer,
                                    orte_gpr_notify_data_t *data)
{
    char *tmp_out;
    orte_gpr_value_t **values;
    size_t i;

    asprintf(&tmp_out, ORTE_SIZE_T_PRINTF " Values from segment %s", data->cnt, data->segment);
    orte_gpr_base_dump_load_string(buffer, &tmp_out);
    
    if (0 < data->cnt && NULL != data->values) {
        values = data->values;
        for (i=0; i < data->cnt; i++) {
            asprintf(&tmp_out, "\nData for value " ORTE_SIZE_T_PRINTF
                               " going to callback num " ORTE_SIZE_T_PRINTF,
                               i, data->cb_num);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            if (NULL == values[i]) {
                asprintf(&tmp_out, "\tError encountered: NULL value pointer");
                orte_gpr_base_dump_load_string(buffer, &tmp_out);
            } else {
                orte_gpr_base_dump_value(buffer, values[i]);
            }
        }
    }
}

int orte_gpr_base_dump_value(orte_buffer_t *buffer, orte_gpr_value_t *value)
{
    char *tmp_out;
    orte_gpr_addr_mode_t addr;
    size_t j;

    asprintf(&tmp_out, "\tValue from segment %s with " ORTE_SIZE_T_PRINTF " keyvals",
                            value->segment, value->cnt);
    orte_gpr_base_dump_load_string(buffer, &tmp_out);
    
    addr = value->addr_mode;
    if (NULL == value->tokens) {
        asprintf(&tmp_out, "\tNULL tokens (wildcard)");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    } else {
        asprintf(&tmp_out, "\t" ORTE_SIZE_T_PRINTF " Tokens returned", value->num_tokens);
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        for (j=0; j < value->num_tokens; j++) {
            asprintf(&tmp_out, "\tToken " ORTE_SIZE_T_PRINTF ": %s", j, value->tokens[j]);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
        }
    }
    asprintf(&tmp_out, "\tToken addressing mode:");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);
    if (ORTE_GPR_TOKENS_AND & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_TOKENS_AND");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TOKENS_OR & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_TOKENS_OR");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TOKENS_XAND & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_TOKENS_XAND");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TOKENS_XOR & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_TOKENS_XOR");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_TOKENS_NOT & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_TOKENS_NOT");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }

    asprintf(&tmp_out, "\n\tKey addressing mode:");
    orte_gpr_base_dump_load_string(buffer, &tmp_out);
    if (0x0000 == addr) {
        asprintf(&tmp_out, "\t\tNONE");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_KEYS_AND & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_KEYS_AND");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_KEYS_OR & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_KEYS_OR");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_KEYS_XAND & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_KEYS_XAND");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_KEYS_XOR & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_KEYS_XOR");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    if (ORTE_GPR_KEYS_NOT & addr) {
        asprintf(&tmp_out, "\t\tORTE_GPR_KEYS_NOT");
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
    }
    
    for (j=0; j < value->cnt; j++) {
        asprintf(&tmp_out, "\t\tData for keyval " ORTE_SIZE_T_PRINTF ": Key: %s", j,
                                (value->keyvals[j])->key);
        orte_gpr_base_dump_load_string(buffer, &tmp_out);
        orte_gpr_base_dump_keyval_value(buffer, value->keyvals[j]);
    }
    
    return ORTE_SUCCESS;
}


void orte_gpr_base_dump_keyval_value(orte_buffer_t *buffer, orte_gpr_keyval_t *iptr)
{
    char *tmp_out;
    
    switch(iptr->type) {

        case ORTE_BYTE:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_BYTE: no value field");
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_BOOL:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_BOOL: no value field");
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_STRING:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_STRING\tValue: %s", iptr->value.strptr);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_SIZE:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_SIZE:\tValue: " ORTE_SIZE_T_PRINTF, iptr->value.size);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_INT:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_INT: no value field");
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_UINT8:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_UINT8\tValue: %d", (int)iptr->value.ui8);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_UINT16:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_UINT16\tValue: %d", (int)iptr->value.ui16);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_UINT32:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_UINT32\tValue: %d", (int)iptr->value.ui32);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
#ifdef HAVE_INT64_T
        case ORTE_UINT64:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_UINT64\tValue: %d", (int)iptr->value.ui64);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
#endif

        case ORTE_INT8:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_INT8\tValue: %d", (int)iptr->value.i8);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
        
        case ORTE_INT16:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_INT16\tValue: %d", (int)iptr->value.i16);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
        
        case ORTE_INT32:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_INT32\tValue: %d", (int)iptr->value.i32);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
        
#ifdef HAVE_INT64_T
        case ORTE_INT64:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_INT64\tValue: %d", (int)iptr->value.i64);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
#endif

        case ORTE_BYTE_OBJECT:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_BYTE_OBJECT\tSize: "
                     ORTE_SIZE_T_PRINTF, (iptr->value.byteobject).size);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_NAME:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_NAME\tValue: ["
                               ORTE_SIZE_T_PRINTF ","
                               ORTE_SIZE_T_PRINTF ","
                               ORTE_SIZE_T_PRINTF "]",
                               ORTE_NAME_ARGS(&(iptr->value.proc)));
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_VPID:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_VPID\tValue: " ORTE_SIZE_T_PRINTF,
                                    iptr->value.vpid);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_JOBID:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_JOBID\tValue: " ORTE_SIZE_T_PRINTF,
                                    iptr->value.jobid);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_CELLID:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_CELLID\tValue: " ORTE_SIZE_T_PRINTF,
                                    iptr->value.cellid);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_NODE_STATE:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_NODE_STATE\tValue: %d", (int)iptr->value.node_state);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_PROC_STATE:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_PROC_STATE\tValue: %d", (int)iptr->value.proc_state);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_EXIT_CODE:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_EXIT_CODE\tValue: %d", (int)iptr->value.exit_code);
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        case ORTE_NULL:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_NULL");
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
        
        case ORTE_APP_CONTEXT:
            asprintf(&tmp_out, "\t\t\tData type: ORTE_APP_CONTEXT");
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
            
        default:
            asprintf(&tmp_out, "\t\t\tData type: UNKNOWN");
            orte_gpr_base_dump_load_string(buffer, &tmp_out);
            break;
    }
 }

static void orte_gpr_base_dump_load_string(orte_buffer_t *buffer, char **tmp)
{
    orte_dps.pack(buffer, tmp, 1, ORTE_STRING);
    free(*tmp);

}
