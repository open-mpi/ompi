/*
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

static void orte_gpr_base_dump_data(orte_gpr_notify_data_t *data, int output_id);
static void orte_gpr_base_dump_keyval_value(orte_gpr_keyval_t *iptr, int output_id);

int orte_gpr_base_dump_notify_msg(orte_gpr_notify_message_t *msg, int output_id)
{
    int i;
    
    ompi_output(output_id, "\n\nDUMP OF NOTIFY MESSAGE STRUCTURE\n");

    if (NULL == msg) {
        ompi_output(output_id, "NULL msg pointer");
        return ORTE_SUCCESS;
    }
    
    ompi_output(output_id, "%d Notify data structures in message", msg->cnt);
    
    if (0 < msg->cnt && NULL != msg->data) {
        for (i=0; i < msg->cnt; i++) {
            ompi_output(output_id, "\n\nDump of data structure %d", i);
            orte_gpr_base_dump_data(msg->data[i], output_id);
        }
    }

    return ORTE_SUCCESS;
}

int orte_gpr_base_dump_notify_data(orte_gpr_notify_data_t *data, int output_id)
{
    ompi_output(output_id, "\n\nDUMP OF NOTIFY DATA STRUCTURE\n");
    if (NULL == data) {
        ompi_output(output_id, "NULL data pointer");
        return ORTE_SUCCESS;
    }
    
    orte_gpr_base_dump_data(data, output_id);
    return ORTE_SUCCESS;
}

static void orte_gpr_base_dump_data(orte_gpr_notify_data_t *data, int output_id)
{
    orte_gpr_addr_mode_t addr;
    orte_gpr_value_t **values;
    int i, j;

    ompi_output(output_id, "%d Values from segment %s", data->cnt, data->segment);
    
    if (0 < data->cnt && NULL != data->values) {
        values = data->values;
        for (i=0; i < data->cnt; i++) {
            ompi_output(output_id, "\nData for value %d", i);
            if (NULL == values[i]) {
                ompi_output(output_id, "\tError encountered: NULL value pointer");
            } else {
                addr = values[i]->addr_mode;
                if (NULL == values[i]->tokens) {
                    ompi_output(output_id, "\tNULL tokens (wildcard)");
                } else {
                    ompi_output(output_id, "\t%d Tokens returned", values[i]->num_tokens);
                    for (j=0; j < values[i]->num_tokens; j++) {
                        ompi_output(output_id, "\tToken %d: %s", j, values[i]->tokens[j]);
                    }
                }
                ompi_output(output_id, "\tToken addressing mode:");
                if (ORTE_GPR_TOKENS_AND & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_TOKENS_AND");
                }
                if (ORTE_GPR_TOKENS_OR & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_TOKENS_OR");
                }
                if (ORTE_GPR_TOKENS_XAND & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_TOKENS_XAND");
                }
                if (ORTE_GPR_TOKENS_XOR & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_TOKENS_XOR");
                }
                if (ORTE_GPR_TOKENS_NOT & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_TOKENS_NOT");
                }

                ompi_output(output_id, "\n\tKey addressing mode:");
                if (0x0000 == addr) {
                    ompi_output(output_id, "\t\tNONE");
                }
                if (ORTE_GPR_KEYS_AND & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_KEYS_AND");
                }
                if (ORTE_GPR_KEYS_OR & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_KEYS_OR");
                }
                if (ORTE_GPR_KEYS_XAND & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_KEYS_XAND");
                }
                if (ORTE_GPR_KEYS_XOR & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_KEYS_XOR");
                }
                if (ORTE_GPR_KEYS_NOT & addr) {
                    ompi_output(output_id, "\t\tORTE_GPR_KEYS_NOT");
                }
                
                if (NULL == values[i]->keyvals) {
                    ompi_output(output_id, "\tNo keyvals returned");
                } else {
                    ompi_output(output_id, "\t%d Keyvals returned", values[i]->cnt);
                    for (j=0; j < values[i]->cnt; j++) {
                        ompi_output(output_id, "\t\tData for keyval %d: Key: %s", j,
                                            (values[i]->keyvals[j])->key);
                        orte_gpr_base_dump_keyval_value(values[i]->keyvals[j], output_id);
                    }
                }
            }
        }
    }
}


static void orte_gpr_base_dump_keyval_value(orte_gpr_keyval_t *iptr, int output_id)
{
    switch(iptr->type) {

        case ORTE_BYTE:
            ompi_output(output_id, "\t\t\tData type: ORTE_BYTE");
            break;
            
        case ORTE_BOOL:
            ompi_output(output_id, "\t\t\tData type: ORTE_BOOL");
            break;
            
        case ORTE_STRING:
            ompi_output(output_id, "\t\t\tData type: ORTE_STRING\tValue: %s", iptr->value.strptr);
            break;
            
        case ORTE_SIZE:
            ompi_output(output_id, "\t\t\tData type: ORTE_SIZE");
            break;
            
        case ORTE_INT:
            ompi_output(output_id, "\t\t\tData type: ORTE_INT\tValue: %d", (int)iptr->value.i32);
            break;
            
        case ORTE_UINT8:
            ompi_output(output_id, "\t\t\tData type: ORTE_UINT8\tValue: %d", (int)iptr->value.ui8);
            break;
            
        case ORTE_UINT16:
            ompi_output(output_id, "\t\t\tData type: ORTE_UINT16\tValue: %d", (int)iptr->value.ui16);
            break;
            
        case ORTE_UINT32:
            ompi_output(output_id, "\t\t\tData type: ORTE_UINT32\tValue: %d", (int)iptr->value.ui32);
            break;
            
#ifdef HAVE_I64
        case ORTE_UINT64:
            ompi_output(output_id, "\t\t\tData type: ORTE_UINT64\tValue: %d", (int)iptr->value.ui64);
            break;
#endif

        case ORTE_INT8:
            ompi_output(output_id, "\t\t\tData type: ORTE_INT8\tValue: %d", (int)iptr->value.i8);
            break;
        
        case ORTE_INT16:
            ompi_output(output_id, "\t\t\tData type: ORTE_INT16\tValue: %d", (int)iptr->value.i16);
            break;
        
        case ORTE_INT32:
            ompi_output(output_id, "\t\t\tData type: ORTE_INT32\tValue: %d", (int)iptr->value.i32);
            break;
        
#ifdef HAVE_I64
        case ORTE_INT64:
            ompi_output(output_id, "\t\t\tData type: ORTE_INT64\tValue: %d", (int)iptr->value.i64);
            break;
#endif

        case ORTE_BYTE_OBJECT:
            ompi_output(output_id, "\t\t\tData type: ORTE_BYTE_OBJECT\tSize: %d", (int)(iptr->value.byteobject).size);
            break;
            
        case ORTE_NAME:
            ompi_output(output_id, "\t\t\tData type: ORTE_NAME\tValue: [%d,%d,%d]", ORTE_NAME_ARGS(&(iptr->value.proc)));
            break;
            
        case ORTE_VPID:
            ompi_output(output_id, "\t\t\tData type: ORTE_VPID\tValue: %d", (int)iptr->value.vpid);
            break;
            
        case ORTE_JOBID:
            ompi_output(output_id, "\t\t\tData type: ORTE_JOBID\tValue: %d", (int)iptr->value.jobid);
            break;
            
        case ORTE_CELLID:
            ompi_output(output_id, "\t\t\tData type: ORTE_CELLID\tValue: %d", (int)iptr->value.cellid);
            break;
            
        case ORTE_NODE_STATE:
            ompi_output(output_id, "\t\t\tData type: ORTE_NODE_STATE\tValue: %d", (int)iptr->value.node_state);
            break;
            
        case ORTE_PROC_STATE:
            ompi_output(output_id, "\t\t\tData type: ORTE_PROC_STATE\tValue: %d", (int)iptr->value.proc_state);
            break;
            
        case ORTE_EXIT_CODE:
            ompi_output(output_id, "\t\t\tData type: ORTE_EXIT_CODE\tValue: %d", (int)iptr->value.exit_code);
            break;
            
        case ORTE_NULL:
            ompi_output(output_id, "\t\t\tData type: ORTE_NULL");
            break;
        
        case ORTE_APP_CONTEXT:
            ompi_output(output_id, "\t\t\tData type: ORTE_APP_CONTEXT");
            break;
            
        default:
            ompi_output(output_id, "\t\t\tData type: UNKNOWN");
            break;
    }
 }
