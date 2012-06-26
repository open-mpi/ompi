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

#include "orca/runtime/orca_dt.h"
#include "opal/dss/dss_internal.h"

static void orca_dt_quick_print(char **output,
                                     char *type_name,
                                     char *prefix,
                                     void *src,
                                     opal_data_type_t real_type);

/*******************************************
 * General DSS Funcations
 *******************************************/


/*******************************************
 * ORCA_DSS_VPID
 *******************************************/
int orca_dt_pack_vpid(opal_buffer_t *buffer,
                           const void *src,
                           int32_t num_vals,
                           opal_data_type_t type)
{
    int ret;
    
    ret = opal_dss_pack_buffer(buffer, src, num_vals, ORCA_VPID_T);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
    }
    
    return ret;
}

int orca_dt_unpack_vpid(opal_buffer_t *buffer,
                             void *dest,
                             int32_t *num_vals,
                             opal_data_type_t type)
{
    int ret;
    
    ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORCA_VPID_T);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
    }
    
    return ret;
}

int orca_dt_copy_vpid(orca_vpid_t **dest,
                           orca_vpid_t *src,
                           opal_data_type_t type)
{
    orca_vpid_t *val;
    
    val = (orca_vpid_t*)malloc(sizeof(orca_vpid_t));
    if (NULL == val) {
        ORCA_ERROR_LOG(ORCA_ERR_OUT_OF_RESOURCE);
        return ORCA_ERR_OUT_OF_RESOURCE;
    }
    
    *val = *src;
    *dest = val;

    return ORCA_SUCCESS;
}

int orca_dt_compare_vpid(orca_vpid_t *value1,
                              orca_vpid_t *value2,
                              opal_data_type_t type)
{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORCA_VPID_WILDCARD ||
        *value2 == ORCA_VPID_WILDCARD) {
        return OPAL_EQUAL;
    }
    
    if (*value1 > *value2) {
        return OPAL_VALUE1_GREATER;
    }
    
    if (*value2 > *value1) {
        return OPAL_VALUE2_GREATER;
    }
    
    return OPAL_EQUAL;
}

int orca_dt_print_vpid(char **output,
                            char *prefix,
                            orca_vpid_t *vpid,
                            opal_data_type_t type)
{
    orca_dt_quick_print(output,
                             "ORCA_DSS_VPID",
                             prefix,
                             vpid,
                             ORCA_VPID_T);
    return ORCA_SUCCESS;
}


/*******************************************
 * ORCA_DSS_JOBID
 *******************************************/
int orca_dt_pack_jobid(opal_buffer_t *buffer,
                            const void *src,
                            int32_t num_vals,
                            opal_data_type_t type)
{
    int ret;

    ret = opal_dss_pack_buffer(buffer, src, num_vals, ORCA_JOBID_T);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
    }
    
    return ret;
}

int orca_dt_unpack_jobid(opal_buffer_t *buffer,
                              void *dest,
                              int32_t *num_vals,
                              opal_data_type_t type)
{
    int ret;

    ret = opal_dss_unpack_buffer(buffer, dest, num_vals, ORCA_JOBID_T);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
    }
    
    return ret;
}

int orca_dt_copy_jobid(orca_jobid_t **dest,
                            orca_jobid_t *src,
                            opal_data_type_t type)
{
    orca_jobid_t *val;
    
    val = (orca_jobid_t*)malloc(sizeof(orca_jobid_t));
    if (NULL == val) {
        ORCA_ERROR_LOG(ORCA_ERR_OUT_OF_RESOURCE);
        return ORCA_ERR_OUT_OF_RESOURCE;
    }
    
    *val = *src;
    *dest = val;
    
    return ORCA_SUCCESS;
}

int orca_dt_compare_jobid(orca_jobid_t *value1,
                               orca_jobid_t *value2,
                               opal_data_type_t type)
{
    /** if either value is WILDCARD, then return equal */
    if (*value1 == ORCA_JOBID_WILDCARD ||
        *value2 == ORCA_JOBID_WILDCARD) {
        return OPAL_EQUAL;
    }
    
    if (*value1 > *value2) {
        return OPAL_VALUE1_GREATER;
    }
    
    if (*value2 > *value1) {
        return OPAL_VALUE2_GREATER;
    }
    
    return OPAL_EQUAL;
}

int orca_dt_print_jobid(char **output,
                             char *prefix,
                             orca_jobid_t *jobid,
                             opal_data_type_t type)
{
    orca_dt_quick_print(output,
                             "ORCA_DSS_JOBID",
                             prefix,
                             jobid,
                             ORCA_JOBID_T);
    return ORCA_SUCCESS;
}


/*******************************************
 * ORCA_DSS_NAME
 *******************************************/
int orca_dt_pack_name(opal_buffer_t *buffer,
                           const void *src,
                           int32_t num_vals,
                           opal_data_type_t type)
{
    int ret;
    int32_t i;
    orca_process_name_t *proc = NULL;
    orca_jobid_t *jobid = NULL;
    orca_vpid_t *vpid = NULL;
    
    /* collect all the jobids in a contiguous array */
    jobid = (orca_jobid_t*)malloc(num_vals * sizeof(orca_jobid_t));
    if (NULL == jobid) {
        ORCA_ERROR_LOG(ORCA_ERR_OUT_OF_RESOURCE);
        return ORCA_ERR_OUT_OF_RESOURCE;
    }

    proc = (orca_process_name_t*)src;
    for (i=0; i < num_vals; i++) {
        jobid[i] = proc->jobid;
        proc++;
    }

    /* now pack them in one shot */
    ret = orca_dt_pack_jobid(buffer, jobid, num_vals, ORCA_DSS_JOBID);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
        free(jobid);
        return ret;
    }
    free(jobid);
    
    /* collect all the vpids in a contiguous array */
    vpid = (orca_vpid_t*)malloc(num_vals * sizeof(orca_vpid_t));
    if (NULL == vpid) {
        ORCA_ERROR_LOG(ORCA_ERR_OUT_OF_RESOURCE);
        return ORCA_ERR_OUT_OF_RESOURCE;
    }

    proc = (orca_process_name_t*)src;
    for (i=0; i < num_vals; i++) {
        vpid[i] = proc->vpid;
        proc++;
    }

    /* now pack them in one shot */
    ret = orca_dt_pack_vpid(buffer, vpid, num_vals, ORCA_DSS_VPID);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
        free(vpid);
        return ret;
    }
    free(vpid);

    return ORCA_SUCCESS;
}

int orca_dt_unpack_name(opal_buffer_t *buffer,
                             void *dest,
                             int32_t *num_vals,
                             opal_data_type_t type)
{
    int ret;
    int32_t i, num;
    orca_process_name_t* proc;
    orca_jobid_t *jobid;
    orca_vpid_t *vpid;
    
    num = *num_vals;
    
    /* allocate space for all the jobids in a contiguous array */
    jobid = (orca_jobid_t*)malloc(num * sizeof(orca_jobid_t));
    if (NULL == jobid) {
        ORCA_ERROR_LOG(ORCA_ERR_OUT_OF_RESOURCE);
        *num_vals = 0;
        return ORCA_ERR_OUT_OF_RESOURCE;
    }

    /* now unpack them in one shot */
    ret = orca_dt_unpack_jobid(buffer, jobid, num_vals, ORCA_DSS_JOBID);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
        *num_vals = 0;
        free(jobid);
        return ret;
    }
    
    /* collect all the vpids in a contiguous array */
    vpid = (orca_vpid_t*)malloc(num * sizeof(orca_vpid_t));
    if (NULL == vpid) {
        ORCA_ERROR_LOG(ORCA_ERR_OUT_OF_RESOURCE);
        *num_vals = 0;
        free(jobid);
        return ORCA_ERR_OUT_OF_RESOURCE;
    }

    /* now unpack them in one shot */
    ret = orca_dt_unpack_vpid(buffer, vpid, num_vals, ORCA_DSS_VPID);
    if( ORCA_SUCCESS != ret ) {
        ORCA_ERROR_LOG(ret);
        *num_vals = 0;
        free(vpid);
        free(jobid);
        return ret;
    }
    
    /* build the names from the jobid/vpid arrays */
    proc = (orca_process_name_t*)dest;
    for (i=0; i < num; i++) {
        proc->jobid = jobid[i];
        proc->vpid = vpid[i];
        proc++;
    }

    /* cleanup */
    free(vpid);
    free(jobid);
    
    return ORCA_SUCCESS;
}

int orca_dt_copy_name(orca_process_name_t **dest,
                           orca_process_name_t *src,
                           opal_data_type_t type)
{
    orca_process_name_t *val;
    
    val = (orca_process_name_t*)malloc(sizeof(orca_process_name_t));
    if (NULL == val) {
        ORCA_ERROR_LOG(ORCA_ERR_OUT_OF_RESOURCE);
        return ORCA_ERR_OUT_OF_RESOURCE;
    }
    
    val->jobid = src->jobid;
    val->vpid = src->vpid;
    
    *dest = val;

    return ORCA_SUCCESS;
}

int orca_dt_compare_name(orca_process_name_t *value1,
                              orca_process_name_t *value2,
                              opal_data_type_t type)
{
    if (NULL == value1 && NULL == value2) {
        return OPAL_EQUAL;
    } else if (NULL == value1) {
        return OPAL_VALUE2_GREATER;
    } else if (NULL == value2) {
        return OPAL_VALUE1_GREATER;
    }
    
    /* If any of the fields are wildcard,
     * then we want to just ignore that one field. In the case
     * of ORCA_NAME_WILDCARD (where ALL of the fields are wildcard), this
     * will automatically result in OPAL_EQUAL for any name in the other
     * value - a totally useless result, but consistent in behavior.
     */
    
    /** check the jobids - if one of them is WILDCARD, then ignore
     * this field since anything is okay
     */
    if (value1->jobid != ORCA_JOBID_WILDCARD &&
        value2->jobid != ORCA_JOBID_WILDCARD) {
        if (value1->jobid < value2->jobid) {
            return OPAL_VALUE2_GREATER;
        } else if (value1->jobid > value2->jobid) {
            return OPAL_VALUE1_GREATER;
        }
    }
    
    /** check the vpids - if one of them is WILDCARD, then ignore
     * this field since anything is okay
     */
    if (value1->vpid != ORCA_VPID_WILDCARD &&
        value2->vpid != ORCA_VPID_WILDCARD) {
        if (value1->vpid < value2->vpid) {
            return OPAL_VALUE2_GREATER;
        } else if (value1->vpid > value2->vpid) {
            return OPAL_VALUE1_GREATER;
        }
    }

    /** only way to get here is if all fields are equal or WILDCARD */
    return OPAL_EQUAL;
}

int orca_dt_print_name(char **output,
                            char *prefix,
                            orca_process_name_t *name,
                            opal_data_type_t type)
{
    *output = NULL;
    
    if (NULL == name) {
        asprintf(output, "%sData type: ORCA_PROCESS_NAME\tData Value: NULL",
                 (NULL == prefix ? " " : prefix));
    } else {
        asprintf(output, "%sData type: ORCA_PROCESS_NAME\tData Value: %s",
                 (NULL == prefix ? " " : prefix), ORCA_NAME_PRINT(name));
    }
    
    return ORCA_SUCCESS;
}


/*******************************************
 * General Support Functions
 *******************************************/
static void orca_dt_quick_print(char **output,
                                     char *type_name,
                                     char *prefix,
                                     void *src,
                                     opal_data_type_t real_type)
{
    int8_t *i8;
    int16_t *i16;
    int32_t *i32;
    int64_t *i64;
    uint8_t *ui8;
    uint16_t *ui16;
    uint32_t *ui32;
    uint64_t *ui64;
    
    /* set default result */
    *output = NULL;
    
    /* check for NULL ptr */
    if (NULL == src) {
        asprintf(output, "%sData type: %s\tData size: 8-bit\tValue: NULL pointer",
                 (NULL == prefix) ? "" : prefix, type_name);
        return;
    }
    
    switch(real_type) {
    case OPAL_INT8:
        i8 = (int8_t*)src;
        asprintf(output, "%sData type: %s\tData size: 8-bit\tValue: %d",
                 (NULL == prefix) ? "" : prefix, type_name, (int) *i8);
        break;
            
    case OPAL_UINT8:
        ui8 = (uint8_t*)src;
        asprintf(output, "%sData type: %s\tData size: 8-bit\tValue: %u",
                 (NULL == prefix) ? "" : prefix, type_name, (unsigned int)*ui8);
        break;

    case OPAL_INT16:
        i16 = (int16_t*)src;
        asprintf(output, "%sData type: %s\tData size: 16-bit\tValue: %d", 
                 (NULL == prefix) ? "" : prefix, type_name, (int) *i16);
        break;
            
    case OPAL_UINT16:
        ui16 = (uint16_t*)src;
        asprintf(output, "%sData type: %s\tData size: 16-bit\tValue: %u", 
                 (NULL == prefix) ? "" : prefix, type_name, (unsigned int) *ui16);
        break;
            
    case OPAL_INT32:
        i32 = (int32_t*)src;
        asprintf(output, "%sData type: %s\tData size: 32-bit\tValue: %ld",
                 (NULL == prefix) ? "" : prefix, type_name, (long) *i32);
        break;
            
    case OPAL_UINT32:
        ui32 = (uint32_t*)src;
        asprintf(output, "%sData type: %s\tData size: 32-bit\tValue: %lu",
                 (NULL == prefix) ? "" : prefix, type_name, (unsigned long) *ui32);
        break;
            
    case OPAL_INT64:
        i64 = (int64_t*)src;
        asprintf(output, "%sData type: %s\tData size: 64-bit\tValue: %ld",
                 (NULL == prefix) ? "" : prefix, type_name, (long) *i64);
        break;
            
    case OPAL_UINT64:
        ui64 = (uint64_t*)src;
        asprintf(output, "%sData type: %s\tData size: 64-bit\tValue: %lu",
                 (NULL == prefix) ? "" : prefix, type_name, (unsigned long) *ui64);
        break;
            
    default:
        return;
    }

    return;
}
