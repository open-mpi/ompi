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
#include "opal/dss/dss.h"

/*******************************************
 * General DSS Funcations
 *******************************************/


/*******************************************
 * ORCA_DSS_VPID
 *******************************************/
int orca_dt_pack_vpid(opal_buffer_t *buffer,
                           const void *src,
                           int32_t num_vals,
                           opal_data_type_t type);
int orca_dt_unpack_vpid(opal_buffer_t *buffer,
                             void *dest,
                             int32_t *num_vals,
                             opal_data_type_t type);
int orca_dt_copy_vpid(orca_vpid_t **dest,
                           orca_vpid_t *src,
                           opal_data_type_t type);
int orca_dt_compare_vpid(orca_vpid_t *value1,
                              orca_vpid_t *value2,
                              opal_data_type_t type);
int orca_dt_print_vpid(char **output,
                            char *prefix,
                            orca_vpid_t *vpid,
                            opal_data_type_t type);


/*******************************************
 * ORCA_DSS_JOBID
 *******************************************/
int orca_dt_pack_jobid(opal_buffer_t *buffer,
                            const void *src,
                            int32_t num_vals,
                            opal_data_type_t type);
int orca_dt_unpack_jobid(opal_buffer_t *buffer,
                              void *dest,
                              int32_t *num_vals,
                              opal_data_type_t type);
int orca_dt_copy_jobid(orca_jobid_t **dest,
                            orca_jobid_t *src,
                            opal_data_type_t type);
int orca_dt_compare_jobid(orca_jobid_t *value1,
                               orca_jobid_t *value2,
                               opal_data_type_t type);
int orca_dt_print_jobid(char **output,
                             char *prefix,
                             orca_jobid_t *jobid,
                             opal_data_type_t type);


/*******************************************
 * ORCA_DSS_NAME
 *******************************************/
int orca_dt_pack_name(opal_buffer_t *buffer,
                           const void *src,
                           int32_t num_vals,
                           opal_data_type_t type);
int orca_dt_unpack_name(opal_buffer_t *buffer,
                             void *dest,
                             int32_t *num_vals,
                             opal_data_type_t type);
int orca_dt_copy_name(orca_process_name_t **dest,
                           orca_process_name_t *src,
                           opal_data_type_t type);
int orca_dt_compare_name(orca_process_name_t *value1,
                              orca_process_name_t *value2,
                              opal_data_type_t type);
int orca_dt_print_name(char **output,
                            char *prefix,
                            orca_process_name_t *name,
                            opal_data_type_t type);
