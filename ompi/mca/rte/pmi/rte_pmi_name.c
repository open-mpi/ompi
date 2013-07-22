/*
 * Copyright (c) 2013      Sandia National Laboratories. All rights reserved.
 *
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include <stdio.h>
#include <pmi.h>
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

#include "opal/dss/dss.h"
#include "opal/threads/tsd.h"
#include "ompi/constants.h"
#include "ompi/mca/rte/rte.h"

#include "rte_pmi.h"
#include "rte_pmi_internal.h"

ompi_process_name_t ompi_rte_my_process_name;
ompi_process_name_t ompi_rte_wildcard_process_name = { UINT32_MAX, UINT32_MAX };

static opal_tsd_key_t print_tsd;

static int
dt_pack_name(opal_buffer_t *buffer, const void *src, int32_t num_vals, opal_data_type_t type)
{
    int ret;
    int32_t i;
    uint32_t *tmp;
    ompi_process_name_t* name = (ompi_process_name_t*) src;

    tmp = malloc(sizeof(uint32_t) * num_vals);
    if (NULL == tmp) return OMPI_ERR_OUT_OF_RESOURCE;

    for (i = 0 ; i < num_vals ; ++i) {
        tmp[i * 2] = name[i].jobid;
        tmp[i * 2  + 1] = name[i].vpid;
    }

    ret = opal_dss.pack(buffer, tmp, 2 * num_vals, OPAL_UINT32);

    free(tmp);

    return ret;
}


static int
dt_unpack_name(opal_buffer_t *buffer, void *dest, int32_t *num_vals, opal_data_type_t type)
{
    
    int ret;
    int32_t i;
    uint32_t *tmp;
    ompi_process_name_t* name = (ompi_process_name_t*) dest;
    int32_t num = *num_vals;

    tmp = malloc(sizeof(uint32_t) * *num_vals);
    if (NULL == tmp) return OMPI_ERR_OUT_OF_RESOURCE;

    num = 2  * *num_vals;
    ret = opal_dss.unpack(buffer, tmp, &num, OPAL_UINT32);

    for (i = 0 ; i < *num_vals ; ++i) {
        name[i].jobid = tmp[i * 2];
        name[i].vpid = tmp[i * 2 + 1];
    }

    free(tmp);

    return ret;
}


static int
dt_copy_name(void **dest, void *src, opal_data_type_t type)
{
    ompi_process_name_t *a, *b;

    *dest = malloc(sizeof(ompi_process_name_t));
    if (NULL == *dest) {
        return OMPI_ERR_OUT_OF_RESOURCE;
    }

    a = *((ompi_process_name_t**) dest);
    b = (ompi_process_name_t*) src;

    a->jobid = b->jobid;
    a->vpid = b->vpid;
    
    return OMPI_SUCCESS;
}


static int
dt_compare_name(const void *value1,
                const void *value2,
                opal_data_type_t type)
{
    return ompi_rte_compare_name_fields(OMPI_RTE_CMP_ALL, 
                                        (ompi_process_name_t*) value1, 
                                        (ompi_process_name_t*) value2);
}


static int
dt_print_name(char **output, char *prefix, void *name, opal_data_type_t type)
{
    asprintf(output, "%sData type: ompi_process_name\tData Value: %s",
             (NULL == prefix ? " " : prefix), 
             ompi_rte_print_process_name((ompi_process_name_t*) name));
    
    return OMPI_SUCCESS;
}


static void
print_tsd_destructor(void *value)
{
    if (NULL != value) free(value);
}


int
ompi_rte_pmi_name_init(void)
{
    int ret;
    opal_data_type_t tmp;

    tmp = OMPI_NAME;
    ret = opal_dss.register_type(dt_pack_name,
                                 dt_unpack_name,
                                 dt_copy_name,
                                 dt_compare_name,
                                 dt_print_name,
                                 OPAL_DSS_UNSTRUCTURED,
                                 "ompi_process_name",
                                 &tmp);
    if (OMPI_SUCCESS != ret) return ret;

    ret = opal_tsd_key_create(&print_tsd, print_tsd_destructor);
    if (OMPI_SUCCESS != ret) return ret;

    return OMPI_SUCCESS;
}


int
ompi_rte_pmi_name_fini(void)
{
    opal_tsd_key_delete(print_tsd);

    return OMPI_SUCCESS;
}


char*
ompi_rte_print_process_name(const ompi_process_name_t *name)
{
    int ret;
    char *buf;

    ret = opal_tsd_getspecific(print_tsd, (void**) &buf);
    if (OMPI_SUCCESS != ret) return NULL;

    if (NULL == buf) {
        buf = malloc(sizeof(char) * 32);
        if (NULL == buf) return NULL;
    }

    ret = opal_tsd_setspecific(print_tsd, buf);
    if (OMPI_SUCCESS != ret) return NULL;

    snprintf(buf, 32, "[%u, %u]", 
             (unsigned int) name->jobid,
             (unsigned int) name->vpid);

    return buf;
}


int
ompi_rte_compare_name_fields(ompi_rte_cmp_bitmask_t fields,
                             const ompi_process_name_t *name1,
                             const ompi_process_name_t *name2)
{
    if (NULL == name1 && NULL == name2) {
        return OPAL_EQUAL;
    } else if (NULL == name1) {
        return OPAL_VALUE2_GREATER;
    } else if (NULL == name2) {
        return OPAL_VALUE1_GREATER;
    }

    if (OMPI_RTE_CMP_JOBID & fields) {
        if (name1->jobid > name2->jobid) {
            return OPAL_VALUE1_GREATER;
        } else if (name1->jobid < name2->jobid) {
            return OPAL_VALUE2_GREATER;
        }
    }

    if (OMPI_RTE_CMP_VPID & fields) {
        if (name1->vpid > name2->vpid) {
            return OPAL_VALUE1_GREATER;
        } else if (name1->vpid < name2->vpid) {
            return OPAL_VALUE2_GREATER;
        }
    }

    return OPAL_EQUAL;
}


uint64_t
ompi_rte_hash_name(const ompi_process_name_t *name)
{ 
    uint64_t hash;
    
    hash = name->jobid;
    hash <<= sizeof(name->jobid) * 8;
    hash += name->vpid;
    
    return hash;
}
