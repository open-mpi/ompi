/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "orte_config.h"
#include "orte/constants.h"
#include "orte/types.h"

#include <string.h>
#include <slurm/pmi.h>

#include "orte/util/proc_info.h"
#include "opal/dss/dss.h"
#include "opal/util/opal_sos.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/odls/base/base.h"
#include "orte/mca/odls/odls_types.h"
#include "orte/mca/ess/ess.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/routed/routed.h"
#include "orte/runtime/orte_globals.h"
#include "orte/util/name_fns.h"
#include "orte/orted/orted.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/grpcomm/base/base.h"
#include "grpcomm_pmi.h"


/* Static API's */
static int init(void);
static void finalize(void);
static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag);
static int pmi_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int pmi_allgather_list(opal_list_t *names,
                              opal_buffer_t *sbuf, opal_buffer_t *rbuf);
static int pmi_barrier(void);
static int pmi_set_proc_attr(const char* attr_name, 
                             const void *buffer, size_t size);
static int pmi_get_proc_attr(const orte_process_name_t name,
                             const char* attr_name,
                             void **buffer, size_t *size);
static int modex(opal_list_t *procs);
static int purge_proc_attrs(void);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_pmi_module = {
    init,
    finalize,
    xcast,
    pmi_allgather,
    pmi_allgather_list,
    pmi_barrier,
    pmi_set_proc_attr,
    pmi_get_proc_attr,
    modex,
    purge_proc_attrs
};

/* useful util */
static char* orte_pmi_error(int pmi_err) {
    char * err_msg;

    switch(pmi_err) {
        case PMI_FAIL: err_msg = "Operation failed"; break;
        case PMI_ERR_INIT: err_msg = "PMI is not initialized"; break;
        case PMI_ERR_NOMEM: err_msg = "Input buffer not large enough"; break;
        case PMI_ERR_INVALID_ARG: err_msg = "Invalid argument"; break;
        case PMI_ERR_INVALID_KEY: err_msg = "Invalid key argument"; break;
        case PMI_ERR_INVALID_KEY_LENGTH: err_msg = "Invalid key length argument"; break;
        case PMI_ERR_INVALID_VAL: err_msg = "Invalid value argument"; break;
        case PMI_ERR_INVALID_VAL_LENGTH: err_msg = "Invalid value length argument"; break;
        case PMI_ERR_INVALID_LENGTH: err_msg = "Invalid length argument"; break;
        case PMI_ERR_INVALID_NUM_ARGS: err_msg = "Invalid number of arguments"; break;
        case PMI_ERR_INVALID_ARGS: err_msg = "Invalid args argument"; break;
        case PMI_ERR_INVALID_NUM_PARSED: err_msg = "Invalid num_parsed length argument"; break;
        case PMI_ERR_INVALID_KEYVALP: err_msg = "Invalid invalid keyvalp atgument"; break;
        case PMI_ERR_INVALID_SIZE: err_msg = "Invalid size argument"; break;
        case PMI_ERR_INVALID_KVS: err_msg = "Invalid kvs argument"; break;
        case PMI_SUCCESS: err_msg = "Success"; break;
        default: err_msg = "Unkown error";
    }
    return err_msg;
}
static char* pmi_encode(const void *val, size_t vallen);
static void* pmi_decode(unsigned char *val, size_t *retlen);


/* Local variables */
static char *pmi_kvs_name = NULL;
static int pmi_vallen_max = -1;

#define ORTE_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "%s[%s:%d:%s] %s: %s\n",                         \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),                 \
                    __FILE__, __LINE__, __func__,                       \
                    pmi_func, orte_pmi_error(pmi_err));                 \
    } while(0);


/**
 * Initialize the module
 */
static int init(void)
{
    return ORTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    return;
}

/**
 *  A "broadcast-like" function to a job's processes.
 *  @param  jobid   The job whose processes are to receive the message
 *  @param  buffer  The data to broadcast
 */

static int xcast(orte_jobid_t job,
                 opal_buffer_t *buffer,
                 orte_rml_tag_t tag)
{
    /* not used in this module */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_barrier(void)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am alone, just return */
    if (1 == orte_process_info.num_procs) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:pmi:barrier only one proc",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        return ORTE_SUCCESS;
    }
    
    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        ORTE_PMI_ERROR(rc, "PMI_Barrier");
        return ORTE_ERROR;
    }

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:pmi barrier complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    return ORTE_SUCCESS;
}

static int pmi_allgather(opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    /* not used in this implementation */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_allgather_list(opal_list_t *names,
                              opal_buffer_t *sbuf, opal_buffer_t *rbuf)
{
    /* no idea how to do this - only occurs for comm_spawn,
     * which this module doesn't support
     */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_set_proc_attr(const char* attr_name, 
                             const void *buffer, size_t size)
{
    char *attr, *attrval;
    int rc;

     if (NULL == pmi_kvs_name) {
        int max_length;

        rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_Get_value_length_max");
            return ORTE_ERROR;
        }

        if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
            return ORTE_ERROR;
        }
        pmi_kvs_name = malloc(max_length);
        if (NULL == pmi_kvs_name) {
            return ORTE_ERR_OUT_OF_RESOURCE;
        }
        rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
            return ORTE_ERROR;
        }
    }

    if (0 > asprintf(&attr, "%s-%s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name)) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: set attr %s of size %lu in KVS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
                         (unsigned long)size, pmi_kvs_name));
    
    attrval = pmi_encode(buffer, size);
    if (NULL == attrval) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    if (strlen(attrval) > (size_t)pmi_vallen_max) {
        opal_output(0, "pmi_proc_set_attr: attribute length is too long\n");
        return ORTE_ERROR;
    }

    rc = PMI_KVS_Put(pmi_kvs_name, attr, attrval);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }

    free(attr);
    free(attrval);

    return ORTE_SUCCESS;
}

static int pmi_get_proc_attr(const orte_process_name_t name,
                             const char* attr_name,
                             void **buffer, size_t *size)
{
    char *attrval, *attr;
    int rc;

    if (NULL == pmi_kvs_name) {
        return ORTE_ERR_UNREACH;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: get attr %s for proc %s in KVS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
                         ORTE_NAME_PRINT(&name), pmi_kvs_name));
    
    attrval = malloc(pmi_vallen_max);
    if (NULL == attrval) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    if (0 > asprintf(&attr, "%s-%s", ORTE_NAME_PRINT(&name), attr_name)) {
        free(attrval);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    rc = PMI_KVS_Get(pmi_kvs_name, attr, attrval, pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
        free(attrval);
        free(attr);
        return ORTE_ERROR;
    }
    *buffer = pmi_decode((unsigned char *)attrval, size);
    free(attrval);
    free(attr);

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: got attr %s of size %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         attr, (unsigned long)(*size)));
    
    if (NULL == buffer) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/
static int modex(opal_list_t *procs)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* commit our modex info */
    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Commit failed");
        return ORTE_ERROR;
    }

    /* Barrier here to ensure all other procs have committed */
    if (ORTE_SUCCESS != (rc = pmi_barrier())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    return rc;
}

static int purge_proc_attrs(void)
{
    /* nothing to do here */
    return ORTE_SUCCESS;
}

/* PMI only supports strings. For now, do a simple base16 
 * encoding. Should do something smarter, both with the 
 * algorith used and its implementation. */
static char* pmi_encode(const void *val, size_t vallen) {
    static unsigned char encodings[] = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
    size_t i;
    unsigned char *ret = malloc(vallen *2 +1);
    if (NULL == ret) {
        return NULL;
    }
    for (i = 0; i < vallen; i++) {
        ret[2 * i] = encodings[((unsigned char *)val)[i] & 0xf];
        ret[2 * i + 1] = encodings[((unsigned char *)val)[i] >> 4];
    }
    ret[vallen *2] = '\0';
    return (char *)ret;
}

static void* pmi_decode(unsigned char *val, size_t *retlen) {
    unsigned char *ret;
    size_t i;
    *retlen = strlen((char*)val)/2;

    ret = malloc(*retlen);
    if (NULL == ret) {
        return ret;
    }

    for (i = 0; i < *retlen; i++) {
        if (*val >= '0' && *val <= '9') {
            ret[i] = *val - '0';
        } else {
            ret[i] = *val - 'a' + 10;
        }
        val++;
        if (*val >= '0' && *val <= '9') {
            ret[i] |= ((*val - '0') << 4);
        } else {
            ret[i] |= ((*val - 'a' + 10) << 4);
        }
        val++;
    }
    return ret;
}

