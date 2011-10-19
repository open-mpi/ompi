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
#include <pmi.h>

#include "opal/dss/dss.h"
#include "opal/mca/hwloc/base/base.h"

#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rml/rml.h"
#include "orte/util/name_fns.h"
#include "orte/util/proc_info.h"

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

static int pmi_encode(const void *val, size_t vallen);
static void* pmi_decode(size_t *retlen);
static char* pmi_error(int pmi_err);
#define ORTE_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "%s[%s:%d:%s] %s: %s\n",                         \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),                 \
                    __FILE__, __LINE__, __func__,                       \
                    pmi_func, pmi_error(pmi_err));                      \
    } while(0);
static int setup_pmi(void);
static int setup_key(const orte_process_name_t *name, const char *key);

/* Local variables */
static char *pmi_kvs_name = NULL;
static char *pmi_kvs_key = NULL;
static char *pmi_attr_val = NULL;
static int pmi_vallen_max = -1;
static int pmi_keylen_max = -1;

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;

    if (NULL == pmi_kvs_name) {
        if (ORTE_SUCCESS != (rc = setup_pmi())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    return ORTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    if (NULL != pmi_kvs_name) {
        free(pmi_kvs_name);
        pmi_kvs_name = NULL;
    }
    if (NULL != pmi_kvs_key) {
        free(pmi_kvs_key);
        pmi_kvs_key = NULL;
    }
    if (NULL != pmi_attr_val) {
        free(pmi_attr_val);
        pmi_attr_val = NULL;
    }
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
    int rc;

     if (NULL == pmi_kvs_name) {
         if (ORTE_SUCCESS != (rc = setup_pmi())) {
             ORTE_ERROR_LOG(rc);
             return rc;
         }
    }

     if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, attr_name))) {
         ORTE_ERROR_LOG(rc);
         return rc;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: set attr %s of size %lu in KVS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
                         (unsigned long)size, pmi_kvs_name));
    
    if (ORTE_SUCCESS != (rc = pmi_encode(buffer, size))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = PMI_KVS_Put(pmi_kvs_name, pmi_kvs_key, pmi_attr_val);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }

    return ORTE_SUCCESS;
}

static int pmi_get_proc_attr(const orte_process_name_t name,
                             const char* attr_name,
                             void **buffer, size_t *size)
{
    int rc;

    /* set default */
    *size = 0;
    *buffer = NULL;

    if (NULL == pmi_kvs_name) {
         if (ORTE_SUCCESS != (rc = setup_pmi())) {
             ORTE_ERROR_LOG(rc);
             return rc;
         }
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: get attr %s for proc %s in KVS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
                         ORTE_NAME_PRINT(&name), pmi_kvs_name));
    
    if (ORTE_SUCCESS != (rc = setup_key(&name, attr_name))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    rc = PMI_KVS_Get(pmi_kvs_name, pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
        return ORTE_ERROR;
    }
    *buffer = pmi_decode(size);
    if (NULL == *buffer) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: got attr %s of size %lu",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         attr_name, (unsigned long)(*size)));
    
    return ORTE_SUCCESS;
}

/***   MODEX SECTION ***/
static int modex(opal_list_t *procs)
{
    int rc, i;
    char *rml_uri, val[64];
    orte_vpid_t v;
    orte_process_name_t name;
    orte_jmap_t *jmap;
    orte_nid_t *nid, *loc;
    orte_pmap_t *pmap;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    if (NULL == pmi_kvs_name) {
        if (ORTE_SUCCESS != (rc = setup_pmi())) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }

    /* provide our hostname so others can know our location */
    if (strlen(orte_process_info.nodename) > (size_t)pmi_vallen_max) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "HOSTNAME"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = PMI_KVS_Put(pmi_kvs_name, pmi_kvs_key, orte_process_info.nodename);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }

    /* add our oob endpoint info so that oob communications
     * can be supported
     */
    rml_uri = orte_rml.get_contact_info();
    if (strlen(rml_uri) > (size_t)pmi_vallen_max) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        return ORTE_ERROR;
    }
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "RMLURI"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    rc = PMI_KVS_Put(pmi_kvs_name, pmi_kvs_key, rml_uri);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        free(rml_uri);
        return ORTE_ERROR;
    }
    free(rml_uri);

#if OPAL_HAVE_HWLOC
    {
        char *locale;

        /* provide the locality info */
        if (NULL != opal_hwloc_topology) {
            /* our cpuset should already be known, but check for safety */
            if (NULL == opal_hwloc_my_cpuset) {
                opal_hwloc_base_get_local_cpuset();
            }
            /* convert to a string */
            hwloc_bitmap_list_asprintf(&locale, opal_hwloc_my_cpuset);
            OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                                 "%s grpcomm:pmi LOCALE %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), locale));
            /*  get the key */
            if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "HWLOC"))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            /* enter the key-value */
            rc = PMI_KVS_Put(pmi_kvs_name, pmi_kvs_key, locale);
            if (PMI_SUCCESS != rc) {
                ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
                free(locale);
                return ORTE_ERROR;
            }
            free(locale);
        }
    }
#endif

    /* get the job map for this job */
    jmap = (orte_jmap_t*)opal_pointer_array_get_item(&orte_jobmap, 0);
    /* get my pidmap entry */
    pmap = (orte_pmap_t*)opal_pointer_array_get_item(&jmap->pmap, ORTE_PROC_MY_NAME->vpid);

    /* add our local/node rank info */
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "LOCALRANK"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    snprintf(val, 64, "%lu", (unsigned long)pmap->local_rank);
    rc = PMI_KVS_Put(pmi_kvs_name, pmi_kvs_key, val);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, "NODERANK"))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    snprintf(val, 64, "%lu", (unsigned long)pmap->node_rank);
    rc = PMI_KVS_Put(pmi_kvs_name, pmi_kvs_key, val);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
        return ORTE_ERROR;
    }

    /* commit our modex info */
    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Commit failed");
        return ORTE_ERROR;
    }

    /* Barrier here to ensure all other procs have committed */
    if (ORTE_SUCCESS != (rc = pmi_barrier())) {
        return rc;
    }

    /* harvest the oob endpoint info and hostname for all other procs
     * in our job so oob wireup can be completed and we
     * can setup their nidmap/pidmap
     */
    name.jobid = ORTE_PROC_MY_NAME->jobid;
    orte_process_info.num_nodes = 1; /* have to account for mine! */
    for (v=0; v < orte_process_info.num_procs; v++) {
        if (v == ORTE_PROC_MY_NAME->vpid) {
            continue;
        }
        name.vpid = v;
        if (ORTE_SUCCESS != (rc = setup_key(&name, "RMLURI"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = PMI_KVS_Get(pmi_kvs_name, pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:pmi: proc %s oob endpoint %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), pmi_attr_val));
        /* set the contact info into the hash table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(pmi_attr_val))) {
            return rc;
        }
        if (ORTE_SUCCESS != (rc = setup_key(&name, "HOSTNAME"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = PMI_KVS_Get(pmi_kvs_name, pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:pmi: proc %s location %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), pmi_attr_val));
        /* see if this node is already in nidmap */
        loc = NULL;
        for (i=0; i < orte_nidmap.size; i++) {
            if (NULL == (nid = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, i))) {
                continue;
            }
            if (0 == strcmp(pmi_attr_val, nid->name)) {
                /* found it */
                loc = nid;
                break;
            }
        }
        if (NULL == loc) {
            /* new node - save it */
            loc = OBJ_NEW(orte_nid_t);
            loc->name = strdup(pmi_attr_val);
            loc->index = opal_pointer_array_add(&orte_nidmap, loc);
            loc->daemon = loc->index;
            /* keep track */
            orte_process_info.num_nodes++;
        }
        /* see if this proc is already in the pidmap */
        if (NULL == opal_pointer_array_get_item(&jmap->pmap, v)) {
            /* nope - add it */
            pmap = OBJ_NEW(orte_pmap_t);
            pmap->node = loc->index;
            if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(&jmap->pmap, v, pmap))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }
        /* get the proc's local/node rank info */
        if (ORTE_SUCCESS != (rc = setup_key(&name, "LOCALRANK"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = PMI_KVS_Get(pmi_kvs_name, pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        pmap->local_rank = (uint16_t)strtoul(pmi_attr_val, NULL, 10);
        if (ORTE_SUCCESS != (rc = setup_key(&name, "NODERANK"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = PMI_KVS_Get(pmi_kvs_name, pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        if (PMI_SUCCESS != rc) {
            ORTE_PMI_ERROR(rc, "PMI_KVS_Get");
            return ORTE_ERROR;
        }
        pmap->node_rank = (uint16_t)strtoul(pmi_attr_val, NULL, 10);
        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:pmi: proc %s lrank %u nrank %u",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name),
                             (unsigned int)pmap->local_rank,
                             (unsigned int)pmap->node_rank));
#if OPAL_HAVE_HWLOC
        /* get the proc's locality info, if available */
        if (ORTE_SUCCESS != (rc = setup_key(&name, "HWLOC"))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
        rc = PMI_KVS_Get(pmi_kvs_name, pmi_kvs_key, pmi_attr_val, pmi_vallen_max);
        /* don't error out here - if not found, that's okay */
        if (PMI_SUCCESS == rc) {
            if (OPAL_EQUAL == orte_util_compare_name_fields(ORTE_NS_CMP_ALL, &name, ORTE_PROC_MY_NAME)) {
                /* if this data is from myself, then set locality to all */
                OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                                     "%s grpcomm:pmi setting proc %s locale ALL",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name)));
                pmap->locality = OPAL_PROC_ALL_LOCAL;
            } else if (loc->daemon != ORTE_PROC_MY_DAEMON->vpid) {
                /* this is on a different node, then mark as non-local */
                OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                                     "%s grpcomm:pmi setting proc %s locale NONLOCAL",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name)));
                pmap->locality = OPAL_PROC_NON_LOCAL;
            } else if (0 == strlen(pmi_attr_val)){
                /* if we share a node, but we don't know anything more, then
                 * mark us as on the node as this is all we know
                 */
                OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                                     "%s grpcomm:pmi setting proc %s locale NODE",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name)));
                pmap->locality = OPAL_PROC_ON_NODE;
            } else {
                /* convert the locale to a cpuset */
                if (NULL == orte_grpcomm_base.working_cpuset) {
                    orte_grpcomm_base.working_cpuset = hwloc_bitmap_alloc();
                }
                if (0 != hwloc_bitmap_list_sscanf(orte_grpcomm_base.working_cpuset, pmi_attr_val)) {
                    /* got a bad locale */
                    ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
                    return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
                }
                /* determine relative location on our node */
                pmap->locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
                                                                       opal_hwloc_my_cpuset,
                                                                       orte_grpcomm_base.working_cpuset);
                OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                                     "%s grpcommpmi setting proc %s locale %04x",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(&name), pmap->locality));
            }
        }
#endif
    }

    /* cycle thru the array of our peers and assign local and node ranks */

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
static int pmi_encode(const void *val, size_t vallen) {
    static unsigned char encodings[] = {'0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'};
    size_t i;

    /* check for size */
    if ((size_t)pmi_vallen_max < ((vallen * 2) + 1)) {
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }
    for (i = 0; i < vallen; i++) {
        pmi_attr_val[2 * i] = encodings[((unsigned char *)val)[i] & 0xf];
        pmi_attr_val[2 * i + 1] = encodings[((unsigned char *)val)[i] >> 4];
    }
    pmi_attr_val[vallen * 2] = '\0';
    return ORTE_SUCCESS;
}

static void* pmi_decode(size_t *retlen) {
    unsigned char *ret, *val;
    size_t i;
    *retlen = strlen(pmi_attr_val)/2;

    ret = malloc(*retlen);
    if (NULL == ret) {
        return ret;
    }

    val = (unsigned char*)pmi_attr_val;
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

/* useful util */
static char* pmi_error(int pmi_err)
{
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

static int setup_pmi(void)
{
    int max_length, rc;

    rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_Get_value_length_max");
        return ORTE_ERROR;
    }
    pmi_attr_val = malloc(pmi_vallen_max);
    if (NULL == pmi_attr_val) {
        return ORTE_ERR_OUT_OF_RESOURCE;
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

    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&pmi_keylen_max))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
        return ORTE_ERROR;
    }
    pmi_kvs_key = malloc(pmi_keylen_max);

    return ORTE_SUCCESS;
}

static int setup_key(const orte_process_name_t *name, const char *key)
{
    if (pmi_keylen_max <= snprintf(pmi_kvs_key, pmi_keylen_max,
                                   "%s-%s", ORTE_NAME_PRINT(name), key)) {
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }

    return ORTE_SUCCESS;
}

