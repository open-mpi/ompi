/* -*- Mode: C; c-basic-offset:4 ; -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2012 Los Alamos National Security, LLC. All
 *                         rights reserved.
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
#if WANT_CRAY_PMI2_EXT
#include <pmi2.h>
#endif

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
static int pmi_allgather(orte_grpcomm_collective_t *coll);
static int pmi_barrier(orte_grpcomm_collective_t *coll);
static int pmi_set_proc_attr(const char* attr_name, 
                             const void *buffer, size_t size);
static int pmi_get_proc_attr(const orte_process_name_t name,
                             const char* attr_name,
                             void **buffer, size_t *size);
static int modex(orte_grpcomm_collective_t *coll);
static int purge_proc_attrs(void);

/* Module def */
orte_grpcomm_base_module_t orte_grpcomm_pmi_module = {
    init,
    finalize,
    xcast,
    pmi_allgather,
    pmi_barrier,
    pmi_set_proc_attr,
    pmi_get_proc_attr,
    modex,
    purge_proc_attrs
};

static int pmi_encode(const void *val, size_t vallen);
static void* pmi_decode(size_t *retlen);
static int setup_pmi(void);
static int setup_key(const orte_process_name_t *name, const char *key);

/* Local variables */
static char *pmi_packed_data = NULL;
static char *pmi_kvs_name = NULL;
static char *pmi_kvs_key = NULL;
static char *pmi_attr_val = NULL;
static int pmi_vallen_max = -1;
static int pmi_keylen_max = -1;
static int pmi_pack_key = 0;
static int pmi_packed_data_off = 0;

/* Because Cray uses PMI2 extensions for some, but not all,
 * PMI functions, we define a set of wrappers for those
 * common functions we will use
 */
static int kvs_put(const char *key, const char *value)
{
#if WANT_CRAY_PMI2_EXT
    return PMI2_KVS_Put(key, value);
#else
    return PMI_KVS_Put(pmi_kvs_name, key, value);
#endif
}

static int kvs_get(const char *key, char *value, int valuelen)
{
#if WANT_CRAY_PMI2_EXT
    int len;

    return PMI2_KVS_Get(pmi_kvs_name, PMI2_ID_NULL, key, value, valuelen, &len);
#else
    return PMI_KVS_Get(pmi_kvs_name, key, value, valuelen);
#endif
}

static int kvs_commit(void)
{
#if WANT_CRAY_PMI2_EXT
    return PMI2_KVS_Fence();
#else
    int rc;

    if (PMI_SUCCESS != (rc = PMI_KVS_Commit(pmi_kvs_name))) {
        return rc;
    }
    /* Barrier here to ensure all other procs have committed */
    return PMI_Barrier();
#endif
}

/**
 * Initialize the module
 */
static int init(void)
{
    int rc;

    if (ORTE_SUCCESS != (rc = setup_pmi())) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    return ORTE_SUCCESS;
}

/**
 * Finalize the module
 */
static void finalize(void)
{
    if (NULL != pmi_packed_data) {
	free(pmi_packed_data);
	pmi_packed_data = NULL;
    }
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

static int pmi_barrier(orte_grpcomm_collective_t *coll)
{
    int rc;
    
    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi entering barrier",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    
    /* if I am alone, just execute the callback */
    if (1 == orte_process_info.num_procs) {
        OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                             "%s grpcomm:pmi:barrier only one proc",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
        coll->active = false;
        if (NULL != coll->cbfunc) {
            coll->cbfunc(NULL, coll->cbdata);
        }
        return ORTE_SUCCESS;
    }
    
#if WANT_CRAY_PMI2_EXT
    /* Cray doesn't provide a barrier, so use the Fence function here */
    if (PMI_SUCCESS != (rc = PMI2_KVS_Fence())) {
        ORTE_PMI_ERROR(rc, "PMI2_KVS_Fence");
        return ORTE_ERROR;
    }
#else
    /* use the PMI barrier function */
    if (PMI_SUCCESS != (rc = PMI_Barrier())) {
        ORTE_PMI_ERROR(rc, "PMI_Barrier");
        return ORTE_ERROR;
    }
#endif

    OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                         "%s grpcomm:pmi barrier complete",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
    /* execute the callback */
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }

    return ORTE_SUCCESS;
}

static int pmi_allgather(orte_grpcomm_collective_t *coll)
{
    /* not used in this implementation */
    return ORTE_ERR_NOT_SUPPORTED;
}

static int pmi_put_last_key (void) {
    char tmp_key[32];
    int rc;

    if (pmi_packed_data_off == 0) {
	/* nothing to write */
	return ORTE_SUCCESS;
    }

    sprintf (tmp_key, "key%d", pmi_pack_key);
	
    if (ORTE_SUCCESS != (rc = setup_key(ORTE_PROC_MY_NAME, tmp_key))) {
	ORTE_ERROR_LOG(rc);
	return rc;
    }

    rc = kvs_put(pmi_kvs_key, pmi_packed_data);
    if (PMI_SUCCESS != rc) {
	ORTE_PMI_ERROR(rc, "PMI_KVS_Put");
	return ORTE_ERROR;
    }

    pmi_packed_data_off = 0;
    pmi_pack_key ++;

    return ORTE_SUCCESS;
}

static int pmi_set_proc_attr(const char *attr_name, 
                             const void *buffer, size_t size)
{
    int rc;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: set attr %s of size %lu in KVS %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
                         (unsigned long)size, pmi_kvs_name));
    
    if (ORTE_SUCCESS != (rc = pmi_encode(buffer, size))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    if ((int)(pmi_packed_data_off + strlen (attr_name) + strlen (pmi_attr_val) + 2) > pmi_vallen_max) {
	pmi_put_last_key ();
    }

    /* pack attribute */
    pmi_packed_data_off += sprintf (pmi_packed_data + pmi_packed_data_off, "%s%s:%s",
				    pmi_packed_data_off ? "," : "", attr_name, pmi_attr_val);

    return ORTE_SUCCESS;
}

static int pmi_get_proc_attr(const orte_process_name_t name,
                             const char* attr_name,
                             void **buffer, size_t *size)
{
    char tmp_val[1024];
    char *tmp, *tok_ctx, *tmp2;
    int remote_key;
    int rc;

    /* set default */
    *size = 0;
    *buffer = NULL;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
			 "%s grpcomm:pmi: get attr %s for proc %s in KVS %s",
			 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), attr_name,
			 ORTE_NAME_PRINT(&name), pmi_kvs_name));

    for (remote_key = 0 ; ; ++remote_key) {
	char tmp_key[32];

	sprintf (tmp_key, "key%d", remote_key);

	if (ORTE_SUCCESS != (rc = setup_key(&name, tmp_key))) {
	    ORTE_ERROR_LOG(rc);
	    return rc;
	}

	rc = kvs_get(pmi_kvs_key, tmp_val, pmi_vallen_max);
	if (PMI_SUCCESS != rc) {
	    break;
	}

	tmp = strtok_r (tmp_val, ",", &tok_ctx);

	do {
	    tmp2 = strchr (tmp, ':');
	    if (NULL == tmp2) {
		continue;
	    }

	    *tmp2 = '\0';

	    if (strcmp (tmp, attr_name) == 0) {
		strcpy (pmi_attr_val, tmp2 + 1);
		*buffer = pmi_decode(size);

		if (NULL == *buffer) {
		    return ORTE_ERR_OUT_OF_RESOURCE;
		}

		break;
	    }
	} while (NULL != (tmp = strtok_r (NULL, ",", &tok_ctx)));

	if (NULL != *buffer) {
	    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
				 "%s grpcomm:pmi: got attr %s of size %lu",
				 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
				 attr_name, (unsigned long)(*size)));
	    return ORTE_SUCCESS;
	}
    }

    return ORTE_ERROR;
}

/***   MODEX SECTION ***/
static int modex(orte_grpcomm_collective_t *coll)
{
    int rc, i;
    size_t len;
    char *rml_uri;
    orte_vpid_t v;
    orte_process_name_t name;
    orte_jmap_t *jmap;
    orte_nid_t *nid, *loc;
    orte_pmap_t *pmap;
    void *tmp_val;

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: modex entered",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* provide our hostname so others can know our location */
    if (strlen(orte_process_info.nodename) > (size_t)pmi_vallen_max) {
        ORTE_ERROR_LOG(ORTE_ERR_VALUE_OUT_OF_BOUNDS);
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }


    rc = pmi_set_proc_attr ("HOSTNAME", orte_process_info.nodename, strlen(orte_process_info.nodename));
    if (ORTE_SUCCESS != rc) {
	return rc;
    }

    /* add our oob endpoint info so that oob communications
     * can be supported
     */
    rml_uri = orte_rml.get_contact_info();
    rc = pmi_set_proc_attr ("RMLURI", rml_uri, strlen (rml_uri));
    if (ORTE_SUCCESS != rc) {
	return rc;
    }
    free(rml_uri);

#if OPAL_HAVE_HWLOC
    rc = pmi_set_proc_attr ("BIND_LEVEL", &orte_process_info.bind_level, sizeof (orte_process_info.bind_level));
    if (ORTE_SUCCESS != rc) {
	return rc;
    }

    rc = pmi_set_proc_attr ("BIND_IDX", &orte_process_info.bind_idx, sizeof (orte_process_info.bind_idx));
    if (ORTE_SUCCESS != rc) {
	return rc;
    }
#endif

    /* get the job map for this job */
    jmap = (orte_jmap_t*)opal_pointer_array_get_item(&orte_jobmap, 0);
    /* get my pidmap entry */
    pmap = (orte_pmap_t*)opal_pointer_array_get_item(&jmap->pmap, ORTE_PROC_MY_NAME->vpid);

    rc = pmi_set_proc_attr ("LOCALRANK", &pmap->local_rank, sizeof (pmap->local_rank));
    if (ORTE_SUCCESS != rc) {
	return rc;
    }
    rc = pmi_set_proc_attr ("NODERANK", &pmap->node_rank, sizeof (pmap->node_rank));
    if (ORTE_SUCCESS != rc) {
	return rc;
    }

    rc = pmi_put_last_key ();
    if (ORTE_SUCCESS != rc) {
	return rc;
    }

    /* commit our modex info */
    if (PMI_SUCCESS != (rc = kvs_commit())) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Commit failed");
        return ORTE_ERROR;
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

	rc = pmi_get_proc_attr (name, "RMLURI", (void **) &rml_uri, &len);
	if (ORTE_SUCCESS != rc) {
	    return rc;
	}

        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:pmi: proc %s oob endpoint %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), rml_uri));
        /* set the contact info into the hash table */
        if (ORTE_SUCCESS != (rc = orte_rml.set_contact_info(rml_uri))) {
            free(rml_uri);
            return rc;
        }
        free(rml_uri);

	rc = pmi_get_proc_attr (name, "HOSTNAME", &tmp_val, &len);
	if (ORTE_SUCCESS != rc) {
	    return rc;
	}

        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:pmi: proc %s location %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name), (char *) tmp_val));

        /* see if this node is already in nidmap */
        for (i = 0, loc = NULL; i < orte_nidmap.size; i++) {
            if (NULL == (nid = (orte_nid_t*)opal_pointer_array_get_item(&orte_nidmap, i))) {
                continue;
            }
            if (0 == strcmp(tmp_val, nid->name)) {
                /* found it */
                loc = nid;
		free (tmp_val);
                break;
            }
        }
        if (NULL == loc) {
            /* new node - save it */
            loc = OBJ_NEW(orte_nid_t);
            loc->name = tmp_val;
            loc->index = opal_pointer_array_add(&orte_nidmap, loc);
            loc->daemon = loc->index;
            /* keep track */
            orte_process_info.num_nodes++;
	}

        /* see if this proc is already in the pidmap */
        if (NULL == (pmap = opal_pointer_array_get_item(&jmap->pmap, v))) {
            /* nope - add it */
            pmap = OBJ_NEW(orte_pmap_t);
            pmap->node = loc->index;
            if (ORTE_SUCCESS != (rc = opal_pointer_array_set_item(&jmap->pmap, v, pmap))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
        }

#if OPAL_HAVE_HWLOC
        {
            opal_hwloc_level_t bind_level;
            unsigned int bind_idx;

            /* get the proc's locality info, if available */
	    pmi_get_proc_attr (name, "BIND_LEVEL", &tmp_val, &len);
	    if (ORTE_SUCCESS == rc && 0 < len) {
		assert (len == sizeof (bind_level));
		memmove (&bind_level, tmp_val, len);
		free (tmp_val);
	    }

	    rc = pmi_get_proc_attr (name, "BIND_IDX", &tmp_val, &len);
	    if (ORTE_SUCCESS == rc && 0 < len) {
		assert (len == sizeof (bind_idx));
		memmove (&bind_idx, tmp_val, len);
		free (tmp_val);
	    }

	    if (name.jobid == ORTE_PROC_MY_NAME->jobid &&
		name.vpid == ORTE_PROC_MY_NAME->vpid) {
		/* if this data is from myself, then set locality to all */
		pmap->locality = OPAL_PROC_ALL_LOCAL;
	    } else if (loc->daemon != ORTE_PROC_MY_DAEMON->vpid) {
		/* this is on a different node, then mark as non-local */
		pmap->locality = OPAL_PROC_NON_LOCAL;
	    } else if (0 == len) {
		/* if we share a node, but we don't know anything more, then
		 * mark us as on the node as this is all we know
		 */
		pmap->locality = OPAL_PROC_ON_NODE;
	    } else {
		/* determine relative location on our node */
		pmap->locality = opal_hwloc_base_get_relative_locality(opal_hwloc_topology,
								       orte_process_info.bind_level,
								       orte_process_info.bind_idx,
								       bind_level, bind_idx);
	    }
	    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
				 "%s grpcomm:pmi setting proc %s locale %s",
				 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
				 ORTE_NAME_PRINT(&name),
				 opal_hwloc_base_print_locality(pmap->locality)));
	}
#endif
        /* get the proc's local/node rank info */
	rc = pmi_get_proc_attr (name, "LOCALRANK", &tmp_val, &len);
	if (ORTE_SUCCESS != rc) {
	    return rc;
	}
	assert (len == sizeof (pmap->local_rank));
	memmove (&pmap->local_rank, tmp_val, len);
	free (tmp_val);

	rc = pmi_get_proc_attr (name, "NODERANK", &tmp_val, &len);
	if (ORTE_SUCCESS != rc) {
	    return rc;
	}
	assert (len == sizeof (pmap->node_rank));
	memmove (&pmap->node_rank, tmp_val, len);
	free (tmp_val);

        OPAL_OUTPUT_VERBOSE((2, orte_grpcomm_base.output,
                             "%s grpcomm:pmi: proc %s lrank %u nrank %u",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_NAME_PRINT(&name),
                             (unsigned int)pmap->local_rank,
                             (unsigned int)pmap->node_rank));
    }

    OPAL_OUTPUT_VERBOSE((1, orte_grpcomm_base.output,
                         "%s grpcomm:pmi: modex completed",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));

    /* execute the callback */
    coll->active = false;
    if (NULL != coll->cbfunc) {
        coll->cbfunc(NULL, coll->cbdata);
    }
    return rc;
}

static int purge_proc_attrs(void)
{
    /* nothing to do here */
    return ORTE_SUCCESS;
}

static inline unsigned char pmi_base64_encsym (unsigned char value) {
    assert (value < 64);

    if (value < 26) {
	return 'A' + value;
    } else if (value < 52) {
	return 'a' + (value - 26);
    } else if (value < 62) {
	return '0' + (value - 52);
    }

    return (62 == value) ? '+' : '/';
}

static inline unsigned char pmi_base64_decsym (unsigned char value) {
    if ('+' == value) {
	return 62;
    } else if ('/' == value) {
	return 63;
    } else if (' ' == value) {
	return 64;
    } else if (value <= '9') {
	return (value - '0') + 52;
    } else if (value <= 'Z') {
	return (value - 'A');
    } else if (value <= 'z') {
	return (value - 'a') + 26;
    }

    return 64;
}

static inline void pmi_base64_encode_block (unsigned char in[3], unsigned char out[4], int len) {
    out[0] = pmi_base64_encsym (in[0] >> 2);
    out[1] = pmi_base64_encsym (((in[0] & 0x03) << 4) | ((in[1] & 0xf0) >> 4));
    /* Cray PMI doesn't allow = in PMI attributes so pad with spaces */
    out[2] = 1 < len ? pmi_base64_encsym(((in[1] & 0x0f) << 2) | ((in[2] & 0xc0) >> 6)) : ' ';
    out[3] = 2 < len ? pmi_base64_encsym(in[2] & 0x3f) : ' ';
}

static inline int pmi_base64_decode_block (unsigned char in[4], unsigned char out[3]) {
    char in_dec[4];

    in_dec[0] = pmi_base64_decsym (in[0]);
    in_dec[1] = pmi_base64_decsym (in[1]);
    in_dec[2] = pmi_base64_decsym (in[2]);
    in_dec[3] = pmi_base64_decsym (in[3]);

    out[0] = in_dec[0] << 2 | in_dec[1] >> 4;
    if (64 == in_dec[2]) {
	return 1;
    }

    out[1] = in_dec[1] << 4 | in_dec[2] >> 2;
    if (64 == in_dec[3]) {
	return 2;
    }

    out[2] = ((in_dec[2] << 6) & 0xc0) | in_dec[3];
    return 3;
}


/* PMI only supports strings. For now, do a simple base16 
 * encoding. Should do something smarter, both with the 
 * algorith used and its implementation. */
static int pmi_encode(const void *val, size_t vallen) {
    unsigned char *tmp = (unsigned char *) pmi_attr_val;
    size_t i;

    /* check for size */
    if ((size_t)pmi_vallen_max < (2 + vallen * 4) / 3 + 1) {
        return ORTE_ERR_VALUE_OUT_OF_BOUNDS;
    }

    for (i = 0 ; i < vallen ; i += 3, tmp += 4) {
        pmi_base64_encode_block ((unsigned char *) val + i, tmp, vallen - i);
    }

    tmp[0] = '\0';

    return ORTE_SUCCESS;
}

static void *pmi_decode (size_t *retlen) {
    size_t input_len = strlen (pmi_attr_val) / 4;
    unsigned char *ret, *val;
    int out_len;
    size_t i;

    ret = calloc (1, 3 * input_len + 1);
    if (NULL == ret) {
        return ret;
    }

    val = (unsigned char *) pmi_attr_val;
    for (i = 0, out_len = 0 ; i < input_len ; i++, val += 4) {
	out_len += pmi_base64_decode_block (val, ret + 3 * i);
    }

    ret[out_len] = '\0';

    *retlen = out_len;

    return ret;
}

static int setup_pmi(void)
{
    int max_length, rc;

#if WANT_CRAY_PMI2_EXT
    pmi_vallen_max = PMI2_MAX_VALLEN;
#else
    rc = PMI_KVS_Get_value_length_max(&pmi_vallen_max);
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_Get_value_length_max");
        return ORTE_ERROR;
    }
#endif
    pmi_attr_val = malloc(pmi_vallen_max);
    if (NULL == pmi_attr_val) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

    pmi_packed_data = malloc (pmi_vallen_max);
    if (NULL == pmi_packed_data) {
	return ORTE_ERR_OUT_OF_RESOURCE;
    }

#if WANT_CRAY_PMI2_EXT
    /* TODO -- is this ok */
    max_length = 1024;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_name_length_max(&max_length))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_name_length_max");
        return ORTE_ERROR;
    }
#endif
    pmi_kvs_name = (char*)malloc(max_length);
    if (NULL == pmi_kvs_name) {
        return ORTE_ERR_OUT_OF_RESOURCE;
    }

#if WANT_CRAY_PMI2_EXT
    rc = PMI2_Job_GetId(pmi_kvs_name, max_length);
#else
    rc = PMI_KVS_Get_my_name(pmi_kvs_name,max_length);
#endif
    if (PMI_SUCCESS != rc) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_my_name");
        return ORTE_ERROR;
    }

#if WANT_CRAY_PMI2_EXT
    pmi_keylen_max = PMI2_MAX_KEYLEN;
#else
    if (PMI_SUCCESS != (rc = PMI_KVS_Get_key_length_max(&pmi_keylen_max))) {
        ORTE_PMI_ERROR(rc, "PMI_KVS_Get_key_length_max");
        return ORTE_ERROR;
    }
#endif
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

