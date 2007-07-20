#include "orte_config.h"

#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/base/rml_contact.h"

#include "opal/util/argv.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "orte/orte_types.h"
#include "opal/class/opal_hash_table.h"
#include "orte/class/orte_proc_table.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/oob/tcp/oob_tcp.h" /* BWB - FIX ME */

extern opal_list_t       orte_rml_base_subscriptions;

struct orte_rml_base_subscription_t {
    opal_list_item_t item;
    orte_jobid_t jobid;
    orte_gpr_subscription_id_t subid;
};
typedef struct orte_rml_base_subscription_t orte_rml_base_subscription_t;
OBJ_CLASS_INSTANCE(orte_rml_base_subscription_t, opal_list_item_t,
                   NULL, NULL);

static int get_contact_info(orte_jobid_t job, char **tokens, orte_gpr_notify_data_t **data)
{
    char *segment;
    char *keys[] = {
        ORTE_OOB_TCP_KEY,
        ORTE_PROC_RML_IP_ADDRESS_KEY,
        NULL
    };
    orte_gpr_value_t **values;
    orte_std_cntr_t cnt, i, idx;
    int rc;
    
    /* define the segment */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    
    /* get the data */
    if (ORTE_SUCCESS != (rc = orte_gpr.get(ORTE_GPR_TOKENS_AND | ORTE_GPR_KEYS_OR,
                                           segment, tokens, keys, &cnt, &values))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        return rc;
    }
    
    /* see if we got data back */
    if (0 < cnt) {
        /* build the data into the notify_data object. If the data
         * pointer is NULL, then we are the first values, so initialize
         * it. Otherwise, just add the data to it
         */
        if (NULL == *data) {
            *data = OBJ_NEW(orte_gpr_notify_data_t);
        }
        for (i=0; i < cnt; i++) {
            if (ORTE_SUCCESS != (rc = orte_pointer_array_add(&idx, (*data)->values, (void*)values[i]))) {
                ORTE_ERROR_LOG(rc);
                return rc;
            }
            ++(*data)->cnt;
        }
    }
    

    return ORTE_SUCCESS;
}


int
orte_rml_base_get_contact_info(orte_process_name_t *name, 
                               orte_gpr_notify_data_t **data)
{
    char **tokens=NULL;
    orte_std_cntr_t num_tokens;
    int rc;
    
    /* if the vpid is WILDCARD, then we want the info from all procs in the specified job. This
     * is the default condition, so do nothing for this case. If the vpid is not WILDCARD,
     * then go get the process tokens
     */
    if (ORTE_VPID_WILDCARD != name->vpid) {
        if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens, name))) {
            ORTE_ERROR_LOG(rc);
            return rc;
        }
    }
    
    /* If the jobid is not WILDCARD, then we only want the info from the specified job -
     * this is the most common case, so treat it first
     */
    if (ORTE_JOBID_WILDCARD != name->jobid) {
        if (ORTE_SUCCESS != (rc = get_contact_info(name->jobid, tokens, data))) {
            ORTE_ERROR_LOG(rc);
        }
        return rc;
    }
    
    /* if the jobid is WILDCARD, then we want the info from all jobs. */
    
    return ORTE_SUCCESS;
}


int
orte_rml_base_register_subscription(orte_jobid_t jobid, char *trigger)
{
    char *sub_name, *segment, *trig_name;
    orte_rml_base_subscription_t *subscription;
    orte_gpr_subscription_id_t sub_id;
    int rc;
    
    /* register subscribe callback to receive notification when all processes have registered */
    subscription = OBJ_NEW(orte_rml_base_subscription_t);
    subscription->jobid = jobid;
    opal_list_append(&orte_rml_base_subscriptions, &subscription->item);

    if (ORTE_SUCCESS != (rc = orte_schema.get_std_subscription_name(&sub_name,
                                ORTE_OOB_SUBSCRIPTION, jobid))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }

    /* attach to the specified trigger */
    if (ORTE_SUCCESS != (rc = orte_schema.get_std_trigger_name(&trig_name,
                                    trigger, jobid))) {
        ORTE_ERROR_LOG(rc);
        free(sub_name);
        return rc;
    }

    /* define the segment */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, jobid))) {
        ORTE_ERROR_LOG(rc);
        free(sub_name);
        free(trig_name);
        return rc;
    }

    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe_1(&sub_id, trig_name, sub_name,
                                         ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG,
                                         ORTE_GPR_KEYS_OR | ORTE_GPR_TOKENS_OR | ORTE_GPR_STRIPPED,
                                         segment,
                                         NULL,  /* look at all containers on this segment */
                                         ORTE_OOB_TCP_KEY,
                                         orte_rml_base_contact_info_notify, NULL))) {
        ORTE_ERROR_LOG(rc);
        free(sub_name);
        free(trig_name);
        free(segment);
        return rc;
    }


    /* the id of each subscription is recorded
     * here so we can (if desired) cancel that subscription later
     */
    subscription->subid = sub_id;
    /* done with these, so release any memory */
    free(trig_name);
    free(sub_name);

    return ORTE_SUCCESS;    
}


int
orte_rml_base_register_contact_info(void)
{
    orte_std_cntr_t i, num_tokens;
    orte_data_value_t *values[2];
    char *tmp, *tmp2, *tmp3;
    char *segment, **tokens;
    char *keys[] = { ORTE_OOB_TCP_KEY, ORTE_PROC_RML_IP_ADDRESS_KEY};
    int rc;
    
    /* setup to put our contact info on registry */
    tmp = orte_rml.get_contact_info();
    values[0] = OBJ_NEW(orte_data_value_t);
    if (NULL == values[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    values[0]->type = ORTE_STRING;
    values[0]->data = strdup(tmp);
    free(tmp);

    /* setup the IP address for storage */
    tmp = orte_rml.get_contact_info();
    tmp2 = strrchr(tmp, '/') + 1;
    tmp3 = strrchr(tmp, ':');
    if(NULL == tmp2 || NULL == tmp3) {
        opal_output(0, "[%lu,%lu,%lu] orte_rml_base_init: invalid address \'%s\' "
                    "returned for selected oob interfaces.\n",
                    ORTE_NAME_ARGS(orte_process_info.my_name), tmp);
        ORTE_ERROR_LOG(ORTE_ERROR);
        free(tmp);
        return ORTE_ERROR;
    }
    *tmp3 = '\0';
    values[1] = OBJ_NEW(orte_data_value_t);
    if (NULL == values[1]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    values[1]->type = ORTE_STRING;
    values[1]->data = strdup(tmp2);
    free(tmp);

    /* define the segment */
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, ORTE_PROC_MY_NAME->jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        OBJ_RELEASE(values[1]);
        return rc;
    }
        
    /* get the process tokens */
    if (ORTE_SUCCESS != (rc = orte_schema.get_proc_tokens(&tokens, &num_tokens,
                                    orte_process_info.my_name))) {
        ORTE_ERROR_LOG(rc);
        free(segment);
        OBJ_RELEASE(values[0]);
        OBJ_RELEASE(values[1]);
        return rc;
    }

    /* put our contact info in registry */
    if (ORTE_SUCCESS != (rc = orte_gpr.put_N(ORTE_GPR_OVERWRITE | ORTE_GPR_TOKENS_XAND,
                                        segment, tokens, 2, keys, values))) {
        ORTE_ERROR_LOG(rc);
    }

    free(segment);
    for(i=0; i < num_tokens; i++) {
        free(tokens[i]);
        tokens[i] = NULL;
    }
    if (NULL != tokens) free(tokens);
    OBJ_RELEASE(values[0]);
    OBJ_RELEASE(values[1]);

    return rc;
}


void
orte_rml_base_contact_info_notify(orte_gpr_notify_data_t* data,
                                  void* cbdata)
{
    orte_std_cntr_t i, j, k;
    orte_gpr_value_t **values, *value;
    orte_gpr_keyval_t *keyval;
    char *contact_info;

    /* process the callback */
    values = (orte_gpr_value_t**)(data->values)->addr;
    for(i = 0, k=0; k < data->cnt &&
                    i < (data->values)->size; i++) {
        if (NULL != values[i]) {
            k++;
            value = values[i];
            for(j = 0; j < value->cnt; j++) {

                /* check to make sure this is the requested key */
                keyval = value->keyvals[j];
                if(strcmp(keyval->key, ORTE_OOB_TCP_KEY) != 0)
                    continue;
                orte_dss.get((void**)&(contact_info), keyval->value, ORTE_STRING);
                orte_rml.set_contact_info(contact_info);
            }
        }
    }
}




int
orte_rml_base_parse_uris(const char* uri,
                         orte_process_name_t* peer, 
                         char*** uris)
{
    orte_process_name_t* proc_name;
    int rc;

    /* parse the process name */
    char* cinfo = strdup(uri);
    char* ptr = strchr(cinfo, ';');
    if(NULL == ptr) {
        ORTE_ERROR_LOG(ORTE_ERR_BAD_PARAM);
        free(cinfo);
        return ORTE_ERR_BAD_PARAM;
    }
    *ptr = '\0';
    ptr++;
    if (ORTE_SUCCESS != (rc = orte_ns.convert_string_to_process_name(&proc_name, cinfo))) {
        ORTE_ERROR_LOG(rc);
        free(cinfo);
        return rc;
    }
    *peer = *proc_name;
    free(proc_name);

    if (NULL != uris) {
	/* parse the remainder of the string into an array of uris */
	*uris = opal_argv_split(ptr, ';');
    }
    free(cinfo);
    return ORTE_SUCCESS;
}
