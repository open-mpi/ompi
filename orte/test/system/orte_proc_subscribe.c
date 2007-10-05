/* -*- C -*-
 *
 * $HEADER$
 *
 * The most basic of MPI applications
 */

#include <stdio.h>

#include "opal/threads/condition.h"
#include "opal/util/argv.h"

#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "orte/dss/dss.h"
#include "orte/mca/gpr/gpr.h"
#include "orte/mca/rmgr/rmgr.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmaps/rmaps_types.h"

bool waitexit;
opal_mutex_t lock;
opal_condition_t cond;

static int orte_subscribe_proc(orte_jobid_t job);
static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state);

int main(int argc, char* argv[])
{
    int rc;
    orte_proc_state_t cb_states;
    orte_app_context_t *app;
    orte_jobid_t job;
    opal_list_t attributes;
    opal_list_item_t *item;
    uint8_t flow;

    OBJ_CONSTRUCT(&lock, opal_mutex_t);
    OBJ_CONSTRUCT(&cond, opal_condition_t);
    waitexit = false;

    if (0 > (rc = orte_init(ORTE_NON_INFRASTRUCTURE))) {
        fprintf(stderr, "couldn't init orte - error code %d\n", rc);
        return rc;
    }

    /* create an app_context */
    app = OBJ_NEW(orte_app_context_t);
    app->app = strdup("hostname");
    opal_argv_append_nosize(&app->argv, "hostname");
    app->num_procs = 3;
    app->cwd = strdup("/private/tmp");
    
    /* setup the job through the setup trigs stage, but don't launch. We need to
     * do this so we can
     * setup the subscription that will return pids and other info to us
     * when all procs achieve LAUNCHED state. We have to go through the MAP
     * stage so that we can handle orterun's that don't specify the number
     * of procs. For that case, the num_procs in the registry isn't set until the
     * MAP stage is completed.
     */
    OBJ_CONSTRUCT(&attributes, opal_list_t);
    flow = ORTE_RMGR_SETUP | ORTE_RMGR_ALLOC | ORTE_RMGR_MAP | ORTE_RMGR_SETUP_TRIGS;
    orte_rmgr.add_attribute(&attributes, ORTE_RMGR_SPAWN_FLOW, ORTE_UINT8, (void*)&flow, ORTE_RMGR_ATTR_OVERRIDE);
    orte_rmgr.add_attribute(&attributes, ORTE_RMAPS_DISPLAY_AFTER_MAP, ORTE_UNDEF, NULL, ORTE_RMGR_ATTR_OVERRIDE);

    cb_states = ORTE_PROC_STATE_TERMINATED;
    rc = orte_rmgr.spawn_job(&app, 1, &job, 0, NULL, job_state_callback, cb_states, &attributes);

    while (NULL != (item = opal_list_remove_first(&attributes))) OBJ_RELEASE(item);

    /* now that all the info is on the registry, we can setup our subscription */
    orte_subscribe_proc(job);
    
    orte_gpr.dump_local_triggers();
    
    /* and now we can go ahead and actually launch! */
    flow = ORTE_RMGR_LAUNCH;
    orte_rmgr.add_attribute(&attributes, ORTE_RMGR_SPAWN_FLOW, ORTE_UINT8, (void*)&flow, ORTE_RMGR_ATTR_OVERRIDE);
    
    rc = orte_rmgr.spawn_job(&app, 1, &job, 0, NULL, NULL, 0, &attributes);

    /* Wait for the app to complete */
    
    OPAL_THREAD_LOCK(&lock);
    while (!waitexit) {
        opal_condition_wait(&cond, &lock);
    }

    /* All done */
    OBJ_RELEASE(app);
    while (NULL != (item = opal_list_remove_first(&attributes))) OBJ_RELEASE(item);
    OBJ_DESTRUCT(&attributes);

    orte_finalize();
    return 0;
}

static void job_state_callback(orte_jobid_t jobid, orte_proc_state_t state)
{    
    OPAL_THREAD_LOCK(&lock);
    
    waitexit = true;
    opal_condition_signal(&cond);
    
    OPAL_THREAD_UNLOCK(&lock);
}

static void eclipse_launch_cb(orte_gpr_notify_data_t *notify_data, void *user_tag)
{
#if 0
	int						len;
	orte_std_cntr_t			i;
	orte_std_cntr_t			j;
	orte_std_cntr_t			k;
	orte_gpr_value_t **		values;
	orte_gpr_value_t *		value;
	orte_gpr_keyval_t **	keyvals;
	char *					str1;
	char *					str2;
	char *					res;
	char *					kv = NULL;
	char *					vpid = NULL;
#endif
	
    fprintf(stderr, "launch cb entered\n");
    orte_dss.dump(0, notify_data, ORTE_GPR_NOTIFY_DATA);
    
#if 0
	values = (orte_gpr_value_t**)(data->values)->addr;
	
	for(i=0, k=0; k<data->cnt && i < (data->values)->size; i++) {
		if(values[i] == NULL) continue;
		
		k++;
		value = values[i];
		keyvals = value->keyvals;
		
		len = strlen(ORTE_VPID_KEY);
		
		if (strlen(value->tokens[1]) <= len
			|| strncmp(value->tokens[1], ORTE_VPID_KEY, len) != 0)
			continue;
        
		asprintf(&vpid, "%s", value->tokens[1]+len+1);
		
		for(j=0; j<value->cnt; j++) {
			orte_gpr_keyval_t *keyval = keyvals[j];
			char *external_key = NULL;
			char * tmp_str = NULL;
            
			if (!strcmp(keyval->key, ORTE_NODE_NAME_KEY))
				asprintf(&external_key, "%s", ATTRIB_PROCESS_NODE_NAME);
			else if (!strcmp(keyval->key, ORTE_PROC_LOCAL_PID_KEY))
				asprintf(&external_key, "%s", ATTRIB_PROCESS_PID);
			else
				external_key = strdup(keyval->key);
            
			if (external_key != NULL) {					
				switch(ORTE_KEYVALUE_TYPE(keyval)) {
					case ORTE_STRING:
						if ((tmp_str = ORTE_GET_STRING_VALUE(keyval)) != NULL);
                        asprintf(&kv, "%s=%s", external_key, tmp_str);
						break;
					case ORTE_UINT32:
						asprintf(&kv, "%s=%d", external_key, ORTE_GET_UINT32_VALUE 
                                 (keyval));
						break;
					case ORTE_PID:
						asprintf(&kv, "%s=%d", external_key, ORTE_GET_PID_VALUE(keyval));
						break;
					default:
						asprintf(&kv, "%s=<unknown type>%d", external_key,  
                                 ORTE_KEYVALUE_TYPE(keyval));
						break;
				}
                
				if (kv != NULL) {
					if (job != NULL) {
						proxy_cstring_to_str("", &str1);
						proxy_cstring_to_str(kv, &str2);
						asprintf(&res, "%d %d 0:0 %s 1 %s %s", RTEV_PATTR, job- 
                                 >ptp_jobid, str1, vpid, str2);
			        	AddToList(eventList, (void *)res);
			        	free(str1);
			        	free(str2);
					}
					free(kv);
					kv = NULL;
				}
				
				free(external_key);
	        }
		}
		
		free(vpid);
	}
#endif
    
    return;
}

/*
 * Subscribe to attribute changes for 'procid' in 'job'.
 */
static int
orte_subscribe_proc(orte_jobid_t job)
{
	int rc;
    char *segment;
    orte_gpr_subscription_t *subs, sub = ORTE_GPR_SUBSCRIPTION_EMPTY;
    orte_gpr_trigger_t *trigs, trig = ORTE_GPR_TRIGGER_EMPTY;
    orte_gpr_value_t *values[1];
	
    subs = &sub;
    if (ORTE_SUCCESS !=
        (rc = orte_schema.get_std_subscription_name(&sub.name, "eclipse-launch-sub", job))) {
        ORTE_ERROR_LOG(rc);
        return rc;
    }
    sub.action = ORTE_GPR_NOTIFY_DELETE_AFTER_TRIG;
    sub.values = values;
    sub.cnt = 1;
    
    if (ORTE_SUCCESS != (rc = orte_schema.get_job_segment_name(&segment, job))) {
        ORTE_ERROR_LOG(rc);
        free(sub.name);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_value(&(values[0]), ORTE_GPR_TOKENS_OR | ORTE_GPR_KEYS_OR,
                                                    segment, 3, 0))) {
        ORTE_ERROR_LOG(rc);
        free(sub.name);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[0]), ORTE_NODE_NAME_KEY, ORTE_UNDEF, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(sub.name);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[1]), ORTE_PROC_LOCAL_PID_KEY, ORTE_UNDEF, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(sub.name);
        return rc;
    }
    if (ORTE_SUCCESS != (rc = orte_gpr.create_keyval(&(values[0]->keyvals[2]), ORTE_PROC_NAME_KEY, ORTE_UNDEF, NULL))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(sub.name);
        return rc;
    }
    sub.cbfunc = eclipse_launch_cb;
    
    /* attach ourselves to the standard launched trigger */
    trigs = &trig;
    if (ORTE_SUCCESS !=
        (rc = orte_schema.get_std_trigger_name(&trig.name, ORTE_ALL_LAUNCHED_TRIGGER, job))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(values[0]);
        free(sub.name);
        return rc;
    }
    
    if (ORTE_SUCCESS != (rc = orte_gpr.subscribe(1, &subs, 1, &trigs))) {
        ORTE_ERROR_LOG(rc);
    }
    OBJ_RELEASE(values[0]);
    free(sub.name);
    free(trig.name);
    
    return 0;
}
