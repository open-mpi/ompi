/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
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

#import "orte_config.h"

#import <stdio.h>

#import "orte/mca/pls/base/base.h"
#import "orte/orte_constants.h"
#import "orte/mca/ns/ns.h"
#import "orte/mca/ras/base/ras_base_node.h"
#import "orte/mca/gpr/gpr.h"
#import "orte/mca/rml/rml.h"
#import "opal/util/path.h"

#import "pls_xgrid_client.h"

char **environ;

/**
 * Set the daemons name in the registry.
 */

static int
mca_pls_xgrid_set_node_name(orte_ras_node_t* node, 
			    orte_jobid_t jobid, 
			    orte_process_name_t* name)
{
    orte_gpr_value_t *values[1], *value;
    orte_gpr_keyval_t *kv;
    char* jobid_string;
    size_t i;
    int rc;

    values[0] = OBJ_NEW(orte_gpr_value_t);
    if (NULL == values[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value = values[0];
    value->cnt = 1;
    value->addr_mode = ORTE_GPR_OVERWRITE;
    value->segment = strdup(ORTE_NODE_SEGMENT);
    value->keyvals = (orte_gpr_keyval_t**)malloc(value->cnt * sizeof(orte_gpr_keyval_t*));
    if (NULL == value->keyvals) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    value->keyvals[0] = OBJ_NEW(orte_gpr_keyval_t);
    if (NULL == value->keyvals[0]) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    kv = value->keyvals[0];
    
    if (ORTE_SUCCESS != 
	(rc = orte_ns.convert_jobid_to_string(&jobid_string, jobid))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        return rc;
    }

    if (ORTE_SUCCESS != 
	(rc = orte_schema.get_node_tokens(&(value->tokens), &(value->num_tokens), 
        node->node_cellid, node->node_name))) {
        ORTE_ERROR_LOG(rc);
        OBJ_RELEASE(value);
        free(jobid_string);
        return rc;
    }

    asprintf(&(kv->key), "%s-%s", ORTE_NODE_BOOTPROXY_KEY, jobid_string);
    kv->value = OBJ_NEW(orte_data_value_t);
    if (NULL == kv->value) {
        ORTE_ERROR_LOG(ORTE_ERR_OUT_OF_RESOURCE);
        OBJ_RELEASE(value);
        return ORTE_ERR_OUT_OF_RESOURCE;
    }
    kv->value->type = ORTE_NAME;
    if (ORTE_SUCCESS != (rc = orte_dss.copy(&(kv->value->data), name, ORTE_NAME))) {
        ORTE_ERROR_LOG(rc);
	OBJ_RELEASE(value);
	return rc;
    }

    rc = orte_gpr.put(1, values);
    if(ORTE_SUCCESS != rc) {
        ORTE_ERROR_LOG(rc);
    }

    OBJ_RELEASE(value);

    return rc;
}


@implementation PlsXGridClient

/* init / finalize */
-(id) init
{
    return [self initWithControllerHostname: NULL
		 AndControllerPassword: NULL
		 AndOrted: NULL
		 AndCleanup: 1];
}

-(id) initWithControllerHostname: (char*) hostname
	   AndControllerPassword: (char*) password
			AndOrted: (char*) ortedname
		      AndCleanup: (int) val
{
    if (self = [super init]) {
	/* class-specific initialization goes here */
	OBJ_CONSTRUCT(&state_cond, opal_condition_t);
	OBJ_CONSTRUCT(&state_mutex, opal_mutex_t);

	if (NULL != password) {
	    controller_password = [NSString stringWithCString: password];
	}
	if (NULL != hostname) {
	    controller_hostname = [NSString stringWithCString: hostname];
	}
	cleanup = val;
	if (NULL != ortedname) {
	    orted = [NSString stringWithCString: ortedname];
	}

	active_jobs = [NSMutableDictionary dictionary];
    }
    return self;
}


-(void) dealloc
{
    /* if supposed to clean up jobs, do so */
    if (cleanup) {
	NSArray *keys = [active_jobs allKeys];
	NSEnumerator *enumerator = [keys objectEnumerator];
	NSString *key;
	XGJob *job;
	XGActionMonitor *actionMonitor;

        while (key = [enumerator nextObject]) {
	    job = [grid jobForIdentifier: [active_jobs objectForKey: key]];

	    actionMonitor = [job performDeleteAction];
	    while (XGActionMonitorOutcomeNone == [actionMonitor outcome]) {
		opal_progress();
	    }

	    /* we should have a result - find out if it worked */
	    if (XGActionMonitorOutcomeSuccess != [actionMonitor outcome]) {
		NSError *err = [actionMonitor error];
		fprintf(stderr, "orte:pls:xgrid: cleanup failed: %s\n", 
			[[err localizedFailureReason] cString]);
	    }
	}
    }

    /* need to shut down connection */
    [connection finalize];

    OBJ_DESTRUCT(&state_mutex);
    OBJ_DESTRUCT(&state_cond);

    [super dealloc];
}


/* accessors */
-(NSString*) getOrted
{
    return orted;
}


-(void) setOrtedAsCString: (char*) name
{
    orted = [NSString stringWithCString: name];
}


-(void) setControllerPasswordAsCString: (char*) name
{
    controller_password = [NSString stringWithCString: name];
}


-(void) setControllerHostnameAsCString: (char*) password
{
    controller_hostname = [NSString stringWithCString: password];
}


-(void) setCleanUp: (int) val
{
    cleanup = val;
}


/* interface for launch */
-(int) connect
{
    connection = [[[XGConnection alloc] initWithHostname: controller_hostname
					portnumber:0] autorelease];
    authenticator = [[[XGTwoWayRandomAuthenticator alloc] init] autorelease];

    /* this seems to be hard coded */
    [authenticator setUsername:@"one-xgrid-client"];
    [authenticator setPassword:controller_password];
    
    [connection setAuthenticator:authenticator];
    [connection setDelegate: self];
    
    /* get us connected */
    opal_mutex_lock(&state_mutex);
    [connection open];
    while ([connection state] == XGConnectionStateOpening) {
	opal_condition_wait(&state_cond, &state_mutex);
    }
    opal_mutex_unlock(&state_mutex);

    opal_output(orte_pls_base.pls_output,
		"pls: xgrid: connection name: %s", [[connection name] cString]);
    
    controller = [[XGController alloc] initWithConnection:connection];
    opal_progress();
    grid = [controller defaultGrid];
#if 0 /* gives a warning - need to figure out "right way" */
    opal_output(orte_pls_base.pls_output,
		"pls: xgrid: grid name: %s", [[grid name] cString]);
#endif

    return ORTE_SUCCESS;
}


-(int) launchJob:(orte_jobid_t) jobid
{
    opal_list_t nodes, mapping_list;
    opal_list_item_t *item;
    int ret;
    size_t num_nodes;
    orte_vpid_t vpid;
    int i = 0;
    char *orted_path;

    /* find orted */
    orted_path = opal_path_findv((char*) [orted cString], 0, environ, NULL); 

    /* query the list of nodes allocated to the job */
    OBJ_CONSTRUCT(&nodes, opal_list_t);
    OBJ_CONSTRUCT(&mapping_list, opal_list_t);
    ret = orte_rmaps_base_mapped_node_query(&mapping_list, &nodes, jobid);
    if (ORTE_SUCCESS != ret) goto cleanup;

    /* allocate vpids for the daemons */
    num_nodes = opal_list_get_size(&nodes);
    if (num_nodes == 0) return ORTE_ERR_BAD_PARAM;
    ret = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != ret) goto cleanup;

    /* build up the array of task specifications */
    NSMutableDictionary *taskSpecifications = [NSMutableDictionary dictionary];
    
    for (item =  opal_list_get_first(&nodes);
	 item != opal_list_get_end(&nodes);
	 item =  opal_list_get_next(item)) {
        orte_ras_node_t* node = (orte_ras_node_t*)item;
        orte_process_name_t* name;
	char *name_str, *nsuri, *gpruri;

        ret = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid);
        if(ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
        }
	ret = orte_ns.get_proc_name_string(&name_str, name);
	if (ORTE_SUCCESS != ret) {
            ORTE_ERROR_LOG(ret);
            goto cleanup;
	}

	if (NULL != orte_process_info.ns_replica_uri) {
	    nsuri = strdup(orte_process_info.ns_replica_uri);
	} else {
	    nsuri = orte_rml.get_uri();
	}

	if (NULL != orte_process_info.gpr_replica_uri) {
	    gpruri = strdup(orte_process_info.gpr_replica_uri);
	} else {
	    gpruri = orte_rml.get_uri();
	}

	NSMutableDictionary *task = [NSMutableDictionary dictionary];
	[task setObject: [NSString stringWithCString: orted_path]
	      forKey: XGJobSpecificationCommandKey];
	NSArray *taskArguments = 
	    [NSArray arrayWithObjects: @"--no-daemonize",
		     @"--bootproxy", [NSString stringWithFormat: @"%d", jobid],
		     @"--name", [NSString stringWithCString: name_str],
		     @"--nodename", [NSString stringWithCString: node->node_name],
		     @"--nsreplica", [NSString stringWithCString: nsuri],
		     @"--gprreplica", [NSString stringWithCString: gpruri],
		     nil];
	[task setObject: taskArguments forKey: XGJobSpecificationArgumentsKey];

	[taskSpecifications setObject: task 
			    forKey: [NSString stringWithFormat: @"%d", i]];

	/* add the node name into the registery */
	mca_pls_xgrid_set_node_name(node, jobid, name);
	
	free(name_str); free(nsuri); free(gpruri);
	vpid++; i++;
    }

    /* job specification */
    NSMutableDictionary *jobSpecification = [NSMutableDictionary dictionary];
    [jobSpecification setObject:XGJobSpecificationTypeTaskListValue 
		      forKey:XGJobSpecificationTypeKey];
    [jobSpecification setObject: [NSString stringWithFormat: 
					       @"org.open-mpi.pls.xgrid"]
		      forKey:XGJobSpecificationSubmissionIdentifierKey];
    [jobSpecification setObject: [NSString stringWithFormat: @"Open MPI Job %d", jobid]
		      forKey:XGJobSpecificationNameKey];
    [jobSpecification setObject:taskSpecifications 
		      forKey:XGJobSpecificationTaskSpecificationsKey];

    /* Submit the request and get our monitor */
    XGActionMonitor *actionMonitor = 
	[controller performSubmitJobActionWithJobSpecification: jobSpecification
		    gridIdentifier: [grid identifier]];

    /* wait until we have some idea if job succeeded or not */
    while (XGActionMonitorOutcomeNone == [actionMonitor outcome]) {
	opal_progress();
    }

    /* we should have a result - find out if it worked */
    if (XGActionMonitorOutcomeSuccess == [actionMonitor outcome]) {
	ret = ORTE_SUCCESS;
    } else {	
	NSError *err = [actionMonitor error];
	fprintf(stderr, "orte:pls:xgrid: launch failed: %s\n", 
		[[err localizedFailureReason] cString]);
	ret = ORTE_ERROR;
    }

    /* save the XGJob identifier somewhere we can get to it */
    [active_jobs setObject: [[actionMonitor results] objectForKey: @"jobIdentifier"]
		 forKey: [NSString stringWithFormat: @"%d", jobid]];

cleanup:
    while(NULL != (item = opal_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);

    while(NULL != (item = opal_list_remove_first(&mapping_list))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&mapping_list);

    return ret;
}


-(int) terminateJob: (orte_jobid_t) jobid
{
    int ret;

    /* get our grid */
    XGJob *job = [grid jobForIdentifier: [active_jobs objectForKey:
			  [NSString stringWithFormat: @"%d", jobid]]];

    XGActionMonitor *actionMonitor = [job performStopAction];
    while (XGActionMonitorOutcomeNone == [actionMonitor outcome]) {
	opal_progress();
    }

    /* we should have a result - find out if it worked */
    if (XGActionMonitorOutcomeSuccess == [actionMonitor outcome]) {
	ret = ORTE_SUCCESS;
    } else {	
	NSError *err = [actionMonitor error];
	fprintf(stderr, "orte:pls:xgrid: terminate failed: %s\n", 
		[[err localizedFailureReason] cString]);
	ret = ORTE_ERROR;
    }

    return ret;
}


/* delegate for changes */
-(void) connectionDidOpen:(XGConnection*) connection
{
    opal_condition_broadcast(&state_cond);
}


-(void) connectionDidNotOpen:(XGConnection*) connection withError: (NSError*) error
{
    opal_output(orte_pls_base.pls_output,
		"pls: xgrid: got connectionDidNotOpen message");
    opal_condition_broadcast(&state_cond);
}


-(void) connectionDidClose:(XGConnection*) connection;
{
    opal_output(orte_pls_base.pls_output,
		"pls: xgrid: got connectionDidClose message");
    opal_condition_broadcast(&state_cond);
}

@end
