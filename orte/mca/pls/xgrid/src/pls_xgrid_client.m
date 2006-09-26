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

#import "opal/util/path.h"

#import "orte/orte_constants.h"
#import "orte/mca/rml/rml.h"
#import "orte/mca/ns/ns.h"
#import "orte/mca/pls/base/base.h"
#import "orte/mca/pls/base/pls_private.h"
#import "orte/mca/pls/pls.h"
#import "orte/mca/errmgr/errmgr.h"
#import "orte/mca/ras/ras_types.h"
#import "orte/mca/rmaps/base/rmaps_private.h"
#import "orte/mca/smr/smr.h"

#import "pls_xgrid_client.h"

char **environ;


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
    /* need to call progress exactly once for some reason to get the
       controller happy enough to allow us to assign the grid */
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
    opal_list_t mapping;
    opal_list_item_t *m_item, *n_item;
    size_t num_nodes;
    orte_vpid_t vpid;
    int rc, i = 0;  
    opal_list_t daemons;
    orte_pls_daemon_info_t *dmn;
    char *orted_path;
    char *nsuri = NULL, *gpruri = NULL;

   /* Query the list of nodes allocated and mapped to this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     */
    OBJ_CONSTRUCT(&mapping, opal_list_t);
    rc = orte_rmaps_base_get_map(jobid, &mapping);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    num_nodes = 0;
    for(m_item = opal_list_get_first(&mapping);
        m_item != opal_list_get_end(&mapping);
        m_item = opal_list_get_next(m_item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)m_item;
        num_nodes += opal_list_get_size(&map->nodes);
    }

    /*
     * Allocate a range of vpids for the daemons.
     */
    if (0 == num_nodes) {
        return ORTE_ERR_BAD_PARAM;
    }
    rc = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    /* setup the orted triggers for passing their launch info */
    if (ORTE_SUCCESS != (rc = orte_smr.init_orted_stage_gates(jobid, num_nodes, NULL, NULL))) {
        ORTE_ERROR_LOG(rc);
        goto cleanup;
    }
    
    /* setup a list that will contain the info for all the daemons
     * so we can store it on the registry when done
     */
    OBJ_CONSTRUCT(&daemons, opal_list_t);

    /* find orted */
    orted_path = opal_path_findv((char*) [orted cString], 0, environ, NULL); 
    
    /* setup ns contact info */
    if (NULL != orte_process_info.ns_replica_uri) {
        nsuri = strdup(orte_process_info.ns_replica_uri);
    } else {
        nsuri = orte_rml.get_uri();
    }

    /* setup gpr contact info */
    if (NULL != orte_process_info.gpr_replica_uri) {
        gpruri = strdup(orte_process_info.gpr_replica_uri);
    } else {
        gpruri = orte_rml.get_uri();
    }

    /* build up the array of task specifications */
    NSMutableDictionary *taskSpecifications = [NSMutableDictionary dictionary];

    /*
     * iterate through each of the contexts
     */
    for (m_item = opal_list_get_first(&mapping);
         m_item != opal_list_get_end(&mapping);
         m_item = opal_list_get_next(m_item)) {
        orte_rmaps_base_map_t* map = (orte_rmaps_base_map_t*)m_item;

        /* Iterate through each of the nodes and spin
         * up a daemon.
         */
        for (n_item =  opal_list_get_first(&map->nodes);
             n_item != opal_list_get_end(&map->nodes);
             n_item =  opal_list_get_next(n_item)) {
            orte_rmaps_base_node_t* rmaps_node = (orte_rmaps_base_node_t*)n_item;
            orte_ras_node_t* node = rmaps_node->node;
            orte_process_name_t* name;
            char* name_string;
            
            /* already launched on this node */
            if (0 != node->node_launched++) {
                continue;
            }
            
            /* new daemon - setup to record its info */
            dmn = OBJ_NEW(orte_pls_daemon_info_t);
            dmn->active_job = jobid;
            opal_list_append(&daemons, &dmn->super);
            
            /* record the node name in the daemon struct */
            dmn->cell = node->node_cellid;
            dmn->nodename = strdup(node->node_name);
            
            /* initialize daemons process name */
            rc = orte_ns.create_process_name(&name, node->node_cellid, 0, vpid);
            if (ORTE_SUCCESS != rc) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            /* save it in the daemon struct */
            if (ORTE_SUCCESS != (rc = orte_dss.copy((void**)&(dmn->name), name, ORTE_NAME))) {
                ORTE_ERROR_LOG(rc);
                goto cleanup;
            }
            
            /* setup per-node options */
	    opal_output(0, "pls:xgrid: launching on node %s", 
			node->node_name);
            
            /* setup process name */
            rc = orte_ns.get_proc_name_string(&name_string, name);
            if (ORTE_SUCCESS != rc) {
                opal_output(0, "pls:xgrid: unable to create process name");
                return rc;
            }

	    NSMutableDictionary *task = [NSMutableDictionary dictionary];
	    [task setObject: [NSString stringWithCString: orted_path]
		  forKey: XGJobSpecificationCommandKey];
	    NSArray *taskArguments = 
		[NSArray arrayWithObjects: @"--no-daemonize",
			 @"--bootproxy", [NSString stringWithFormat: @"%d", jobid],
			 @"--name", [NSString stringWithCString: name_string],
			 @"--nodename", [NSString stringWithCString: node->node_name],
			 @"--nsreplica", [NSString stringWithCString: nsuri],
			 @"--gprreplica", [NSString stringWithCString: gpruri],
			 nil];
	    [task setObject: taskArguments forKey: XGJobSpecificationArgumentsKey];

	    [taskSpecifications setObject: task 
				forKey: [NSString stringWithFormat: @"%d", i]];

	    vpid++; i++;
	}
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
	rc = ORTE_SUCCESS;
    } else {	
	NSError *err = [actionMonitor error];
	fprintf(stderr, "orte:pls:xgrid: launch failed: %s\n", 
		[[err localizedFailureReason] cString]);
	rc = ORTE_ERROR;
	goto cleanup;
    }

    /* save the XGJob identifier somewhere we can get to it */
    [active_jobs setObject: [[actionMonitor results] objectForKey: @"jobIdentifier"]
		 forKey: [NSString stringWithFormat: @"%d", jobid]];

    /* all done, so store the daemon info on the registry */
    if (ORTE_SUCCESS != (rc = orte_pls_base_store_active_daemons(&daemons, jobid))) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    if (NULL != nsuri) free(nsuri);
    if (NULL != gpruri) free(gpruri);

    while (NULL != (m_item = opal_list_remove_first(&mapping))) {
        OBJ_RELEASE(m_item);
    }
    OBJ_DESTRUCT(&mapping);

    /* deconstruct the daemon list */
    while (NULL != (m_item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(m_item);
    }
    OBJ_DESTRUCT(&daemons);

    opal_output(0, "pls:xgrid:launch: finished\n");

    return rc;
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
