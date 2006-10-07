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
#import "orte/mca/rmaps/rmaps.h"
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
			[[err localizedDescription] cString]);
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


- (NSString *)servicePrincipal;
{
    NSString *myServicePrincipal = [connection servicePrincipal];

    if (myServicePrincipal == nil) {
	myServicePrincipal = [NSString stringWithFormat:@"xgrid/%@", [connection name]];
    }

    opal_output_verbose(1, orte_pls_base.pls_output,
			"orte:pls:xgrid: Kerberos servicePrincipal: %s",
			[myServicePrincipal cString]);

    return myServicePrincipal;
}


/* interface for launch */
-(int) connect
{
    connection = [[[XGConnection alloc] initWithHostname: controller_hostname
					portnumber:0] autorelease];

    if (nil == controller_password) {
	opal_output_verbose(1, orte_pls_base.pls_output,
			    "orte:pls:xgrid: Using Kerberos authentication");

	XGGSSAuthenticator *authenticator = 
	    [[[XGGSSAuthenticator alloc] init] autorelease];

	opal_output_verbose(1, orte_pls_base.pls_output,
			    "orte:pls:xgrid: Kerberos principal: %s",
			    [[self servicePrincipal] cString]);
		
	[authenticator setServicePrincipal:[self servicePrincipal]];
	[connection setAuthenticator:authenticator];

    } else {
	opal_output_verbose(1, orte_pls_base.pls_output,
			    "orte:pls:xgrid: Using password authentication");

       XGTwoWayRandomAuthenticator *authenticator =
	    [[[XGTwoWayRandomAuthenticator alloc] init] autorelease];

	/* this seems to be hard coded */
	[authenticator setUsername:@"one-xgrid-client"];
	[authenticator setPassword:controller_password];
    
	[connection setAuthenticator:authenticator];
    }
    [connection setDelegate: self];

    /* get us connected */
    opal_mutex_lock(&state_mutex);
    [connection open];
    while ([connection state] == XGConnectionStateOpening) {
	opal_condition_wait(&state_cond, &state_mutex);
    }
    opal_mutex_unlock(&state_mutex);

    /* if we're not connected when the condition is triggered, we
       dont' have a connection and can't start.  exit. */
    if ([connection state] != XGConnectionStateOpen) {
	return ORTE_ERR_NOT_AVAILABLE;
    }

    opal_output_verbose(1, orte_pls_base.pls_output,
			"orte:pls:xgrid: connection name: %s",
			[[connection name] cString]);
    
    controller = [[XGController alloc] initWithConnection:connection];
    /* need to call progress exactly once for some reason to get the
       controller happy enough to allow us to assign the grid */
    opal_progress();
    grid = [controller defaultGrid];

    opal_output_verbose(1, orte_pls_base.pls_output,
			"pls: xgrid: grid name: %s",
			[[grid identifier] cString]);

    return ORTE_SUCCESS;
}


-(int) launchJob:(orte_jobid_t) jobid
{
    orte_job_map_t *map;
    opal_list_item_t *item;
    size_t num_nodes;
    orte_vpid_t vpid;
    int rc, i = 0;  
    opal_list_t daemons;
    orte_pls_daemon_info_t *dmn;
    char *orted_path;
    char *nsuri = NULL, *gpruri = NULL;

   /* Query the map for this job.
     * We need the entire mapping for a couple of reasons:
     *  - need the prefix to start with.
     *  - need to know if we are launching on a subset of the allocated nodes
     */
    rc = orte_rmaps.get_job_map(&map, jobid);
    if (ORTE_SUCCESS != rc) {
        goto cleanup;
    }

    num_nodes = opal_list_get_size(&map->nodes);

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

    /* Iterate through each of the nodes and spin
     * up a daemon.
     */
    for (item =  opal_list_get_first(&map->nodes);
         item != opal_list_get_end(&map->nodes);
         item =  opal_list_get_next(item)) {
        orte_mapped_node_t* rmaps_node = (orte_mapped_node_t*)item;
        orte_process_name_t* name;
        char* name_string;
        
        /* new daemon - setup to record its info */
        dmn = OBJ_NEW(orte_pls_daemon_info_t);
        dmn->active_job = jobid;
        opal_list_append(&daemons, &dmn->super);
        
        /* record the node name in the daemon struct */
        dmn->cell = rmaps_node->cell;
        dmn->nodename = strdup(rmaps_node->nodename);
        
        /* initialize daemons process name */
        rc = orte_ns.create_process_name(&name, rmaps_node->cell, 0, vpid);
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
        opal_output_verbose(1, orte_pls_base.pls_output,
            "orte:pls:xgrid: launching on node %s", 
            rmaps_node->nodename);
        
        /* setup process name */
        rc = orte_ns.get_proc_name_string(&name_string, name);
        if (ORTE_SUCCESS != rc) {
            opal_output(orte_pls_base.pls_output,
            "orte:pls:xgrid: unable to create process name");
            return rc;
        }

        NSMutableDictionary *task = [NSMutableDictionary dictionary];
        [task setObject: [NSString stringWithCString: orted_path]
            forKey: XGJobSpecificationCommandKey];
        NSArray *taskArguments = 
        [NSArray arrayWithObjects: @"--no-daemonize",
                @"--bootproxy", [NSString stringWithFormat: @"%d", jobid],
                @"--name", [NSString stringWithCString: name_string],
                            @"--num_procs", [NSString stringWithFormat: @"%d", 1],
                @"--nodename", [NSString stringWithCString: rmaps_node->nodename],
                @"--nsreplica", [NSString stringWithCString: nsuri],
                @"--gprreplica", [NSString stringWithCString: gpruri],
                nil];
        [task setObject: taskArguments forKey: XGJobSpecificationArgumentsKey];
    
        [taskSpecifications setObject: task 
                forKey: [NSString stringWithFormat: @"%d", i]];
    
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
	rc = ORTE_SUCCESS;
    } else {	
	NSError *err = [actionMonitor error];
	fprintf(stderr, "orte:pls:xgrid: launch failed: (%d) %s\n", 
		[actionMonitor outcome],
		[[err localizedDescription] cString]);
	rc = ORTE_ERROR;
	goto cleanup;
    }

    /* save the XGJob identifier somewhere we can get to it */
    [active_jobs setObject: [[actionMonitor results] objectForKey: @"jobIdentifier"]
		 forKey: [NSString stringWithFormat: @"%d", jobid]];

    /* all done, so store the daemon info on the registry */
    if (ORTE_SUCCESS != (rc = orte_pls_base_store_active_daemons(&daemons))) {
        ORTE_ERROR_LOG(rc);
    }

cleanup:
    OBJ_RELEASE(map);
    
    if (NULL != nsuri) free(nsuri);
    if (NULL != gpruri) free(gpruri);

    /* deconstruct the daemon list */
    while (NULL != (item = opal_list_remove_first(&daemons))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&daemons);

    opal_output_verbose(1, orte_pls_base.pls_output,
			"orte:pls:xgrid:launch: finished\n");

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
		[[err localizedDescription] cString]);
	ret = ORTE_ERROR;
    }

    return ret;
}


/* delegate for changes */
-(void) connectionDidOpen:(XGConnection*) myConnection
{
    /* this isn't an error condition -- we finally opened the
       connection, so trigger the condition variable we're waiting
       on */
    opal_condition_broadcast(&state_cond);
}


-(void) connectionDidNotOpen:(XGConnection*) myConnection withError: (NSError*) error
{
    opal_output(orte_pls_base.pls_output,
		"orte:pls:xgrid: Controller connection did not open: (%d) %s",
		[error code],
		[[error localizedDescription] cString]);
    opal_condition_broadcast(&state_cond);
}


-(void) connectionDidClose:(XGConnection*) myConnection;
{
    // check for success
    if ([myConnection error] != nil) {
	switch ([[myConnection error] code]) {
	case 200:
	    /* success */
	    break;
	case 530:
	case 535:
	    opal_output(orte_pls_base.pls_output,
			"orte:pls:xgrid: Connection to XGrid controller failed due to authentication error (%d):",
			[[myConnection error] code]);
	    break;
	default:
	    opal_output(orte_pls_base.pls_output,
			"orte:pls:xgrid: Connection to XGrid controller unexpectedly closed: (%d) %s",
			[[myConnection error] code],
			[[[myConnection error] localizedDescription] cString]);
	    break;
	}
    } else {
	opal_output(orte_pls_base.pls_output,
		    "orte:pls:xgrid: Connection to XGrid controller unexpectedly closed");
    }

    opal_condition_broadcast(&state_cond);
}

@end
