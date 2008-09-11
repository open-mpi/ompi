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

#import "orte/constants.h"
#import "orte/mca/rml/rml.h"
#import "orte/mca/plm/base/base.h"
#import "orte/mca/plm/base/plm_private.h"
#import "orte/mca/plm/plm.h"
#import "orte/mca/errmgr/errmgr.h"
#import "orte/mca/ras/ras_types.h"
#import "orte/mca/rmaps/rmaps.h"

#import "plm_xgrid_client.h"


@implementation PlmXGridClient

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

	active_xgrid_jobs = [NSMutableDictionary dictionary];
    }
    return self;
}


-(void) dealloc
{
    /* if supposed to clean up jobs, do so */
    if (cleanup) {
	NSArray *keys = [active_xgrid_jobs allKeys];
	NSEnumerator *enumerator = [keys objectEnumerator];
	NSString *key;
	XGJob *job;
	XGActionMonitor *actionMonitor;

        while (key = [enumerator nextObject]) {
	    job = [grid jobForIdentifier: [active_xgrid_jobs objectForKey: key]];

	    actionMonitor = [job performDeleteAction];
	    while (XGActionMonitorOutcomeNone == [actionMonitor outcome]) {
		opal_progress();
	    }

	    /* we should have a result - find out if it worked */
	    if (XGActionMonitorOutcomeSuccess != [actionMonitor outcome]) {
		NSError *err = [actionMonitor error];
		fprintf(stderr, "orte:plm:xgrid: cleanup failed: %s\n", 
			[[err localizedDescription] UTF8String]);
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

    opal_output_verbose(1, orte_plm_globals.output,
			"orte:plm:xgrid: Kerberos servicePrincipal: %s",
			[myServicePrincipal UTF8String]);

    return myServicePrincipal;
}


/* interface for launch */
-(int) connect
{
    connection = [[[XGConnection alloc] initWithHostname: controller_hostname
					portnumber:0] autorelease];

    if (nil == controller_password) {
	opal_output_verbose(1, orte_plm_globals.output,
			    "orte:plm:xgrid: Using Kerberos authentication");

	XGGSSAuthenticator *authenticator = 
	    [[[XGGSSAuthenticator alloc] init] autorelease];

	opal_output_verbose(1, orte_plm_globals.output,
			    "orte:plm:xgrid: Kerberos principal: %s",
			    [[self servicePrincipal] UTF8String]);
		
	[authenticator setServicePrincipal:[self servicePrincipal]];
	[connection setAuthenticator:authenticator];

    } else {
	opal_output_verbose(1, orte_plm_globals.output,
			    "orte:plm:xgrid: Using password authentication");

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

    opal_output_verbose(1, orte_plm_globals.output,
			"orte:plm:xgrid: connection name: %s",
			[[connection name] UTF8String]);
    
    controller = [[XGController alloc] initWithConnection:connection];
    /* need to call progress exactly once for some reason to get the
       controller happy enough to allow us to assign the grid */
    opal_progress();
    grid = [controller defaultGrid];

    opal_output_verbose(1, orte_plm_globals.output,
			"plm: xgrid: grid name: %s",
			[[grid identifier] UTF8String]);

    return ORTE_SUCCESS;
}


-(int) launchOrteds:(orte_job_t*) jdata
{
    orte_job_map_t *map = NULL;
    opal_list_item_t *item;
    int rc = ORTE_SUCCESS;
    char *orted_path = NULL;
    bool failed_launch = true;
    orte_node_t **nodes;
    orte_std_cntr_t nnode;
    char *vpid_string;

    /* Get the map for this job */
    if (NULL == (map = orte_rmaps.get_job_map(jdata->jobid))) {
        ORTE_ERROR_LOG(ORTE_ERR_NOT_FOUND);
        rc = ORTE_ERR_NOT_FOUND;
        goto cleanup;
    }

    /* get the nodes list */
    nodes = (orte_node_t**)map->nodes->addr;

    /* Shortcut out of here */
    if (0 == map->num_new_daemons) {
        /* have all the daemons we need - launch app */
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:rsh: no new daemons to launch",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME)));
	return ORTE_SUCCESS;
    }

    /* find orted */
    orted_path = opal_path_findv((char*) [orted UTF8String], 0, environ, NULL); 
    
    /* build up the array of task specifications */
    NSMutableDictionary *taskSpecifications = [NSMutableDictionary dictionary];
    for (nnode=0 ; nnode < map->num_nodes ; nnode++) {
        opal_output_verbose(1, orte_plm_globals.output,
			    "orte:plm:xgrid: launching on node %s", 
			    nodes[nnode]->name);

	/* Create the task */
        NSMutableDictionary *task = [NSMutableDictionary dictionary];

	/* fill in applicaton to start */
        [task setObject: [NSString stringWithCString: orted_path]
            forKey: XGJobSpecificationCommandKey];

	/* fill in task arguments */
        NSMutableArray *taskArguments = [self getArgumentsForOrtedLaunch];

	[taskArguments addObject: @"-mca"];
	[taskArguments addObject: @"orte_ess_vpid"];
	rc = orte_util_convert_vpid_to_string(&vpid_string, 
					      nodes[nnode]->daemon->name.vpid);
	if (ORTE_SUCCESS != rc) {
	    opal_output(0, "orte_plm_rsh: unable to get daemon vpid as string");
	    goto cleanup;
	}
	[taskArguments addObject: [NSString stringWithCString: vpid_string]];
	free(vpid_string);

	[taskArguments addObject: @"--nodename"];
	[taskArguments addObject: [NSString stringWithCString: nodes[nnode]->name]];

        [task setObject: taskArguments forKey: XGJobSpecificationArgumentsKey];

	/* Add task to the task specification dictionary */
        [taskSpecifications setObject: task 
                forKey: [NSString stringWithFormat: @"%d", nnode]];
    }

    /* job specification */
    NSMutableDictionary *jobSpecification = [NSMutableDictionary dictionary];
    [jobSpecification setObject:XGJobSpecificationTypeTaskListValue 
		      forKey:XGJobSpecificationTypeKey];
    [jobSpecification setObject: [NSString stringWithFormat: 
					       @"org.open-mpi.plm.xgrid"]
		      forKey:XGJobSpecificationSubmissionIdentifierKey];
    [jobSpecification setObject: [NSString stringWithFormat: @"Open MPI Job %u", 
					   jdata->jobid]
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
	fprintf(stderr, "orte:plm:xgrid: launch failed: (%d) %s\n", 
		[actionMonitor outcome],
		[[err localizedDescription] UTF8String]);
	rc = ORTE_ERROR;
	goto cleanup;
    }

    /* save the XGJob identifier somewhere we can get to it */
    [active_xgrid_jobs setObject: [[actionMonitor results] objectForKey: @"jobIdentifier"]
		 forKey: [NSString stringWithFormat: @"%u", jdata->jobid]];

    /* wait for daemons to callback */
    if (ORTE_SUCCESS != (rc = orte_plm_base_daemon_callback(map->num_new_daemons))) {
        OPAL_OUTPUT_VERBOSE((1, orte_plm_globals.output,
                             "%s plm:xgrid: daemon launch failed for job %s on error %s",
                             ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                             ORTE_JOBID_PRINT(jdata->jobid), ORTE_ERROR_NAME(rc)));
        goto cleanup;
    }

cleanup:
    opal_output_verbose(1, orte_plm_globals.output,
			"orte:plm:xgrid:launch: finished, rc=%d\n", rc);

    return rc;
}


-(int) terminateOrteds
{
    NSArray *keys = [active_xgrid_jobs allKeys];
    NSEnumerator *enumerator = [keys objectEnumerator];
    NSString *key;
    XGJob *job;
    XGActionMonitor *actionMonitor;
    int ret = ORTE_SUCCESS;

    while (key = [enumerator nextObject]) {
	job = [grid jobForIdentifier: [active_xgrid_jobs objectForKey: key]];

	actionMonitor = [job performStopAction];
	while (XGActionMonitorOutcomeNone == [actionMonitor outcome]) {
	    opal_progress();
	}

	/* we should have a result - find out if it worked */
	if (XGActionMonitorOutcomeSuccess != [actionMonitor outcome]) {
	    NSError *err = [actionMonitor error];
	    fprintf(stderr, "orte:plm:xgrid: terminate failed: %s\n", 
		    [[err localizedDescription] UTF8String]);
	    ret = ORTE_ERROR;
	}
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
    opal_output(orte_plm_globals.output,
		"orte:plm:xgrid: Controller connection did not open: (%d) %s",
		[error code],
		[[error localizedDescription] UTF8String]);
    opal_condition_broadcast(&state_cond);
}


-(void) connectionDidClose:(XGConnection*) myConnection
{
    // check for success
    if ([myConnection error] != nil) {
	switch ([[myConnection error] code]) {
	case 200:
	    /* success */
	    break;
	case 530:
	case 535:
	    opal_output(orte_plm_globals.output,
			"orte:plm:xgrid: Connection to XGrid controller failed due to authentication error (%d):",
			[[myConnection error] code]);
	    break;
	default:
	    opal_output(orte_plm_globals.output,
			"orte:plm:xgrid: Connection to XGrid controller unexpectedly closed: (%d) %s",
			[[myConnection error] code],
			[[[myConnection error] localizedDescription] UTF8String]);
	    break;
	}
    } else {
	opal_output(orte_plm_globals.output,
		    "orte:plm:xgrid: Connection to XGrid controller unexpectedly closed");
    }

    opal_condition_broadcast(&state_cond);
}

-(NSMutableArray*) getArgumentsForOrtedLaunch
{
    char **argv = NULL;
    int argc = 0;
    int i;

    orte_plm_base_orted_append_basic_args(&argc, &argv,
                                          "env",
                                          NULL,
                                          true);

    /* Note that capacity is a starting capacity, not max */
    NSMutableArray *ret = [NSMutableArray arrayWithCapacity: argc];
    for (i = 0 ; i < argc ; ++i) {
	[ret addObject: [NSString stringWithCString: argv[i]]];
    }

    if (NULL != argv) opal_argv_free(argv);

    return ret;
}

@end
