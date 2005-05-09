/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#import "ompi_config.h"
#import "pls_xgrid_config.h"

#import <stdio.h>

#import "mca/pls/base/base.h"
#import "include/orte_constants.h"
#import "include/constants.h"
#import "mca/ns/ns.h"
#import "mca/ras/base/ras_base_node.h"
#import "mca/rml/rml.h"
#import "util/path.h"

#import "pls_xgrid_client.h"

char **environ;

@implementation PlsXgridClient

/* init / finalize */
-(id) init
{
    if (self = [super init]) {
	/* class-specific initialization goes here */
	OBJ_CONSTRUCT(&state_cond, ompi_condition_t);
	OBJ_CONSTRUCT(&state_mutex, ompi_mutex_t);
    }
    return self;
}


-(void) dealloc
{
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
    ompi_mutex_lock(&state_mutex);
    [connection open];
    while ([connection state] == XGConnectionStateOpening) {
	ompi_condition_wait(&state_cond, &state_mutex);
    }
    ompi_mutex_unlock(&state_mutex);

    ompi_output(orte_pls_base.pls_output,
		"pls: xgrid: connection name: %s", [[connection name] cString]);
    
    controller = [[XGController alloc] initWithConnection:connection];
    ompi_progress();
    grid = [controller defaultGrid];
    ompi_output(orte_pls_base.pls_output,
		"pls: xgrid: grid name: %s", [[grid name] cString]);

    return ORTE_SUCCESS;
}


-(int) launchJob:(orte_jobid_t) jobid
{
    ompi_list_t nodes;
    ompi_list_item_t *item;
    int ret;
    size_t num_nodes;
    orte_vpid_t vpid;
    int i = 0;
    char *orted_path;

    /* find orted */
    orted_path = ompi_path_findv((char*) [orted cString], 0, environ, NULL); 

    /* query the list of nodes allocated to the job */
    OBJ_CONSTRUCT(&nodes, ompi_list_t);
    ret = orte_ras_base_node_query_alloc(&nodes, jobid);
    if (ORTE_SUCCESS != ret) goto cleanup;

    /* allocate vpids for the daemons */
    num_nodes = ompi_list_get_size(&nodes);
    if (num_nodes == 0) return OMPI_ERR_BAD_PARAM;
    ret = orte_ns.reserve_range(0, num_nodes, &vpid);
    if (ORTE_SUCCESS != ret) goto cleanup;

    /* build up the array of task specifications */
    NSMutableDictionary *taskSpecifications = [NSMutableDictionary dictionary];
    
    for (item =  ompi_list_get_first(&nodes);
	 item != ompi_list_get_end(&nodes);
	 item =  ompi_list_get_next(item)) {
        orte_ras_base_node_t* node = (orte_ras_base_node_t*)item;
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
		     @"--nodename", [NSString stringWithFormat: @"xgrid-node-%d", i],
		     @"--nsreplica", [NSString stringWithCString: nsuri],
		     @"--gprreplica", [NSString stringWithCString: gpruri],
		     nil];
	[task setObject: taskArguments forKey: XGJobSpecificationArgumentsKey];

	[taskSpecifications setObject: task 
			    forKey: [NSString stringWithFormat: @"%d", i]];

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
		    gridIdentifier: nil];

    /* wait until we have some idea if job succeeded or not */
    while (XGActionMonitorOutcomeNone == [actionMonitor outcome]) {
	ompi_progress();
    }

    /* we should have a result - find out if it worked */
    if (XGActionMonitorOutcomeSuccess == [actionMonitor outcome]) {
	ret = OMPI_SUCCESS;
    } else {	
	NSError *err = [actionMonitor error];
	printf("launch failed: %s\n", [[err localizedFailureReason] cString]);
	ret = OMPI_ERROR;
    }

cleanup:
    while(NULL != (item = ompi_list_remove_first(&nodes))) {
        OBJ_RELEASE(item);
    }
    OBJ_DESTRUCT(&nodes);
    return ret;
}


/* delegate for changes */
-(void) connectionDidOpen:(XGConnection*) connection
{
    ompi_output(orte_pls_base.pls_output,
		"pls: xgrid: got connectionDidOpen message");
    ompi_condition_broadcast(&state_cond);
}

-(void) connectionDidNotOpen:(XGConnection*) connection withError: (NSError*) error
{
    ompi_output(orte_pls_base.pls_output,
		"pls: xgrid: got connectionDidNotOpen message");
    ompi_condition_broadcast(&state_cond);
}

-(void) connectionDidClose:(XGConnection*) connection;
{
    ompi_output(orte_pls_base.pls_output,
		"pls: xgrid: got connectionDidClose message");
    ompi_condition_broadcast(&state_cond);
}

@end
