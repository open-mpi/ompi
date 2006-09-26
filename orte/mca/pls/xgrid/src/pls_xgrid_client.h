/* -*- ObjC -*-
 *
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
 *
 * These symbols are in a file by themselves to provide nice linker
 * semantics.  Since linkers generally pull in symbols by object
 * files, keeping these symbols as the only symbols in this file
 * prevents utility programs such as "ompi_info" from having to import
 * entire components just to query their version and parameters.
 */

#import <Foundation/Foundation.h>
#import <XgridFoundation/XgridFoundation.h>
#import <Foundation/NSString.h>

#import "opal/threads/condition.h"
#include "orte/mca/ns/ns_types.h"

@interface PlsXGridClient : NSObject
{
    NSString *orted;
    NSString *controller_hostname;
    NSString *controller_password;

    opal_condition_t startup_cond;
    volatile int startup_val;

    /* state of the world... */
    opal_condition_t state_cond;
    opal_mutex_t state_mutex;

    XGConnection *connection;
    XGController *controller;
    XGGrid *grid;
    int cleanup;

    NSMutableDictionary *active_jobs;
}

/* init / finalize */
-(id) init;
-(id) initWithControllerHostname: (char*) hostnam
	   AndControllerPassword: (char*) password
			AndOrted: (char*) ortedname
		      AndCleanup: (int) val;
-(void) dealloc;

/* accessors */
-(NSString*) getOrted;

-(void) setOrtedAsCString: (char*) name;
-(void) setControllerPasswordAsCString: (char*) name;
-(void) setControllerHostnameAsCString: (char*) password;
-(void) setCleanUp: (int) val;

-(NSString*)servicePrincipal;

/* interface for launch */
-(int) connect;
-(int) launchJob:(orte_jobid_t) jobid;
-(int) terminateJob: (orte_jobid_t) jobid;

/* delegate for changes */
-(void) connectionDidOpen:(XGConnection*) connection;
-(void) connectionDidNotOpen:(XGConnection*)connection withError:(NSError*) error;
-(void) connectionDidClose:(XGConnection *) connection;

@end
