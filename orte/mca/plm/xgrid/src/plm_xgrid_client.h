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
 */

#import <Foundation/Foundation.h>
#import <XgridFoundation/XgridFoundation.h>
#import <Foundation/NSString.h>

#import "opal/threads/condition.h"

@interface PlmXGridClient : NSObject
{
    NSString *orted;
    NSString *controller_hostname;
    NSString *controller_password;

    /* state of the world... */
    opal_condition_t state_cond;
    opal_mutex_t state_mutex;

    XGConnection *connection;
    XGController *controller;
    XGGrid *grid;
    int cleanup;

    NSMutableDictionary *active_xgrid_jobs;
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
-(int) launchOrteds:(orte_job_t*) jdata;
-(int) terminateOrteds;

/* delegate for changes */
-(void) connectionDidOpen:(XGConnection*) connection;
-(void) connectionDidNotOpen:(XGConnection*)connection withError:(NSError*) error;
-(void) connectionDidClose:(XGConnection *) connection;

/* Helper function */
-(NSMutableArray*) getArgumentsForOrtedLaunch;

@end
