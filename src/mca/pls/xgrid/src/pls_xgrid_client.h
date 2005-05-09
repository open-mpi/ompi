/* -*- ObjC -*-
 *
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

#import <Foundation/Foundation.h>
#import <XgridFoundation/XgridFoundation.h>
#import <Foundation/NSString.h>

#import "threads/condition.h"
#include "mca/ns/ns_types.h"

@interface PlsXgridClient : NSObject
{
    NSString *orted;
    NSString *controller_hostname;
    NSString *controller_password;

    ompi_condition_t startup_cond;
    volatile int startup_val;

    /* state of the world... */
    ompi_condition_t state_cond;
    ompi_mutex_t state_mutex;

    XGConnection *connection;
    XGTwoWayRandomAuthenticator *authenticator;
    XGController *controller;
    XGGrid *grid;
}

/* init / finalize */
-(id) init;
-(void) dealloc;

/* accessors */
-(NSString*) getOrted;

-(void) setOrtedAsCString: (char*) name;
-(void) setControllerPasswordAsCString: (char*) name;
-(void) setControllerHostnameAsCString: (char*) password;


/* interface for launch */
-(int) connect;
-(int) launchJob:(orte_jobid_t) jobid;

/* delegate for changes */
-(void) connectionDidOpen:(XGConnection*) connection;
-(void) connectionDidNotOpen:(XGConnection*)connection withError:(NSError*) error;
-(void) connectionDidClose:(XGConnection *) connection;

@end
