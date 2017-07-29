/*
 * Copyright (c) 2017-XXXX Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef TEST_REACHABLE_SHARED
#define TEST_REACHABLE_SHARED 1

#include <assert.h>

#include "opal/runtime/opal.h"
#include "opal/mca/reachable/reachable.h"
#include "opal/util/if.h"

BEGIN_C_DECLS

/* Create and populate opal_if_t with information required by opal_reachable */
opal_if_t* create_if(int af_family, char *address, int mask, int bandwidth)
{
    opal_if_t *interface = OBJ_NEW(opal_if_t);
    strncpy(interface->if_name, "interface0", IF_NAMESIZE);
    interface->af_family = af_family;
    ((struct sockaddr *)&(interface->if_addr))->sa_family = af_family; 

    if (AF_INET == af_family){
	assert(1 == inet_pton(af_family, address, &((struct sockaddr_in *)&(interface->if_addr))->sin_addr));
    } else if (AF_INET6 == af_family){
	assert(1 == inet_pton(af_family, address, &((struct sockaddr_in6 *)&(interface->if_addr))->sin6_addr));
    }

    interface->if_mask = mask;
    interface->if_bandwidth = bandwidth;

    return interface;
}


/* Run a test between a pair of interfaces
 * and clean up the memory afterwards.
 * Return the weight between the pair of
 * interfaces
 */
int run_single_test(opal_if_t *local_if, opal_if_t *remote_if)
{

    opal_list_t *local_list = OBJ_NEW(opal_list_t);
    opal_list_t *remote_list = OBJ_NEW(opal_list_t);

    opal_list_append(local_list, &(local_if->super));
    opal_list_append(remote_list, &(remote_if->super));

    opal_reachable_t *results;
    results = opal_reachable.reachable(local_list, remote_list);
    OBJ_RELEASE(local_list);
    OBJ_RELEASE(remote_list);
    int result = results->weights[0][0];

    /* release results */
    OBJ_RELEASE(results);
    return result;
}

END_C_DECLS

#endif
