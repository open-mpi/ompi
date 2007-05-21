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

#include "orte_config.h"
#include "orte/orte_constants.h"
#include "orte/orte_types.h"


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "opal/runtime/opal.h"
#include "orte/util/proc_info.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/runtime/runtime.h"

#include "orte/dss/dss.h"

#define NUM_ITERS 100
#define NUM_ELEMS 1024

static bool test1(void);        /* verify dss_copy_payload */
static bool test2(void);        /* verify dss_xfer_payload */

FILE *test_out;


int main (int argc, char* argv[])
{
    int ret;

    orte_init(ORTE_INFRASTRUCTURE, ORTE_NON_BARRIER);

    test_out = stderr;

    /* run the tests */

    fprintf(test_out, "executing test1\n");
    if (test1()) {
        fprintf(test_out, "Test1 succeeded\n");
    }
    else {
      fprintf(test_out, "Test1 failed\n");
    }

    fprintf(test_out, "executing test2\n");
    if (test2()) {
        fprintf(test_out, "Test2 succeeded\n");
    }
    else {
      fprintf(test_out, "Test2 failed\n");
    }

    orte_dss_close();

    opal_finalize();

    return(0);
}

static bool test1(void)        /* verify dss_copy_payload */
{
    orte_buffer_t *bufA, *bufB;
    int rc;
    int32_t i;
    int16_t src[NUM_ELEMS];
    int16_t dst[NUM_ELEMS];
    int32_t src32[NUM_ELEMS];
    int32_t dst32[NUM_ELEMS];
    
    /* init src arrays */
    for (i=0; i < NUM_ELEMS; i++) {
        src[i] = i;
        src32[i] = 132 * i;
    }
    
    /* init A */
    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    orte_dss.set_buffer_type(bufA, ORTE_DSS_BUFFER_NON_DESC);
    
    /* pack something in A */
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_INT16);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }
    
    /* setup bufB */
    bufB = OBJ_NEW(orte_buffer_t);
    if (NULL == bufB) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }
    
    orte_dss.set_buffer_type(bufB, ORTE_DSS_BUFFER_NON_DESC);
    
    /* pack something in B */
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufB, src32, NUM_ELEMS, ORTE_INT32);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }
    
    /* copy payload to bufB */
    if (ORTE_SUCCESS != (rc = orte_dss.copy_payload(bufB, bufA))) {
        fprintf(test_out, "orte_dss.copy_payload failed with return code %d\n", rc);
        return(false);
    }
    
    /* pack some more stuff in B */
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufB, src32, NUM_ELEMS, ORTE_INT32);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }
    
    /* validate the results */
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        orte_std_cntr_t count;
        
        for(j=0; j<NUM_ELEMS; j++)
            dst32[j] = -1;
        
        count = NUM_ELEMS;
        rc = orte_dss.unpack(bufB, dst32, &count, ORTE_INT32);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack of dest buffer failed with return code %d\n", rc);
            return(false);
        }
        
        for(j=0; j<NUM_ELEMS; j++) {
            if(src32[j] != dst32[j]) {
                fprintf(test_out, "test2: invalid results from unpack of dest buffer\n");
                return(false);
            }
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        orte_std_cntr_t count;
        
        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;
        
        count = NUM_ELEMS;
        rc = orte_dss.unpack(bufB, dst, &count, ORTE_INT16);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack of dest buffer failed with return code %d\n", rc);
            return(false);
        }
        
        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack of dest buffer\n");
                return(false);
            }
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        orte_std_cntr_t count;
        
        for(j=0; j<NUM_ELEMS; j++)
            dst32[j] = -1;
        
        count = NUM_ELEMS;
        rc = orte_dss.unpack(bufB, dst32, &count, ORTE_INT32);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack of dest buffer failed with return code %d\n", rc);
            return(false);
        }
        
        for(j=0; j<NUM_ELEMS; j++) {
            if(src32[j] != dst32[j]) {
                fprintf(test_out, "test2: invalid results from unpack of dest buffer\n");
                return(false);
            }
        }
    }
    
    /* check that A is still okay */
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        orte_std_cntr_t count;
        
        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;
        
        count = NUM_ELEMS;
        rc = orte_dss.unpack(bufA, dst, &count, ORTE_INT16);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack of src buffer failed with return code %d\n", rc);
            return(false);
        }
        
        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack of src buffer\n");
                return(false);
            }
        }
    }
    
    OBJ_RELEASE(bufA);
    OBJ_RELEASE(bufB);
    if (NULL != bufA || NULL != bufB) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }
    
    return (true);
}


static bool test2(void)        /* verify dss_xfer_payload */
{
    orte_buffer_t *bufA, *bufB;
    int rc;
    int32_t i;
    int16_t src[NUM_ELEMS];
    int16_t dst[NUM_ELEMS];
    
    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }
    
    orte_dss.set_buffer_type(bufA, ORTE_DSS_BUFFER_NON_DESC);
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dss.pack(bufA, src, NUM_ELEMS, ORTE_INT16);
        if (ORTE_SUCCESS != rc) {
            fprintf(test_out, "orte_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }
    
    /* setup bufB */
    bufB = OBJ_NEW(orte_buffer_t);
    if (NULL == bufB) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }
    
    orte_dss.set_buffer_type(bufB, ORTE_DSS_BUFFER_NON_DESC);
    
    /* xfer payload to bufB */
    if (ORTE_SUCCESS != (rc = orte_dss.xfer_payload(bufB, bufA))) {
        fprintf(test_out, "orte_dss.xfer_payload failed with return code %d\n", rc);
        return(false);
    }
    
    /* validate the results */
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        orte_std_cntr_t count;
        
        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;
        
        count = NUM_ELEMS;
        rc = orte_dss.unpack(bufB, dst, &count, ORTE_INT16);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "orte_dss.unpack of dest buffer failed with return code %d\n", rc);
            return(false);
        }
        
        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack of dest buffer\n");
                return(false);
            }
        }
    }
    
    OBJ_RELEASE(bufA);
    OBJ_RELEASE(bufB);
    if (NULL != bufA || NULL != bufB) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }
    
    return (true);
}
