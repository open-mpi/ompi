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
 * Copyright (c) 2016      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <stdio.h>
#include <string.h>
#include <stdlib.h>

#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"

#define NUM_ITERS 100
#define NUM_ELEMS 1024

static bool test1(void);        /* verify dss_copy_payload */

static FILE *test_out;


int main (int argc, char* argv[])
{
    int ret = 0;

    opal_init(&argc, &argv);

    test_out = stderr;

    /* run the tests */

    fprintf(test_out, "executing test1\n");
    if (test1()) {
        fprintf(test_out, "Test1 succeeded\n");
    } else {
        fprintf(test_out, "Test1 failed\n");
        ret = 1;
    }

    opal_finalize();

    return ret;
}

static bool test1(void)        /* verify dss_copy_payload */
{
    opal_buffer_t *bufA, *bufB;
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
    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    bufA->type = OPAL_DSS_BUFFER_NON_DESC;

    /* pack something in A */
    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_INT16);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    /* setup bufB */
    bufB = OBJ_NEW(opal_buffer_t);
    if (NULL == bufB) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    bufB->type = OPAL_DSS_BUFFER_NON_DESC;

    /* pack something in B */
    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufB, src32, NUM_ELEMS, OPAL_INT32);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    /* copy payload to bufB */
    if (OPAL_SUCCESS != (rc = opal_dss.copy_payload(bufB, bufA))) {
        fprintf(test_out, "opal_dss.copy_payload failed with return code %d\n", rc);
        return(false);
    }

    /* pack some more stuff in B */
    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufB, src32, NUM_ELEMS, OPAL_INT32);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    /* validate the results */
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst32[j] = -1;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufB, dst32, &count, OPAL_INT32);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack of dest buffer failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src32[j] != dst32[j]) {
                fprintf(test_out, "test1: invalid results from unpack of dest buffer\n");
                return(false);
            }
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufB, dst, &count, OPAL_INT16);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack of dest buffer failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test1: invalid results from unpack of dest buffer\n");
                return(false);
            }
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst32[j] = -1;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufB, dst32, &count, OPAL_INT32);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack of dest buffer failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src32[j] != dst32[j]) {
                fprintf(test_out, "test1: invalid results from unpack of dest buffer\n");
                return(false);
            }
        }
    }

    /* check that A is still okay */
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dst, &count, OPAL_INT16);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack of src buffer failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test1: invalid results from unpack of src buffer\n");
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


