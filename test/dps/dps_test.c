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
 */

#include "orte_config.h"
#include "orte/include/orte_constants.h"
#include "orte/include/orte_types.h"
#include "orte/dps/dps.h"
#include "orte/dps/dps_types.h"


#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#ifdef HAVE_NETINET_IN_H
#include <netinet/in.h>
#endif

#include "include/constants.h"

#include "support.h"
#include "orte/runtime/runtime.h"
#include "orte/util/proc_info.h"
#include "opal/util/sys_info.h"
#include "opal/util/malloc.h"
#include "opal/util/output.h"
#include "opal/mca/base/base.h"
#include "orte/dps/dps.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/ns/base/base.h"
#include "orte/mca/gpr/base/base.h"
#include "orte/mca/rmgr/base/base.h"
#include "orte/mca/soh/base/base.h"

#define NUM_ITERS 3
#define NUM_ELEMS 10

static bool test1(void);        /* verify different buffer inits */
static bool test2(void);        /* verify int16 */
static bool test2_1(void);      /* verify int */
static bool test3(void);        /* verify int32 */
static bool test3_1(void);      /* verify int64 */
static bool test4(void);        /* verify string */
static bool test5(void);        /* verify name */
static bool test6(void);        /* verify BOOL */
static bool test7(void);        /* verify OBJECT */
static bool test8(void);        /* verify composite (multiple types and element counts) */
static bool test9(void);        /* verify GPR_KEYVAL */
static bool test10(void);        /* verify GPR_VALUE */
static bool test11(void);        /* verify size_t */
static bool test12(void);        /* verify APP_CONTEXT */
static bool test13(void);        /* verify ORTE_GPR_SUBSCRIPTION */
static bool test14(void);        /* verify ORTE_GPR_NOTIFY_DATA */
static bool test15(void);        /* verify pid_t */

FILE *test_out;

int main (int argc, char* argv[])
{
    int ret;

    test_init("orte_dps");
    test_out = stderr;
    
    /* Open up the output streams */
    if (!opal_output_init()) {
        return OMPI_ERROR;
    }

    /* 
     * If threads are supported - assume that we are using threads -
     * and reset otherwise.
     */
    opal_set_using_threads(OMPI_HAVE_THREAD_SUPPORT);

    /* For malloc debugging */
    opal_malloc_init();

    /* Ensure the system_info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_sys_info())) {
        return ret;
    }

    /* Ensure the process info structure is instantiated and initialized */
    if (ORTE_SUCCESS != (ret = orte_proc_info())) {
        return ret;
    }

    orte_process_info.seed = true;
    orte_process_info.my_name = (orte_process_name_t*)malloc(sizeof(orte_process_name_t));
    orte_process_info.my_name->cellid = 0;
    orte_process_info.my_name->jobid = 0;
    orte_process_info.my_name->vpid = 0;

    /* startup the MCA */
    if (OMPI_SUCCESS == mca_base_open()) {
        fprintf(test_out, "MCA started\n");
    } else {
        fprintf(test_out, "MCA could not start\n");
        exit (1);
    }

    /* open the dps */
    if (ORTE_SUCCESS == orte_dps_open()) {
        fprintf(test_out, "DPS started\n");
    } else {
        fprintf(test_out, "DPS could not start\n");
        exit (1);
    }

    /* open the name services */
    if (ORTE_SUCCESS == orte_ns_base_open()) {
        fprintf(test_out, "NS started\n");
    } else {
        fprintf(test_out, "NS could not start\n");
        exit (1);
    }

    /* open the registry */
    if (ORTE_SUCCESS == orte_gpr_base_open()) {
        fprintf(test_out, "GPR started\n");
    } else {
        fprintf(test_out, "GPR could not start\n");
        exit (1);
    }

    /* open the resource manager */
    if (ORTE_SUCCESS == orte_rmgr_base_open()) {
        fprintf(test_out, "RMGR started\n");
    } else {
        fprintf(test_out, "RMGR could not start\n");
        exit (1);
    }

    /* open the soh */
    if (ORTE_SUCCESS == orte_soh_base_open()) {
        fprintf(test_out, "SOH started\n");
    } else {
        fprintf(test_out, "SOH could not start\n");
        exit (1);
    }

    /* run the tests */
    
    fprintf(test_out, "executing test1\n");
    if (test1()) {
        test_success();
    }
    else {
      test_failure("orte_dps test1 failed");
    }

    fprintf(test_out, "executing test2\n");
    if (test2()) {
        test_success();
    }
    else {
      test_failure("orte_dps test2 failed");
    }

    fprintf(test_out, "executing test2_1\n");
    if (test2_1()) {
        test_success();
    }
    else {
      test_failure("orte_dps test2_1 failed");
    }

    fprintf(test_out, "executing test3\n");
    if (test3()) {
        test_success();
    }
    else {
      test_failure("orte_dps test3 failed");
    }

    fprintf(test_out, "executing test3_1\n");
    if (test3_1()) {
        test_success();
    }
    else {
      test_failure("orte_dps test3_1 failed");
    }

    fprintf(test_out, "executing test4\n");
    if (test4()) {
        test_success();
    }
    else {
      test_failure("orte_dps test4 failed");
    }

    fprintf(test_out, "executing test5\n");
    if (test5()) {
        test_success();
    }
    else {
      test_failure("orte_dps test5 failed");
    }

    fprintf(test_out, "executing test6\n");
    if (test6()) {
        test_success();
    }
    else {
      test_failure("orte_dps test6 failed");
    }

    fprintf(test_out, "executing test7\n");
    if (test7()) {
        test_success();
    }
    else {
      test_failure("orte_dps test7 failed");
    }

    fprintf(test_out, "executing test8\n");
    if (test8()) {
        test_success();
    }
    else {
      test_failure("orte_dps test8 failed");
    }

    fprintf(test_out, "executing test9\n");
    if (test9()) {
        test_success();
    }
    else {
      test_failure("orte_dps test9 failed");
    }

    fprintf(test_out, "executing test10\n");
    if (test10()) {
        test_success();
    }
    else {
      test_failure("orte_dps test10 failed");
    }

    fprintf(test_out, "executing test11\n");
    if (test11()) {
        test_success();
    }
    else {
      test_failure("orte_dps test11 failed");
    }

    fprintf(test_out, "executing test12\n");
    if (test12()) {
        test_success();
    }
    else {
      test_failure("orte_dps test12 failed");
    }

    fprintf(test_out, "executing test13\n");
    if (test13()) {
        test_success();
    }
    else {
      test_failure("orte_dps test13 failed");
    }

    fprintf(test_out, "executing test14\n");
    if (test14()) {
        test_success();
    }
    else {
      test_failure("orte_dps test14 failed");
    }

    fprintf(test_out, "executing test15\n");
    if (test15()) {
        test_success();
    }
    else {
      test_failure("orte_dps test15 failed");
    }

    ret = test_finalize();
    fclose(test_out);
    return ret;
}

static bool test1(void)        /* verify different buffer inits */
{
    orte_buffer_t *bufA;
    
    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }
    return (true);
}

/*
 * OMPI_INT16 pack/unpack
 */
static bool test2(void) 
{
    orte_buffer_t *bufA;
    int rc;
    int32_t i;
    int16_t src[NUM_ELEMS];
    int16_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = i;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_INT16);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dst, &count, ORTE_INT16);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                test_comment ("test2: invalid results from unpack");
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/*
 * OMPI_INT pack/unpack
 */
static bool test2_1(void) 
{
    orte_buffer_t *bufA;
    int rc;
    int32_t i;
    int src[NUM_ELEMS];
    int dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = i;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_INT);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dst, &count, ORTE_INT);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                test_comment ("test2: invalid results from unpack");
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/*
 * OMPI_INT32 pack/unpack 
 */
static bool test3(void)
{
    orte_buffer_t *bufA;
    int rc;
    int32_t i;
    int32_t src[NUM_ELEMS];
    int32_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = i;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_INT32);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_INT32);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                test_comment ("test2: invalid results from unpack");
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/*
 * ORTE_INT64 pack/unpack 
 */
static bool test3_1(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i;
    int64_t src[NUM_ELEMS];
    int64_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = 1000*i;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_INT64);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_int64 failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_INT64);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_unpack_int64 failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                test_comment ("test2: invalid results from unpack int64");
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/*
 * OMPI_STRING pack/unpack
 */

static bool test4(void)
{
    orte_buffer_t *bufA;
    int rc;
    int32_t i;
    char* src[NUM_ELEMS];
    char* dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        asprintf(&src[i], "%d", i);
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_STRING);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = NULL;

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_STRING);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("test4: orte_dps.unpack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(strcmp(src[j],dst[j]) != 0) {
                test_comment ("test4: invalid results from unpack");
                fprintf(test_out, "item %d src=[%s] len=%d dst=[%s] len=%d\n", j, src[j], (int)strlen(src[j]), dst[j], (int)strlen(dst[j]));
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}


/**
 *  OMPI_NAME pack/unpack 
 */

static bool test5(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i;
    orte_process_name_t src[NUM_ELEMS];
    orte_process_name_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i].cellid = 1000+i;
        src[i].jobid = 100+i;
        src[i].vpid = i;
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_NAME);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_NAME);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("test5: orte_dps.unpack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j].cellid != dst[j].cellid ||
               src[j].jobid != dst[j].jobid ||
               src[j].vpid != dst[j].vpid) {
                test_comment ("test5: invalid results from unpack");
                return(false);
            }
        }
    }
    
    /* cleanup */
     
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return(true);
}

/**
 * OMPI_BOOL pack/unpack
 */

static bool test6(void)
{
    orte_buffer_t *bufA;
    int rc;
    int32_t i;
    bool src[NUM_ELEMS];
    bool dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = ((i % 2) == 0) ? true : false;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_BOOL);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_BOOL);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                test_comment ("test6: invalid results from unpack");
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/**
 * OMPI_BYTE_OBJECT pack/unpack
 */

static bool test7(void) 
{

    orte_buffer_t *bufA;
    int rc;
    int32_t i;
    orte_byte_object_t src[NUM_ELEMS];
    orte_byte_object_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        asprintf((char**)&(src[i].bytes), "%d", i);
        src[i].size = strlen((char*)(src[i].bytes)) + 1;
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_BYTE_OBJECT);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_dps.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_BYTE_OBJECT);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("test7: orte_dps.unpack failed");
            fprintf(test_out, "orte_dps.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j].size != dst[j].size ||
               memcmp(src[j].bytes,dst[j].bytes,src[j].size) != 0) {
                test_comment ("test7: invalid results from unpack");
                fprintf(test_out, "test7: object element %d has incorrect unpacked value\n", j);
                return(false);
            }
        }
    }
    
    /* cleanup */
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/**
 * ompi everything composite multipack/unpack 
 */

static bool test8(void) 
{

    orte_buffer_t *bufA;
    int rc;
    int32_t i;

	/* pack and unpack in this order */
	/* each block now has an offset to make debugging easier.. first block=100, 200,... */
    orte_byte_object_t srco[NUM_ELEMS];
    orte_byte_object_t dsto[NUM_ELEMS];
    orte_process_name_t srcp[NUM_ELEMS];
    orte_process_name_t dstp[NUM_ELEMS];
    char* srcs[NUM_ELEMS];
    char* dsts[NUM_ELEMS];
    bool srcb[NUM_ELEMS];
    bool dstb[NUM_ELEMS];
    int32_t src32[NUM_ELEMS];
    int32_t dst32[NUM_ELEMS];
    int16_t src16[NUM_ELEMS];
    int16_t dst16[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
		/* object offset 100 */
        asprintf((char**)&(srco[i].bytes), "%d", i+100);
        srco[i].size = strlen((char*)(srco[i].bytes)) + 1;

		/* process name */
        srcp[i].cellid = 1000+i;
        srcp[i].jobid = 100+i;
        srcp[i].vpid = i;

		/* strings +200 */
        asprintf(&srcs[i], "%d", i+200);

		/* bool */
        srcb[i] = ((i % 2) == 0) ? true : false;

		/* INT32 +300 */
        src32[i] = i+300;

		/* INT16 +400 */
        src16[i] = i+400;
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
		/* object first */
        rc = orte_dps.pack(bufA, srco, NUM_ELEMS, ORTE_BYTE_OBJECT);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack on object failed");
            fprintf(test_out, "orte_dps.pack failed with return code %d\n", rc);
            return(false);
        }
		/* NAME */
        rc = orte_dps.pack(bufA, srcp, NUM_ELEMS, ORTE_NAME);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack on name failed");
            fprintf(test_out, "orte_dps.pack failed with return code %d\n", rc);
            return(false);
        }
		/* STRING */
        rc = orte_dps.pack(bufA, srcs, NUM_ELEMS, ORTE_STRING);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack on string failed");
            fprintf(test_out, "orte_dps.pack failed with return code %d\n", rc);
            return(false);
        }
		/* BOOL */
        rc = orte_dps.pack(bufA, srcb, NUM_ELEMS, ORTE_BOOL);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack on bool failed");
            fprintf(test_out, "orte_dps.pack failed with return code %d\n", rc);
            return(false);
        }
		/* INT32 */
        rc = orte_dps.pack(bufA, src32, NUM_ELEMS, ORTE_INT32);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack on INT32 failed");
            fprintf(test_out, "orte_dps.pack failed with return code %d\n", rc);
            return(false);
        }
		/* INT16 */
        rc = orte_dps.pack(bufA, src16, NUM_ELEMS, ORTE_INT16);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack on INT16 failed");
            fprintf(test_out, "orte_dps.pack failed with return code %d\n", rc);
            return(false);
        }
    }

/* 	fprintf(test_out,"test8:packed buffer info for STRING with %d iterations %d elements each\n", NUM_ITERS, NUM_ELEMS); */
   
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count;

		/* string */
        for(j=0; j<NUM_ELEMS; j++) dsts[j] = NULL;
		/* bool */
        memset(dstb,-1,sizeof(dstb));
		/* int32 */
        for(j=0; j<NUM_ELEMS; j++) dst32[j] = -1;
		/* int16 */
        for(j=0; j<NUM_ELEMS; j++) dst16[j] = -1;


		/* object */
		count=NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dsto, &count, ORTE_BYTE_OBJECT);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("test8: orte_dps.unpack on object failed");
            fprintf(test_out, "orte_dps.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(srco[j].size != dsto[j].size ||
               memcmp(srco[j].bytes,dsto[j].bytes,srco[j].size) != 0) {
                test_comment ("test8: invalid results from unpack");
                fprintf(test_out, "test8: object element %d has incorrect unpacked value\n", j);
                return(false);
            }
        }

		/* name */
        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dstp, &count, ORTE_NAME);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("test8: orte_dps.unpack on name failed");
            fprintf(test_out, "test8: unpack name failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(srcp[j].cellid != dstp[j].cellid ||
               srcp[j].jobid != dstp[j].jobid ||
               srcp[j].vpid != dstp[j].vpid) {
                test_comment ("test8: invalid results from unpack");
                fprintf(test_out, "test8: name %d has incorrect unpacked value\n", j);
                return(false);
            }
        }

		/* string */
        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dsts, &count, ORTE_STRING);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("test8: orte_dps.unpack on string failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(strcmp(srcs[j],dsts[j]) != 0) {
                test_comment ("test8: invalid results from unpack");
                fprintf(test_out, "item %d src=[%s] len=%d dst=[%s] len=%d\n", j, srcs[j], (int)strlen(srcs[j]), dsts[j], (int)strlen(dsts[j]));
                return(false);
            }
        }
	
		/* bool */
        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dstb, &count, ORTE_BOOL);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack on bool failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
    
        for(j=0; j<NUM_ELEMS; j++) {
            if(srcb[j] != dstb[j]) {
                test_comment ("test8: invalid results from unpack");
                return(false);
            }
        }

		/* int32 */
        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dst32, &count, ORTE_INT32);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack on int32 failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src32[j] != dst32[j]) {
                test_comment ("test8: invalid results from unpack");
                return(false);
            }
        }

		/* int16 */
        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dst16, &count, ORTE_INT16);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack on int16 failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src16[j] != dst16[j]) {
                test_comment ("test8: invalid results from unpack");
                return(false);
            }
        }


    } /* per iteration */
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/* ORTE_KEYVAL */
static bool test9(void)
{
    orte_buffer_t *bufA;
    int rc;
    int32_t i;
    orte_gpr_keyval_t *src[NUM_ELEMS];
    orte_gpr_keyval_t *dst[NUM_ELEMS];

    /* setup source array of keyvals */
    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_gpr_keyval_t);
        asprintf(&(src[i]->key), "%d", i);
        src[i]->type = ((i % 2) == 0) ? ORTE_INT16 : ORTE_INT32;
        if (ORTE_INT16 == src[i]->type)
            src[i]->value.i16 = i;
        else
            src[i]->value.i32 = i;
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_KEYVAL);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with error %s\n",
                                ORTE_ERROR_NAME(rc));
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_KEYVAL);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_unpack (KEYVAL) failed on iteration %d with error %s\n",
                                i, ORTE_ERROR_NAME(rc));
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if (0 != strcmp(src[j]->key, dst[j]->key) ||
                src[j]->type != dst[j]->type) {
                test_comment ("test9: invalid results type/key mismatch from unpack");
                return(false);
            }
            if (ORTE_INT16 == src[j]->type) {
                if (src[j]->value.i16 != dst[j]->value.i16) {
                    test_comment ("test9: invalid results i16 value mismatch from unpack");
                    return(false);
                }
            } else if (ORTE_INT32 == src[j]->type) {
                if (src[j]->value.i32 != dst[j]->value.i32) {
                    test_comment ("test9: invalid results i32 mismatch from unpack");
                    return(false);
                }
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}


/* ORTE_GPR_VALUE */
static bool test10(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i, j, k;
    orte_gpr_value_t *src[NUM_ELEMS];
    orte_gpr_value_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_gpr_value_t);
        src[i]->segment = strdup("test-segment");
        src[i]->num_tokens = (i % 10) + 1; /* ensure there is always at least one */
        src[i]->tokens = (char**)malloc(src[i]->num_tokens * sizeof(char*));
        for (j=0; j < src[i]->num_tokens; j++) {
            src[i]->tokens[j] = strdup("test-token");
        }
        src[i]->cnt = (i % 20) + 1;
        src[i]->keyvals = (orte_gpr_keyval_t**)malloc(src[i]->cnt * sizeof(orte_gpr_keyval_t*));
        for (j=0; j < src[i]->cnt; j++) {
            src[i]->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
            asprintf(&((src[i]->keyvals[j])->key), "%lu", 
                     (unsigned long) j);
            (src[i]->keyvals[j])->type = ((j % 2) == 0) ? ORTE_INT16 : ORTE_INT32;
            if (ORTE_INT16 == (src[i]->keyvals[j])->type)
                (src[i]->keyvals[j])->value.i16 = j;
            else
                (src[i]->keyvals[j])->value.i32 = j;
        }
    }

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_GPR_VALUE);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_dps.pack failed with error %s\n",
                                ORTE_ERROR_NAME(rc));
            return(false);
        }
    }

/* debugging */
/* printf( "memory of dps object %u\n", (unsigned int) orte_dps_memory_required(src, NUM_ELEMS, ORTE_GPR_VALUE)); */
/* printf("Dumping buffers\n"); */
/* printf("Dump rc = %d\n", orte_dps_dump_buffer((orte_buffer_t *) bufA, 1)); */
/* debugging */



    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_GPR_VALUE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_dps.unpack failed with error %s\n",
                                ORTE_ERROR_NAME(rc));
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(0 != strcmp(src[j]->segment, dst[j]->segment) ||
                src[j]->num_tokens != dst[j]->num_tokens ||
                src[j]->cnt != dst[j]->cnt) {
                test_comment ("test10: invalid results from unpack");
                return(false);
            }
            for (k=0; k<src[j]->num_tokens; k++) {
                if (0 != strcmp(src[j]->tokens[k], dst[j]->tokens[k])) {
                   test_comment ("test10: invalid results (tokens) from unpack");
                    return(false);
                }
            }
            for (k=0; k < src[j]->cnt; k++) {
                if (0 != strcmp((src[j]->keyvals[k])->key,
                                (dst[j]->keyvals[k])->key)) {
                    test_comment ("test10: invalid results (keyvalues) from unpack");
                    return(false);
                }
                if ((src[j]->keyvals[k])->type != (dst[j]->keyvals[k])->type) {
                    test_comment ("test10: invalid results (keyvalue types) from unpack");
                    return(false);
                }
                if (ORTE_INT16 == (src[j]->keyvals[k])->type &&
                    (src[j]->keyvals[k])->value.i16 != (dst[j]->keyvals[k])->value.i16) {
                    test_comment ("test10: invalid results (keyvalues.16) from unpack");
                    return(false);
                }
                else if (ORTE_INT32 == (src[j]->keyvals[k])->type &&
                    (src[j]->keyvals[k])->value.i32 != (dst[j]->keyvals[k])->value.i32) {
                    test_comment ("test10: invalid results (keyvalues.32) from unpack");
                    return(false);
                }
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}


/* size_t */
static bool test11(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i;
    size_t src[NUM_ELEMS];
    size_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = 1000*i;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_SIZE);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack size_t failed");
            fprintf(test_out, "orte_pack_size_t failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        size_t j;
        size_t count;

        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dst, &count, ORTE_SIZE);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack size_t failed");
            fprintf(test_out, "orte_unpack_size_t failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                test_comment ("test2: invalid results from unpack size_t");
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}


/* ORTE_APP_CONTEXT */
static bool test12(void)
{
    orte_buffer_t *bufA;
    int rc, n;
    size_t i, j, k;
    orte_app_context_t *src[NUM_ELEMS];
    orte_app_context_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_app_context_t);
		src[i]->idx = i; 
        src[i]->app = strdup("test-application-name");
		src[i]->num_procs = i; /* test between 0 and NUM_ELEMS-1 proc counts */

		/* test arg counts of 1 to NUM_ELEMS+1 */
		src[i]->argc = i+1;
		if (src[i]->argc) { /* if to allow testing of argv count of zero */
        	src[i]->argv = (char**)malloc(src[i]->argc * sizeof(char*));
        	for (n=0; n < src[i]->argc; n++) {
            	src[i]->argv[n] = strdup("test-argv");
        	}
		}

		/* test env counts of 1 to NUM_ELEMS+1 */
		src[i]->num_env = i+1;
		if (src[i]->num_env) { /* if to allow testing of num_env count of zero */
        	src[i]->env = (char**)malloc(src[i]->num_env * sizeof(char*));
        	for (j=0; j < src[i]->num_env; j++) {
            	src[i]->env[j] = strdup("test-env");
        	}
		}

		src[i]->cwd = strdup ("test-cwd");

		/* test imap data for map count = num_procs  */
		src[i]->num_map = i+1;
		if (src[i]->num_map) { /* if to allow testing of map count of zero */
        	src[i]->map_data = (orte_app_context_map_t**)malloc(src[i]->num_map * sizeof(orte_app_context_map_t *));	/* map data type */
        	for (j=0; j < src[i]->num_map; j++) {
        		src[i]->map_data[j] = OBJ_NEW(orte_app_context_map_t);	/* assume we create with new rather than malloc? */
            	src[i]->map_data[j]->map_type = (uint8_t) j;
            	src[i]->map_data[j]->map_data = strdup("test-map-data");
        	}
		}
	}

	/* source data set, now create buffer and pack source data */

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }

/* fprintf(test_out,"New buffer ready\n"); */
/* fflush(test_out); */

/* 	orte_dps_dump_buffer_simple (bufA, 0); */

    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_APP_CONTEXT);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value failed with return code %d\n", rc);
            return(false);
        }
/* 		fprintf(test_out,"Packed iter %d\n", i); */
/* 		fflush(test_out); */
/* 		orte_dps_dump_buffer_simple (bufA, 0); */
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_APP_CONTEXT);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_unpack_value failed with return code %d (count=%lu)\n", rc, (unsigned long) count);
            return(false);
        }

/* 		fprintf(test_out,"Unpacked iter %d\n", i); */
/* 		fflush(test_out); */
/* 		orte_dps_dump_buffer_simple (bufA, 0); */

        for(j=0; j<NUM_ELEMS; j++) {

            if ( 
				src[j]->idx != dst[j]->idx ||
				0 != strcmp(src[j]->app, dst[j]->app) ||
                src[j]->num_procs != dst[j]->num_procs ||
                src[j]->argc != dst[j]->argc ||
                src[j]->num_env != dst[j]->num_env ||
				0 != strcmp(src[j]->cwd, dst[j]->cwd) ||
                src[j]->num_map != dst[j]->num_map 
				) {
                test_comment ("test12: invalid results from unpack");
                return(false);
            }

			/* now compare each of the size/cnt depedant values */
            for (n=0; n<src[j]->argc; n++) {
                if (0 != strcmp(src[j]->argv[n], dst[j]->argv[n])) {
                   test_comment ("test12: invalid results (argv) from unpack");
                    return(false);
                }
            }
            for (k=0; k<src[j]->num_env; k++) {
                if (0 != strcmp(src[j]->env[k], dst[j]->env[k])) {
                   test_comment ("test12: invalid results (envs) from unpack");
                    return(false);
                }
            }

            for (k=0; k< src[j]->num_map; k++) {
                if ((src[j]->map_data[k])->map_type != (dst[j]->map_data[k])->map_type) {
                    test_comment ("test12: invalid results (map_data types) from unpack");
                    return(false);
                }
                if (0 != strcmp((src[j]->map_data[k])->map_data,
                                (dst[j]->map_data[k])->map_data)) {
                    test_comment ("test12: invalid results (map_data data) from unpack");
                    return(false);
                }
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/* ORTE_GPR_SUBSCRIPTION */
static bool test13(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i, j, k, m;
    orte_gpr_subscription_t *src[NUM_ELEMS];
    orte_gpr_subscription_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_gpr_subscription_t);
        if (i % 2) {
            src[i]->name = strdup("dummy-name");
        }
        src[i]->id = (orte_gpr_subscription_id_t)i;
        
        /* test value counts of 1 to NUM_ELEMS+1 */
        src[i]->cnt = i + 1;
        src[i]->values = (orte_gpr_value_t**)malloc(src[i]->cnt * sizeof(orte_gpr_value_t*));
        
        for (k=0; k < src[i]->cnt; k++) {
            src[i]->values[k] = OBJ_NEW(orte_gpr_value_t);
            src[i]->values[k]->addr_mode = (uint16_t) i;
            src[i]->values[k]->segment = strdup("test-segment");
            
    		/* test token counts of 0! to NUM_ELEMS */
    		src[i]->values[k]->num_tokens = i;
    		if (src[i]->values[k]->num_tokens) { /* if to allow testing of token count of zero */
            	src[i]->values[k]->tokens = (char**)malloc(src[i]->values[k]->num_tokens * sizeof(char*));
            	for (j=0; j < src[i]->values[k]->num_tokens; j++) {
                	src[i]->values[k]->tokens[j] = strdup("test-token");
            	}
    		}

    		/* test key counts of 0 to NUM_ELEMS */
    		src[i]->values[k]->cnt = i;
    		if (src[i]->values[k]->cnt) { /* if to allow testing of num_keys count of zero */
            	src[i]->values[k]->keyvals = (orte_gpr_keyval_t**)malloc(
                            src[i]->values[k]->cnt * sizeof(orte_gpr_keyval_t*));
            	for (j=0; j < src[i]->values[k]->cnt; j++) {
                    src[i]->values[k]->keyvals[j] = OBJ_NEW(orte_gpr_keyval_t);
                	src[i]->values[k]->keyvals[j]->key = strdup("test-key");
            	}
    		}
                /* skip the pointers for cb_func and user_tag */
        }
	}

	/* source data set, now create buffer and pack source data */

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }

    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_GPR_SUBSCRIPTION);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value (ORTE_GPR_SUBSCRIPTION) failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_GPR_SUBSCRIPTION);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_unpack_value (ORTE_GPR_SUBSCRIPTION) failed with return code %d (count=%lu)\n", rc, (unsigned long) count);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {

            if ((NULL == src[j]->name && NULL != dst[j]->name) ||
                (NULL != src[j]->name && NULL == dst[j]->name)
                ) {
                test_comment ("test13: invalid results from unpack");
                return(false);
            }
            
            if ((NULL != src[j]->name &&
                0 != strcmp(src[j]->name, dst[j]->name)) ||
                src[j]->id != dst[j]->id ||
                src[j]->cnt != dst[j]->cnt
				) {
                test_comment ("test13: invalid results from unpack");
                return(false);
            }

			/* now compare each of the size/cnt dependent values */
            for (k=0; k<src[j]->cnt; k++) {
                if (src[j]->values[k]->num_tokens != dst[j]->values[k]->num_tokens) {
                   test_comment ("test13: invalid results (value num_tokens) from unpack");
                    return(false);
                }
                
                for (m=0; m < src[j]->values[k]->num_tokens; m++) {
                    if (0 != strcmp(src[j]->values[k]->tokens[m], dst[j]->values[k]->tokens[m])) {
                       test_comment ("test13: invalid results (tokens) from unpack");
                        return(false);
                    }
                }

                if (src[j]->values[k]->cnt != dst[j]->values[k]->cnt) {
                   test_comment ("test13: invalid results (value cnt) from unpack");
                    return(false);
                }
                
                for (m=0; m < src[j]->values[k]->cnt; m++) {
                    if (0 != strcmp(src[j]->values[k]->keyvals[m]->key,
                                    dst[j]->values[k]->keyvals[m]->key)) {
                        test_comment ("test13: invalid results (keys) from unpack");
                        return(false);
                    }
                }
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

/* ORTE_GPR_NOTIFY_DATA */
static bool test14(void)
{
    orte_buffer_t *bufA;
    int rc;
    size_t i, j, k, l;
    orte_gpr_notify_data_t *src[NUM_ELEMS];
    orte_gpr_notify_data_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = OBJ_NEW(orte_gpr_notify_data_t);
		src[i]->id = i; 

		/* test value counts of 0 to NUM_ELEMS-1 */
		src[i]->cnt = i; /* value count */

		if (0 < src[i]->cnt) { /* if to allow testing of GPR value count of zero */

        	src[i]->values = (orte_gpr_value_t**)malloc(src[i]->cnt * sizeof(orte_gpr_value_t*));

        	for (j=0; j < src[i]->cnt; j++) {
            	src[i]->values[j] = OBJ_NEW(orte_gpr_value_t);
				src[i]->values[j]->addr_mode = (orte_gpr_addr_mode_t) i+j+1; 
				src[i]->values[j]->segment = strdup("test-gpr-notify-value-segment-name");	/* ek segment name again! */

				/* tokens */
				src[i]->values[j]->num_tokens = j; /* test tokens within gpr values within notify message between 0-NUM_ELEMS-1 */
				if (src[i]->values[j]->num_tokens) { /* if to allow testing of num_tokens count of zero */
        			src[i]->values[j]->tokens = (char**)malloc(src[i]->values[j]->num_tokens * sizeof(char*));
        			for (k=0; k < src[i]->values[j]->num_tokens; k++) {
            			src[i]->values[j]->tokens[k] = strdup("test-grp-notify-value-token");
        			} /* for each token */
				} /* if tokens */

				/* keyval pairs (field name is 'cnt' same as used for value count so be carefull) */
				src[i]->values[j]->cnt = j; /* test keyval pairs within gpr values within notify message between 0-NUM_ELEMS-1 */
				if (src[i]->values[j]->cnt) { /* if to allow testing of keyval pair count of zero */
        			src[i]->values[j]->keyvals = (orte_gpr_keyval_t**)malloc(src[i]->values[j]->cnt * sizeof(orte_gpr_keyval_t*));
        			for (k=0; k < src[i]->values[j]->cnt; k++) {
            			src[i]->values[j]->keyvals[k] = OBJ_NEW (orte_gpr_keyval_t);
            			src[i]->values[j]->keyvals[k]->key = strdup("test-grp-notify-value-key");
            			src[i]->values[j]->keyvals[k]->type = ORTE_INT32; /* make it simplier */
						src[i]->values[j]->keyvals[k]->value.i32 = (uint32_t) (i*100)+(j*10)+k; /* something variable */
        			} /* for each keyval pair */
				} /* if keyvals */

        	} /* for each value */
		}
	}

	/* source data set, now create buffer and pack source data */

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }

    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_GPR_NOTIFY_DATA);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack_value (ORTE_GPR_NOTIFY_DATA) failed with return code %d\n", rc);
            return(false);
        }
    }

    
    for (i=0; i<NUM_ITERS; i++) {
        int j;
        size_t count = NUM_ELEMS;

        rc = orte_dps.unpack(bufA, dst, &count, ORTE_GPR_NOTIFY_DATA);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_unpack_value (ORTE_GPR_NOTIFY_DATA) failed with return code %d (count=%lu)\n", rc, (unsigned long) count);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {

            if ( 
                src[j]->id != dst[j]->id ||
                src[j]->cnt != dst[j]->cnt 
				) {
                test_comment ("test14: invalid results from unpack");
                return(false);
            }

			/* now compare each value of the cnt depedant values */
            for (k=0; k<src[j]->cnt; k++) {

				if (src[j]->values[k]->addr_mode != dst[j]->values[k]->addr_mode) {
                   test_comment ("test14: invalid results (values-addr-mode) from unpack");
                    return(false);
				}

                if (0 != strcmp(src[j]->values[k]->segment, dst[j]->values[k]->segment)) {
                   test_comment ("test14: invalid results (values-segment) from unpack");
                    return(false);
                }

				if (src[j]->values[k]->num_tokens != dst[j]->values[k]->num_tokens) {
                   test_comment ("test14: invalid results (values-num_tokens) from unpack");
                    return(false);
				}
            	for (l=0; l<src[j]->values[k]->num_tokens; l++) {
				   if (0 != strcmp(src[j]->values[k]->tokens[l], dst[j]->values[k]->tokens[l])) {
					  test_comment ("test14: invalid results (values-tokens) from unpack");
					   return(false);
				   }
			    } /* for each token inside each grp value */

				if (src[j]->values[k]->cnt != dst[j]->values[k]->cnt) {
                   test_comment ("test14: invalid results (values-cnt (of keyval pairs)) from unpack");
                    return(false);
				}
            	for (l=0; l< src[j]->values[k]->cnt; l++) { 
                	if (0 != strcmp(src[j]->values[k]->keyvals[l]->key, dst[j]->values[k]->keyvals[l]->key)) {
					   test_comment ("test14: invalid results (values-keyvals-key) from unpack");
					   return(false);
                	}
                	if (src[j]->values[k]->keyvals[l]->type != dst[j]->values[k]->keyvals[l]->type) {
					   test_comment ("test14: invalid results (values-keyvals-type) from unpack");
					   return(false);
                	}
                	if (src[j]->values[k]->keyvals[l]->value.i32 != dst[j]->values[k]->keyvals[l]->value.i32) {
					   test_comment ("test14: invalid results (values-keyvals-value.i32) from unpack");
					   return(false);
                	}
			   }/* for each keyvalpair inside each grp value */
            } /* for each grp value */

        } /* for each ELEMENT */
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}


/*
 * pid_t pack/unpack
 */
static bool test15(void) 
{
    orte_buffer_t *bufA;
    int rc;
    size_t i;
    pid_t src[NUM_ELEMS];
    pid_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = (pid_t)i;

    bufA = OBJ_NEW(orte_buffer_t);
    if (NULL == bufA) {
        test_comment("orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }
    
    for (i=0;i<NUM_ITERS;i++) {
        rc = orte_dps.pack(bufA, src, NUM_ELEMS, ORTE_PID);
        if (ORTE_SUCCESS != rc) {
            test_comment ("orte_dps.pack failed");
            fprintf(test_out, "orte_pack pid_t failed with return code %d\n", rc);
            return(false);
        }
    }
    
    for (i=0; i<NUM_ITERS; i++) {
        size_t j;
        size_t count;

        count = NUM_ELEMS;
        rc = orte_dps.unpack(bufA, dst, &count, ORTE_PID);
        if (ORTE_SUCCESS != rc || count != NUM_ELEMS) {
            test_comment ("orte_dps.unpack failed");
            fprintf(test_out, "orte_pack pid_t failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                test_comment ("test2: invalid results from unpack");
                return(false);
            }
        }
    }
         
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        test_comment("OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}

