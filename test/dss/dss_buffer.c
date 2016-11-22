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
#ifdef HAVE_SYS_PARAM_H
#include <sys/param.h>
#endif
#include <string.h>

#include "opal/runtime/opal.h"
#include "opal/dss/dss.h"

#define NUM_ITERS 100
#define NUM_ELEMS 1024

static bool test1(void);        /* verify different buffer inits */
static bool test2(void);        /* verify int16 */
static bool test3(void);      /* verify int */
static bool test4(void);        /* verify int32 */
static bool test5(void);      /* verify int64 */
static bool test6(void);        /* verify string */
static bool test7(void);        /* verify BOOL */
static bool test8(void);        /* verify OBJECT */
static bool test9(void);        /* verify composite (multiple types and element counts) */
static bool test10(void);        /* verify KEYVAL */
static bool test11(void);        /* verify int32_t */
static bool test12(void);        /* verify pid_t */

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

    fprintf(test_out, "executing test2\n");
    if (test2()) {
        fprintf(test_out, "Test2 succeeded\n");
    } else {
      fprintf(test_out, "Test2 failed\n");
      ret = 2;
    }

    fprintf(test_out, "executing test3\n");
    if (test3()) {
        fprintf(test_out, "Test3 succeeded\n");
    } else {
      fprintf(test_out, "Test3 failed\n");
      ret = 3;
    }

    fprintf(test_out, "executing test4\n");
    if (test4()) {
        fprintf(test_out, "Test4 succeeded\n");
    } else {
      fprintf(test_out, "Test4 failed\n");
      ret = 4;
    }

    fprintf(test_out, "executing test5\n");
    if (test5()) {
        fprintf(test_out, "Test5 succeeded\n");
    } else {
      fprintf(test_out, "Test5 failed\n");
      ret = 5;
    }

    fprintf(test_out, "executing test6\n");
    if (test6()) {
        fprintf(test_out, "Test6 succeeded\n");
    } else {
      fprintf(test_out, "Test6 failed\n");
      ret = 6;
    }

    fprintf(test_out, "executing test7\n");
    if (test7()) {
        fprintf(test_out, "Test7 succeeded\n");
    } else {
      fprintf(test_out, "Test7 failed\n");
      ret = 7;
    }

    fprintf(test_out, "executing test8\n");
    if (test8()) {
        fprintf(test_out, "Test8 succeeded\n");
    } else {
      fprintf(test_out, "Test8 failed\n");
      ret = 8;
    }

    fprintf(test_out, "executing test9\n");
    if (test9()) {
        fprintf(test_out, "Test9 succeeded\n");
    } else {
      fprintf(test_out, "opal_dss test9 failed\n");
      ret = 9;
    }

    fprintf(test_out, "executing test10\n");
    if (test10()) {
        fprintf(test_out, "Test10 succeeded\n");
    } else {
      fprintf(test_out, "opal_dss test10 failed\n");
      ret = 10;
    }

    fprintf(test_out, "executing test11\n");
    if (test11()) {
        fprintf(test_out, "Test11 succeeded\n");
    } else {
      fprintf(test_out, "opal_dss test11 failed\n");
      ret = 11;
    }

    fprintf(test_out, "executing test12\n");
    if (test12()) {
        fprintf(test_out, "Test12 succeeded\n");
    } else {
      fprintf(test_out, "opal_dss test12 failed\n");
      ret = 12;
    }

    fclose(test_out);

    opal_finalize();

    return ret;
}

static bool test1(void)        /* verify different buffer inits */
{
    opal_buffer_t *bufA;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }
    return (true);
}

/*
 * OMPI_INT16 pack/unpack
 */
static bool test2(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    int16_t src[NUM_ELEMS];
    int16_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = i;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    bufA->type = OPAL_DSS_BUFFER_NON_DESC;

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_INT16);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dst, &count, OPAL_INT16);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack\n");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

/*
 * OMPI_INT pack/unpack
 */
static bool test3(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    int src[NUM_ELEMS];
    int dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = i;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_INT);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dst, &count, OPAL_INT);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack\n");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

/*
 * OMPI_INT32 pack/unpack
 */
static bool test4(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    int32_t src[NUM_ELEMS];
    int32_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = i;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_INT32);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count = NUM_ELEMS;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        rc = opal_dss.unpack(bufA, dst, &count, OPAL_INT32);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack\n");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

/*
 * OPAL_INT64 pack/unpack
 */
static bool test5(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    int64_t src[NUM_ELEMS];
    int64_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = 1000*i;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_INT64);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack int64 failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count = NUM_ELEMS;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = -1;

        rc = opal_dss.unpack(bufA, dst, &count, OPAL_INT64);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack int64 failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack int64\n");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

/*
 * OMPI_STRING pack/unpack
 */

static bool test6(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    char* src[NUM_ELEMS];
    char* dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        asprintf(&src[i], "%d", i);
    }

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_STRING);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count = NUM_ELEMS;

        for(j=0; j<NUM_ELEMS; j++)
            dst[j] = NULL;

        rc = opal_dss.unpack(bufA, dst, &count, OPAL_STRING);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(strcmp(src[j],dst[j]) != 0) {
                fprintf(test_out, "test4: invalid results from unpack\n");
                fprintf(test_out, "item %d src=[%s] len=%d dst=[%s] len=%d\n", j, src[j], (int)strlen(src[j]), dst[j], (int)strlen(dst[j]));
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}


/**
 * OMPI_BOOL pack/unpack
 */

static bool test7(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    bool src[NUM_ELEMS];
    bool dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = ((i % 2) == 0) ? true : false;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_BOOL);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = opal_dss.unpack(bufA, dst, &count, OPAL_BOOL);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test6: invalid results from unpack\n");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

/**
 * OMPI_BYTE_OBJECT pack/unpack
 */

static bool test8(void)
{

    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    opal_byte_object_t *src[NUM_ELEMS];
    opal_byte_object_t *dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++) {
        src[i] = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        asprintf((char**)&(src[i]->bytes), "%d", i);
        src[i]->size = strlen((char*)(src[i]->bytes)) + 1;
    }

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_BYTE_OBJECT);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count = NUM_ELEMS;

        rc = opal_dss.unpack(bufA, dst, &count, OPAL_BYTE_OBJECT);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j]->size != dst[j]->size ||
               memcmp(src[j]->bytes,dst[j]->bytes,src[j]->size) != 0) {
                fprintf(test_out, "test7: invalid results from unpack\n");
                fprintf(test_out, "test7: object element %d has incorrect unpacked value\n", j);
                return(false);
            }
        }
    }

    /* cleanup */
    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

/**
 * ompi everything composite multipack/unpack
 */

static bool test9(void)
{

    opal_buffer_t *bufA;
    int rc;
    int32_t i;

    /* pack and unpack in this order */
    /* each block now has an offset to make debugging easier.. first block=100, 200,... */
    opal_byte_object_t *srco[NUM_ELEMS];
    opal_byte_object_t *dsto[NUM_ELEMS];
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
        srco[i] = (opal_byte_object_t*)malloc(sizeof(opal_byte_object_t));
        asprintf((char**)&(srco[i]->bytes), "%d", i+100);
        srco[i]->size = strlen((char*)(srco[i]->bytes)) + 1;

        /* strings +200 */
        asprintf(&srcs[i], "%d", i+200);

        /* bool */
        srcb[i] = ((i % 2) == 0) ? true : false;

        /* INT32 +300 */
        src32[i] = i+300;

        /* INT16 +400 */
        src16[i] = i+400;
    }

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        /* object first */
        rc = opal_dss.pack(bufA, srco, NUM_ELEMS, OPAL_BYTE_OBJECT);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack on object failed with return code %d\n", rc);
            return(false);
        }
        /* STRING */
        rc = opal_dss.pack(bufA, srcs, NUM_ELEMS, OPAL_STRING);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack on string failed with return code %d\n", rc);
            return(false);
        }
        /* BOOL */
        rc = opal_dss.pack(bufA, srcb, NUM_ELEMS, OPAL_BOOL);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack on bool failed with return code %d\n", rc);
            return(false);
        }
        /* INT32 */
        rc = opal_dss.pack(bufA, src32, NUM_ELEMS, OPAL_INT32);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack on INT32 failed with return code %d\n", rc);
            return(false);
        }
        /* INT16 */
        rc = opal_dss.pack(bufA, src16, NUM_ELEMS, OPAL_INT16);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack on INT16 failed with return code %d\n", rc);
            return(false);
        }
    }

/*  fprintf(test_out,"test8:packed buffer info for STRING with %d iterations %d elements each\n", NUM_ITERS, NUM_ELEMS); */

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count;

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
        rc = opal_dss.unpack(bufA, dsto, &count, OPAL_BYTE_OBJECT);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack on object failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(srco[j]->size != dsto[j]->size ||
               memcmp(srco[j]->bytes,dsto[j]->bytes,srco[j]->size) != 0) {
                fprintf(test_out, "test8: object element %d has incorrect unpacked value\n", j);
                return(false);
            }
        }

        /* string */
        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dsts, &count, OPAL_STRING);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack on string failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(strcmp(srcs[j],dsts[j]) != 0) {
                fprintf(test_out, "test8: invalid results from unpack\n");
                fprintf(test_out, "item %d src=[%s] len=%d dst=[%s] len=%d\n", j, srcs[j], (int)strlen(srcs[j]), dsts[j], (int)strlen(dsts[j]));
                return(false);
            }
        }

        /* bool */
        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dstb, &count, OPAL_BOOL);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack on bool failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(srcb[j] != dstb[j]) {
                fprintf(test_out, "test8: invalid results from unpack\n");
                return(false);
            }
        }

        /* int32 */
        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dst32, &count, OPAL_INT32);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack on int32 failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src32[j] != dst32[j]) {
                fprintf(test_out, "test8: invalid results from unpack\n");
                return(false);
            }
        }

        /* int16 */
        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dst16, &count, OPAL_INT16);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack on int16 failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src16[j] != dst16[j]) {
                fprintf(test_out, "test8: invalid results from unpack\n");
                return(false);
            }
        }


    } /* per iteration */

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}

/* OPAL_DATA_VALUE */
static bool test10(void)
{
    opal_buffer_t *bufA;
    int rc;
    int i;
    int16_t i16[NUM_ELEMS];
    opal_value_t *src[NUM_ELEMS];
    opal_value_t *dst[NUM_ELEMS];

    /* setup source array of data values */
    for(i=0; i<NUM_ELEMS; i++) {
        i16[i] = (int16_t)i;
        src[i] = OBJ_NEW(opal_value_t);
        src[i]->type = ((i % 2) == 0) ? OPAL_INT16 : OPAL_STRING;
        if (OPAL_INT16 == src[i]->type)
            src[i]->data.uint16 = i16[i];
        else
            src[i]->data.string = strdup("truly-a-dumb-test");
    }

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_VALUE);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed with error code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int j;
        int32_t count = NUM_ELEMS;
        memset(dst,-1,sizeof(dst));

        rc = opal_dss.unpack(bufA, dst, &count, OPAL_VALUE);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out,
            "opal_dss.unpack (DATA_VALUE) failed on iteration %d with error code %d\n",
                                i, rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if (src[j]->type != dst[j]->type) {
                fprintf(test_out, "opal_dss.unpack (DATA_VALUE) invalid results type mismatch from unpack\n");
                return(false);
            }
            if (0 != opal_dss.compare(src[j], dst[j], src[j]->type)) {
                fprintf(test_out, "opal_dss.unpack (DATA_VALUE) invalid results value mismatch from unpack");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}


/* int32_t */
static bool test11(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    int32_t src[NUM_ELEMS];
    int32_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = 1000*i;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW\n");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_INT32);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack int32_t failed");
            fprintf(test_out, "orte_pack_int32_t failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int32_t j;
        int32_t count;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dst, &count, OPAL_INT32);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack int32_t failed");
            fprintf(test_out, "orte_unpack_int32_t failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test11: invalid results from unpack int32_t");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer");
        return false;
    }

    return (true);
}


/*
 * pid_t pack/unpack
 */
static bool test12(void)
{
    opal_buffer_t *bufA;
    int rc;
    int32_t i;
    pid_t src[NUM_ELEMS];
    pid_t dst[NUM_ELEMS];

    for(i=0; i<NUM_ELEMS; i++)
        src[i] = (pid_t)i;

    bufA = OBJ_NEW(opal_buffer_t);
    if (NULL == bufA) {
        fprintf(test_out, "orte_buffer failed init in OBJ_NEW");
        fprintf(test_out, "OBJ_NEW failed\n");
        return false;
    }

    for (i=0;i<NUM_ITERS;i++) {
        rc = opal_dss.pack(bufA, src, NUM_ELEMS, OPAL_PID);
        if (OPAL_SUCCESS != rc) {
            fprintf(test_out, "opal_dss.pack failed");
            fprintf(test_out, "orte_pack pid_t failed with return code %d\n", rc);
            return(false);
        }
    }

    for (i=0; i<NUM_ITERS; i++) {
        int32_t j;
        int32_t count;

        count = NUM_ELEMS;
        rc = opal_dss.unpack(bufA, dst, &count, OPAL_PID);
        if (OPAL_SUCCESS != rc || count != NUM_ELEMS) {
            fprintf(test_out, "opal_dss.unpack failed");
            fprintf(test_out, "orte_pack pid_t failed with return code %d\n", rc);
            return(false);
        }

        for(j=0; j<NUM_ELEMS; j++) {
            if(src[j] != dst[j]) {
                fprintf(test_out, "test2: invalid results from unpack");
                return(false);
            }
        }
    }

    OBJ_RELEASE(bufA);
    if (NULL != bufA) {
        fprintf(test_out, "OBJ_RELEASE did not NULL the buffer pointer\n");
        return false;
    }

    return (true);
}
