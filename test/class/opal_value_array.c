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
 * Copyright (c) 2008-2014 Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2026      Jeffrey M. Squyres.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

/*
 * This test is intended to test the opal_value_array class
 */

#include "opal_config.h"
#include <assert.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "opal/class/opal_value_array.h"
#include "opal/runtime/opal.h"
#include "support.h"

#define NUM_ITEMS 10

int main(int argc, char **argv)
{
    uint64_t i, val;
    uint64_t count;
    opal_value_array_t array;

    test_init("opal_value_array_t");

    i = opal_init_util(&argc, &argv);
    test_verify_int(OPAL_SUCCESS, i);
    if (OPAL_SUCCESS != i) {
        test_finalize();
        exit(1);
    }

    OBJ_CONSTRUCT(&array, opal_value_array_t);

    opal_value_array_init(&array, sizeof(uint64_t));
    test_verify_int(0, opal_value_array_get_size(&array));

    /* add several items to the array */
    for (i = 0; i < NUM_ITEMS; i++) {
        opal_value_array_append_item(&array, &i);
    }
    test_verify_int(NUM_ITEMS, opal_value_array_get_size(&array));

    /* verify contents */
    for (i = 0; i < NUM_ITEMS; i++) {
        val = OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i);
        if (val != i) {
            test_failure("Comparison failure");
            fprintf(stderr, " Expected result: %lld\n", (long long) i);
            fprintf(stderr, " Test result: %lld\n", (long long) val);
            fflush(stderr);
        }
    }

    /* re-init array with new type */
    opal_value_array_init(&array, sizeof(uint64_t));
    test_verify_int(0, opal_value_array_get_size(&array));

    /* set fixed size */
    opal_value_array_set_size(&array, NUM_ITEMS);

    /* initialize array */
    count = 0;
    for (i = 0; i < NUM_ITEMS; i++) {
        OPAL_VALUE_ARRAY_SET_ITEM(&array, uint64_t, i, count++);
    }

    /* grow it */
    for (i = 0; i < NUM_ITEMS; i++) {
        opal_value_array_append_item(&array, &count);
        count++;
    }
    /* check size */
    test_verify_int(count, opal_value_array_get_size(&array));

    /* validate contents */
    for (i = 0; i < count; i++) {
        test_verify_int(i, OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
    }

    /* remove an item */
    opal_value_array_remove_item(&array, NUM_ITEMS);

    /* check size */
    test_verify_int(count - 1, opal_value_array_get_size(&array));

    /* validate contents */
    for (i = 0; i < count - 1; i++) {
        if (i >= NUM_ITEMS) {
            test_verify_int(i + 1, OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
        } else {
            test_verify_int(i, OPAL_VALUE_ARRAY_GET_ITEM(&array, uint64_t, i));
        }
    }

    OBJ_DESTRUCT(&array);

    /* ------- Additional coverage tests ------- */

    /* opal_value_array_reserve: reserve should grow alloc without changing size */
    {
        opal_value_array_t ra;
        int rc;

        OBJ_CONSTRUCT(&ra, opal_value_array_t);
        opal_value_array_init(&ra, sizeof(uint64_t));

        rc = opal_value_array_reserve(&ra, 50);
        test_verify("opal_value_array_reserve succeeds", OPAL_SUCCESS == rc);
        test_verify("reserve grows allocated capacity", ra.array_alloc_size >= 50);
        test_verify("reserve does not change logical size", 0 == opal_value_array_get_size(&ra));

        /* reserving a smaller size is a no-op */
        rc = opal_value_array_reserve(&ra, 1);
        test_verify("reserve smaller is a no-op (succeeds)", OPAL_SUCCESS == rc);
        test_verify("reserve smaller does not shrink alloc", ra.array_alloc_size >= 50);

        OBJ_DESTRUCT(&ra);
    }

    /* opal_value_array_get_item auto-grow path */
    {
        opal_value_array_t ga;
        void *ptr;
        uint64_t *uptr;

        OBJ_CONSTRUCT(&ga, opal_value_array_t);
        opal_value_array_init(&ga, sizeof(uint64_t));

        /* index 9 is beyond current size (0); get_item should auto-grow */
        ptr = opal_value_array_get_item(&ga, 9);
        test_verify("get_item auto-grows array when index beyond size",
                    NULL != ptr);
        test_verify("get_item auto-grow updates logical size",
                    opal_value_array_get_size(&ga) >= 10);

        /* write to the returned pointer and read it back via GET_ITEM */
        uptr = (uint64_t *) ptr;
        *uptr = UINT64_C(0xCAFEBABE);
        test_verify("get_item auto-grow: stored value readable via GET_ITEM",
                    UINT64_C(0xCAFEBABE) == OPAL_VALUE_ARRAY_GET_ITEM(&ga, uint64_t, 9));

        OBJ_DESTRUCT(&ga);
    }

    /* opal_value_array_set_item (function, not macro) with auto-grow */
    {
        opal_value_array_t si;
        uint64_t wval, rval;
        int rc;

        OBJ_CONSTRUCT(&si, opal_value_array_t);
        opal_value_array_init(&si, sizeof(uint64_t));

        wval = UINT64_C(0x1234567890ABCDEF);
        rc = opal_value_array_set_item(&si, 5, &wval);
        test_verify("set_item auto-grows and succeeds", OPAL_SUCCESS == rc);
        test_verify("set_item auto-grow updates logical size",
                    opal_value_array_get_size(&si) >= 6);

        rval = OPAL_VALUE_ARRAY_GET_ITEM(&si, uint64_t, 5);
        test_verify("set_item stored correct value", rval == wval);

        /* overwrite an existing index */
        wval = UINT64_C(0xDEADBEEF);
        rc = opal_value_array_set_item(&si, 5, &wval);
        test_verify("set_item overwrite succeeds", OPAL_SUCCESS == rc);
        rval = OPAL_VALUE_ARRAY_GET_ITEM(&si, uint64_t, 5);
        test_verify("set_item overwrite stored new value", rval == wval);

        OBJ_DESTRUCT(&si);
    }

    /* OPAL_VALUE_ARRAY_GET_BASE */
    {
        opal_value_array_t ba;
        uint64_t *base;
        size_t j;

        OBJ_CONSTRUCT(&ba, opal_value_array_t);
        opal_value_array_init(&ba, sizeof(uint64_t));
        opal_value_array_set_size(&ba, 8);
        for (j = 0; j < 8; j++) {
            OPAL_VALUE_ARRAY_SET_ITEM(&ba, uint64_t, j, (uint64_t) (j * 10));
        }

        base = OPAL_VALUE_ARRAY_GET_BASE(&ba, uint64_t);
        test_verify("OPAL_VALUE_ARRAY_GET_BASE returns non-NULL pointer",
                    NULL != base);
        {
            int all_ok = 1;
            for (j = 0; j < 8; j++) {
                if (base[j] != (uint64_t) (j * 10)) {
                    all_ok = 0;
                }
            }
            test_verify("OPAL_VALUE_ARRAY_GET_BASE elements match SET_ITEM values",
                        1 == all_ok);
        }

        OBJ_DESTRUCT(&ba);
    }

    opal_finalize_util();

    return test_finalize();
}
