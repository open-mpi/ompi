/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2011 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#include <stdio.h>
#include <string.h>

#include "support.h"
#include "opal/util/bit_ops.h"
#include "opal/util/output.h"

/*
#define DEBUG
*/

static int test_hibit(int value, int start);
static int test_cube_dim(int value);
static int test_next_poweroftwo(int value);
static int test_next_poweroftwo_inclusive(int value);

int main(int argc, char* argv[])
{
    int i;
    int vals[] = {0, 1, 2, 3, 4, 5, 127, 128, 129, (1 << 29) -1, (1 << 29), (1 << 29) +1, (1 << 30) -1, (1 << 30) /* And NOT (1 << 30) +1 */};
    test_init("opal_bit_ops()");

#ifdef DEBUG
    printf ("Test usage: ./opal_bit_ops [VALUES]\n");
#endif

    if (1 < argc) {
        for (i = 1; i < argc; i++) {
            int value;
            value = atoi (argv[i]);
            printf ("Testing %d. argument test_next_poweroftwo(%d): %s\n",
                    i, value, test_next_poweroftwo(value) ? "correct" : "wrong");
        }
    }

    for (i = 0; i < (int)(sizeof(vals)/sizeof(vals[0])); i++) {
        test_hibit (vals[i], 8 * sizeof(int) -2);
        test_hibit (vals[i], 3);
        test_cube_dim (vals[i]);
        test_next_poweroftwo (vals[i]);
        test_next_poweroftwo_inclusive (vals[i]);
    }

    /* All done */
    return test_finalize();
}


/* REFERENCE FUNCTION */
static int hibit(int value, int start)
{
    unsigned int mask;

    --start;
    mask = 1 << start;

    for (; start >= 0; --start, mask >>= 1) {
        if (value & mask) {
            break;
        }
    }

    return start;
}

static int test_hibit(int value, int start)
{
    int out;
    int bit = hibit (value, start);

#ifdef DEBUG
    printf ("test_hibit(): value:%d expect:%d\n",
            value, bit);
#endif

    if (bit == (out = opal_hibit (value, start))) {
        test_success();
        return 1;
    } else {
        char * msg;
        asprintf(&msg, "Mismatch for hibit (w/ start:%d): value:%d, expected:%d got:%d\n",
                 start, value, bit, out);
        test_failure(msg);
        free(msg);
    }
    return 0;
}


/* REFERENCE FUNCTION */
static int cube_dim(int value)
{
    int dim, size;

    for (dim = 0, size = 1; size < value; ++dim, size <<= 1);

    return dim;
}

static int test_cube_dim(int value)
{
    int out;
    int dim = cube_dim (value);

#ifdef DEBUG
    printf ("test_cube_dim(): value:%d expect:%d\n",
            value, dim);
#endif

    if (dim == (out = opal_cube_dim (value))) {
        test_success();
        return 1;
    } else {
        char * msg;
        asprintf(&msg, "Mismatch for cube_dim: value:%d, expected:%d got:%d\n",
                 value, dim, out);
        test_failure(msg);
        free(msg);
    }
    return 0;
}


/* REFERENCE FUNCTION */
static int next_poweroftwo(int value)
{
    int power2;

    for (power2 = 1; value; value >>=1, power2 <<=1) /* empty */;

    return power2;
}


static int test_next_poweroftwo(int value)
{
    int out;
    int power2 = next_poweroftwo (value);

#ifdef DEBUG
    printf ("test_next_poweroftwo(): value:%d expect:%d\n",
            value, power2);
#endif

    if (power2 == (out = opal_next_poweroftwo (value))) {
        test_success();
        return 1;
    } else {
        char * msg;
        asprintf(&msg, "Mismatch for power-of-two: value:%d, expected:%d got:%d\n",
                 value, power2, out);
        test_failure(msg);
        free(msg);
    }
    return 0;
}



/* REFERENCE FUNCTION */
static int next_poweroftwo_inclusive(int value)
{
    int power2 = 1;

    while ( power2 < value )
        power2 <<= 1;

    return power2;
}

static int test_next_poweroftwo_inclusive(int value)
{
    int out;
    int power2 = next_poweroftwo_inclusive (value);

#ifdef DEBUG
    printf ("test_next_poweroftwo(): value:%d expect:%d\n",
            value, power2);
#endif

    if (power2 == (out = opal_next_poweroftwo_inclusive (value))) {
        test_success();
        return 1;
    } else {
        char * msg;
        asprintf(&msg, "Mismatch for power-of-two-inclusive: value:%d, expected:%d got:%d\n",
                 value, power2, out);
        test_failure(msg);
        free(msg);
    }

    return 0;
}




