/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "opal_config.h"
#include <assert.h>

#include "support.h"
#include "opal/class/opal_fifo.h"
#include "opal/runtime/opal.h"
#include "opal/constants.h"

#include <stdlib.h>
#include <stdio.h>
#include <stddef.h>

#include <sys/time.h>


int main (int argc, char *argv[]) {
    opal_atomic_lock_t lock;
    int rc;

    rc = opal_init_util (&argc, &argv);
    test_verify_int(OPAL_SUCCESS, rc);
    if (OPAL_SUCCESS != rc) {
        test_finalize();
        exit (1);
    }

    test_init("opal_atomic_lock_t");

    opal_atomic_init(&lock, OPAL_ATOMIC_UNLOCKED);

    printf ("opal_atomic_trylock on an UNLOCKED atomic\n");

    if (opal_atomic_trylock(&lock)) {
        test_failure("trylock could not lock an UNLOCKED atomic\n");
    } else {
        test_success();
    }

    printf ("opal_atomic_trylock on a LOCKED atomic\n");

    if (opal_atomic_trylock(&lock)) {
        test_success();
    } else {
        test_failure("trylock could lock an atomic previously locked with trylock\n");
    }

    opal_atomic_unlock(&lock);

    opal_atomic_lock(&lock);

    printf ("opal_atomic_trylock on a LOCKED atomic\n");

    if (opal_atomic_trylock(&lock)) {
        test_success();
    } else {
        test_failure("trylock could lock an atomic previously locked with lock\n");
    }

    opal_atomic_unlock(&lock);

    opal_finalize_util ();

    return test_finalize ();
}
