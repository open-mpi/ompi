/*
 * Copyright (c) 2017      Amazon.com, Inc. or its affiliates.
 *                         All Rights reserved.
 * Copyright (c) 2020      Intel, Inc.  All rights reserved.
 * Copyright (c) 2020      Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"

#include "src/class/pmix_object.h"

#include "src/mca/prtereachable/base/base.h"
#include "src/mca/prtereachable/prtereachable.h"

static void prte_reachable_construct(prte_reachable_t *reachable)
{
    reachable->weights = NULL;
}

static void prte_reachable_destruct(prte_reachable_t *reachable)
{
    if (NULL != reachable->memory) {
        free(reachable->memory);
    }
}

prte_reachable_t *prte_reachable_allocate(unsigned int num_local, unsigned int num_remote)
{
    char *memory;
    unsigned int i;
    prte_reachable_t *reachable = PMIX_NEW(prte_reachable_t);

    reachable->num_local = num_local;
    reachable->num_remote = num_remote;

    /* allocate all the pieces of the two dimensional array in one
       malloc, rather than a bunch of little allocations */
    memory = malloc(sizeof(int *) * num_local + num_local * (sizeof(int) * num_remote));
    if (memory == NULL)
        return NULL;

    reachable->memory = (void *) memory;
    reachable->weights = (int **) reachable->memory;
    memory += (sizeof(int *) * num_local);

    for (i = 0; i < num_local; i++) {
        reachable->weights[i] = (int *) memory;
        memory += (sizeof(int) * num_remote);
    }

    return reachable;
}

PMIX_CLASS_INSTANCE(prte_reachable_t, pmix_object_t, prte_reachable_construct,
                    prte_reachable_destruct);
