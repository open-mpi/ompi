/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
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
 * Copyright (c) 2007-2020 Cisco Systems, Inc.  All rights reserved
 * Copyright (c) 2013-2017 Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2015-2019 Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2017-2019 Intel, Inc.  All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "prte_config.h"
#include "constants.h"

#include "src/class/pmix_bitmap.h"
#include "src/mca/base/pmix_base.h"
#include "src/mca/mca.h"
#include "src/pmix/pmix-internal.h"
#include "src/runtime/prte_progress_threads.h"
#include "src/util/pmix_output.h"

#include "src/mca/oob/base/base.h"
#include "src/rml/rml.h"

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public pmix_mca_base_component_t struct.
 */

#include "src/mca/oob/base/static-components.h"

/*
 * Global variables
 */
prte_oob_base_t prte_oob_base = {0};

static int prte_oob_base_close(void)
{
    prte_oob_base_component_t *component;
    pmix_mca_base_component_list_item_t *cli;

    /* shutdown all active transports */
    while (NULL
           != (cli = (pmix_mca_base_component_list_item_t *) pmix_list_remove_first(
                   &prte_oob_base.actives))) {
        component = (prte_oob_base_component_t *) cli->cli_component;
        if (NULL != component->shutdown) {
            component->shutdown();
        }
        PMIX_RELEASE(cli);
    }

    /* destruct our internal lists */
    PMIX_DESTRUCT(&prte_oob_base.actives);

    /* release all peers from the list */
    PMIX_LIST_DESTRUCT(&prte_oob_base.peers);

    return pmix_mca_base_framework_components_close(&prte_oob_base_framework, NULL);
}

/**
 * Function for finding and opening either all MCA components,
 * or the one that was specifically requested via a MCA parameter.
 */
static int prte_oob_base_open(pmix_mca_base_open_flag_t flags)
{
    /* setup globals */
    prte_oob_base.max_uri_length = -1;
    PMIX_CONSTRUCT(&prte_oob_base.peers, pmix_list_t);
    PMIX_CONSTRUCT(&prte_oob_base.actives, pmix_list_t);

    /* Open up all available components */
    return pmix_mca_base_framework_components_open(&prte_oob_base_framework, flags);
}

PMIX_MCA_BASE_FRAMEWORK_DECLARE(prte, oob, "Out-of-Band Messaging Subsystem", NULL,
                                prte_oob_base_open, prte_oob_base_close,
                                prte_oob_base_static_components,
                                PMIX_MCA_BASE_FRAMEWORK_FLAG_DEFAULT);

PMIX_CLASS_INSTANCE(prte_oob_send_t, pmix_object_t, NULL, NULL);

static void pr_cons(prte_oob_base_peer_t *ptr)
{
    PMIX_LOAD_PROCID(&ptr->name, NULL, PMIX_RANK_INVALID);
    ptr->component = NULL;
    PMIX_CONSTRUCT(&ptr->addressable, pmix_bitmap_t);
    pmix_bitmap_init(&ptr->addressable, 8);
}
static void pr_des(prte_oob_base_peer_t *ptr)
{
    PMIX_DESTRUCT(&ptr->addressable);
}
PMIX_CLASS_INSTANCE(prte_oob_base_peer_t, pmix_list_item_t, pr_cons, pr_des);
