/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * Copyright (c) 2013      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(OPAL_MCA_COMMON_PMI)
#define OPAL_MCA_COMMON_PMI

#include "opal/util/output.h"

/**
 * mca_common_pmi_init:
 *
 * Attempt to initialize PMI
 *
 * @retval true PMI successfully initialized
 * @retval false PMI could not be initialized
 */
bool mca_common_pmi_init (void);

/**
 * mca_common_pmi_finalize:
 *
 * Finalize PMI. PMI initialization is reference counted. The last
 * caller to mca_common_pmi_finalize will cause PMI to be finalized.
 */
void mca_common_pmi_finalize (void);

#define OPAL_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "[%s:%d:%s] %s: %s\n",                         \
                    __FILE__, __LINE__, __func__,                       \
                    pmi_func, opal_errmgr_base_pmi_error(pmi_err));     \
    } while(0);
OPAL_DECLSPEC char* opal_errmgr_base_pmi_error(int pmi_err);

#endif

bool mca_common_pmi_rank(int *rank);
bool mca_common_pmi_size(int *size);


#if WANT_PMI2_SUPPORT
OPAL_DECLSPEC int *mca_common_pmi2_parse_pmap(char *pmap, int my_rank,
                                              int *node, int *nlrs);
#endif


/**
 * mca_common_pmi_local_ranks:
 *
 * @param my_rank
 * @param local_rank_count set to the number of local ranks returned
 *
 * @retval array that contains ranks local to my_rank or NULL
 * on failure. Array must be freed by the caller.
 */
OPAL_DECLSPEC int *mca_common_pmi_local_ranks (int my_rank, int *local_rank_count);
