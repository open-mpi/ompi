/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2007      The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2011      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2011-2013 Los Alamos National Security, LLC. All
 *                         rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#if !defined(ORTE_MCA_COMMON_PMI)
#define ORTE_MCA_COMMON_PMI

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

#define ORTE_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "%s[%s:%d:%s] %s: %s\n",                         \
                    ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),                 \
                    __FILE__, __LINE__, __func__,                       \
                    pmi_func, orte_errmgr_base_pmi_error(pmi_err));     \
    } while(0);
OPAL_DECLSPEC char* orte_errmgr_base_pmi_error(int pmi_err);

#endif
