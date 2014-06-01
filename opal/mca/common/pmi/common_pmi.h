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

#ifndef COMMON_PMI_H
#define COMMON_PMI_H

#include <pmi.h>
#if WANT_PMI2_SUPPORT
#include <pmi2.h>
#endif

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
int mca_common_pmi_init (int preferred_version);

/**
 * mca_common_pmi_finalize:
 *
 * Finalize PMI. PMI initialization is reference counted. The last
 * caller to mca_common_pmi_finalize will cause PMI to be finalized.
 */
void mca_common_pmi_finalize (void);

#define OPAL_PMI_ERROR(pmi_err, pmi_func)                               \
    do {                                                                \
        opal_output(0, "%s [%s:%d:%s]: %s\n",                         \
                    pmi_func, __FILE__, __LINE__, __func__,                       \
                    opal_errmgr_base_pmi_error(pmi_err));     \
    } while(0);

OPAL_DECLSPEC char* opal_errmgr_base_pmi_error(int pmi_err);

int mca_common_pmi_rank(void);
int mca_common_pmi_size(void);
int mca_common_pmi_appnum(void);
int mca_common_pmi_universe(void);
int mca_common_pmi_kvsname(char *buf, int len);

int mca_common_pmi_kvslen(void);
int mca_common_pmi_keylen(void);
int mca_common_pmi_vallen(void);

int mca_common_pmi_id(char **pmi_id_ret, char **error);
int mca_common_pmi_local_info(int vpid, int **ranks_ret,
                              int *procs_ret, char **error);
void mca_common_pmi_abort(int status, char *msg);

// Publish-subscribe operations
int mca_common_pmi_publish(const char *service_name, const char *port_name);
int mca_common_pmi_lookup(const char *service_name, char **port_ret);
int mca_common_pmi_unpublish ( const char *service_name );

// KVS put/get
int mca_common_pmi_put(const char *kvs_name,
                              const char *key, const char *value);

int mca_common_pmi_get(const char *kvs_name, const char *key,
                       char *value, int valuelen);
int mca_common_pmi_commit(char *kvs_name);
int mca_common_pmi_barrier(void);

#endif
#endif
