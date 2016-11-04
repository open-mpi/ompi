/*
 * Copyright (c) 2014-2016 Intel, Inc. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 */

#ifndef MCA_PMI_BASE_H
#define MCA_PMI_BASE_H

#include "opal_config.h"
#include "opal/types.h"

#include "opal/mca/mca.h"
#include "opal/mca/base/mca_base_framework.h"

#include "opal/mca/pmix/pmix_types.h"
#include "opal/mca/pmix/pmix.h"

BEGIN_C_DECLS

OPAL_DECLSPEC extern mca_base_framework_t opal_pmix_base_framework;

/**
 * Select a pmix module
 */
OPAL_DECLSPEC int opal_pmix_base_select(void);

OPAL_DECLSPEC extern bool opal_pmix_base_allow_delayed_server;

OPAL_DECLSPEC void opal_pmix_base_register_handler(opal_list_t *event_codes,
                                                   opal_list_t *info,
                                                   opal_pmix_notification_fn_t evhandler,
                                                   opal_pmix_evhandler_reg_cbfunc_t cbfunc,
                                                   void *cbdata);
OPAL_DECLSPEC void opal_pmix_base_deregister_handler(size_t errhandler,
                                                     opal_pmix_op_cbfunc_t cbfunc,
                                                     void *cbdata);
OPAL_DECLSPEC int opal_pmix_base_notify_event(int status,
                                              const opal_process_name_t *source,
                                              opal_pmix_data_range_t range,
                                              opal_list_t *info,
                                              opal_pmix_op_cbfunc_t cbfunc, void *cbdata);
OPAL_DECLSPEC void opal_pmix_base_evhandler(int status,
                                            const opal_process_name_t *source,
                                            opal_list_t *info, opal_list_t *results,
                                            opal_pmix_notification_complete_fn_t cbfunc, void *cbdata);
OPAL_DECLSPEC int opal_pmix_base_exchange(opal_value_t *info,
                                          opal_pmix_pdata_t *pdat,
                                          int timeout);

OPAL_DECLSPEC void opal_pmix_base_set_evbase(opal_event_base_t *evbase);

typedef struct {
    opal_event_base_t *evbase;
} opal_pmix_base_t;

extern opal_pmix_base_t opal_pmix_base;

END_C_DECLS

#endif
