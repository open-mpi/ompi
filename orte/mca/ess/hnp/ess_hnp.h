/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2008 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2006 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2017      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * Copyright (c) 2017      Intel, Inc.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef ORTE_ESS_HNP_H
#define ORTE_ESS_HNP_H

BEGIN_C_DECLS

/*
 * Module open / close
 */
typedef struct {
    opal_list_item_t super;
    char *signame;
    int signal;
} ess_hnp_signal_t;
OBJ_CLASS_DECLARATION(ess_hnp_signal_t);

typedef struct {
    orte_ess_base_component_t base;
    opal_list_t signals;
} orte_ess_hnp_component_t;

ORTE_MODULE_DECLSPEC extern orte_ess_hnp_component_t mca_ess_hnp_component;

END_C_DECLS

#endif /* ORTE_ESS_HNP_H */
