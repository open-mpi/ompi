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
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
/** @file:
 *
 * the oob framework
 */

#ifndef _MCA_OOB_BASE_H_
#define _MCA_OOB_BASE_H_

#include "orte_config.h"

#ifdef HAVE_UNISTD_H
#include <unistd.h>
#endif
#ifdef HAVE_SYS_UIO_H
#include <sys/uio.h>
#endif
#ifdef HAVE_NET_UIO_H
#include <net/uio.h>
#endif

#include "orte/mca/oob/oob.h"

#include "opal/mca/mca.h"
#include "orte/dss/dss_types.h"
#include "orte/mca/ns/ns_types.h"
#include "orte/mca/gpr/gpr_types.h"
#include "orte/mca/oob/oob_types.h"
#include "orte/mca/rml/rml_types.h"

#if defined(c_plusplus) || defined(__cplusplus)
extern "C" {
#endif

/*
 * global flag for use in timing tests
 */
ORTE_DECLSPEC extern int mca_oob_base_output;
ORTE_DECLSPEC extern orte_std_cntr_t orte_oob_xcast_num_active;

/*
 * Flag indicating if this framework has been opened
 */
ORTE_DECLSPEC extern bool orte_oob_base_already_opened;

/*
 * OOB API
 */

/*
 * Non-blocking versions of send/recv.
*/


/**
 * associate a component and a module that belongs to it
 */
struct mca_oob_base_info_t {
  opal_list_item_t super;
  mca_oob_base_component_t *oob_component;
  mca_oob_t *oob_module;
};
/**
 * Convenience Typedef
 */
typedef struct mca_oob_base_info_t mca_oob_base_info_t;

/**
 * declare the association structure as a class
 */
ORTE_DECLSPEC OBJ_CLASS_DECLARATION(mca_oob_base_info_t);


/*
 * Global functions for MCA overall collective open and close
 */
ORTE_DECLSPEC int mca_oob_base_open(void);
ORTE_DECLSPEC int mca_oob_base_init(void);
ORTE_DECLSPEC int mca_oob_base_module_init(void);
ORTE_DECLSPEC int mca_oob_base_close(void);


/*
 * Global struct holding the selected module's function pointers
 */
ORTE_DECLSPEC extern int mca_oob_base_output;
extern char* mca_oob_base_include;
extern char* mca_oob_base_exclude;
ORTE_DECLSPEC extern opal_list_t mca_oob_base_components;
ORTE_DECLSPEC extern opal_list_t mca_oob_base_modules;

#if defined(c_plusplus) || defined(__cplusplus)
}
#endif
#endif

