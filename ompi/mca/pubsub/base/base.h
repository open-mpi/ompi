/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2007 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2012-2013 Los Alamos National Security, LLC.
 *                         All rights reserved
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */
#ifndef OMPI_MCA_PUBSUB_BASE_H
#define OMPI_MCA_PUBSUB_BASE_H

#include "ompi_config.h"
#include "ompi/constants.h"

#include "ompi/mca/pubsub/pubsub.h"

/*
 * Global functions for MCA overall PUBSUB
 */

BEGIN_C_DECLS

/*
 * MCA framework
 */
OMPI_DECLSPEC extern mca_base_framework_t ompi_pubsub_base_framework;
/*
 * Select an available component.
 */
OMPI_DECLSPEC int ompi_pubsub_base_select(void);

/* NULL functions */
OMPI_DECLSPEC int ompi_pubsub_base_null_publish(const char *service, ompi_info_t *info, const char *port);
OMPI_DECLSPEC int ompi_pubsub_base_null_unpublish(const char *service, ompi_info_t *info);
OMPI_DECLSPEC char* ompi_pubsub_base_null_lookup(const char *service, ompi_info_t *info);

/* useful globals */
OMPI_DECLSPEC extern ompi_pubsub_base_module_t ompi_pubsub;

END_C_DECLS

#endif /* OMPI_MCA_PUBSUB_BASE_H */
