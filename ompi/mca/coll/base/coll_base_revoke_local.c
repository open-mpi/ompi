/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2017 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      Los Alamos National Security, LLC. All Rights
 *                         reserved.
 * Copyright (c) 2015-2016 Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * Copyright (c) 2016-2017 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Siberian State University of Telecommunications
 *                         and Information Science. All rights reserved.
 * Copyright (c) 2022      Cisco Systems, Inc.  All rights reserved.
 * Copyright (c) 2024      Stony Brook University.  All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "mpi.h"
#include "opal/class/opal_list.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/coll/base/coll_base_functions.h"
#include "ompi/mca/coll/base/coll_base_util.h"

int mca_coll_base_revoke_local(ompi_communicator_t* comm){
    // Called on each initialized component, to give each the opportunity to
    // revoke any subcomms
    mca_coll_base_avail_coll_t* avail;
    OPAL_LIST_FOREACH(avail, comm->c_coll->module_list, mca_coll_base_avail_coll_t){
        if(NULL == avail->ac_module) continue;
        if(NULL == avail->ac_module->coll_revoke_local) continue;
        avail->ac_module->coll_revoke_local(comm, avail->ac_module);
    }
    return OMPI_SUCCESS;
}
