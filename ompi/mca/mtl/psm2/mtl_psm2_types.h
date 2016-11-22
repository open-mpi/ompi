/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2007 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2011      Los Alamos National Security, LLC.
 *                         All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef MTL_PSM2_TYPES_H_HAS_BEEN_INCLUDED
#define MTL_PSM2_TYPES_H_HAS_BEEN_INCLUDED

#include "ompi_config.h"
#include "mtl_psm2.h"

#include "ompi/communicator/communicator.h"

#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/base.h"
#include "mtl_psm2_endpoint.h"

#include "psm2.h"


BEGIN_C_DECLS

/**
 * MTL Module Interface
 */
struct mca_mtl_psm2_module_t {
    mca_mtl_base_module_t super; /**< base MTL interface */

    int32_t      connect_timeout;

    psm2_ep_t	 ep;
    psm2_mq_t	 mq;
    psm2_epid_t  epid;
    psm2_epaddr_t epaddr;
};

typedef struct mca_mtl_psm2_module_t mca_mtl_psm2_module_t;

extern mca_mtl_psm2_module_t ompi_mtl_psm2;

struct mca_mtl_psm2_component_t {
    mca_mtl_base_component_2_0_0_t          super;  /**< base MTL component */
};
typedef struct mca_mtl_psm2_component_t mca_mtl_psm2_component_t;

OMPI_DECLSPEC extern mca_mtl_psm2_component_t mca_mtl_psm2_component;

#define PSM2_MAKE_MQTAG(ctxt,rank,utag,tag) \
    do {                                    \
      (tag).tag0 = utag;                    \
      (tag).tag1 = rank;                    \
      (tag).tag2 = ctxt;                    \
    } while (0)

#define PSM2_MAKE_TAGSEL(user_rank, user_tag, user_ctxt, tag, _tagsel)  \
	do {								\
        (tag).tag0 = user_tag;                                          \
        (tag).tag1 = user_rank;                                         \
        (tag).tag2 = user_ctxt;                                         \
        (_tagsel).tag0 = 0xffffffffULL;                                 \
        (_tagsel).tag1 = 0xffffffffULL;                                 \
        (_tagsel).tag2 = 0xffffffffULL;                                 \
        if((user_tag) == MPI_ANY_TAG)                                   \
        {                                                               \
            (_tagsel).tag0 = 0x80000000ULL;                             \
            (tag).tag0 = 0x00000000ULL;                                 \
        }                                                               \
        if((user_rank) == MPI_ANY_SOURCE)                               \
        {                                                               \
            (_tagsel).tag1 = 0x00000000ULL;                             \
        }                                                               \
    } while (0)

END_C_DECLS

#endif  /* MTL_PSM2_TYPES_H_HAS_BEEN_INCLUDED */
