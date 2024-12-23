/*
 * Copyright (c) 2021-2024 Computer Architecture and VLSI Systems (CARV)
 *                         Laboratory, ICS Forth. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"
#include "mpi.h"

#include "ompi/constants.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/communicator/communicator.h"
#include "ompi/op/op.h"

#include "opal/mca/rcache/base/base.h"
#include "opal/util/show_help.h"
#include "opal/util/minmax.h"

#include "coll_xhc.h"

int mca_coll_xhc_reduce(const void *sbuf, void *rbuf,
        size_t count, ompi_datatype_t *datatype, ompi_op_t *op, int root,
        ompi_communicator_t *ompi_comm, mca_coll_base_module_t *ompi_module) {

    xhc_module_t *module = (xhc_module_t *) ompi_module;

    /* Currently, XHC's reduce only supports the top-level
     * owner as the root (typically rank 0). */
    if(0 == root) {
        return mca_coll_xhc_allreduce_internal(sbuf, rbuf, count,
            datatype, op, ompi_comm, ompi_module, false);
    } else {
        WARN_ONCE("coll:xhc: Warning: XHC does not currently support "
            "non-zero-root reduce; utilizing fallback component");
        return XHC_CALL_FALLBACK(module->prev_colls, XHC_REDUCE,
            reduce, sbuf, rbuf, count, datatype, op, root, ompi_comm);
    }
}
