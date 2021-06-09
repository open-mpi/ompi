/*
 * Copyright (c) 2021      Mellanox Technologies. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#ifndef MCA_SCOLL_UCC_COMMON_H
#define MCA_SCOLL_UCC_COMMON_H

#define SCOLL_UCC_CHECK(_call) do { \
    if (UCC_OK != (_call)) {        \
        goto fallback;              \
    }                               \
} while(0)

#define SCOLL_UCC_REQ_INIT(_req, _coll, _module) do {       \
    SCOLL_UCC_CHECK(ucc_collective_init(&_coll, _req,       \
                                       _module->ucc_team)); \
} while(0)

static inline ucc_status_t scoll_ucc_req_wait(ucc_coll_req_h req)
{
    ucc_status_t status;
    while (UCC_OK != (status = ucc_collective_test(req))) {
        if (0 > status) {
            UCC_ERROR("ucc_collective_test failed: %s",
                      ucc_status_string(status));
            return status;
        }
        ucc_context_progress(mca_scoll_ucc_component.ucc_context);
        opal_progress();
    }
    return ucc_collective_finalize(req);
}

#endif
 
