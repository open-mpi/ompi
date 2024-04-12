/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2020 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2024 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#ifndef PMIX_GDS_SHMEM2_UTILS_H
#define PMIX_GDS_SHMEM2_UTILS_H

#include "gds_shmem2.h"

#define PMIX_GDS_SHMEM2_OUT(...)                                               \
do {                                                                           \
    pmix_output(0, "gds:" PMIX_GDS_SHMEM2_NAME ":" __VA_ARGS__);               \
} while (0)

#define PMIX_GDS_SHMEM2_VOUT(...)                                              \
do {                                                                           \
    pmix_output_verbose(2, pmix_gds_base_framework.framework_output,           \
                        "gds:" PMIX_GDS_SHMEM2_NAME ":" __VA_ARGS__);          \
} while (0)

#if PMIX_ENABLE_DEBUG
#define PMIX_GDS_SHMEM2_VVOUT(...)                                             \
do {                                                                           \
    pmix_output_verbose(8, pmix_gds_base_framework.framework_output,           \
                        "gds:" PMIX_GDS_SHMEM2_NAME ":" __VA_ARGS__);          \
} while (0)

#define PMIX_GDS_SHMEM2_VVVOUT(...)                                            \
do {                                                                           \
    pmix_output_verbose(9, pmix_gds_base_framework.framework_output,           \
                        "gds:" PMIX_GDS_SHMEM2_NAME ":" __VA_ARGS__);          \
} while (0)
#else
#define PMIX_GDS_SHMEM2_VVOUT(...)                                             \
do { } while (0)

#define PMIX_GDS_SHMEM2_VVVOUT(...)                                            \
do { } while (0)
#endif

#define PMIX_GDS_SHMEM2_VVOUT_HERE()                                           \
PMIX_GDS_SHMEM2_VVOUT("HERE AT %s,%d", __func__, __LINE__)

BEGIN_C_DECLS

PMIX_EXPORT pmix_status_t
pmix_gds_shmem2_get_job_tracker(
    const pmix_nspace_t nspace,
    bool create,
    pmix_gds_shmem2_job_t **job
);

PMIX_EXPORT pmix_gds_shmem2_session_t *
pmix_gds_shmem2_get_session_tracker(
    pmix_gds_shmem2_job_t *job,
    uint32_t sid,
    bool create
);

PMIX_EXPORT bool
pmix_gds_shmem2_hostnames_eq(
    const char *h1,
    const char *h2
);

/**
 * Sets shmem2 to the appropriate pmix_shmem_t *.
 */
PMIX_EXPORT pmix_status_t
pmix_gds_shmem2_get_job_shmem2_by_id(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_shmem_t **shmem2
);

PMIX_EXPORT void
pmix_gds_shmem2_set_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_gds_shmem2_status_flag_t flag
);

PMIX_EXPORT void
pmix_gds_shmem2_clear_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_gds_shmem2_status_flag_t flag
);

PMIX_EXPORT void
pmix_gds_shmem2_clearall_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id
);

PMIX_EXPORT bool
pmix_gds_shmem2_has_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_gds_shmem2_status_flag_t flag
);

static inline pmix_tma_t *
pmix_gds_shmem2_get_job_tma(
    pmix_gds_shmem2_job_t *job
) {
    return &job->smdata->tma;
}

static inline pmix_tma_t *
pmix_gds_shmem2_get_session_tma(
    pmix_gds_shmem2_job_t *job
) {
    assert(job->session);
    if (!job->session) {
        return NULL;
    }
    return &job->session->smdata->tma;
}

static inline void
pmix_gds_shmem2_vout_smdata(
    pmix_gds_shmem2_job_t *job
) {
    PMIX_GDS_SHMEM2_VOUT(
        "shmem2_hdr@%p, "
        "shmem2_data@%p, "
        "smdata tma@%p, "
        "jobinfo@%p, "
        "appinfo@%p, "
        "nodeinfo@%p, "
        "local_hashtab@%p",
        (void *)job->shmem2->hdr_address,
        (void *)job->shmem2->data_address,
        (void *)&job->smdata->tma,
        (void *)job->smdata->jobinfo,
        (void *)job->smdata->appinfo,
        (void *)job->smdata->nodeinfo,
        (void *)job->smdata->local_hashtab
    );
}

static inline void
pmix_gds_shmem2_vout_smmodex(
    pmix_gds_shmem2_job_t *job
) {
    PMIX_GDS_SHMEM2_VOUT(
        "modex_shmem2@%p, "
        "smmodex tma@%p, "
        "hashtab@%p",
        (void *)job->modex_shmem2->data_address,
        (void *)&job->smmodex->tma,
        (void *)job->smmodex->hashtab
    );
}

static inline void
pmix_gds_shmem2_vout_smsession(
    pmix_gds_shmem2_session_t *sesh
) {
    PMIX_GDS_SHMEM2_VOUT(
        "shmem2_hdr@%p, "
        "shmem2_data@%p, "
        "smdata tma@%p, "
        "sessioninfo@%p, "
        "nodeinfo@%p",
        (void *)sesh->shmem2->hdr_address,
        (void *)sesh->shmem2->data_address,
        (void *)&sesh->smdata->tma,
        (void *)sesh->smdata->sessioninfo,
        (void *)sesh->smdata->nodeinfo
    );
}

END_C_DECLS

#endif

/*
 * vim: ft=cpp ts=4 sts=4 sw=4 expandtab
 */
