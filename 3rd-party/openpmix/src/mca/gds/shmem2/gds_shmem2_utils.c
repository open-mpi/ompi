/*
 * Copyright (c) 2015-2020 Intel, Inc.  All rights reserved.
 * Copyright (c) 2016-2018 IBM Corporation.  All rights reserved.
 * Copyright (c) 2018      Research Organization for Information Science
 *                         and Technology (RIST).  All rights reserved.
 * Copyright (c) 2018-2020 Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2021-2024 Nanook Consulting  All rights reserved.
 * Copyright (c) 2022-2023 Triad National Security, LLC. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "gds_shmem2_utils.h"

pmix_status_t
pmix_gds_shmem2_get_job_tracker(
    const pmix_nspace_t nspace,
    bool create,
    pmix_gds_shmem2_job_t **job
) {
    pmix_status_t rc = PMIX_SUCCESS;

    // Try to find the tracker for this job.
    pmix_gds_shmem2_job_t *ti = NULL, *ijob = NULL;
    pmix_gds_shmem2_component_t *const component = &pmix_mca_gds_shmem2_component;
    PMIX_LIST_FOREACH (ti, &component->jobs, pmix_gds_shmem2_job_t) {
        if (0 == strcmp(nspace, ti->nspace_id)) {
            ijob = ti;
            break;
        }
    }
    // If we didn't find the requested target and we aren't asked
    // to create a new one, then the request cannot be fulfilled.
    if (!ijob && !create) {
        rc = PMIX_ERR_NOT_FOUND;
        goto out;
    }
    // Create one if not found and asked to create one.
    if (!ijob && create) {
        ijob = PMIX_NEW(pmix_gds_shmem2_job_t);
        if (PMIX_UNLIKELY(!ijob)) {
            rc = PMIX_ERR_NOMEM;
            goto out;
        }
        ijob->nspace_id = strdup(nspace);
        if (PMIX_UNLIKELY(!ijob->nspace_id)) {
            rc = PMIX_ERR_NOMEM;
            goto out;
        }
        // See if we already have this nspace in global namespaces.
        pmix_namespace_t *nsi = NULL, *inspace = NULL;
        PMIX_LIST_FOREACH (nsi, &pmix_globals.nspaces, pmix_namespace_t) {
            if (0 == strcmp(nsi->nspace, nspace)) {
                inspace = nsi;
                break;
            }
        }
        // If not, create one and update global namespace list.
        if (!inspace) {
            inspace = PMIX_NEW(pmix_namespace_t);
            if (PMIX_UNLIKELY(!inspace)) {
                rc = PMIX_ERR_NOMEM;
                goto out;
            }
            inspace->nspace = strdup(nspace);
            if (PMIX_UNLIKELY(!inspace->nspace)) {
                rc = PMIX_ERR_NOMEM;
                goto out;
            }
            pmix_list_append(&pmix_globals.nspaces, &inspace->super);
        }
        PMIX_RETAIN(inspace);
        ijob->nspace = inspace;
        // Add it to the list of jobs I'm supporting.
        pmix_list_append(&component->jobs, &ijob->super);
    }
out:
    if (PMIX_UNLIKELY(PMIX_SUCCESS != rc)) {
        if (ijob) {
            PMIX_RELEASE(ijob);
            ijob = NULL;
        }
    }
    *job = ijob;
    return rc;
}

pmix_gds_shmem2_session_t *
pmix_gds_shmem2_get_session_tracker(
    pmix_gds_shmem2_job_t *job,
    uint32_t sid,
    bool create
) {
    // This is an error: we should always be given a job.
    if (PMIX_UNLIKELY(!job)) {
        return NULL;
    }

    pmix_tma_t *const tma = pmix_gds_shmem2_get_session_tma(job);
    pmix_gds_shmem2_component_t *const comp = &pmix_mca_gds_shmem2_component;

    if (NULL == job->session) {
        // No session has been assigned to this job. See
        // if the given ID has already been registered.
        pmix_gds_shmem2_session_t *si;
        PMIX_LIST_FOREACH(si, &comp->sessions, pmix_gds_shmem2_session_t) {
            if (si->smdata->id == sid) {
                // Found it. Point the job tracker at this session.
                PMIX_RETAIN(si);
                job->session = si;
                return si;
            }
        }
        // If it wasn't found, then create it if permitted.
        if (create) {
            si = PMIX_NEW(pmix_gds_shmem2_session_t, tma);
            si->smdata->id = sid;
            PMIX_RETAIN(si);
            job->session = si;
            pmix_list_append(&comp->sessions, &si->super);
            return si;
        }
        else {
            return NULL;
        }
    }
    // If the current session object is pointing to the default global session
    // and we were given a specific session ID, then update it.
    if (UINT32_MAX == job->session->smdata->id) {
        if (UINT32_MAX == sid) {
            // If the given SID is also UINT32_MAX, then we just add to it.
            return job->session;
        }
        // See if the given ID has already been registered.
        pmix_gds_shmem2_session_t *si;
        PMIX_LIST_FOREACH(si, &comp->sessions, pmix_gds_shmem2_session_t) {
            if (si->smdata->id == sid) {
                // Found it. Update the refcount on the current session object.
                PMIX_RELEASE(job->session);
                // Point the job tracker at the new place.
                PMIX_RETAIN(si);
                job->session = si;
                return si;
            }
        }
        // If it wasn't found, then create it if permitted.
        if (create) {
            si = PMIX_NEW(pmix_gds_shmem2_session_t, tma);
            si->smdata->id = sid;
            PMIX_RETAIN(si);
            job->session = si;
            pmix_list_append(&comp->sessions, &si->super);
            return si;
        }
    }
    else if (UINT32_MAX == sid) {
        // It's a wildcard request, so return the job-tracker session.
        return job->session;
    }
    // The job tracker already was assigned a session ID.
    // Check if the new one matches.
    if (PMIX_UNLIKELY(job->session->smdata->id != sid)) {
        // This is an error: you cannot assign a given job to multiple sessions.
        PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
        return NULL;
    }
    // The two must match, so return it.
    return job->session;
}

bool
pmix_gds_shmem2_hostnames_eq(
    const char *h1,
    const char *h2
) {
    return (0 == strcmp(h1, h2));
}

pmix_status_t
pmix_gds_shmem2_get_job_shmem2_by_id(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_shmem_t **shmem2
) {
    switch (shmem2_id) {
        case PMIX_GDS_SHMEM2_JOB_ID:
            *shmem2 = job->shmem2;
            break;
        case PMIX_GDS_SHMEM2_SESSION_ID:
            *shmem2 = job->session->shmem2;
            break;
        case PMIX_GDS_SHMEM2_MODEX_ID:
            *shmem2 = job->modex_shmem2;
            break;
        case PMIX_GDS_SHMEM2_INVALID_ID:
        default:
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            abort();
            return PMIX_ERR_BAD_PARAM;
    }
    return PMIX_SUCCESS;
}

static inline pmix_gds_shmem2_status_t *
get_job_shmem2_status_flagp(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id
) {
    switch (shmem2_id) {
        case PMIX_GDS_SHMEM2_JOB_ID:
            return &job->shmem2_status;
        case PMIX_GDS_SHMEM2_SESSION_ID:
            return &job->session->shmem2_status;
        case PMIX_GDS_SHMEM2_MODEX_ID:
            return &job->modex_shmem2_status;
        case PMIX_GDS_SHMEM2_INVALID_ID:
        default:
            // This is a fatal internal error.
            PMIX_ERROR_LOG(PMIX_ERR_BAD_PARAM);
            abort();
            return NULL;
    }
}

void
pmix_gds_shmem2_set_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_gds_shmem2_status_flag_t flag
) {
    *get_job_shmem2_status_flagp(job, shmem2_id) |= flag;
}

void
pmix_gds_shmem2_clear_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_gds_shmem2_status_flag_t flag
) {
    *get_job_shmem2_status_flagp(job, shmem2_id) &= ~flag;
}

void
pmix_gds_shmem2_clearall_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id
) {
    *get_job_shmem2_status_flagp(job, shmem2_id) = 0;
}

bool
pmix_gds_shmem2_has_status(
    pmix_gds_shmem2_job_t *job,
    pmix_gds_shmem2_job_shmem2_id_t shmem2_id,
    pmix_gds_shmem2_status_flag_t flag
) {
    return (*get_job_shmem2_status_flagp(job, shmem2_id) & flag);
}

/*
 * vim: ft=cpp ts=4 sts=4 sw=4 expandtab
 */
