/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "base_job_track.h"
#include "include/constants.h"
#include "class/ompi_list.h"
#include "threads/mutex.h"

/*
 * This is a component-level structure (ie - shared among instances)
 */
static ompi_list_t base_jobs;
static ompi_mutex_t base_jobs_mutex;


void
mca_pcm_base_job_list_init(void)
{
    OBJ_CONSTRUCT(&base_jobs_mutex, ompi_mutex_t);
    OBJ_CONSTRUCT(&base_jobs, ompi_list_t);
}

void
mca_pcm_base_job_list_fini(void)
{
    OBJ_DESTRUCT(&base_jobs);
    OBJ_DESTRUCT(&base_jobs_mutex);
}



/*
 * internal functions - no locking
 */
static mca_pcm_base_job_item_t *
get_job_item(mca_ns_base_jobid_t jobid)
{
    ompi_list_item_t *item;

    for (item = ompi_list_get_first(&base_jobs) ;
         item != ompi_list_get_end(&base_jobs) ;
         item = ompi_list_get_next(item) ) {
        mca_pcm_base_job_item_t  *job_item = (mca_pcm_base_job_item_t*) item;
        if (job_item->jobid == jobid) return job_item;
    }

    return NULL;
}


static mca_pcm_base_pids_t *
get_pids_entry(mca_pcm_base_job_item_t *job_item, mca_ns_base_vpid_t vpid)
{
    ompi_list_item_t *item;
    for (item = ompi_list_get_first(job_item->pids) ;
         item != ompi_list_get_end(job_item->pids) ;
         item = ompi_list_get_next(item) ) {
        mca_pcm_base_pids_t *pids = (mca_pcm_base_pids_t*) item;
        if (pids->lower < vpid && pids->upper > vpid) {
            return pids;
        }
    }

    return NULL;
}


/*
 * Public functions - locked at top (should not call each other)
 */
int
mca_pcm_base_add_started_pids(mca_ns_base_jobid_t jobid, pid_t child_pid,
                             mca_ns_base_vpid_t lower, mca_ns_base_vpid_t upper)
{
    int ret = OMPI_SUCCESS;
    mca_pcm_base_job_item_t *job_item;
    mca_pcm_base_pids_t *pids;

    OMPI_LOCK(&base_jobs_mutex);

    job_item = get_job_item(jobid);
    if (NULL == job_item) {
        job_item = OBJ_NEW(mca_pcm_base_job_item_t);
        if (NULL == job_item) {
            ret = OMPI_ERROR;
            goto finished;
        }
        job_item->jobid = jobid;
        ompi_list_append(&base_jobs, (ompi_list_item_t*) job_item);
    }

    pids = OBJ_NEW(mca_pcm_base_pids_t);
    if (NULL == pids) {
        ret = OMPI_ERROR;
        goto finished;
    }
    pids->lower = lower;
    pids->upper = upper;
    pids->child = child_pid;

    ompi_list_append(job_item->pids, (ompi_list_item_t*) pids);
    ret = OMPI_SUCCESS;

 finished:
    OMPI_UNLOCK(&base_jobs_mutex);
    return ret;
}


pid_t
mca_pcm_base_get_started_pid(mca_ns_base_jobid_t jobid, mca_ns_base_vpid_t vpid,
                            bool remove_started_pid)
{
    pid_t ret = -1;
    mca_pcm_base_job_item_t *job_item;
    mca_pcm_base_pids_t *pids;

    OMPI_LOCK(&base_jobs_mutex);

    job_item = get_job_item(jobid);
    if (NULL == job_item) { 
        ret = -1;
        goto finished;
    }

    pids = get_pids_entry(job_item, vpid);
    if (NULL == pids) {
        ret =  -1;
        goto finished;
    }

    ret = pids->child;

    if (remove_started_pid) {
        ompi_list_remove_item(job_item->pids, (ompi_list_item_t*) pids);
        OBJ_RELEASE(pids);
        if (0 == ompi_list_get_size(job_item->pids)) {
            ompi_list_remove_item(&base_jobs, (ompi_list_item_t*) job_item);
            OBJ_RELEASE(job_item);
        }
    }

 finished:
    OMPI_UNLOCK(&base_jobs_mutex);
    return ret;
}


int
mca_pcm_base_get_started_pid_list(mca_ns_base_jobid_t jobid, pid_t **pids, size_t *len,
                                 bool remove_started_pids)
{
    int ret = OMPI_SUCCESS;
    mca_pcm_base_job_item_t *job_item;
    mca_pcm_base_pids_t *pid_item;
    ompi_list_item_t *item;
    int i;

    OMPI_LOCK(&base_jobs_mutex);

    job_item = get_job_item(jobid);
    if (NULL == job_item) { 
        ret = OMPI_ERROR;
        goto finished;
    }

    *len = ompi_list_get_size(job_item->pids);
    if (0 == *len) {
        *pids = NULL;
        goto cleanup;
    }

    *pids = malloc(sizeof(pid_t*) * *len);
    if (NULL == *pids) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto finished;
    }

    for (item = ompi_list_get_first(job_item->pids), i = 0 ;
         item != ompi_list_get_end(job_item->pids) ;
         item = ompi_list_get_next(item), ++i) {
        pid_item = (mca_pcm_base_pids_t*) item;
        (*pids)[i] = pid_item->child;
    }

 cleanup:
    if (remove_started_pids) {
        ompi_list_remove_item(&base_jobs, (ompi_list_item_t*) job_item);
        OBJ_RELEASE(job_item);
    }

 finished:
    OMPI_UNLOCK(&base_jobs_mutex);
    return ret;
}


int
mca_pcm_base_get_job_info(pid_t pid, mca_ns_base_jobid_t *jobid,
                          mca_ns_base_vpid_t *lower,
                          mca_ns_base_vpid_t *upper)
{
    int ret = OMPI_ERR_NOT_FOUND;
    ompi_list_item_t *job_item, *pid_item;
    mca_pcm_base_pids_t *pids;
    mca_pcm_base_job_item_t *jobs;

    OMPI_LOCK(&base_jobs_mutex);

    /* yeah, this is the worst of the search cases :( */
    for (job_item = ompi_list_get_first(&base_jobs) ;
         job_item != ompi_list_get_end(&base_jobs) ;
         job_item = ompi_list_get_next(job_item)) {
        jobs = (mca_pcm_base_job_item_t*) job_item;

        for (pid_item = ompi_list_get_first(jobs->pids) ;
             pid_item != ompi_list_get_end(jobs->pids) ;
             pid_item = ompi_list_get_next(pid_item)) {
            pids = (mca_pcm_base_pids_t*) pid_item;

            if (pids->child == pid) {
                *jobid = jobs->jobid;
                *lower = pids->lower;
                *upper = pids->upper;
                ret = OMPI_SUCCESS;
                goto finished;
            }
        }
    }

 finished:
    OMPI_UNLOCK(&base_jobs_mutex);
    return ret;
}


int
mca_pcm_base_remove_job(mca_ns_base_jobid_t jobid)
{
    int ret = OMPI_SUCCESS;
    mca_pcm_base_job_item_t *job_item;

    OMPI_LOCK(&base_jobs_mutex);

    job_item = get_job_item(jobid);
    if (NULL == job_item) {
        ret = OMPI_ERROR;
        goto finished;
    }

    ompi_list_remove_item(&base_jobs, (ompi_list_item_t*) job_item);

 finished:
    OMPI_UNLOCK(&base_jobs_mutex);
    return ret;
}



static
void
mca_pcm_base_job_item_construct(ompi_object_t *obj)
{
    mca_pcm_base_job_item_t *data = (mca_pcm_base_job_item_t*) obj;
    data->pids = OBJ_NEW(ompi_list_t);
}

static
void
mca_pcm_base_job_item_destruct(ompi_object_t *obj)
{
    mca_pcm_base_job_item_t *data = (mca_pcm_base_job_item_t*) obj;
    if (data->pids != NULL) {
        ompi_list_item_t *item;
        while (NULL != (item = ompi_list_remove_first(data->pids))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(data->pids);
    }
}

OBJ_CLASS_INSTANCE(mca_pcm_base_job_item_t,
                   ompi_list_item_t,
                   mca_pcm_base_job_item_construct,
                   mca_pcm_base_job_item_destruct);

OBJ_CLASS_INSTANCE(mca_pcm_base_pids_t,
                   ompi_list_item_t,
                   NULL, NULL);
