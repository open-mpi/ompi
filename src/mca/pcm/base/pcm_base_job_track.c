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
 * setup internal data structures
 */
static void mca_pcm_base_job_item_construct(ompi_object_t *obj);
static void mca_pcm_base_job_item_destruct(ompi_object_t *obj);

static void mca_pcm_base_job_list_construct(ompi_object_t *obj);
static void mca_pcm_base_job_list_destruct(ompi_object_t *obj);

struct mca_pcm_base_pids_t {
    ompi_list_item_t super;
    mca_ns_base_vpid_t lower;
    mca_ns_base_vpid_t upper;
    pid_t starter;
};
typedef struct mca_pcm_base_pids_t mca_pcm_base_pids_t;
static OBJ_CLASS_INSTANCE(mca_pcm_base_pids_t,
                          ompi_list_item_t,
                          NULL, NULL);

struct mca_pcm_base_job_item_t {
    ompi_list_item_t super;
    mca_ns_base_jobid_t jobid;
    ompi_list_t *pids;
};
typedef struct mca_pcm_base_job_item_t mca_pcm_base_job_item_t;
static OBJ_CLASS_INSTANCE(mca_pcm_base_job_item_t,
                          ompi_list_item_t,
                          mca_pcm_base_job_item_construct,
                          mca_pcm_base_job_item_destruct);

struct mca_pcm_base_job_list_t {
    ompi_object_t super;
    ompi_list_t *jobs_list;
    ompi_mutex_t *jobs_mutex;
};
static OBJ_CLASS_INSTANCE(mca_pcm_base_job_list_t,
                          ompi_object_t,
                          mca_pcm_base_job_list_construct,
                          mca_pcm_base_job_list_destruct);



/*
 * internal functions - no locking
 */
static mca_pcm_base_job_item_t *
get_job_item(ompi_list_t *jobs_list, mca_ns_base_jobid_t jobid)
{
    ompi_list_item_t *item;

    for (item = ompi_list_get_first(jobs_list) ;
         item != ompi_list_get_end(jobs_list) ;
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
        if (pids->lower <= vpid && pids->upper >= vpid) {
            return pids;
        }
    }

    return NULL;
}


/*
 * Public functions - locked at top (should not call each other)
 */
mca_pcm_base_job_list_t*
mca_pcm_base_job_list_init(void)
{
    return OBJ_NEW(mca_pcm_base_job_list_t);
}


void
mca_pcm_base_job_list_fini(mca_pcm_base_job_list_t *me)
{
    OBJ_RELEASE(me);
}


int
mca_pcm_base_job_list_add_job_info(mca_pcm_base_job_list_t *me,
                                   mca_ns_base_jobid_t jobid, 
                                   pid_t starter_pid,
                                   mca_ns_base_vpid_t lower, 
                                   mca_ns_base_vpid_t upper)
{
    int ret = OMPI_SUCCESS;
    mca_pcm_base_job_item_t *job_item;
    mca_pcm_base_pids_t *pids;

    OMPI_LOCK(me->jobs_mutex);

    /* get the job item, containing all local information for the
       given jobid.  Create and add if it doesn't exist */
    job_item = get_job_item(me->jobs_list, jobid);
    if (NULL == job_item) {
        job_item = OBJ_NEW(mca_pcm_base_job_item_t);
        if (NULL == job_item) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto finished;
        }
        job_item->jobid = jobid;
        ompi_list_append(me->jobs_list, (ompi_list_item_t*) job_item);
    }

    /* add our little starter to the info for the given jobid */
    pids = OBJ_NEW(mca_pcm_base_pids_t);
    if (NULL == pids) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto finished;
    }
    pids->lower = lower;
    pids->upper = upper;
    pids->starter = starter_pid;

    ompi_list_append(job_item->pids, (ompi_list_item_t*) pids);
    ret = OMPI_SUCCESS;

 finished:
    OMPI_UNLOCK(me->jobs_mutex);
    return ret;
}


pid_t
mca_pcm_base_job_list_get_starter(mca_pcm_base_job_list_t *me,
                                  mca_ns_base_jobid_t jobid, 
                                  mca_ns_base_vpid_t vpid,
                                  bool remove_started_pid)
{
    pid_t ret = -1;
    mca_pcm_base_job_item_t *job_item;
    mca_pcm_base_pids_t *pids;

    OMPI_LOCK(me->jobs_mutex);

    /* try to find the given jobid */
    job_item = get_job_item(me->jobs_list, jobid);
    if (NULL == job_item) { 
        ret = -1;
        goto finished;
    }

    /* and now the given vpid in that jobid */
    pids = get_pids_entry(job_item, vpid);
    if (NULL == pids) {
        ret =  -1;
        goto finished;
    }

    ret = pids->starter;

    if (remove_started_pid) {
        /* remove the starter entry from the jobid entry */
        ompi_list_remove_item(job_item->pids, (ompi_list_item_t*) pids);
        OBJ_RELEASE(pids);
        /* remove the entire jobid entry if there are no more starters
           in the list */
        if (0 == ompi_list_get_size(job_item->pids)) {
            ompi_list_remove_item(me->jobs_list, (ompi_list_item_t*) job_item);
            OBJ_RELEASE(job_item);
        }
    }

 finished:
    OMPI_UNLOCK(me->jobs_mutex);
    return ret;
}


int
mca_pcm_base_job_list_get_starters(mca_pcm_base_job_list_t *me,
                                   mca_ns_base_jobid_t jobid, 
                                   pid_t **pids, size_t *len,
                                   bool remove_started_pids)
{
    int ret = OMPI_SUCCESS;
    mca_pcm_base_job_item_t *job_item;
    mca_pcm_base_pids_t *pid_item;
    ompi_list_item_t *item;
    int i;

    OMPI_LOCK(me->jobs_mutex);

    /* find the given jobid */
    job_item = get_job_item(me->jobs_list, jobid);
    if (NULL == job_item) { 
        ret = OMPI_ERROR;
        goto finished;
    }

    /* and all the starters */
    *len = ompi_list_get_size(job_item->pids);
    if (0 == *len) {
        *pids = NULL;
        goto cleanup;
    }

    *pids = malloc(sizeof(pid_t) * *len);
    if (NULL == *pids) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto finished;
    }

    for (item = ompi_list_get_first(job_item->pids), i = 0 ;
         item != ompi_list_get_end(job_item->pids) ;
         item = ompi_list_get_next(item), ++i) {
        pid_item = (mca_pcm_base_pids_t*) item;
        (*pids)[i] = pid_item->starter;
    }

 cleanup:
    if (remove_started_pids) {
        ompi_list_remove_item(me->jobs_list, (ompi_list_item_t*) job_item);
        /* releasing the job_item will kill all attached pid list stuff */
        OBJ_RELEASE(job_item);
    }

 finished:
    OMPI_UNLOCK(me->jobs_mutex);
    return ret;
}


int
mca_pcm_base_job_list_get_all_starters(mca_pcm_base_job_list_t *me,
                                       pid_t **pids, size_t *len,
                                       bool remove_started_pids)
{
    int ret = OMPI_ERR_NOT_FOUND;
    ompi_list_item_t *job_item, *pid_item;
    mca_pcm_base_pids_t *pids_entry;
    mca_pcm_base_job_item_t *jobs_entry;
    int i = 0;

    OMPI_LOCK(me->jobs_mutex);

    *len = 0;
    *pids = NULL;

    /* go through the array, adding as we go */
    for (job_item = ompi_list_get_first(me->jobs_list) ;
         job_item != ompi_list_get_end(me->jobs_list) ;
         job_item = ompi_list_get_next(job_item)) {
        jobs_entry = (mca_pcm_base_job_item_t*) job_item;

        /* and all the starters */
        *len += ompi_list_get_size(jobs_entry->pids);
        *pids = realloc(*pids, sizeof(pid_t) * *len);
        if (NULL == *pids) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto finished;
        }

        for (pid_item = ompi_list_get_first(jobs_entry->pids) ;
             pid_item != ompi_list_get_end(jobs_entry->pids) ;
             pid_item = ompi_list_get_next(pid_item)) {
            pids_entry = (mca_pcm_base_pids_t*) pid_item;
            (*pids)[i] = pids_entry->starter;
        }
    }

    /* just remove *everyone*, as they all need to go */
    if (remove_started_pids) {
        while (NULL != (job_item = ompi_list_remove_first(me->jobs_list))) {
            OBJ_RELEASE(job_item);
        }
    }    

 finished:
    OMPI_UNLOCK(me->jobs_mutex);
    return ret;
}


int
mca_pcm_base_job_list_get_job_info(mca_pcm_base_job_list_t *me,
                                   pid_t pid, 
                                   mca_ns_base_jobid_t *jobid,
                                   mca_ns_base_vpid_t *lower,
                                   mca_ns_base_vpid_t *upper,
                                   bool remove_started_pids)
{
    int ret = OMPI_ERR_NOT_FOUND;
    ompi_list_item_t *job_item, *pid_item;
    mca_pcm_base_pids_t *pids;
    mca_pcm_base_job_item_t *jobs;

    OMPI_LOCK(me->jobs_mutex);

    /* yeah, this is the worst of the search cases :( */
    for (job_item = ompi_list_get_first(me->jobs_list) ;
         job_item != ompi_list_get_end(me->jobs_list) ;
         job_item = ompi_list_get_next(job_item)) {
        jobs = (mca_pcm_base_job_item_t*) job_item;

        for (pid_item = ompi_list_get_first(jobs->pids) ;
             pid_item != ompi_list_get_end(jobs->pids) ;
             pid_item = ompi_list_get_next(pid_item)) {
            pids = (mca_pcm_base_pids_t*) pid_item;

            if (pids->starter == pid) {
                *jobid = jobs->jobid;
                *lower = pids->lower;
                *upper = pids->upper;
                ret = OMPI_SUCCESS;
                goto cleanup;
            }
        }
    }

 cleanup:
    if (remove_started_pids) {
        /* remove the starter entry from the jobid entry */
        ompi_list_remove_item(jobs->pids, (ompi_list_item_t*) pids);
        OBJ_RELEASE(pids);
        /* remove the entire jobid entry if there are no more starters
           in the list */
        if (0 == ompi_list_get_size(jobs->pids)) {
            ompi_list_remove_item(me->jobs_list, (ompi_list_item_t*) jobs);
            OBJ_RELEASE(jobs);
        }
    }

    OMPI_UNLOCK(me->jobs_mutex);
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
    if (NULL != data->pids) {
        ompi_list_item_t *item;
        while (NULL != (item = ompi_list_remove_first(data->pids))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(data->pids);
    }
}



static
void
mca_pcm_base_job_list_construct(ompi_object_t *obj)
{
    mca_pcm_base_job_list_t *data = (mca_pcm_base_job_list_t*) obj;

    data->jobs_list = OBJ_NEW(ompi_list_t);
    data->jobs_mutex = OBJ_NEW(ompi_mutex_t);
}


static
void
mca_pcm_base_job_list_destruct(ompi_object_t *obj)
{
    mca_pcm_base_job_list_t *data = (mca_pcm_base_job_list_t*) obj;

    if (NULL != data->jobs_list) {
        ompi_list_item_t *item;
    
        while (NULL != (item = ompi_list_remove_first(data->jobs_list))) {
            OBJ_RELEASE(item);
        }

        OBJ_RELEASE(data->jobs_list);
    }

    if (NULL != data->jobs_mutex) OBJ_RELEASE(data->jobs_mutex);
}
