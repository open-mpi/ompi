/* -*- C -*-
 *
 * Copyright (c) 2004-2005 The Trustees of Indiana University.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 The Trustees of the University of Tennessee.
 *                         All rights reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "base_data_store.h"

#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "runtime/runtime_types.h"
#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "include/constants.h"

/*****************************************************************************/

/*
 * Constructor / Destructor functions
 */

static void data_store_construct(ompi_object_t *obj);
static void data_store_destruct(ompi_object_t *obj);

static void job_item_construct(ompi_object_t *obj);
static void job_item_destruct(ompi_object_t *obj);

static void vpid_item_construct(ompi_object_t *obj);
static void vpid_item_destruct(ompi_object_t *obj);

/*****************************************************************************/


/*
 * Types
 */
struct mca_pcm_base_data_store_t {
  ompi_object_t super;
  ompi_mutex_t *mutex;
  ompi_list_t *job_list;
  mca_ns_base_cellid_t cellid;
};
static OBJ_CLASS_INSTANCE(mca_pcm_base_data_store_t, ompi_object_t, 
			  data_store_construct, data_store_destruct);

struct job_item_t {
  ompi_list_item_t super;
  mca_ns_base_jobid_t jobid;
  ompi_list_t *vpid_list;
};
typedef struct job_item_t job_item_t;
static OBJ_CLASS_INSTANCE(job_item_t, ompi_list_item_t, 
			  job_item_construct, job_item_destruct);

struct vpid_item_t {
  ompi_list_item_t super;
  mca_ns_base_vpid_t vpid;
  ompi_object_t *obj;
};
typedef struct vpid_item_t vpid_item_t;
static OBJ_CLASS_INSTANCE(vpid_item_t, ompi_list_item_t,
			  vpid_item_construct, vpid_item_destruct);

/*****************************************************************************/

/*
 * local functions
 */

static job_item_t *get_job_item(ompi_list_t *list, mca_ns_base_jobid_t jobid, 
				int flags);
static void clean_job_item(ompi_list_t *list, mca_ns_base_jobid_t jobid, 
			   int flags);
static vpid_item_t *get_vpid_item(ompi_list_t *list, 
				  mca_ns_base_vpid_t vpid, int flags);

/*****************************************************************************/

#define DS_CREATE   0x001
#define DS_EXCL     0x002
#define DS_REMOVE   0x004
#define DS_IF_EMPTY 0x008

/*****************************************************************************/

mca_pcm_base_data_store_t*
mca_pcm_base_data_store_init(void)
{
  return OBJ_NEW(mca_pcm_base_data_store_t);
}


void
mca_pcm_base_data_store_finalize(mca_pcm_base_data_store_t *me)
{
  if (NULL != me) OBJ_RELEASE(me);
}


int
mca_pcm_base_data_store_add_data(mca_pcm_base_data_store_t *me,
				 ompi_process_name_t *name,
				 ompi_object_t *obj)
{
  int ret = OMPI_SUCCESS;
  job_item_t *job_item;
  vpid_item_t *vpid_item;

  OMPI_LOCK(me->mutex);
  if (MCA_NS_BASE_CELLID_MAX == me->cellid) {
    me->cellid = ompi_name_server.get_cellid(name);
  }

  job_item = get_job_item(me->job_list, ompi_name_server.get_jobid(name), DS_CREATE);
  if (NULL == job_item) {
    ret = errno;
    goto cleanup;
  }
 
  vpid_item = get_vpid_item(job_item->vpid_list, 
			    ompi_name_server.get_vpid(name), DS_CREATE|DS_EXCL);
  if (NULL == vpid_item) {
    ret = errno;
    goto cleanup;
  }

  vpid_item->obj = obj;

 cleanup:
  OMPI_UNLOCK(me->mutex);
  return ret;
}


ompi_object_t *
mca_pcm_base_data_store_get_proc_data(mca_pcm_base_data_store_t *me,
				      ompi_process_name_t *name,
				      bool remove)
{
  ompi_object_t *ret = NULL;
  job_item_t *job_item;
  vpid_item_t *vpid_item;
  int flags = remove ? DS_REMOVE : 0;

  OMPI_LOCK(me->mutex);
  job_item = get_job_item(me->job_list, ompi_name_server.get_jobid(name), 0);
  if (NULL == job_item) {
    goto cleanup;
  }

  vpid_item = get_vpid_item(job_item->vpid_list, 
			    ompi_name_server.get_vpid(name), flags);
  if (NULL == vpid_item) {
    goto cleanup;
  }

  if (remove) {
    /* clean up after ourselves */
    clean_job_item(me->job_list, ompi_name_server.get_jobid(name), DS_IF_EMPTY);
  }

  if (! remove) OBJ_RETAIN(vpid_item->obj);
  ret = vpid_item->obj;

 cleanup:
  OMPI_UNLOCK(me->mutex);
  return ret;
}


ompi_pointer_array_t*
mca_pcm_base_data_store_get_job_data(mca_pcm_base_data_store_t *me,
				     mca_ns_base_jobid_t jobid,
				     bool remove)
{
  job_item_t *job_item;
  vpid_item_t *vpid_item;
  ompi_pointer_array_t *ret = NULL;
  ompi_list_item_t *vpid_list_item;

  OMPI_LOCK(me->mutex);
  job_item = get_job_item(me->job_list, jobid, 0);
  if (NULL == job_item) {
    goto cleanup;
  }

  ret = OBJ_NEW(ompi_pointer_array_t);
  if (NULL == ret) {
    goto cleanup;
  }

  for (vpid_list_item = ompi_list_get_first(job_item->vpid_list) ;
       vpid_list_item != ompi_list_get_end(job_item->vpid_list) ;
       vpid_list_item = ompi_list_get_next(vpid_list_item)) {
    vpid_item = (vpid_item_t*) vpid_list_item;

    ompi_pointer_array_add(ret, vpid_item->obj);
    /* always retain - the removal of the lists will cause one
       release and the user get sthe existing status */
    OBJ_RETAIN(vpid_item->obj);
  }

  if (remove) {
    /* clean up after ourselves */
    clean_job_item(me->job_list, jobid, DS_REMOVE);
  }

 cleanup:
  OMPI_UNLOCK(me->mutex);
  return ret;
}


ompi_pointer_array_t*
mca_pcm_base_data_store_get_all_data(mca_pcm_base_data_store_t *me,
				     bool remove)
{
  job_item_t *job_item;
  vpid_item_t *vpid_item;
  ompi_pointer_array_t *ret = NULL;
  ompi_list_item_t *vpid_list_item, *job_list_item;

  OMPI_LOCK(me->mutex);

  ret = OBJ_NEW(ompi_pointer_array_t);
  if (NULL == ret) {
    goto cleanup;
  }

  for (job_list_item = ompi_list_get_first(me->job_list) ;
       job_list_item != ompi_list_get_end(me->job_list) ;
       job_list_item = ompi_list_get_next(job_list_item)) {
    job_item = (job_item_t*) job_list_item;
    for (vpid_list_item = ompi_list_get_first(job_item->vpid_list) ;
	 vpid_list_item != ompi_list_get_end(job_item->vpid_list) ;
	 vpid_list_item = ompi_list_get_next(vpid_list_item)) {
      vpid_item = (vpid_item_t*) vpid_list_item;

      ompi_pointer_array_add(ret, vpid_item->obj);
      /* always retain - the removal of the lists will cause one
         release and the user get sthe existing status */
      OBJ_RETAIN(vpid_item->obj);
    }
  }

  if (remove) {
    while (NULL != (job_list_item = ompi_list_remove_first(me->job_list)) ) {
      OBJ_RELEASE(job_list_item);
    }
  }

 cleanup:
  OMPI_UNLOCK(me->mutex);
  return ret;
}


ompi_pointer_array_t*
mca_pcm_base_data_store_search(mca_pcm_base_data_store_t *me,
			       mca_pcm_base_data_store_search_fn_t search,
			       void *data,
			       bool remove)
{
  
  job_item_t *job_item;
  vpid_item_t *vpid_item;
  ompi_pointer_array_t *ret = NULL;
  ompi_list_item_t *vpid_list_item, *job_list_item, *tmp;

  OMPI_LOCK(me->mutex);

  ret = OBJ_NEW(ompi_pointer_array_t);
  if (NULL == ret) {
    goto cleanup;
  }


  for (job_list_item = ompi_list_get_first(me->job_list) ;
       job_list_item != ompi_list_get_end(me->job_list) ;
       job_list_item = ompi_list_get_next(job_list_item)) {
    job_item = (job_item_t*) job_list_item;

    for (vpid_list_item = ompi_list_get_first(job_item->vpid_list) ;
	 vpid_list_item != ompi_list_get_end(job_item->vpid_list) ;
	 vpid_list_item = ompi_list_get_next(vpid_list_item)) {
      vpid_item = (vpid_item_t*) vpid_list_item;

      if (0 != search(vpid_item->obj, data) ) {
	ompi_pointer_array_add(ret, 
			       ompi_name_server.create_process_name(me->cellid,
								    job_item->jobid,
								    vpid_item->vpid));

	if (remove) {
	  tmp = vpid_list_item;
	  vpid_list_item = ompi_list_remove_item(job_item->vpid_list, 
						 vpid_list_item);
	  OBJ_RELEASE(tmp);
	} /* if (remove) */
      } /* if (search) */
    } /* for (vpid_list_item ... ) */

    if (remove) {
      if (0 == ompi_list_get_size(job_item->vpid_list)) {
	tmp = job_list_item;
	job_list_item = ompi_list_remove_item(me->job_list, job_list_item);
	OBJ_RELEASE(tmp);
      }
    }
  } /* for (job_list_item ... ) */

 cleanup:
  OMPI_UNLOCK(me->mutex);
  return ret;
}

bool
mca_pcm_base_data_store_is_empty(mca_pcm_base_data_store_t *me)
{
  return ompi_list_is_empty(me->job_list);
}

/*****************************************************************************/

static
void
data_store_construct(ompi_object_t *obj)
{
  mca_pcm_base_data_store_t *data_store = (mca_pcm_base_data_store_t*) obj;

  data_store->mutex = OBJ_NEW(ompi_mutex_t);
  data_store->job_list = OBJ_NEW(ompi_list_t);
  data_store->cellid = MCA_NS_BASE_CELLID_MAX;
}


static
void
data_store_destruct(ompi_object_t *obj)
{
  mca_pcm_base_data_store_t *data_store = (mca_pcm_base_data_store_t*) obj;
  ompi_list_item_t *item;

  while (NULL != (item = ompi_list_remove_first(data_store->job_list)) ) {
    OBJ_RELEASE(item);
  }

  if (NULL != data_store->mutex) OBJ_RELEASE(data_store->mutex);
  if (NULL != data_store->job_list) OBJ_RELEASE(data_store->job_list);
  data_store->cellid = MCA_NS_BASE_CELLID_MAX;
}


static
void
job_item_construct(ompi_object_t *obj)
{
  job_item_t *job_item = (job_item_t*) obj;

  job_item->vpid_list = OBJ_NEW(ompi_list_t);
  job_item->jobid = MCA_NS_BASE_JOBID_MAX;
}


static
void
job_item_destruct(ompi_object_t *obj)
{
  job_item_t *job_item = (job_item_t*) obj;
  ompi_list_item_t *item;

  while (NULL != (item = ompi_list_remove_first(job_item->vpid_list)) ) {
    OBJ_RELEASE(item);
  }

  if (NULL != job_item->vpid_list) OBJ_RELEASE(job_item->vpid_list);
  job_item->jobid = MCA_NS_BASE_JOBID_MAX;
}


static
void
vpid_item_construct(ompi_object_t *obj)
{
  vpid_item_t *vpid_item = (vpid_item_t*) obj;

  vpid_item->vpid = MCA_NS_BASE_VPID_MAX;
  vpid_item->obj = NULL;
}


static
void
vpid_item_destruct(ompi_object_t *obj)
{
  vpid_item_t *vpid_item = (vpid_item_t*) obj;

  vpid_item->vpid = MCA_NS_BASE_VPID_MAX;
  if (NULL != vpid_item->obj) OBJ_RELEASE(vpid_item->obj);
}

/*****************************************************************************/

static
job_item_t *
get_job_item(ompi_list_t *list, mca_ns_base_jobid_t jobid, int flags)
{
  ompi_list_item_t *job_list_item;
  job_item_t *job_item;

  for (job_list_item = ompi_list_get_first(list) ;
       job_list_item != ompi_list_get_end(list) ;
       job_list_item = ompi_list_get_next(job_list_item)) {
    job_item = (job_item_t*) job_list_item;

    if (job_item->jobid == jobid) {
      if (0 != (flags & DS_EXCL)) return NULL;
      if (0 != (flags & DS_REMOVE)) ompi_list_remove_item(list, job_list_item);
      return job_item;
    }
  }

  job_item = NULL;
  if (0 != (flags & DS_CREATE)) {
    job_item = OBJ_NEW(job_item_t);
    job_item->jobid = jobid;
    ompi_list_append(list, (ompi_list_item_t*) job_item);
  }

  return job_item;
}


static
void
clean_job_item(ompi_list_t *list, mca_ns_base_jobid_t jobid, int flags)
{
  ompi_list_item_t *job_list_item, *tmp;
  job_item_t *job_item;

  if (0 == flags) return;

  for (job_list_item = ompi_list_get_first(list) ;
       job_list_item != ompi_list_get_end(list) ;
       job_list_item = ompi_list_get_next(job_list_item)) {
    job_item = (job_item_t*) job_list_item;

    if (job_item->jobid == jobid) {
      bool should_remove = false;

      if (0 != (flags & DS_REMOVE)) {
	should_remove = true;
      } else if (0 != (flags &DS_IF_EMPTY)) {
	if (ompi_list_is_empty(list)) should_remove = true;
      }

      if (should_remove) {
	tmp = job_list_item;
	job_list_item = ompi_list_remove_item(list, job_list_item);
	/* the obj_release will also clean up all the vpid lists */
	OBJ_RELEASE(tmp);
      }
    }
  }
}


static
vpid_item_t *
get_vpid_item(ompi_list_t *list, mca_ns_base_vpid_t vpid, int flags)
{
  ompi_list_item_t *vpid_list_item;
  vpid_item_t *vpid_item;

  for (vpid_list_item = ompi_list_get_first(list) ;
       vpid_list_item != ompi_list_get_end(list) ;
       vpid_list_item = ompi_list_get_next(vpid_list_item)) {
    vpid_item = (vpid_item_t*) vpid_list_item;

    if (vpid_item->vpid == vpid) {
      if (0 != (flags & DS_EXCL)) return NULL;
      if (0 != (flags & DS_REMOVE)) ompi_list_remove_item(list, vpid_list_item);
      return vpid_item;
    }
  }

  vpid_item = NULL;
  if (0 != (flags & DS_CREATE)) {
    vpid_item = OBJ_NEW(vpid_item_t);
    vpid_item->vpid = vpid;
    ompi_list_append(list, (ompi_list_item_t*) vpid_item);
  }

  return vpid_item;
}

/*****************************************************************************/

int
mca_pcm_base_data_store_procs_search(ompi_object_t *obj, void *data)
{
  mca_pcm_base_data_store_pid_t *data_obj = (mca_pcm_base_data_store_pid_t*) obj;
  pid_t pid;

  if (NULL == data) return 0;
  pid = *((pid_t*) data);

  if (data_obj->pid == pid) return 1;

  return 0;
}

OBJ_CLASS_INSTANCE(mca_pcm_base_data_store_pid_t, ompi_object_t, NULL, NULL);

static
int
pids_cmp(const void *leftp, const void *rightp)
{
    pid_t *left = (pid_t*) leftp;
    pid_t *right = (pid_t*) rightp;

    return *left - *right;
}

int
mca_pcm_base_data_store_pids_uniqify(pid_t **pids, size_t *len)
{
    size_t i, j;

    if (0 == *len) return OMPI_SUCCESS;

    qsort(*pids, *len, sizeof(pid_t), pids_cmp);

    for (i = 0 ; i < *len - 1 ;) {
        if ((*pids)[i] == (*pids)[i + 1]) {
            for (j = i + 1 ; j < *len - 1 ; ++j) {
                (*pids)[j] = (*pids)[j + 1];
            }
            (*len)--;
        }
        if ((*pids)[i] != (*pids)[i + 1]) i++;
    }

    return OMPI_SUCCESS;
}
