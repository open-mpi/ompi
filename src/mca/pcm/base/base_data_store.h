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

/**
 * @file@
 *
 * \brief Code for storing Job and Process specific local data for PCMs.
 * 
 * PCM helper code for tracking data specific to a job or process
 * started in this process. Data stored via this mechanism should be
 * data that is useful only in this particular process (ie, not data
 * that is useful to store in the registery).
 *
 * Data storage is unique to each mca_pcm_base_data_store_t.  A search
 * or update on one handle does not affect another handle.
 * Synchronization is performed if needed within a single data store,
 * making the interface thread safe (but locking is most likely not
 * optimal).
 */

#ifndef MCA_PCM_BASE_DATA_STORE_H
#define MCA_PCM_BASE_DATA_STORE_H

#ifdef HAVE_SYS_TYPES_H
#include <sys/types.h>
#endif
#include <stdlib.h>
#include <errno.h>

#include "mca/ns/ns.h"
#include "mca/ns/base/base.h"
#include "runtime/runtime_types.h"
#include "class/ompi_object.h"
#include "class/ompi_list.h"
#include "class/ompi_pointer_array.h"
#include "include/constants.h"

/**
 * Opaque data store handle
 */
struct mca_pcm_base_data_store_t;
typedef struct mca_pcm_base_data_store_t mca_pcm_base_data_store_t;

typedef int (*mca_pcm_base_data_store_search_fn_t)(ompi_object_t *obj, void *data);

/**
 * Initialize a data store
 *
 * The returned data store will contain no information and be
 * initialized properly for use in the other functions, including the
 * \c pid_t specific functions
 *
 * @return NULL on failure, useable handle otherwise
 */
mca_pcm_base_data_store_t* mca_pcm_base_data_store_init(void);

/**
 * Clean up a data store
 *
 * Release all data stored in the data store, as well as the data
 * store handle.  The data store handle can not be used after calling
 * this function.
 *
 * @param me Handle returned from \c mca_pcm_base_data_store_init
 */
void mca_pcm_base_data_store_finalize(mca_pcm_base_data_store_t *me);

/**
 * Add information into the data store
 *
 * Add \c obj into the data store \c me, associating it with process
 * name \c name.
 *
 * @param me Handle returned from \c mca_pcm_base_data_store_init
 * @param name Process name with which to associate \c obj
 * @param obj Data to store in data store \c me
 * @return OMPI_SUCCESS on success
 *         error code otherwise
 */
int mca_pcm_base_data_store_add_data(mca_pcm_base_data_store_t *me,
				     ompi_process_name_t *name,
				     ompi_object_t *obj);

/**
 * Search for info on one process
 *
 * Returned object must always be OBJ_RELEASE'd.  If remove is not
 * true, object will remain in list and internally the object will be
 * OBJ_RETAIN'ed before return to user.
 *
 * @param me Handle returned from \c mca_pcm_base_data_store_init
 * @param name Process name to use in data search
 * @param remove Remove returned data from data store if true
 * @return NULL on error (error in errno)
 *        object put in data store if non-NULL
 */
ompi_object_t *mca_pcm_base_data_store_get_proc_data(mca_pcm_base_data_store_t *me,
						     ompi_process_name_t *name,
						     bool remove);

/**
 * Search for info on one job
 *
 * Returned object must always be OBJ_RELEASE'd.  If remove is not
 * true, object will remain in list and internally the object will be
 * OBJ_RETAIN'ed before return to user.
 *
 * @param me Handle returned from \c mca_pcm_base_data_store_init
 * @param jobid Jobid to use in data search
 * @param remove Remove returned data from data store if true
 * @return NULL on error (error in errno)
 *        arrray of pointers to objects otherwise
 */
ompi_pointer_array_t *mca_pcm_base_data_store_get_job_data(
						  mca_pcm_base_data_store_t *me,
						  mca_ns_base_jobid_t jobid,
						  bool remove);
						  

/**
 * Get all data in data store
 *
 * Returned object must always be OBJ_RELEASE'd.  If remove is not
 * true, object will remain in list and internally the object will be
 * OBJ_RETAIN'ed before return to user.
 *
 * @param me Handle returned from \c mca_pcm_base_data_store_init
 * @param remove Remove returned data from data store if true
 * @return NULL on error (error in errno)
 *        arrray of pointers to objects otherwise
 */
ompi_pointer_array_t *mca_pcm_base_data_store_get_all_data(
						  mca_pcm_base_data_store_t *me,
						  bool remove);

/**
 * Search data for matching information
 *
 * Look for object matching criteria.  \c search_fn_t should return 0
 * if object should not be added to the list, non-zero if the object should
 * be returned in the list
 *
 * each process name returned must be freed and the list must be freed
 */
ompi_pointer_array_t *mca_pcm_base_data_store_search(mca_pcm_base_data_store_t *me,
				   mca_pcm_base_data_store_search_fn_t search,
				   void *data,
				   bool remove);

bool mca_pcm_base_data_store_is_empty(mca_pcm_base_data_store_t *me);


/*****************************************************************************
 *
 *    Small wrapper functions to allow easy access to pid_t based data
 *
 *****************************************************************************/

struct mca_pcm_base_data_store_pid_t {
  ompi_object_t super;
  pid_t pid;
};
typedef struct mca_pcm_base_data_store_pid_t mca_pcm_base_data_store_pid_t;
OBJ_CLASS_DECLARATION(mca_pcm_base_data_store_pid_t);

static inline
int
mca_pcm_base_data_store_add_pid(mca_pcm_base_data_store_t *me,
				ompi_process_name_t *name,
				pid_t pid)
{
  mca_pcm_base_data_store_pid_t *data;
  int ret;

  data = OBJ_NEW(mca_pcm_base_data_store_pid_t);
  data->pid = pid;

  ret = mca_pcm_base_data_store_add_data(me, name, (ompi_object_t*) data);

  if (OMPI_SUCCESS != ret) OBJ_RELEASE(data);

  return ret;
}


static inline
pid_t
mca_pcm_base_data_store_get_proc_pid(mca_pcm_base_data_store_t *me,
					   ompi_process_name_t *name,
					   bool remove)
{
  pid_t ret;
  mca_pcm_base_data_store_pid_t *data = (mca_pcm_base_data_store_pid_t*) 
      mca_pcm_base_data_store_get_proc_data(me, name, remove);
  
  if (data == NULL) {
    ret = -1;
  } else {
    ret = data->pid;
    OBJ_RELEASE(data);
  }

  return ret;
}

int mca_pcm_base_data_store_pids_uniqify(pid_t **pids, size_t *len);


static inline 
int
mca_pcm_base_data_store_get_job_pids(mca_pcm_base_data_store_t *me,
				     mca_ns_base_jobid_t jobid,
				     pid_t **pids, size_t *len,
				     bool remove)
{
  ompi_pointer_array_t *array;
  int i;

  array = mca_pcm_base_data_store_get_job_data(me, jobid, remove);
  if (NULL == array) return errno;

  *len = ompi_pointer_array_get_size(array);
  if (0 == *len) {
      *pids = NULL;
  } else {
      *pids = malloc(sizeof(pid_t) * *len);
      if (NULL == *pids) return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (i = 0 ; i < (int) *len ; ++i) {
    mca_pcm_base_data_store_pid_t *data = 
      (mca_pcm_base_data_store_pid_t*) ompi_pointer_array_get_item(array, i);

    (*pids)[i] = data->pid;
    OBJ_RELEASE(data);
  }

  OBJ_RELEASE(array);

  mca_pcm_base_data_store_pids_uniqify(pids, len);

  return OMPI_SUCCESS;
}


static inline
int
mca_pcm_base_data_store_get_all_pids(mca_pcm_base_data_store_t *me,
				     pid_t **pids, size_t *len,
				     bool remove)
{
  ompi_pointer_array_t *array;
  int i = 0;

  array = mca_pcm_base_data_store_get_all_data(me, remove);
  if (NULL == array) return errno;

  *len = ompi_pointer_array_get_size(array);
  if (0 == *len) {
      *pids = NULL;
  } else {
      *pids = malloc(sizeof(pid_t) * *len);
      if (NULL == *pids) return OMPI_ERR_OUT_OF_RESOURCE;
  }

  for (i = 0 ; i < (int) *len ; ++i) {
    mca_pcm_base_data_store_pid_t *data = 
      (mca_pcm_base_data_store_pid_t*) ompi_pointer_array_get_item(array, i);

    (*pids)[i] = data->pid;
    OBJ_RELEASE(data);
  }

  OBJ_RELEASE(array);

  mca_pcm_base_data_store_pids_uniqify(pids, len);

  return OMPI_SUCCESS;
}

/* can't static inline the search callback */
int mca_pcm_base_data_store_procs_search(ompi_object_t *obj, void *data);

static inline
int
mca_pcm_base_data_store_get_procs(mca_pcm_base_data_store_t *me,
				  pid_t pid, 
				  ompi_process_name_t ***procs, size_t *procs_len,
				  bool remove)
{
  ompi_pointer_array_t *array;
  ompi_process_name_t *name;
  int i = 0;

  array = mca_pcm_base_data_store_search(me, mca_pcm_base_data_store_procs_search,
					 &pid, remove);
  if (NULL == array) return errno;
  
  *procs_len = ompi_pointer_array_get_size(array);
  if (0 != *procs_len) {
      *procs = malloc(sizeof(ompi_process_name_t*) * *procs_len);
      if (NULL == *procs) return OMPI_ERR_OUT_OF_RESOURCE;

      for (i = 0 ; i < (int) *procs_len ; ++i) {
          name = (ompi_process_name_t*) ompi_pointer_array_get_item(array, i);
          (*procs)[i] = name;
      }
  } else {
      *procs = NULL;
  }

  OBJ_RELEASE(array);

  return OMPI_SUCCESS;
}

#endif
