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

#include <errno.h>

#include "base_kill_track.h"
#include "include/constants.h"
#include "mca/gpr/base/base.h"
#include "mca/gpr/gpr.h"
#include "class/ompi_list.h"
#include "class/ompi_pointer_array.h"
#include "runtime/runtime.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base_data_store.h"

#define KILL_KEY_STRING "kill-contact-info"

/* local data types */
struct pcm_obj_t {
  ompi_object_t super;
  mca_pcm_base_module_t *pcm;
};
typedef struct pcm_obj_t pcm_obj_t;
static OBJ_CLASS_INSTANCE(pcm_obj_t, ompi_object_t, NULL, NULL);

/* local data */
static ompi_mutex_t mutex;
static mca_pcm_base_data_store_t *data_store;
static volatile bool have_initialized_recv;

/*
 * Callback from communication layer when a kill message has arrived
 *
 * Expects payload to be in the form:
 *
 * ompi_process_name_t return_address
 * int32 mode_flag
 * ompi_process_name_t proc_name to kill
 * int32 signal
 * int32 flags
 *
 * Note that unlike the initiating side, we don't have to lock to
 * guarantee messages won't cross - we can deal with multiple messages
 * at once, as long as they are from different hosts.  And we already
 * know that messages are serialized per-host
 */
static void
mca_pcm_base_kill_cb(int status, ompi_process_name_t *peer,
                      ompi_buffer_t buffer, int tag, void *cbdata)
{
    int32_t mode_flag, signal, flags;
    ompi_process_name_t proc_name, return_address;
    int ret;
    int32_t send_ret = OMPI_SUCCESS;

    if (status < 0) goto cleanup;

    /* unpack */
    ompi_unpack(buffer, &return_address, 1, OMPI_NAME);
    ompi_unpack(buffer, &mode_flag, 1, OMPI_INT32);
    ompi_unpack(buffer, &proc_name, 1, OMPI_NAME);
    ompi_unpack(buffer, &signal, 1, OMPI_INT32);
    ompi_unpack(buffer, &flags, 1, OMPI_INT32);

    /* get pcm entry - function to call depends on mode.  We release
     * the object from the data store because it expects us to
     * OBJ_RELEASE any return (it OBJ_RETAINs if the remove flag is
     * not set
     */
    if (MCA_PCM_BASE_KILL_JOB == mode_flag || MCA_PCM_BASE_TERM_JOB == mode_flag) {
      /* we have a job - search and iterate */
      ompi_pointer_array_t *array;
      mca_ns_base_jobid_t jobid;
      int i;
      pcm_obj_t *pcm_obj;

      jobid = ompi_name_server.get_jobid(&proc_name);

      array = mca_pcm_base_data_store_get_job_data(data_store, jobid, false);
      if (NULL == array) goto cleanup;

      /* for each pcm, call the kill function with all the info */ 
      for (i = 0 ; i < ompi_pointer_array_get_size(array) ; ++i) {
	pcm_obj = (pcm_obj_t*) ompi_pointer_array_get_item(array, i);
	if (NULL == pcm_obj) continue;

	ret = pcm_obj->pcm->pcm_kill(pcm_obj->pcm, mode_flag, &proc_name, 
				     signal, flags);
	if (OMPI_SUCCESS != ret) send_ret = (int32_t) ret;

	OBJ_RELEASE(pcm_obj);
      }

      OBJ_RELEASE(array);

    } else {
      /* all we have is a single process - find it and call the pcm kill */
      pcm_obj_t *pcm_obj;
      pcm_obj = (pcm_obj_t*) mca_pcm_base_data_store_get_proc_data(data_store,
								   &proc_name, 
								   false);
      if (NULL == pcm_obj) goto cleanup;

      ret = pcm_obj->pcm->pcm_kill(pcm_obj->pcm, mode_flag, &proc_name, 
				   signal, flags);
      send_ret = (int32_t) ret;

      OBJ_RELEASE(pcm_obj);
    }

 cleanup:
    /* send back status response */
    ompi_buffer_init(&buffer, 0);
    ompi_pack(buffer, &send_ret, 1, OMPI_INT32);
    mca_oob_send_packed(&return_address, buffer, MCA_OOB_TAG_PCM_KILL_ACK, 0);
    ompi_buffer_free(buffer);

    mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_PCM_KILL, 0,
			   mca_pcm_base_kill_cb, NULL);
}


/*
 * start non-blocking receive if there isn't already one posted.
 * separate function to get the locking just right
 */ 
static int
start_recv(void)
{
    int rc = 0;

    if (! have_initialized_recv) {
        OMPI_LOCK(&mutex);
        if (!have_initialized_recv) {
            rc = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, 
                                        MCA_OOB_TAG_PCM_KILL, 0,
                                        mca_pcm_base_kill_cb, NULL);
            have_initialized_recv = true;
        }
        OMPI_UNLOCK(&mutex);
        if (rc != OMPI_SUCCESS) return rc;
    }

    return OMPI_SUCCESS;
}


/*
 * cancel any posted non-blocking rceive for the kill code
 *
 * will cancel receive if either force is true or there are no
 * jobs waiting for kill messages
 */
static int
stop_recv(bool force)
{
    int rc = OMPI_SUCCESS;

    if (!have_initialized_recv) return OMPI_SUCCESS;

    OMPI_LOCK(&mutex);
    if (have_initialized_recv) {
      if (force || mca_pcm_base_data_store_is_empty(data_store)) {
	rc =  mca_oob_recv_cancel(MCA_OOB_NAME_ANY, MCA_OOB_TAG_PCM_KILL);
	have_initialized_recv = false;
      }
    }
    OMPI_UNLOCK(&mutex);

    return rc;
}


/*
 * send a kill message to everyone associated with the given jobid.
 * Filtering will happen on the receive side.
 *
 * Message is packed as described in comments for receive callback.
 */
int
mca_pcm_base_kill(int how, ompi_process_name_t *name, int signal, int flags)
{
    char *keys[2] = { NULL, NULL };
    char *segment;
    ompi_list_t *reg_entries;
    ompi_list_item_t *item;
    int ret = OMPI_SUCCESS;
    mca_ns_base_jobid_t jobid;
    char *jobid_str;
    int32_t recv_ret;
    int recv_tag;

    /*
     * Get the contact data
     */
    keys[0] = KILL_KEY_STRING;
    keys[1] = NULL;

    jobid = ompi_name_server.get_jobid(name);
    jobid_str = ompi_name_server.convert_jobid_to_string(jobid);
    asprintf(&segment, "%s-%s", "kill-segment", jobid_str);
    free(jobid_str);

    reg_entries = ompi_registry.get(OMPI_REGISTRY_OR, segment, keys);
    if (0 == ompi_list_get_size(reg_entries)) {
        ret = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }

    /*
     * send a message to everyone
     */
    for (item = ompi_list_get_first(reg_entries) ;
         item != ompi_list_get_end(reg_entries) ;
         item = ompi_list_get_next(item)) {
        ompi_registry_value_t *value = (ompi_registry_value_t*) item;
        ompi_buffer_t buf;
        ompi_process_name_t proc_name;
        int32_t tmp;

	/* setup buffer from data given by registry */
        ret = ompi_buffer_init_preallocated(&buf, value->object, value->object_size);
        if (ret != OMPI_SUCCESS) {
	  goto cleanup;
        }

        /* pull out the vpid range and ignore - we don't care */
        ret = ompi_unpack(buf, &proc_name, 1, OMPI_NAME);
        if (ret != OMPI_SUCCESS) {
	  goto cleanup;
        }

        /* free the buffer and start over for packing */
        ompi_buffer_free(buf);
        ompi_buffer_init(&buf, 0);

        /* pack in the kill message */
	ompi_pack(buf, ompi_rte_get_self(), 1, OMPI_NAME);
	tmp = (int32_t) how;
	ompi_pack(buf, &tmp, 1, OMPI_INT32);
	ompi_pack(buf, name, 1, OMPI_NAME);
	tmp = (int32_t) signal;
	ompi_pack(buf, &tmp, 1, OMPI_INT32);
	tmp = (int32_t) flags;
	ompi_pack(buf, &tmp, 1, OMPI_INT32);

	/* Have to lock this small section so messages don't get crossed :( */
	OMPI_LOCK(&mutex);

        /* send the kill message */
        ret = mca_oob_send_packed(&proc_name, buf, MCA_OOB_TAG_PCM_KILL, 0);
        ompi_buffer_free(buf);

	if (ret < 0) {
	  OMPI_UNLOCK(&mutex);
	  goto cleanup;
	}

	/* wait for the response */
	recv_tag = MCA_OOB_TAG_PCM_KILL_ACK;
	ret = mca_oob_recv_packed(&proc_name, &buf, &recv_tag);
	if (ret < 0) {
	  OMPI_UNLOCK(&mutex);
	  ompi_buffer_free(buf);
	  goto cleanup;
	}
	OMPI_UNLOCK(&mutex);
	/* End section that must be locked - we have the receive from the other host */

	/* unpack response */
	ompi_unpack(buf, &recv_ret, 1, OMPI_INT32);
	ompi_buffer_free(buf);
	if (OMPI_SUCCESS != recv_ret) {
	  ret = (int) recv_ret;
	  goto cleanup;
	}
        ret = OMPI_SUCCESS;
    }

 cleanup:
    if (NULL != segment) free(segment);
    return ret;
}


int
mca_pcm_base_kill_register(mca_pcm_base_module_t* pcm,
			   ompi_process_name_t *name)
{
    char *keys[3] = { NULL, NULL, NULL };
    char *segment = NULL;
    ompi_buffer_t buf;
    int ret = OMPI_SUCCESS;
    void *bptr;
    int bufsize;
    mca_ns_base_jobid_t jobid;
    char *jobid_str;
    pcm_obj_t *pcm_obj;

    ret = start_recv();
    if (ret != OMPI_SUCCESS) goto cleanup;

    /* pack the buffer */
    ompi_buffer_init(&buf, sizeof(ompi_process_name_t));
    ompi_pack(buf, ompi_rte_get_self(), 1, OMPI_NAME);

    /* fill out segment string */
    jobid = ompi_name_server.get_jobid(name);
    jobid_str = ompi_name_server.convert_jobid_to_string(jobid);
    asprintf(&segment, "%s-%s", "kill-segment", jobid_str);
    free(jobid_str);

    /* fill out the keys */
    keys[0] = KILL_KEY_STRING;
    keys[1] = 
      ompi_name_server.convert_vpid_to_string(ompi_name_server.get_vpid(name));
    keys[2] = NULL;

    /* register with the data store */
    pcm_obj = OBJ_NEW(pcm_obj_t);
    pcm_obj->pcm = pcm;
    mca_pcm_base_data_store_add_data(data_store, name, (ompi_object_t*) pcm_obj);

    /* update the registery */
    ompi_buffer_get(buf, &bptr, &bufsize);
    ret = ompi_registry.put(OMPI_REGISTRY_OVERWRITE, segment, 
                           keys, (ompi_registry_object_t) bptr, bufsize);

    ompi_buffer_free(buf);

 cleanup:
    if (NULL != keys[1]) free(keys[1]);
    if (NULL != segment) free(segment);
    return ret;
}


int
mca_pcm_base_kill_unregister(ompi_process_name_t *name)
{
    ompi_object_t *obj;

    obj = mca_pcm_base_data_store_get_proc_data(data_store, name, true);
    if (NULL == obj) return OMPI_ERR_NOT_FOUND;
    OBJ_RELEASE(obj);

    stop_recv(false);

    return OMPI_SUCCESS;
}


int
mca_pcm_base_kill_init(void)
{
    OBJ_CONSTRUCT(&mutex, ompi_mutex_t);
    data_store = mca_pcm_base_data_store_init();
    have_initialized_recv = false;

    return OMPI_SUCCESS;
}


int
mca_pcm_base_kill_fini(void)
{
    stop_recv(true);

    mca_pcm_base_data_store_finalize(data_store);
    OBJ_DESTRUCT(&mutex);

    return OMPI_SUCCESS;
}
