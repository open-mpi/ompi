/* -*- C -*-
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
#include "runtime/runtime.h"

#define KILLJOB_SEGMENT_STRING "pcm-kill-job"

#define OMPI_KILL_PROC 0x0001
#define OMPI_KILL_JOB  0x0002


/*
 * setup internal data structures
 */
static void mca_pcm_base_kill_item_construct(ompi_object_t *obj);
static void mca_pcm_base_kill_item_destruct(ompi_object_t *obj);

struct mca_pcm_base_kill_data_t {
    ompi_list_item_t super;
    mca_ns_base_vpid_t lower;
    mca_ns_base_vpid_t upper;
    mca_pcm_base_module_t* pcm;
};
typedef struct mca_pcm_base_kill_data_t mca_pcm_base_kill_data_t;
static OBJ_CLASS_INSTANCE(mca_pcm_base_kill_data_t,
                          ompi_list_item_t,
                          NULL, NULL);

struct mca_pcm_base_kill_item_t {
    ompi_list_item_t super;
    mca_ns_base_jobid_t jobid;
    ompi_list_t *entries;
};
typedef struct mca_pcm_base_kill_item_t mca_pcm_base_kill_item_t;
static OBJ_CLASS_INSTANCE(mca_pcm_base_kill_item_t,
                          ompi_list_item_t,
                          mca_pcm_base_kill_item_construct,
                          mca_pcm_base_kill_item_destruct);


/*
 * Global data
 */
static ompi_list_t job_list;
static ompi_mutex_t mutex;

static mca_pcm_base_kill_item_t*
get_kill_item(mca_ns_base_jobid_t jobid)
{
    ompi_list_item_t *kill_item;
    mca_pcm_base_kill_item_t *kill_entry = NULL;

    for (kill_item = ompi_list_get_first(&job_list) ;
         kill_item != ompi_list_get_end(&job_list) ;
         kill_item = ompi_list_get_next(kill_item)) {
        kill_entry = (mca_pcm_base_kill_item_t*) kill_item;
        if (kill_entry->jobid == jobid) {
            return kill_entry;
        }
    }

    return NULL;
}


static mca_pcm_base_kill_data_t*
get_data_item(mca_pcm_base_kill_item_t *kill_entry, 
              mca_ns_base_vpid_t vpid)
{
    ompi_list_item_t *item;
    for (item = ompi_list_get_first(kill_entry->entries) ;
         item != ompi_list_get_end(kill_entry->entries) ;
         item = ompi_list_get_next(item) ) {
        mca_pcm_base_kill_data_t *data = (mca_pcm_base_kill_data_t*) item;
        if (data->lower <= vpid && data->upper >= vpid) {
            return data;
        }
    }

    return NULL;
}


/*
 * local list manip functions
 */
static int
add_job_info(mca_ns_base_jobid_t jobid, 
             mca_ns_base_vpid_t lower,
             mca_ns_base_vpid_t upper,
             mca_pcm_base_module_t *pcm)
{
    mca_pcm_base_kill_item_t *kill_entry;
    mca_pcm_base_kill_data_t *data_entry;
    int ret = OMPI_SUCCESS;

    kill_entry = get_kill_item(jobid);
    if (NULL == kill_entry) {
        kill_entry = OBJ_NEW(mca_pcm_base_kill_item_t);
        if (NULL == kill_entry) {
            ret = OMPI_ERR_OUT_OF_RESOURCE;
            goto cleanup;
        }
        kill_entry->jobid = jobid;
        ompi_list_append(&job_list, (ompi_list_item_t*) kill_entry);
    }

    /* add our info */
    data_entry = OBJ_NEW(mca_pcm_base_kill_data_t);
    if (NULL == data_entry) {
        ret = OMPI_ERR_OUT_OF_RESOURCE;
        goto cleanup;
    }
    data_entry->lower = lower;
    data_entry->upper = upper;
    data_entry->pcm = pcm;
    ompi_list_append(kill_entry->entries, (ompi_list_item_t*) data_entry);
    
 cleanup:
    return ret;
}


static int
remove_job_info(mca_ns_base_jobid_t jobid,
                mca_ns_base_vpid_t lower,
                mca_ns_base_vpid_t upper)
{
    int ret = OMPI_SUCCESS;
    mca_pcm_base_kill_item_t *kill_entry;
    mca_pcm_base_kill_data_t *data_entry;

    kill_entry = get_kill_item(jobid);
    if (NULL == kill_entry) {
        ret = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }

    data_entry = get_data_item(kill_entry, lower);
    if (NULL == data_entry) {
        ret = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }

    ompi_list_remove_item(kill_entry->entries, (ompi_list_item_t*) data_entry);

 cleanup:
    return ret;
}


static mca_pcm_base_module_t*
get_job_pcm(mca_ns_base_jobid_t jobid, mca_ns_base_vpid_t vpid)
{
    mca_pcm_base_kill_item_t *kill_entry;
    mca_pcm_base_kill_data_t *data_entry = NULL;
    mca_pcm_base_module_t *ret = NULL;;

    kill_entry = get_kill_item(jobid);
    if (NULL == kill_entry) {
        errno = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }

    data_entry = get_data_item(kill_entry, vpid);
    if (NULL == data_entry) {
        errno = OMPI_ERR_NOT_FOUND;
        goto cleanup;
    }

    ret = data_entry->pcm;

 cleanup:
    return ret;
 }


static void
mca_pcm_base_kill_cb(int status, ompi_process_name_t *peer,
                      ompi_buffer_t buffer, int tag, void *cbdata)
{
    mca_pcm_base_module_t *pcm;
    int32_t mode_flag;
    ompi_process_name_t dead_name;
    int ret;

    printf("mca_pcm_base_kill_cb(%d, ...)\n", status);

    if (status < 0) goto cleanup;

    /* unpack */
    ompi_unpack(buffer, &mode_flag, 1, OMPI_INT32);
    ompi_unpack(buffer, &dead_name, 1, OMPI_NAME);

    /* get pcm entry */
    pcm = get_job_pcm(dead_name.jobid, dead_name.vpid);
    if (NULL == pcm) goto cleanup;

    /* fire and forget, baby! */
    if (mode_flag == OMPI_KILL_JOB) {
        pcm->pcm_kill_job(pcm, dead_name.jobid, 0);
    } else if (mode_flag == OMPI_KILL_PROC) {
        pcm->pcm_kill_proc(pcm, &dead_name, 0);
    } else {
        goto cleanup;
    }
    
 cleanup:
    ret = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_PCM_KILL, 0,
                                 mca_pcm_base_kill_cb, NULL);
}


int
mca_pcm_base_kill_send_job_msg(mca_ns_base_jobid_t jobid,
                               int sig, int errorcode, int flags)
{
    char *keys[2] = { NULL, NULL };
    char segment[256];
    ompi_list_t *reg_entries;
    ompi_list_item_t *item;
    int ret = OMPI_SUCCESS;

    /*
     * Get the contact data
     */
    keys[0] = ns_base_convert_jobid_to_string(jobid);
    keys[1] = NULL;

    snprintf(segment, 256, KILLJOB_SEGMENT_STRING);

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
        ompi_process_name_t proc_name, dead_name;
        int32_t mode_flag;

        ret = ompi_buffer_init_preallocated(&buf, value->object, value->object_size);
        if (ret != OMPI_SUCCESS) {
            printf("ompi_buffer_init_prealloc returned %d\n", ret);
        }

        /* pull out the vpid range and ignore - we don't care */
        ret = ompi_unpack(buf, &proc_name, 1, OMPI_NAME);
        if (ret != OMPI_SUCCESS) {
            printf("ompi_unpack returned %d\n", ret);
        }
        printf("lower: %s\n", ns_base_get_proc_name_string(&proc_name));
        ompi_unpack(buf, &proc_name, 1, OMPI_NAME);
        printf("upper: %s\n", ns_base_get_proc_name_string(&proc_name));
        /* get the contact name */
        ompi_unpack(buf, &proc_name, 1, OMPI_NAME);
        printf("contact: %s\n", ns_base_get_proc_name_string(&proc_name));


        /* free the buffer and start over for packing */
        ompi_buffer_free(buf);
        ompi_buffer_init(&buf, 0);

        /* pack in the kill message */
        mode_flag = OMPI_KILL_JOB;
        ompi_pack(buf, &mode_flag, 1, OMPI_INT32);
        dead_name.cellid = 0;
        dead_name.jobid = jobid;
        dead_name.vpid = 0;
        ompi_pack(buf, &dead_name, 1, OMPI_NAME);

        /* send the kill message */
        mca_oob_send_packed(&proc_name, buf, MCA_OOB_TAG_PCM_KILL, 0);

        ompi_buffer_free(buf);
    }

 cleanup:
    if (NULL != keys[0]) free(keys[0]);
    return ret;
}


int
mca_pcm_base_kill_send_proc_msg(ompi_process_name_t name,
                                int sig, int errorcode, int flags)
{
    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_pcm_base_kill_register(mca_pcm_base_module_t* pcm,
                           mca_ns_base_jobid_t jobid,
                           mca_ns_base_vpid_t lower_vpid,
                           mca_ns_base_vpid_t upper_vpid)
{
    char *keys[3] = { NULL, NULL, NULL };
    char segment[256];
    ompi_buffer_t buf;
    ompi_process_name_t high, low;
    int ret = OMPI_SUCCESS;
    void *bptr;
    int bufsize;
    int rc;

    /* setup data for the buffer */
    high.cellid = low.cellid = 0;
    high.jobid = low.jobid = jobid;
    high.vpid = upper_vpid;
    low.vpid = lower_vpid;

    /* pack the buffer */
    ompi_buffer_init(&buf, sizeof(ompi_process_name_t) * 3);
    ompi_pack(buf, &low, 1, OMPI_NAME);
    ompi_pack(buf, &high, 1, OMPI_NAME);
    ompi_pack(buf, ompi_rte_get_self(), 1, OMPI_NAME);

    /* fill out the keys */
    keys[0] = ns_base_get_jobid_string(&low);
    keys[1] = ns_base_get_vpid_string(&low);
    keys[2] = NULL;

    snprintf(segment, 256, KILLJOB_SEGMENT_STRING);

    ompi_buffer_get(buf, &bptr, &bufsize);
    add_job_info(jobid, lower_vpid, upper_vpid, pcm);
    rc = ompi_registry.put(OMPI_REGISTRY_OVERWRITE, segment, 
                           keys, (ompi_registry_object_t) bptr, bufsize);

    ompi_buffer_free(buf);
    if (NULL != keys[0]) free(keys[0]);
    if (NULL != keys[1]) free(keys[1]);
    return ret;
}


int
mca_pcm_base_kill_unregister(mca_pcm_base_module_t* pcm,
                             mca_ns_base_jobid_t jobid,
                             mca_ns_base_vpid_t lower_vpid,
                             mca_ns_base_vpid_t upper_vpid)
{
    remove_job_info(jobid, lower_vpid, upper_vpid);

    return OMPI_ERR_NOT_IMPLEMENTED;
}


int
mca_pcm_base_kill_init(void)
{
    int ret;

    OBJ_CONSTRUCT(&job_list, ompi_list_t);
    OBJ_CONSTRUCT(&mutex, ompi_mutex_t);

    ret = mca_oob_recv_packed_nb(MCA_OOB_NAME_ANY, MCA_OOB_TAG_PCM_KILL, 0,
                                 mca_pcm_base_kill_cb, NULL);

    return OMPI_SUCCESS;
}


int
mca_pcm_base_kill_fini(void)
{
    OBJ_DESTRUCT(&mutex);
    OBJ_DESTRUCT(&job_list);

    mca_oob_recv_cancel(MCA_OOB_NAME_ANY, MCA_OOB_TAG_PCM_KILL);

    return OMPI_SUCCESS;
}


static
void
mca_pcm_base_kill_item_construct(ompi_object_t *obj)
{
    mca_pcm_base_kill_item_t *data = (mca_pcm_base_kill_item_t*) obj;
    data->entries = OBJ_NEW(ompi_list_t);
}


static
void
mca_pcm_base_kill_item_destruct(ompi_object_t *obj)
{
    mca_pcm_base_kill_item_t *data = (mca_pcm_base_kill_item_t*) obj;
    if (NULL != data->entries) {
        ompi_list_item_t *item;
        while (NULL != (item = ompi_list_remove_first(data->entries))) {
            OBJ_RELEASE(item);
        }
        OBJ_RELEASE(data->entries);
    }
}
