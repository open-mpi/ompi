/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/* Copyright (c) 2014      Mellanox Technologies, Inc.
 *                         All rights reserved.
 * Copyright (c) 2014      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 *
 */

#include "opal_config.h"
#include "opal/constants.h"

#include <time.h>
#include <string.h>

#include "opal_stdint.h"
#include "opal/dss/dss_types.h"
#include "opal/util/error.h"
#include "opal/util/output.h"
#include "opal/util/show_help.h"

#include "opal/mca/dstore/base/base.h"
#include "dstore_sm.h"
#include "opal/mca/pmix/pmix.h"
#include "opal/mca/shmem/base/base.h"

static uint32_t cur_offset = 0;
static int32_t cur_seg_index = -1;

static int init(struct opal_dstore_base_module_t *imod);
static void finalize(struct opal_dstore_base_module_t *imod);
static int store(struct opal_dstore_base_module_t *imod,
                 const opal_process_name_t *proc,
                 opal_value_t *val);
static int fetch(struct opal_dstore_base_module_t *imod,
                 const opal_process_name_t *proc,
                 const char *key,
                 opal_list_t *kvs);
static int remove_data(struct opal_dstore_base_module_t *imod,
                       const opal_process_name_t *proc, const char *key);

static void smtrkcon(opal_sm_tracker_t *p)
{
    p->jobid = 0;
    p->addr = NULL;
}
static void smtrkdes(opal_sm_tracker_t *p)
{
}

OBJ_CLASS_INSTANCE(opal_sm_tracker_t,
        opal_list_item_t,
        smtrkcon, smtrkdes);

#define SHARED_SEGMENT_SIZE (1<<22)

mca_dstore_sm_module_t opal_dstore_sm_module = {
    {
        init,
        finalize,
        store,
        fetch,
        remove_data
    }
};

segment_info *segments = NULL;
static int max_segment_num;

/* Initialize our sm region */
static int init(struct opal_dstore_base_module_t *imod)
{
    int i;
    mca_dstore_sm_module_t *mod;
    mod = (mca_dstore_sm_module_t*)imod;

    max_segment_num = META_OFFSET/sizeof(seg_info_short);
    segments = malloc(max_segment_num * sizeof(segment_info));
    for (i = 0; i < max_segment_num; i++) {
        segments[i].addr = NULL;
        segments[i].seg_ds = NULL;
    }
    OBJ_CONSTRUCT(&mod->tracklist, opal_list_t);
    return OPAL_SUCCESS;
}

static void finalize(struct opal_dstore_base_module_t *imod)
{
    mca_dstore_sm_module_t *mod;
    opal_sm_tracker_t *trk;
    opal_list_item_t *item;

    mod = (mca_dstore_sm_module_t*)imod;

    int i;
    for (i = 0; i < max_segment_num; i++) {
        if (NULL != segments[i].seg_ds) {
            if (segments[i].seg_ds->seg_cpid == getpid()) {
                opal_shmem_unlink (segments[i].seg_ds);
            }
            opal_shmem_segment_detach (segments[i].seg_ds);
            free(segments[i].seg_ds);
        }
    }
    free(segments);

    /* release tracker object */
    for (item = opal_list_remove_first(&mod->tracklist);
            NULL != item;
            item = opal_list_remove_first(&mod->tracklist)) {
        trk = (opal_sm_tracker_t*) item;
        opal_shmem_segment_detach (&trk->seg_ds);
        if (trk->seg_ds.seg_cpid == getpid()) {
            opal_shmem_unlink (&trk->seg_ds);
        }
        OBJ_RELEASE(trk);
    }
    OPAL_LIST_DESTRUCT(&mod->tracklist);
}



static int store(struct opal_dstore_base_module_t *imod,
                 const opal_process_name_t *uid,
                 opal_value_t *val)
{
    mca_dstore_sm_module_t *mod;
    void *addr;
    int32_t data_size;
    opal_shmem_ds_t *seg_ds;
    meta_info my_info;
    seg_info_short sinfo;
    char* seg_addr;
    char *sm_file = NULL;
    char *ch, *path;
    int idx;
    opal_sm_tracker_t *trk;
    bool found_trk = false;
    if (OPAL_BYTE_OBJECT != val->type) {
        return OPAL_ERROR;
    }
    mod = (mca_dstore_sm_module_t*)imod;
    data_size = val->data.bo.size;

    idx = uid->vpid;
    /* look for segment info for target jobid */
    OPAL_LIST_FOREACH(trk, &mod->tracklist, opal_sm_tracker_t) {
        if (trk->jobid == uid->jobid) {
            found_trk = true;
            break;
        }
    }
    if (!found_trk) {
        opal_output_verbose(0, opal_dstore_base_framework.framework_output,
                "%s dstore:sm:store: tracker object wasn't found for job id %u, proc %s",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                uid->jobid,
                OPAL_NAME_PRINT(*uid));
        return OPAL_ERROR;
    }
    /* look for data for this process in meta_info segment */
    addr = ((uint8_t*)trk->addr + META_OFFSET + idx * sizeof(meta_info));
    memcpy(&my_info, addr, sizeof(meta_info));
    if (0 < my_info.data_size && 0 <= my_info.seg_index) {
        /* we should replace existing data for this process
         * by new ones */
        if (my_info.data_size >= data_size) {
            opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                    "%s dstore:sm:store: replace existing data for proc %s be the new ones",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    OPAL_NAME_PRINT(*uid));
            /* we can just simply replace the old data with the new ones */
            /* get existing segment from the list */
            seg_addr = segments[my_info.seg_index].addr;
            seg_ds = segments[my_info.seg_index].seg_ds;
            /* store data in this segment */
            addr = seg_addr + my_info.offset;
            memset((uint8_t*)addr, 0, my_info.data_size);
            memcpy((uint8_t*)addr, val->data.bo.bytes, val->data.bo.size);
            /* update information about data size in meta info segment */
            my_info.data_size = data_size;
            memcpy((uint8_t*)trk->addr + META_OFFSET + idx*sizeof(meta_info), &my_info, sizeof(meta_info));
            return OPAL_SUCCESS;
        }
    }
    /* there is no data for this process, or there is data for new process
     * but their size is smaller than the size of new data, so
     * store them in the separate slot*/

    /* store in another segment */
    if (0 > cur_seg_index || (cur_offset + data_size) > SHARED_SEGMENT_SIZE) {
        if (max_segment_num == cur_seg_index+1) {
            opal_output_verbose(0, opal_dstore_base_framework.framework_output,
                    "%s dstore:sm:store: exceeded limit on number of segments %d. This value is managed by META_OFFSET macro.",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    max_segment_num);
            return OPAL_ERROR;
        }
        opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                "%s dstore:sm:store: create new segment to store data for proc %s",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                OPAL_NAME_PRINT(*uid));
        /* create new segment, attach to it and add it to the list of segments */
        cur_seg_index++;
        cur_offset = 0;
        if (0 < strlen(trk->seg_ds.seg_name)) {
            path = strdup(trk->seg_ds.seg_name);
            ch = strrchr(path, OPAL_PATH_SEP[0]) + 1;
            if (NULL != ch) {
                *ch = '\0';
                (void)asprintf(&sm_file, "%sdstore_segment.%d", path, cur_seg_index);
            }
            free(path);
        }
        if (NULL == sm_file) {
            (void)asprintf(&sm_file, "%s", "noname");
        }
        if (NULL != sm_file) {
            seg_ds = (opal_shmem_ds_t*)malloc(sizeof(opal_shmem_ds_t));
            memset(seg_ds, 0, sizeof(opal_shmem_ds_t));
            if (OPAL_SUCCESS != opal_shmem_segment_create (seg_ds, sm_file, SHARED_SEGMENT_SIZE)) {
                opal_output_verbose(0, opal_dstore_base_framework.framework_output,
                                 "%s dstore:sm:store: couldn't create new shared segment to store key %s on proc %s",
                                 OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                                 (NULL == val->key) ? "NULL" : val->key,
                                 OPAL_NAME_PRINT(*uid));
                free(seg_ds);
                if (NULL != sm_file) {
                    free (sm_file);
                }
                return OPAL_ERROR;
            }
            if (NULL != sm_file) {
                free (sm_file);
            }
        } else {
            return OPAL_ERROR;
        }
        seg_addr = opal_shmem_segment_attach (seg_ds);
        if (NULL == seg_addr) {
            opal_shmem_unlink (seg_ds);
            free(seg_ds);
            return OPAL_ERROR;
        }
        segments[cur_seg_index].seg_ds = seg_ds;
        segments[cur_seg_index].addr = seg_addr;
        /* store information about new created segment in header section. */
        sinfo.seg_cpid = seg_ds->seg_cpid;
        sinfo.seg_id = seg_ds->seg_id;
        sinfo.seg_size = seg_ds->seg_size;
        if (0 < strlen(seg_ds->seg_name)) {
            ch = strrchr(seg_ds->seg_name, OPAL_PATH_SEP[0]) + 1;
            memcpy(sinfo.file_name, ch, strlen(ch)+1);
        } else {
            memcpy(sinfo.file_name, "noname", strlen("noname")+1);
        }
        memcpy((uint8_t*)trk->addr + cur_seg_index * sizeof(seg_info_short), &sinfo, sizeof(seg_info_short));
    } else {
        /* get existing segment from the array */
        opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                "%s dstore:sm:store: getting current segment info to store data for proc %s",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                OPAL_NAME_PRINT(*uid));
        seg_addr = segments[cur_seg_index].addr;
        seg_ds = segments[cur_seg_index].seg_ds;
        memcpy(&sinfo, (uint8_t*)trk->addr + cur_seg_index * sizeof(seg_info_short), sizeof(seg_info_short));
        if (sinfo.seg_cpid != seg_ds->seg_cpid) {
            /* store information about new created segment in header section. */
            sinfo.seg_cpid = seg_ds->seg_cpid;
            sinfo.seg_id = seg_ds->seg_id;
            sinfo.seg_size = seg_ds->seg_size;
            if (0 < strlen(seg_ds->seg_name)) {
                ch = strrchr(seg_ds->seg_name, OPAL_PATH_SEP[0]) + 1;
                memcpy(sinfo.file_name, ch, strlen(ch)+1);
            } else {
                memcpy(sinfo.file_name, "noname", strlen("noname")+1);
            }
            memcpy((uint8_t*)trk->addr + cur_seg_index * sizeof(seg_info_short), &sinfo, sizeof(seg_info_short));
        }
    }
    /* store data in this segment */
    addr = seg_addr + cur_offset;
    memcpy((uint8_t*)addr, val->data.bo.bytes, val->data.bo.size);

    /* store segment index and offset for this process
     * in meta info segment. */
    my_info.seg_index = cur_seg_index;
    my_info.offset = cur_offset;
    my_info.data_size = data_size;
    memcpy((uint8_t*)trk->addr + META_OFFSET + idx*sizeof(meta_info), &my_info, sizeof(meta_info));
    cur_offset += data_size;

    opal_output_verbose(5, opal_dstore_base_framework.framework_output,
            "%s dstore:sm:store: data for proc %s stored successfully",
            OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
            OPAL_NAME_PRINT(*uid));
    return OPAL_SUCCESS;
}

static int fetch(struct opal_dstore_base_module_t *imod,
                 const opal_process_name_t *uid,
                 const char *key, opal_list_t *kvs)
{
    int rc;
    int32_t size;
    mca_dstore_sm_module_t *mod;
    void *addr, *ptr;
    opal_buffer_t *bptr, buf;
    int32_t cnt;
    opal_value_t *kp;
    opal_shmem_ds_t *seg_ds;
    meta_info my_info;
    seg_info_short sinfo;
    char* seg_addr;
    int found = 0;
    int32_t seg_index;
    char *ch, *path;
    opal_sm_tracker_t *trk;
    bool found_trk = false;
    int idx;

    mod = (mca_dstore_sm_module_t*)imod;
    /* look for segment info for target jobid */
    OPAL_LIST_FOREACH(trk, &mod->tracklist, opal_sm_tracker_t) {
        if (trk->jobid == uid->jobid) {
            found_trk = true;
            break;
        }
    }
    if (!found_trk) {
        opal_output_verbose(0, opal_dstore_base_framework.framework_output,
                "%s dstore:sm:fetch: tracker object wasn't found for job id %u, proc %s",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                uid->jobid,
                OPAL_NAME_PRINT(*uid));
        return OPAL_ERROR;
    }
    /* look for data for this process in meta_info segment */
    idx = uid->vpid;
    addr = ((uint8_t*)trk->addr + META_OFFSET + idx * sizeof(meta_info));
    memcpy(&my_info, addr, sizeof(meta_info));
    if (0 == my_info.data_size) {
        /* there is no data for this process */
        opal_output_verbose(0, opal_dstore_base_framework.framework_output,
                "%s dstore:sm:fetch: data for proc %s wasn't found.",
                OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                OPAL_NAME_PRINT(*uid));
        return OPAL_ERROR;
    }
    seg_index = my_info.seg_index;
    /* look for this seg index in array of attached segments.
     * If not found, attach to this segment and
     * store it in the array. */
    if (NULL != segments[seg_index].addr) {
        seg_addr = segments[seg_index].addr;
    } else {
        seg_ds = (opal_shmem_ds_t*)malloc(sizeof(opal_shmem_ds_t));
        memset(seg_ds, 0, sizeof(opal_shmem_ds_t));
        memcpy(&sinfo, (uint8_t*)trk->addr + seg_index * sizeof(seg_info_short), sizeof(seg_info_short));
        seg_ds->seg_cpid = sinfo.seg_cpid;
        seg_ds->seg_id = sinfo.seg_id;
        seg_ds->seg_size = sinfo.seg_size;
        if (0 < strlen(trk->seg_ds.seg_name)) {
            path = strdup(trk->seg_ds.seg_name);
            ch = strrchr(path, OPAL_PATH_SEP[0]) + 1;
            if (NULL != ch) {
                *ch = '\0';
                sprintf(seg_ds->seg_name, "%s%s", path, sinfo.file_name);
            }
            free(path);
        }
        seg_addr = opal_shmem_segment_attach (seg_ds);
        if (NULL == seg_addr) {
            return OPAL_ERROR;
        }
        segments[seg_index].addr = seg_addr;
        segments[seg_index].seg_ds = seg_ds;
    }

    size = my_info.data_size;
    ptr = (uint8_t*)seg_addr + my_info.offset;

    cnt = 1;
    OBJ_CONSTRUCT(&buf, opal_buffer_t);
    opal_dss.load(&buf, ptr, size);
    while (OPAL_SUCCESS == (rc = opal_dss.unpack(&buf, &bptr, &cnt, OPAL_BUFFER))) {
        while (OPAL_SUCCESS == (rc = opal_dss.unpack(bptr, &kp, &cnt, OPAL_VALUE))) {
            if (0 == strcmp(key, kp->key)) {
                opal_list_append(kvs, &kp->super);
                found = 1;
            } else {
                OBJ_RELEASE(kp);
            }
        }
        if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
            OPAL_ERROR_LOG(rc);
        }
        OBJ_RELEASE(bptr);
        cnt = 1;
    }
    if (OPAL_ERR_UNPACK_READ_PAST_END_OF_BUFFER != rc) {
        OPAL_ERROR_LOG(rc);
    } else {
        if (1 == found) {
            opal_output_verbose(5, opal_dstore_base_framework.framework_output,
                    "%s dstore:sm:fetch: data for proc %s successfully fetched.",
                    OPAL_NAME_PRINT(OPAL_PROC_MY_NAME),
                    OPAL_NAME_PRINT(*uid));
            rc = OPAL_SUCCESS;
        }
    }
    /* protect the data */
    buf.base_ptr = NULL;
    OBJ_DESTRUCT(&buf);
    return rc;
}

static int remove_data(struct opal_dstore_base_module_t *imod,
        const opal_process_name_t *uid, const char *key)
{
    return OPAL_ERR_NOT_IMPLEMENTED;
}

