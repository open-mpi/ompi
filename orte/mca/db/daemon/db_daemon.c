/*
 * Copyright (c) 2010      Cisco Systems, Inc.  All rights reserved. 
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 *
 */

#include "orte_config.h"
#include "orte/constants.h"

#include <time.h>

#include "opal/class/opal_pointer_array.h"
#include "opal/dss/dss_types.h"
#include "opal/util/output.h"

#include "orte/util/show_help.h"
#include "orte/util/name_fns.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmcast/rmcast.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/orte_wait.h"

#include "orte/mca/db/base/base.h"
#include "db_daemon.h"

static int init(void);
static int finalize(void);
static int store(char *key, void *object, opal_data_type_t type);
static int set_source(orte_process_name_t *name);
static int fetch(char *key, void *object, opal_data_type_t type);
static int update(char *key, void *object, opal_data_type_t type);
static int remove_data(char *key);

orte_db_base_module_t orte_db_daemon_module = {
    init,
    finalize,
    store,
    set_source,
    fetch,
    update,
    remove_data
};

/* local types */
typedef struct {
    opal_object_t super;
    orte_process_name_t name;
    char *key;
    int32_t size;
    uint8_t *bytes;
} orte_db_data_t;
static void dtconstructor(orte_db_data_t *dt)
{
    dt->key = NULL;
    dt->bytes = NULL;
    dt->size = 0;
}
static void dtdestructor(orte_db_data_t *dt)
{
    if (NULL != dt->key) {
        free(dt->key);
    }
    if (NULL != dt->bytes) {
        free(dt->bytes);
    }
}
OBJ_CLASS_INSTANCE(orte_db_data_t,
                   opal_object_t,
                   dtconstructor,
                   dtdestructor);

/* local variables */
static orte_vpid_t num_recvd;
static bool ack_reqd;
static opal_pointer_array_t datastore;
static orte_rmcast_channel_t my_group_channel;

/* local functions */
static void callback_fn(int status,
                        orte_rmcast_channel_t channel,
                        orte_rmcast_seq_t seq_num,
                        orte_rmcast_tag_t tag,
                        orte_process_name_t *sender,
                        opal_buffer_t *buf, void* cbdata);

static void recv_cmd(int status,
                     orte_rmcast_channel_t channel,
                     orte_rmcast_seq_t seq_num,
                     orte_rmcast_tag_t tag,
                     orte_process_name_t *sender,
                     opal_buffer_t *buf, void* cbdata);

static void recv_ack(int status,
                     orte_rmcast_channel_t channel,
                     orte_rmcast_seq_t seq_num,
                     orte_rmcast_tag_t tag,
                     orte_process_name_t *sender,
                     opal_buffer_t *buf, void* cbdata);

#include MCA_timer_IMPLEMENTATION_HEADER
static inline double gettime(void) __opal_attribute_always_inline__;
static inline double gettime(void)
{
    double wtime;
#if OPAL_TIMER_USEC_NATIVE
    wtime = ((double) opal_timer_base_get_usec()) / 1000000.0;
#else
    struct timeval tv;
    gettimeofday(&tv, NULL);
    wtime = tv.tv_sec;
    wtime += (double)tv.tv_usec / 1000000.0;
#endif
    return wtime;
}
#define TIMER_START(x) (x) = gettime();
#define TIMER_STOP(y,x) (y) = (gettime() - (x));

static int init(void)
{
    int rc;
    
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        /* daemons recv data server cmds */
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_DATA_SERVER_CHANNEL,
                                                             ORTE_RMCAST_TAG_DATA,
                                                             ORTE_RMCAST_PERSISTENT,
                                                             recv_cmd, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        OBJ_CONSTRUCT(&datastore, opal_pointer_array_t);
        opal_pointer_array_init(&datastore, 16, INT_MAX, 16);
    } else if (ORTE_PROC_IS_APP) {
        /* get my multicast output group */
        orte_rmcast.query_channel(&my_group_channel, NULL);
        
        /* recv responses */
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(my_group_channel,
                                                             ORTE_RMCAST_TAG_CMD_ACK,
                                                             ORTE_RMCAST_PERSISTENT,
                                                             recv_ack, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
    }
    return ORTE_SUCCESS;
}

static int finalize(void)
{
    int i;
    orte_db_data_t *dat;
    
    /* cancel the callbacks */
    if (ORTE_PROC_IS_DAEMON || ORTE_PROC_IS_HNP) {
        orte_rmcast.cancel_recv(ORTE_RMCAST_DATA_SERVER_CHANNEL, ORTE_RMCAST_TAG_DATA);
        for (i=0; i < datastore.size; i++) {
            if (NULL != (dat = (orte_db_data_t*)opal_pointer_array_get_item(&datastore, i))) {
                OBJ_RELEASE(dat);
            }
        }
        OBJ_DESTRUCT(&datastore);
    } else if (ORTE_PROC_IS_APP) {
        orte_rmcast.cancel_recv(my_group_channel, ORTE_RMCAST_TAG_WILDCARD);
    }

    return ORTE_SUCCESS;
}

static int send_data(orte_db_cmd_t cmd, char *key, void *object, opal_data_type_t type)
{
    opal_buffer_t *buf, dat;
    orte_job_t *jdata;
    orte_proc_t *proc;
    orte_job_state_t *job_state;
    orte_proc_state_t *proc_state;
    int rc;
    bool got_response;
    opal_byte_object_t bo;
    
    /* construct the buffer we will use for packing the data */
    buf = OBJ_NEW(opal_buffer_t);
    opal_dss.pack(buf, &cmd, 1, ORTE_DB_CMD_T); /* add cmd */
    opal_dss.pack(buf, &my_group_channel, 1, ORTE_RMCAST_CHANNEL_T);  /* tell the server my channel */
    opal_dss.pack(buf, &key, 1, OPAL_STRING);  /* pack the key */
    
    if (NULL != object) {
        OBJ_CONSTRUCT(&dat, opal_buffer_t);
        /* pack the data */
        switch (type) {
            case ORTE_JOB:
                jdata = (orte_job_t*)object;
                opal_dss.pack(&dat, &jdata, 1, ORTE_JOB);
                break;
            case ORTE_JOB_STATE:
                job_state = (orte_job_state_t*)object;
                opal_dss.pack(&dat, job_state, 1, ORTE_JOB_STATE);
                break;
                
            case ORTE_PROC:
                proc = (orte_proc_t*)object;
                opal_dss.pack(&dat, &proc, 1, ORTE_PROC);
                break;
            case ORTE_PROC_STATE:
                proc_state = (orte_proc_state_t*)object;
                opal_dss.pack(&dat, proc_state, 1, ORTE_PROC_STATE);
                break;
                
            default:
                orte_show_help("help-db-base.txt", "unrecognized-type", true, type);
                rc = ORTE_ERR_BAD_PARAM;
                goto cleanup;
                break;
        }
        opal_dss.unload(&dat, (void**)&bo.bytes, &bo.size);
        opal_dss.pack(buf, &bo.size, 1, OPAL_INT32);
        opal_dss.pack(buf, &bo.bytes, bo.size, OPAL_UINT8);
        OBJ_DESTRUCT(&dat);
        free(bo.bytes);
    }
    
    got_response = false;
    num_recvd = 0;
    ack_reqd = true;
    
    /* send the data to all the daemons */
    if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer_nb(ORTE_RMCAST_DATA_SERVER_CHANNEL,
                                                         ORTE_RMCAST_TAG_DATA, buf,
                                                         callback_fn, NULL))) {
        ORTE_ERROR_LOG(rc);
    }
    /* wait for all daemons to ack the request */
    ORTE_PROGRESSED_WAIT(got_response, num_recvd, orte_process_info.num_daemons);
    ack_reqd = false;
    
cleanup:
    return rc;
}

static int store(char *key, void *object, opal_data_type_t type)
{
    int rc;
    double start;
    double cpu_time_used;

    TIMER_START(start);
    
    if (ORTE_SUCCESS != (rc = send_data(ORTE_DB_STORE_CMD, key, object, type))) {
        ORTE_ERROR_LOG(rc);
    }
    
    TIMER_STOP(cpu_time_used, start);
    opal_output(0, "%s TOOK %g usecs TO STORE",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                cpu_time_used * 1000000.0);

    return rc;
}

static int set_source(orte_process_name_t *name)
{
    return ORTE_SUCCESS;
}

static int fetch(char *key, void *object, opal_data_type_t type)
{
    int rc;
    double cpu_time_used, start;
    
    TIMER_START(start);
    
    if (ORTE_SUCCESS != (rc = send_data(ORTE_DB_FETCH_CMD, key, NULL, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
    }
    
    TIMER_STOP(cpu_time_used, start);
    opal_output(0, "%s TOOK %g usecs TO FETCH",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                cpu_time_used * 1000000.0);

    return ORTE_SUCCESS;
}

static int update(char *key, void *object, opal_data_type_t type)
{
    int rc;
    double start;
    double cpu_time_used;
    
    TIMER_START(start);
    
    if (ORTE_SUCCESS != (rc = send_data(ORTE_DB_UPDATE_CMD, key, object, type))) {
        ORTE_ERROR_LOG(rc);
    }
    
    TIMER_STOP(cpu_time_used, start);
    opal_output(0, "%s TOOK %g usecs TO UPDATE",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                cpu_time_used * 1000000.0);
    
    return rc;
}

static int remove_data(char *key)
{
    int rc;
    double start;
    double cpu_time_used;

    TIMER_START(start);
    
    if (ORTE_SUCCESS != (rc = send_data(ORTE_DB_REMOVE_CMD, key, NULL, OPAL_INT32))) {
        ORTE_ERROR_LOG(rc);
    }
    
    TIMER_STOP(cpu_time_used, start);
    opal_output(0, "%s TOOK %g usecs TO REMOVE",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                cpu_time_used * 1000000.0);
    
    return rc;
}

static void callback_fn(int status,
                        orte_rmcast_channel_t channel,
                        orte_rmcast_seq_t seq_num,
                        orte_rmcast_tag_t tag,
                        orte_process_name_t *sender,
                        opal_buffer_t *buf, void* cbdata)
{
    OBJ_RELEASE(buf);
}

static void recv_ack(int status,
                     orte_rmcast_channel_t channel,
                     orte_rmcast_seq_t seq_num,
                     orte_rmcast_tag_t tag,
                     orte_process_name_t *sender,
                     opal_buffer_t *buf, void* cbdata)
{
    if (ack_reqd) {
        num_recvd++;
    }
}

static void recv_cmd(int status,
                     orte_rmcast_channel_t channel,
                     orte_rmcast_seq_t seq_num,
                     orte_rmcast_tag_t tag,
                     orte_process_name_t *sender,
                     opal_buffer_t *buf, void* cbdata)
{
    orte_db_cmd_t cmd;
    opal_buffer_t *ans;
    int count, i;
    int32_t rc;
    char *key;
    orte_db_data_t *dat;
    orte_rmcast_channel_t ch;
    char *ch_name;
    
    OPAL_OUTPUT_VERBOSE((2, orte_db_base_output,
                         "%s db:daemon: cmd recvd from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
 
    count=1;
    opal_dss.unpack(buf, &cmd, &count, ORTE_DB_CMD_T);
    count=1;
    opal_dss.unpack(buf, &ch, &count, ORTE_RMCAST_CHANNEL_T);
    count=1;
    opal_dss.unpack(buf, &ch_name, &count, OPAL_STRING);
    count=1;
    opal_dss.unpack(buf, &key, &count, OPAL_STRING);
    
    ans = OBJ_NEW(opal_buffer_t);
    opal_dss.pack(ans, &cmd, 1, ORTE_DB_CMD_T);
    
    switch (cmd) {
        case ORTE_DB_STORE_CMD:
            dat = OBJ_NEW(orte_db_data_t);
            dat->name.jobid = sender->jobid;
            dat->name.vpid = sender->vpid;
            dat->key = key;
            count=1;
            opal_dss.unpack(buf, &dat->size, &count, OPAL_INT32);
            dat->bytes = (uint8_t*)malloc(dat->size);
            opal_dss.unpack(buf, dat->bytes, &dat->size, OPAL_UINT8);
            opal_pointer_array_add(&datastore, dat);
            OPAL_OUTPUT_VERBOSE((2, orte_db_base_output,
                                 "%s db:daemon: data from %s stored: key %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(sender), key));
            rc = ORTE_SUCCESS;
            opal_dss.pack(ans, &rc, 1, OPAL_INT32);
            break;
        case ORTE_DB_FETCH_CMD:
            /* find the key */
            for (i=0; i < datastore.size; i++) {
                if (NULL == (dat = (orte_db_data_t*)opal_pointer_array_get_item(&datastore, i))) {
                    continue;
                }
                if (0 != strcmp(key, dat->key)) {
                    continue;
                }
                /* found the data - return it */
                rc = ORTE_SUCCESS;
                opal_dss.pack(ans, &rc, 1, OPAL_INT32);
                opal_dss.pack(ans, &dat->size, 1, OPAL_INT32);
                opal_dss.pack(ans, dat->bytes, dat->size, OPAL_UINT8);
                OPAL_OUTPUT_VERBOSE((2, orte_db_base_output,
                                     "%s db:daemon: data fetched for %s: key %s",
                                     ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                     ORTE_NAME_PRINT(sender), key));
                break;
            }
            rc = ORTE_ERR_NOT_FOUND;
            opal_dss.pack(ans, &rc, 1, OPAL_INT32);
            OPAL_OUTPUT_VERBOSE((2, orte_db_base_output,
                                 "%s db:daemon: data fetch request from %s not found: key %s",
                                 ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                                 ORTE_NAME_PRINT(sender), key));
            break;
            
        default:
            rc = ORTE_ERR_NOT_FOUND;
            break;
    }
    
    /* ensure the return channel is open */
    orte_rmcast.open_channel(ch, ch_name, NULL, -1, NULL, ORTE_RMCAST_XMIT);
    
    orte_rmcast.send_buffer_nb(ch, ORTE_RMCAST_TAG_CMD_ACK, ans, callback_fn, NULL);
}
