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
#include "orte/mca/errmgr/base/base.h"
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
    char *key;
    char *dptr;
    int size;
} orte_db_data_t;
static void constructor(orte_db_data_t *dt)
{
    dt->key = NULL;
    dt->dptr = NULL;
    dt->size = 0;
}
static void destructor(orte_db_data_t *dt)
{
    if (NULL != dt->key) {
        free(dt->key);
    }
    if (NULL != dt->dptr) {
        free(dt->dptr);
    }
}
OBJ_CLASS_INSTANCE(orte_db_data_t,
                   opal_object_t,
                   constructor,
                   destructor);

/* local variables */
static orte_vpid_t num_recvd;
static bool ack_reqd;
static opal_pointer_array_t datastore;

/* local functions */
static void callback_fn(int status,
                        orte_rmcast_channel_t channel,
                        orte_rmcast_tag_t tag,
                        orte_process_name_t *sender,
                        opal_buffer_t *buf, void* cbdata);

static void recv_cmd(int status,
                     orte_rmcast_channel_t channel,
                     orte_rmcast_tag_t tag,
                     orte_process_name_t *sender,
                     opal_buffer_t *buf, void* cbdata);

static void recv_data(int status,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      orte_process_name_t *sender,
                      opal_buffer_t *buf, void* cbdata);

static void recv_ack(int status,
                     orte_rmcast_channel_t channel,
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
        /* recv data back */
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_GROUP_CHANNEL,
                                                             ORTE_RMCAST_TAG_DATA,
                                                             ORTE_RMCAST_PERSISTENT,
                                                             recv_data, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        
        /* recv cmd acks */
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_GROUP_CHANNEL,
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
        orte_rmcast.cancel_recv(ORTE_RMCAST_GROUP_CHANNEL, ORTE_RMCAST_TAG_DATA);
        orte_rmcast.cancel_recv(ORTE_RMCAST_GROUP_CHANNEL, ORTE_RMCAST_TAG_CMD_ACK);
    }

    return ORTE_SUCCESS;
}

static int send_data(orte_db_cmd_t cmd, char *key, void *object, opal_data_type_t type)
{
    opal_buffer_t *buf;
    orte_job_t *jdata;
    orte_proc_t *proc;
    orte_job_state_t *job_state;
    orte_proc_state_t *proc_state;
    int rc;
    bool got_response;
    
    /* construct the buffer we will use for packing the data */
    buf = OBJ_NEW(opal_buffer_t);
    opal_dss.pack(buf, &cmd, 1, ORTE_DB_CMD_T); /* add cmd */
    opal_dss.pack(buf, &key, 1, OPAL_STRING);  /* pack the key */
    
    /* pack the data */
    switch (type) {
        case ORTE_JOB:
            jdata = (orte_job_t*)object;
            opal_dss.pack(buf, &jdata, 1, ORTE_JOB);
            break;
        case ORTE_JOB_STATE:
            job_state = (orte_job_state_t*)object;
            opal_dss.pack(buf, job_state, 1, ORTE_JOB_STATE);
            break;
            
        case ORTE_PROC:
            proc = (orte_proc_t*)object;
            opal_dss.pack(buf, &proc, 1, ORTE_PROC);
            break;
        case ORTE_PROC_STATE:
            proc_state = (orte_proc_state_t*)object;
            opal_dss.pack(buf, proc_state, 1, ORTE_PROC_STATE);
            break;
            
        default:
            orte_show_help("help-db-base.txt", "unrecognized-type", true, type);
            rc = ORTE_ERR_BAD_PARAM;
            goto cleanup;
            break;
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
    ORTE_PROGRESSED_WAIT(got_response, num_recvd, orte_process_info.num_procs-1);
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
    opal_buffer_t *buf;
    int rc;
    bool got_response;
    double cpu_time_used, start;
    orte_db_cmd_t cmd=ORTE_DB_FETCH_CMD;
    
    TIMER_START(start);
    
    /* construct the buffer we will use for packing the data */
    buf = OBJ_NEW(opal_buffer_t);
    opal_dss.pack(buf, &cmd, 1, ORTE_DB_CMD_T); /* add cmd */
    opal_dss.pack(buf, &key, 1, OPAL_STRING);  /* pack the key */

    got_response = false;
    num_recvd = 0;
    ack_reqd = true;
    
    /* send the cmd to all the daemons */
    if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer_nb(ORTE_RMCAST_DATA_SERVER_CHANNEL,
                                                         ORTE_RMCAST_TAG_DATA, buf,
                                                         callback_fn, NULL))) {
        ORTE_ERROR_LOG(rc);
    }

    /* wait for all daemons to respond */
    ORTE_PROGRESSED_WAIT(got_response, num_recvd, orte_process_info.num_procs-1);
    ack_reqd = false;
    
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
    opal_buffer_t *buf;
    int rc;
    bool got_response;
    double start;
    double cpu_time_used;
    orte_db_cmd_t cmd=ORTE_DB_REMOVE_CMD;

    TIMER_START(start);
    
    /* construct the buffer we will use for packing the data */
    buf = OBJ_NEW(opal_buffer_t);
    opal_dss.pack(buf, &cmd, 1, ORTE_DB_CMD_T); /* add cmd */
    opal_dss.pack(buf, &key, 1, OPAL_STRING);  /* pack the key */
    
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
    ORTE_PROGRESSED_WAIT(got_response, num_recvd, orte_process_info.num_procs-1);
    ack_reqd = false;
    
    TIMER_STOP(cpu_time_used, start);
    opal_output(0, "%s TOOK %g usecs TO REMOVE",
                ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                cpu_time_used * 1000000.0);
    
    return rc;
}

static void callback_fn(int status,
                        orte_rmcast_channel_t channel,
                        orte_rmcast_tag_t tag,
                        orte_process_name_t *sender,
                        opal_buffer_t *buf, void* cbdata)
{
    OBJ_RELEASE(buf);
}

static void recv_ack(int status,
                     orte_rmcast_channel_t channel,
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
                     orte_rmcast_tag_t tag,
                     orte_process_name_t *sender,
                     opal_buffer_t *buf, void* cbdata)
{
    orte_db_cmd_t cmd;
    opal_buffer_t *ans, xfer;
    int count, i;
    int32_t rc;
    char *key;
    orte_db_data_t *dat;
    
    OPAL_OUTPUT_VERBOSE((2, orte_db_base_output,
                         "%s db:daemon: cmd recvd from %s",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                         ORTE_NAME_PRINT(sender)));
 
    count=1;
    opal_dss.unpack(buf, &cmd, &count, ORTE_DB_CMD_T);
    count=1;
    opal_dss.unpack(buf, &key, &count, OPAL_STRING);
    
    ans = OBJ_NEW(opal_buffer_t);
    opal_dss.pack(ans, &cmd, 1, ORTE_DB_CMD_T);
    
    switch (cmd) {
        case ORTE_DB_STORE_CMD:
            dat = OBJ_NEW(orte_db_data_t);
            dat->key = key;
            opal_dss.unload(buf, (void**)&dat->dptr, &dat->size);
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
                OBJ_CONSTRUCT(&xfer, opal_buffer_t);
                opal_dss.load(&xfer, dat->dptr, dat->size);
                opal_dss.copy_payload(ans, &xfer);
                OBJ_DESTRUCT(&xfer);
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
    
    orte_rmcast.send_buffer(channel, ORTE_RMCAST_TAG_CMD_ACK, ans);
}

static void recv_data(int status,
                      orte_rmcast_channel_t channel,
                      orte_rmcast_tag_t tag,
                      orte_process_name_t *sender,
                      opal_buffer_t *buf, void* cbdata)
{
    
}
