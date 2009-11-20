/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/dss/dss.h"
#include "opal/event/event.h"
#include "opal/util/output.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/errmgr/errmgr.h"
#include "orte/mca/rmcast/rmcast.h"
#include "orte/mca/grpcomm/grpcomm.h"

static void cbfunc(int status,
                   orte_rmcast_channel_t channel,
                   orte_rmcast_tag_t tag,
                   orte_process_name_t *sender,
                   opal_buffer_t *buf, void *cbdata);
static void cbfunc_buf_snt(int status,
                           orte_rmcast_channel_t channel,
                           orte_rmcast_tag_t tag,
                           orte_process_name_t *sender,
                           opal_buffer_t *buf, void *cbdata);

static void cbfunc_iovec(int status,
                         orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         orte_process_name_t *sender,
                         struct iovec *msg, int count, void* cbdata);

int main(int argc, char* argv[])
{
    int rc, i;
    char hostname[512];
    pid_t pid;
    opal_buffer_t buf, *bfptr;
    int32_t i32=1;
    struct iovec iovec_array[3];
    orte_rmcast_channel_t chan=4;
    
    if (0 > (rc = orte_init(ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_nodename: couldn't init orte - error code %d\n", rc);
        return rc;
    }
    
    gethostname(hostname, 512);
    pid = getpid();
    
    printf("orte_mcast: Node %s Name %s Pid %ld\n",
           hostname, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)pid);
    
    
    
    if (0 == ORTE_PROC_MY_NAME->vpid) {
        /* open a new channel */
        if (ORTE_SUCCESS != (rc = orte_rmcast.open_channel(&chan, "orte_mcast", NULL, -1, NULL, ORTE_RMCAST_XMIT))) {
            ORTE_ERROR_LOG(rc);
            goto blast;
        }
        orte_grpcomm.barrier();
        
        OBJ_CONSTRUCT(&buf, opal_buffer_t);
        opal_dss.pack(&buf, &i32, 1, OPAL_INT32);
        if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer(ORTE_RMCAST_APP_PUBLIC_CHANNEL,
                                                          ORTE_RMCAST_TAG_ANNOUNCE, &buf))) {
            ORTE_ERROR_LOG(rc);
            OBJ_DESTRUCT(&buf);
            goto blast;
        }
        orte_grpcomm.barrier();
        OBJ_DESTRUCT(&buf);
        bfptr = OBJ_NEW(opal_buffer_t);
        i32 = 2;
        opal_dss.pack(bfptr, &i32, 1, OPAL_INT32);
        if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer_nb(chan,
                                                             ORTE_RMCAST_TAG_OUTPUT, bfptr,
                                                             cbfunc_buf_snt, NULL))) {
            ORTE_ERROR_LOG(rc);
            OBJ_RELEASE(bfptr);
            goto blast;
        }        
        orte_grpcomm.barrier();
        /* create an iovec array */
        for (i=0; i < 3; i++) {
            iovec_array[i].iov_base = (uint8_t*)malloc(30);
            iovec_array[i].iov_len = 30;
        }
        /* send it out */
        if (ORTE_SUCCESS != (rc = orte_rmcast.send(ORTE_RMCAST_APP_PUBLIC_CHANNEL,
                                                   ORTE_RMCAST_TAG_WILDCARD,
                                                   iovec_array, 3))) {
            ORTE_ERROR_LOG(rc);
            goto blast;
        }    
        orte_grpcomm.barrier();
        orte_finalize();
        return 0;
    } else {
        /* open a new channel */
        if (ORTE_SUCCESS != (rc = orte_rmcast.open_channel(&chan, "orte_mcast", NULL, -1, NULL, ORTE_RMCAST_RECV))) {
            ORTE_ERROR_LOG(rc);
        }
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(ORTE_RMCAST_APP_PUBLIC_CHANNEL,
                                                             ORTE_RMCAST_TAG_WILDCARD,
                                                             ORTE_RMCAST_PERSISTENT,
                                                             cbfunc, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_buffer_nb(chan,
                                                             ORTE_RMCAST_TAG_WILDCARD,
                                                             ORTE_RMCAST_PERSISTENT,
                                                             cbfunc, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        if (ORTE_SUCCESS != (rc = orte_rmcast.recv_nb(ORTE_RMCAST_APP_PUBLIC_CHANNEL,
                                                      ORTE_RMCAST_TAG_WILDCARD,
                                                      ORTE_RMCAST_PERSISTENT,
                                                      cbfunc_iovec, NULL))) {
            ORTE_ERROR_LOG(rc);
        }
        
        orte_grpcomm.barrier();
        
    }
    opal_event_dispatch();
    
blast:    
    orte_finalize();
    return 0;
}

static void cbfunc(int status,
                   orte_rmcast_channel_t channel,
                   orte_rmcast_tag_t tag,
                   orte_process_name_t *sender,
                   opal_buffer_t *buffer, void *cbdata)
{
    int32_t i32, rc;

    /* retrieve the value sent */
    rc = 1;
    opal_dss.unpack(buffer, &i32, &rc, OPAL_INT32);

    opal_output(0, "%s GOT BUFFER MESSAGE from %s on channel %d tag %d with value %d\n",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
            ORTE_NAME_PRINT(sender), channel, tag, i32);

    orte_grpcomm.barrier();

#if 0
    opal_buffer_t *buf;
    int32_t i32=2, rc;

    buf = OBJ_NEW(opal_buffer_t);
    opal_dss.pack(buf, &i32, 1, OPAL_INT32);
    opal_dss.pack(buf, &i32, 1, OPAL_INT32);
    opal_dss.pack(buf, &i32, 1, OPAL_INT32);

    

    if (0 != ORTE_PROC_MY_NAME->vpid) {
        if (1 == i32) {
            if (ORTE_SUCCESS != (rc = orte_rmcast.send_buffer(ORTE_RMCAST_APP_PUBLIC_CHANNEL,
                                                              ORTE_RMCAST_TAG_WILDCARD, buf))) {
                ORTE_ERROR_LOG(rc);
            }
        }
        OBJ_RELEASE(buf);
    }
#endif
}

static void cbfunc_iovec(int status,
                         orte_rmcast_channel_t channel,
                         orte_rmcast_tag_t tag,
                         orte_process_name_t *sender,
                         struct iovec *msg, int count, void* cbdata)
{
    int rc;
    
    opal_output(0, "%s GOT IOVEC MESSAGE from %s of %d elements\n",
            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), ORTE_NAME_PRINT(sender), count);

#if 0
    if (0 != ORTE_PROC_MY_NAME->vpid) {
        /* send it back */
        if (ORTE_SUCCESS != (rc = orte_rmcast.send(ORTE_RMCAST_APP_PUBLIC_CHANNEL,
                                                   ORTE_RMCAST_TAG_WILDCARD, msg, count))) {
            ORTE_ERROR_LOG(rc);
        }    
    }
#endif
    orte_grpcomm.barrier();
    orte_finalize();
    exit(0);
}

static void cbfunc_buf_snt(int status,
                           orte_rmcast_channel_t channel,
                           orte_rmcast_tag_t tag,
                           orte_process_name_t *sender,
                           opal_buffer_t *buf, void *cbdata)
{
    opal_output(0, "%s BUFFERED_NB SEND COMPLETE\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    
    OBJ_RELEASE(buf);
}
