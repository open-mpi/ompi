#include "orte_config.h"

#include <stdio.h>
#include <signal.h>
#include <math.h>

#include "opal/runtime/opal_progress.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/mca/qos/qos.h"
#include "orte/util/attr.h"

#define MY_TAG 12345
#define MAX_COUNT 3

static volatile bool msgs_recvd;
static volatile bool channel_inactive = false;
static volatile bool msg_active = false;
static volatile orte_rml_channel_num_t channel;
static volatile int num_msgs_recvd = 0;
static void send_channel_callback(int status,
                                  orte_rml_channel_num_t channel_num,
                                  orte_process_name_t * peer,
                                  opal_list_t *qos_attributes,
                                  void * cbdata)
{
    if (ORTE_SUCCESS != status) {
        opal_output(0, "open channel not successful status =%d", status);

    } else {
        channel = channel_num;
        opal_output(0, "Open channel successful - channel num = %d", channel_num);

    }
    channel_inactive = false;
}
static void send_callback(int status, orte_process_name_t *peer,
                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata)

{
    OBJ_RELEASE(buffer);
    if (ORTE_SUCCESS != status) {
        opal_output(0, "rml_send_nb  not successful status =%d", status);
    }
    msg_active = false;
}

static void recv_callback(int status, orte_process_name_t *sender,
                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata)

{
    //orte_rml_recv_cb_t *blob = (orte_rml_recv_cb_t*)cbdata;
    num_msgs_recvd++;
    opal_output(0, "recv_callback received msg =%d", num_msgs_recvd);
    if ( num_msgs_recvd == 5) {
        num_msgs_recvd =0;
        msgs_recvd = false;
        /* transfer the sender */
      //  blob->name.jobid = sender->jobid;
       // blob->name.vpid = sender->vpid;
       /* just copy the payload to the buf */
        //opal_dss.copy_payload(&blob->data, buffer);
        /* flag as complete */
       // blob->active = false;
    }
    //else
       // OBJ_DESTRUCT(blob);

}

static void channel_send_callback (int status, orte_rml_channel_num_t channel,
                                   opal_buffer_t * buffer, orte_rml_tag_t tag,
                                   void *cbdata)
{
    OBJ_RELEASE(buffer);
    if (ORTE_SUCCESS != status) {
        opal_output(0, "send_nb_channel not successful status =%d", status);
    }
    msg_active = false;
}


int main(int argc, char *argv[]){
    int count;
    int msgsize;
    int *type, type_val;
    int *i, j, rc, n;
    orte_process_name_t peer;
    double maxpower;
    opal_buffer_t *buf;
    orte_rml_recv_cb_t blob;
    opal_list_t *qos_attributes;
    int  window;
    uint32_t timeout = 1;
    bool retry = false;
    uint8_t *msg;
    /*
     * Init
     */
    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);

    if (argc > 1) {
        count = atoi(argv[1]);
        if (count < 0) {
            count = INT_MAX-1;
        }
    } else {
        count = MAX_COUNT;
    }

    peer.jobid = ORTE_PROC_MY_NAME->jobid;
    peer.vpid = ORTE_PROC_MY_NAME->vpid + 1;
    if (peer.vpid == orte_process_info.num_procs) {
        peer.vpid = 0;
    }
    type_val = orte_qos_ack;
    type = &type_val;
    window = 5;
    count =3;
    if (ORTE_PROC_MY_NAME->vpid == 0) {
        qos_attributes = OBJ_NEW (opal_list_t);
        if (ORTE_SUCCESS == (rc = orte_set_attribute( qos_attributes,
                                  ORTE_QOS_TYPE, ORTE_ATTR_GLOBAL, (void*)type, OPAL_UINT8))) {
            type = &window;
            if (ORTE_SUCCESS == (rc = orte_set_attribute(qos_attributes, ORTE_QOS_WINDOW_SIZE,
                                      ORTE_ATTR_GLOBAL, (void*) type, OPAL_UINT32))) {
                //  orte_get_attribute( &qos_attributes, ORTE_QOS_WINDOW_SIZE, (void**)&type, OPAL_UINT32);
                // opal_output(0, "%s set attribute window =%d complete \n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), *type );
                type = &timeout;
                orte_set_attribute (qos_attributes, ORTE_QOS_ACK_NACK_TIMEOUT, ORTE_ATTR_GLOBAL,
                                    (void*)type, OPAL_UINT32);

                orte_set_attribute (qos_attributes, ORTE_QOS_MSG_RETRY, ORTE_ATTR_GLOBAL,
                                    NULL, OPAL_BOOL);
                /* Uncomment following lines to print channel attributes */
                /*
                opal_output(0, "%s set attribute retry =%d complete \n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), retry );

                orte_get_attribute( qos_attributes, ORTE_QOS_TYPE, (void**)&type, OPAL_UINT8);
                opal_output(0, "%s set attribute type =%d complete \n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), *type );
                orte_get_attribute( qos_attributes, ORTE_QOS_WINDOW_SIZE, (void**)&type, OPAL_UINT32);
                opal_output(0, "%s set attribute window =%d complete \n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), *type );
                orte_get_attribute( qos_attributes, ORTE_QOS_ACK_NACK_TIMEOUT, (void**)&type, OPAL_UINT32);
                opal_output(0, "%s set attribute timeout =%d complete \n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), *type );*/

                channel_inactive = true;
                orte_rml.open_channel ( &peer, qos_attributes, send_channel_callback, NULL);
                opal_output(0, "%s process sent open channel request %d waiting for completion \n",
                            ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);

                ORTE_WAIT_FOR_COMPLETION(channel_inactive);
                opal_output(0, "%s open channel complete", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME) );
            }
        }
    }
    else {
        // other process waits to recv a buffer from rank 0
        orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                ORTE_RML_PERSISTENT,
                                recv_callback, NULL);
    }
    /* send a window of messages to peer on the channel */
    for (j=1; j < count+1; j++) {
        if (ORTE_PROC_MY_NAME->vpid == 0) {
            /* rank0 starts ring */
            msg_active = true;
            for (n = 0; n< window; n++ ) {
                buf = OBJ_NEW(opal_buffer_t);
                maxpower = (double)(j%7);
                msgsize = (int)pow(10.0, maxpower);
                opal_output(0, "Ring %d message %d size %d bytes", j,n, msgsize);
                msg = (uint8_t*)malloc(msgsize);
                opal_dss.pack(buf, msg, msgsize, OPAL_BYTE);
                free(msg);
                orte_rml.send_buffer_channel_nb(channel, buf, MY_TAG, channel_send_callback, NULL);
                //orte_rml.send_buffer_nb(&peer, buf,MY_TAG, send_callback, NULL)
            }
            //orte_rml.send_buffer_nb(&peer, buf,MY_TAG, send_callback, NULL)
            /* wait for it to come around */
            OBJ_CONSTRUCT(&blob, orte_rml_recv_cb_t);
            blob.active = true;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                      ORTE_RML_NON_PERSISTENT,
                                      orte_rml_recv_callback, &blob);
            ORTE_WAIT_FOR_COMPLETION(blob.active);
            OBJ_DESTRUCT(&blob);
            ORTE_WAIT_FOR_COMPLETION(msg_active);
            opal_output(0, "%s Ring %d completed", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);
            sleep(2);
       }
       else {
            msgs_recvd = true;
            ORTE_WAIT_FOR_COMPLETION(msgs_recvd);
            buf = OBJ_NEW(opal_buffer_t);
            /* send it along */
            msg_active = true;
            maxpower = (double)(j%7);
            msgsize = (int)pow(10.0, maxpower);
            opal_output(0, "Ring %d message %d size %d bytes", j,n, msgsize);
            msg = (uint8_t*)malloc(msgsize);
            opal_dss.pack(buf, msg, msgsize, OPAL_BYTE);
            free(msg);
            orte_rml.send_buffer_nb(&peer, buf, MY_TAG, send_callback, NULL);
            ORTE_WAIT_FOR_COMPLETION(msg_active);
            sleep (2);
        }
    }

    orte_finalize();

    return 0;
}
