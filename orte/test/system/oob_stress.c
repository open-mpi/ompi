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

#define MY_TAG 12345
#define MAX_COUNT 3

static bool msg_recvd;


int
main(int argc, char *argv[]){
    int count;
    int msgsize;
    uint8_t *msg;
    int i, j, rc;
    orte_process_name_t peer;
    double maxpower;
    opal_buffer_t *buf;
    orte_rml_recv_cb_t blob;

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
            
    for (j=1; j < count+1; j++) {
        /* rank0 starts ring */
        if (ORTE_PROC_MY_NAME->vpid == 0) {
            /* setup the initiating buffer - put random sized message in it */
            buf = OBJ_NEW(opal_buffer_t);
            
            maxpower = (double)(j%7);
            msgsize = (int)pow(10.0, maxpower);
            opal_output(0, "Ring %d message size %d bytes", j, msgsize);
            msg = (uint8_t*)malloc(msgsize);
            opal_dss.pack(buf, msg, msgsize, OPAL_BYTE);
            free(msg);
            orte_rml.send_buffer_nb(&peer, buf, MY_TAG, orte_rml_send_callback, NULL);

            /* wait for it to come around */
            OBJ_CONSTRUCT(&blob, orte_rml_recv_cb_t);
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                    ORTE_RML_NON_PERSISTENT,
                                    orte_rml_recv_callback, &blob);
            ORTE_WAIT_FOR_COMPLETION(blob.waiting);
            OBJ_DESTRUCT(&blob);

            opal_output(0, "%s Ring %d completed", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);
        } else {
            /* wait for msg */
            OBJ_CONSTRUCT(&blob, orte_rml_recv_cb_t);
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                    ORTE_RML_NON_PERSISTENT,
                                    orte_rml_recv_callback, &blob);
            ORTE_WAIT_FOR_COMPLETION(blob.waiting);

            opal_output(0, "%s received message %d from %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j, ORTE_NAME_PRINT(&blob.name));

            /* send it along */
            buf = OBJ_NEW(opal_buffer_t);
            opal_dss.copy_payload(buf, &blob.data);
            OBJ_DESTRUCT(&blob);
            orte_rml.send_buffer_nb(&peer, buf, MY_TAG, orte_rml_send_callback, NULL);
        }
    }

    orte_finalize();

    return 0;
}
