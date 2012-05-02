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
static opal_buffer_t buf;


static void recv_ack(int status, orte_process_name_t* sender,
                     opal_buffer_t* buffer, orte_rml_tag_t tag,
                     void* cbdata)
{
    /* save the buffer */
    opal_dss.copy_payload(&buf, buffer);
    msg_recvd = true;
    
    /* repost recv */
    orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                            ORTE_RML_NON_PERSISTENT, recv_ack, NULL);

}

int
main(int argc, char *argv[]){
    int count;
    int msgsize;
    uint8_t *msg;
    int i, j, rc;
    orte_process_name_t peer;
    double maxpower;
    
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
    
    for (j=1; j < count+1; j++) {
        peer.vpid = (ORTE_PROC_MY_NAME->vpid + j) % orte_process_info.num_procs;
        
        /* rank0 starts ring */
        if (ORTE_PROC_MY_NAME->vpid == 0) {
            /* setup the initiating buffer - put random sized message in it */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            
            maxpower = (double)(j%7);
            msgsize = (int)pow(10.0, maxpower);
            opal_output(0, "Ring %d message size %d bytes", j, msgsize);
            msg = (uint8_t*)malloc(msgsize);
            opal_dss.pack(&buf, msg, msgsize, OPAL_BYTE);
            
            if (0 > (rc = orte_rml.send_buffer(&peer,&buf, MY_TAG, 0))) {
                opal_output(0, "error sending to %s %s\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer), ORTE_ERROR_NAME(rc));
                exit(1);
            }
            OBJ_DESTRUCT(&buf);
            /* wait for it to come around */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            msg_recvd = false;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                    ORTE_RML_NON_PERSISTENT, recv_ack, NULL);
            
            while (!msg_recvd) {
                opal_progress();
            }

            opal_output(0, "%s Ring %d completed", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);
        } else {
            /* wait for msg */
            OBJ_CONSTRUCT(&buf, opal_buffer_t);
            msg_recvd = false;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                    ORTE_RML_NON_PERSISTENT, recv_ack, NULL);
            
            while (!msg_recvd) {
                opal_progress();
            }

            /* send it along */
            if (0 > (rc = orte_rml.send_buffer(&peer, &buf, MY_TAG, 0))) {
                opal_output(0, "%s error sending to %s %s\n", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME),
                            ORTE_NAME_PRINT(&peer), ORTE_ERROR_NAME(rc));
                exit(1);
            }
            OBJ_DESTRUCT(&buf);
        }
    }

    orte_finalize();

    return 0;
}
