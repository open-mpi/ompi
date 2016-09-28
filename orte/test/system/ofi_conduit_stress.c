#include "orte_config.h"

#include <stdio.h>
#include <signal.h>
#include <math.h>
#include <sys/time.h>

#include "opal/runtime/opal_progress.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"
#include "orte/util/attr.h"

#define MY_TAG 12345
#define MAX_COUNT 3

static bool msg_recvd;
static volatile bool msg_active;

static void send_callback(int status, orte_process_name_t *peer,
                          opal_buffer_t* buffer, orte_rml_tag_t tag,
                          void* cbdata)

{
    OBJ_RELEASE(buffer);
    if (ORTE_SUCCESS != status) {
        exit(1);
    }
    msg_active = false;
}

//debug routine to print the opal_value_t returned by query interface
void print_transports_query()
{
    opal_value_t *providers=NULL;
    char* prov_name = NULL;
    int ret;
    int32_t *protocol_ptr, protocol;
    int8_t conduit_id;
    int8_t *prov_num=&conduit_id;

    protocol_ptr = &protocol;
     opal_output(0, "\n Current conduits loaded in rml-ofi ==>");
    /*opal_output(0,"\n print_transports_query() Begin- %s:%d",__FILE__,__LINE__);
    opal_output(0,"\n calling the orte_rml_ofi_query_transports() ");*/
    if( ORTE_SUCCESS == orte_rml.query_transports(&providers)) {
        //opal_output(0,"\n query_transports() completed, printing details\n");
        while (providers) {
            //get the first opal_list_t;
            opal_list_t temp;
            opal_list_t *prov = &temp;

            ret = opal_value_unload(providers,(void **)&prov,OPAL_PTR);
            if (ret == OPAL_SUCCESS) {
                //opal_output(0,"\n %s:%d opal_value_unload() succeeded, opal_list* prov = %x",__FILE__,__LINE__,prov);
                if( orte_get_attribute( prov, ORTE_CONDUIT_ID, (void **)&prov_num,OPAL_UINT8)) {
                    opal_output(0," Provider conduit_id  : %d",*prov_num);
                }
                if( orte_get_attribute( prov, ORTE_PROTOCOL, (void **)&protocol_ptr,OPAL_UINT32)) {
                   opal_output(0," Protocol  : %d",*protocol_ptr);
                }
                if( orte_get_attribute( prov, ORTE_PROV_NAME, (void **)&prov_name ,OPAL_STRING)) {
                    opal_output(0," Provider name : %s",prov_name);
                } else {
                    opal_output(0," Error in getting Provider name");
                }
            } else {
                opal_output(0," %s:%d opal_value_unload() failed, opal_list* prov = %x",__FILE__,__LINE__,prov);
            }
            providers = (opal_value_t *)providers->super.opal_list_next;
            // opal_output_verbose(1,orte_rml_base_framework.framework_output,"\n %s:%d -
            //                                Moving on to next provider provders=%x",__FILE__,__LINE__,providers);
        }
    } else {
        opal_output(0,"\n query_transports() returned Error ");
    }
    //opal_output(0,"\n End of print_transports_query() from ofi_query_test.c \n");

  //need to free all the providers here
}


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
    int conduit_id = 0;  //use the first available conduit
    struct timeval start, end;
    opal_list_t *conduit_attr;


    /*
     * Init
     */
    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);

    print_transports_query();
    conduit_attr = OBJ_NEW(opal_list_t);
   if( ORTE_SUCCESS ==
            ( orte_set_attribute( conduit_attr, ORTE_RML_OFI_PROV_NAME_ATTRIB, ORTE_ATTR_GLOBAL,"sockets",OPAL_STRING)))   {
    if( ORTE_SUCCESS ==
            ( orte_set_attribute( conduit_attr, ORTE_RML_INCLUDE_COMP_ATTRIB, ORTE_ATTR_GLOBAL,"ofi",OPAL_STRING)))   {
        opal_output(0, "%s calling open_conduit with ORTE_RML_INCLUDE_COMP_ATTRIB and ORTE_RML_OFI_PROV_NAME_ATTRIB",
                         ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
        conduit_id = orte_rml_API_open_conduit(conduit_attr);
        if (0 > conduit_id ) {
            opal_output(0, "Conduit could not be opened for OFI, exiting");
            return;
        }
     }
   }

    opal_output(0, "Using conduit-id %d ", conduit_id);

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

    gettimeofday(&start, NULL);
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
            orte_rml.send_buffer_nb_conduit(conduit_id,&peer, buf, MY_TAG, orte_rml_send_callback, NULL);

            /* wait for it to come around */
            OBJ_CONSTRUCT(&blob, orte_rml_recv_cb_t);
            blob.active = true;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                    ORTE_RML_NON_PERSISTENT,
                                    orte_rml_recv_callback, &blob);
            ORTE_WAIT_FOR_COMPLETION(blob.active);
            OBJ_DESTRUCT(&blob);

            opal_output(0, "%s Ring %d completed", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j);
        } else {
            /* wait for msg */
            OBJ_CONSTRUCT(&blob, orte_rml_recv_cb_t);
            blob.active = true;
            orte_rml.recv_buffer_nb(ORTE_NAME_WILDCARD, MY_TAG,
                                    ORTE_RML_NON_PERSISTENT,
                                    orte_rml_recv_callback, &blob);
            ORTE_WAIT_FOR_COMPLETION(blob.active);

            opal_output(0, "%s received message %d from %s", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), j, ORTE_NAME_PRINT(&blob.name));

            /* send it along */
            buf = OBJ_NEW(opal_buffer_t);
            opal_dss.copy_payload(buf, &blob.data);
            OBJ_DESTRUCT(&blob);
            msg_active = true;
            orte_rml.send_buffer_nb_conduit(conduit_id,&peer, buf, MY_TAG, send_callback, NULL);
            ORTE_WAIT_FOR_COMPLETION(msg_active);
        }
    }
    gettimeofday(&end, NULL);
    orte_finalize();
    printf("start: %d secs, %d usecs\n",start.tv_sec,start.tv_usec);
    printf("end: %d secs, %d usecs\n",end.tv_sec,end.tv_usec);
    printf("Total minutes = %d, Total seconds = %d", (end.tv_sec - start.tv_sec)/60, (end.tv_sec - start.tv_sec)   );
    return 0;
}
