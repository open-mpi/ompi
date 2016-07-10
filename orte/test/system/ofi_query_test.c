#include "orte_config.h"

#include <stdio.h>
#include <signal.h>
#include <math.h>

#include <rdma/fabric.h>
#include <rdma/fi_cm.h>
#include <rdma/fi_domain.h>
#include <rdma/fi_endpoint.h>
#include <rdma/fi_errno.h>
#include <rdma/fi_tagged.h>

#include "opal/runtime/opal_progress.h"

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/rml/base/base.h"
#include "orte/mca/rml/rml_types.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"
#include "orte/runtime/orte_wait.h"

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
	
    opal_output(0,"\n print_transports_query() Begin- %s:%d",__FILE__,__LINE__);
    opal_output(0,"\n calling the orte_rml_ofi_query_transports() ");
    if( ORTE_SUCCESS == orte_rml.query_transports(&providers))
    {
	opal_output(0,"\n query_transports() completed, printing details\n");
	while (providers)
	{

		//get the first opal_list_t;
        opal_list_t temp;
		opal_list_t *prov = &temp;

		ret = opal_value_unload(providers,(void **)&prov,OPAL_PTR);
		if (ret == OPAL_SUCCESS) {
    		    opal_output_verbose(1,orte_rml_base_framework.framework_output,"\n %s:%d opal_value_unload() succeeded, opal_list* prov = %x",
                                                                                                                          __FILE__,__LINE__,prov); 
            if( orte_get_attribute( prov, ORTE_CONDUIT_ID, (void **)&prov_num,OPAL_UINT8)) {
                opal_output(0," Provider conduit_id  : %d",*prov_num);
            }	
		    if( orte_get_attribute( prov, ORTE_PROTOCOL, (void **)&protocol_ptr,OPAL_UINT32)) {
		      	opal_output(0," Protocol  : %s",fi_tostr(protocol_ptr,FI_TYPE_PROTOCOL));
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
    	//	opal_output_verbose(1,orte_rml_base_framework.framework_output,"\n %s:%d - 
        //                                Moving on to next provider provders=%x",__FILE__,__LINE__,providers);
	}
    } else {
        opal_output(0,"\n query_transports() returned Error ");
    }
    opal_output(0,"\n End of print_transports_query() from ofi_query_test.c \n");

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

    
     opal_output(0, "%s pid = %d ", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), getpid());

     /*
     * Init
     */
    orte_init(&argc, &argv, ORTE_PROC_NON_MPI);
   //  orte_init(&argc, &argv, ORTE_PROC_MPI);

        /*
     * Runtime Messaging Layer  - added this as RML was not being initialised in the app process, 
     * but now ompimaster has code added to call this automatically
     */
/*
     if (ORTE_SUCCESS == ( mca_base_framework_open(&orte_rml_base_framework, 0))) {
	 opal_output(0, "%s RML framework opened successfully ", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), getpid());
	if (ORTE_SUCCESS == orte_rml_base_select()) {
	    opal_output(0, "%s RML framework base_select completed successfully ", ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), getpid());
	    print_transports_query();
	}
    }
*/

    print_transports_query();
    opal_output(0, "%s calling orte_finalize() from ofi_query_test.c ",ORTE_NAME_PRINT(ORTE_PROC_MY_NAME));
    orte_finalize();

    return 0;
}
