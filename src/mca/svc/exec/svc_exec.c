#include "ompi_config.h"

#include <unistd.h>

#include "include/constants.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "svc_exec.h"


mca_svc_base_module_t mca_svc_exec_module = {
    mca_svc_exec_module_init,
    mca_svc_exec_module_fini
};


/**
 *  Process an exec command.
 */

static void mca_svc_exec_exec(const ompi_process_name_t* peer, ompi_buffer_t request )
{
    pid_t pid = -1;
    int32_t   i;
    int32_t   base_pid;
    int32_t   num_procs;
    int32_t   num_argv;
    int32_t   num_env;
    int32_t   status = OMPI_SUCCESS;

    char  **argv = NULL;
    char  **env = NULL;

	/* Initialize a buffer for the response */
    ompi_buffer_t  response;
	ompi_buffer_init( &response, 128 );

    /* unpack request */
    ompi_unpack( request, &num_procs, 1, OMPI_INT32 );
    ompi_unpack( request, &base_pid, 1, OMPI_INT32 );

    /* unpack command line */
	ompi_unpack( request, &num_argv, 1, OMPI_INT32 );
	if ( NULL == ( *argv = malloc ( (num_argv+1) * sizeof( char* ) ) ) ) {
	    status =  OMPI_ERR_OUT_OF_RESOURCE;
        for(i=0; i<num_procs; i++)
            ompi_pack(response, &pid, 1, OMPI_INT32);
        goto failure;
    }
	for ( i = 0; i < num_argv; i++){
	    ompi_unpack_string( request, &argv[i] );
	}
	argv[num_argv] = NULL;

    /* unpack environment */
	ompi_unpack( request, &num_env, 1, OMPI_INT32 );
	if ( NULL == ( *argv = malloc ( (num_env+1) * sizeof( char* ) ) ) ) {
	    status =  OMPI_ERR_OUT_OF_RESOURCE;
        for(i=0; i<num_procs; i++)
            ompi_pack(response, &pid, 1, OMPI_INT32);
        goto failure;
    }
	for ( i = 0; i < num_env; i++){
	    ompi_unpack_string( request, &env[i] );
	}
	env[num_env] = NULL;

	/* exec each process */
	for ( i=0; i < num_procs; i++ ) {

	    if ((pid = fork( )) < 0 ) {
	        /* Send back something to let the mpirun, etc
		       that a process failed.
		    */
	        status = OMPI_ERR_OUT_OF_RESOURCE;
	        ompi_pack( response, &pid, 1, OMPI_INT32 );
	    }

	    /* Child process */
	    else if ( pid == 0 ) {
	        /* Need to set up the file descriptors here. */
	    
	        /* This is the child go off and exec things */
	        execve (argv[0], argv, env );
            exit(-1);
	    }

	    /* Parent process */
	    else if ( pid > 0 ) {
	        /* pack childs pid */
	        ompi_pack( response, &pid, 1, OMPI_INT32 );
	    }
	}

	/* If nothing bad happened, status is OMPI_SUCCESS */
failure:
	ompi_pack( response, &status, 1, OMPI_INT32 );
	mca_oob_send_packed((ompi_process_name_t*)peer, response, MCA_OOB_TAG_EXEC, 0);
    ompi_buffer_free(response);
}


/**
 *  Process a kill command.
 */

static void mca_svc_exec_kill(
    ompi_process_name_t* peer,
    ompi_buffer_t request)
{

}


/**
 *  Process an OOB request.
 */

static void mca_svc_exec_recv(
    int status,
    ompi_process_name_t* peer,
    ompi_buffer_t request,
    int tag,
    void* cbdata)
{
    /* unpack message */
    int32_t command;
    ompi_unpack( request, &command, 1, OMPI_INT32 );

    switch (command){
    case OMPID_EXEC_CMD:
        mca_svc_exec_exec(peer, request);
        break;

    case OMPID_KILL_CMD:
        mca_svc_exec_kill(peer, request);
        break;
    }
}


/**
 * Register a callback to receive OOB requests.
 */

int mca_svc_exec_module_init(mca_svc_base_module_t* module)
{
    return mca_oob_recv_packed_nb(
        MCA_OOB_NAME_ANY,  
        MCA_OOB_TAG_EXEC, 
        MCA_OOB_ALLOC, 
        mca_svc_exec_recv,
        NULL);
}


/**
 *  Cleanup
 */

int mca_svc_exec_module_fini(mca_svc_base_module_t* module)
{
    return OMPI_SUCCESS;
}

