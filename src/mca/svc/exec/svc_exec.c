#include "ompi_config.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "svc_exec.h"

mca_svc_base_module_t mca_svc_exec_module = {
    mca_svc_exec_module_init,
    mca_svc_exec_module_fini
};


/**
 *  Process an OOB request.
 */

static void mca_svc_exec_recv(
    int status,
    ompi_process_name_t* peer,
    ompi_buffer_t buffer,
    int tag,
    void* cbdata)
{
    pid_t pid;
    int   i;
    int   base_pid;
    int   command;
    int   num_procs;
    int   num_argv;
    int   num_env;
    int   failure = OMPI_SUCCESS;

    char  **argv;
    char  **env;

    ompi_buffer_t  return_msg;
 
    /* unpack message */
    ompi_unpack( buffer, &command, 1, OMPI_INT32 );

    switch (command){
    case OMPID_EXEC_CMD:

        ompi_unpack( buffer, &num_procs, 1, OMPI_INT32 );

	ompi_unpack( buffer, &num_argv, 1, OMPI_INT32 );
	if ( NULL == ( *argv = malloc ( (num_argv+1) * sizeof( char* ) ) ) ) 
	    failure =  OMPI_ERR_OUT_OF_RESOURCE;

	for ( i = 0; i < num_argv; i++){
	    ompi_unpack_string( buffer, &argv[i] );
	}
	argv[num_argv] = NULL;

	ompi_unpack( buffer, &num_env, 1, OMPI_INT32 );
	if ( NULL == ( *argv = malloc ( (num_env+1) * sizeof( char* ) ) ) )
	    failure =  OMPI_ERR_OUT_OF_RESOURCE;

	for ( i = 0; i < num_env; i++){
	    ompi_unpack_string( buffer, &env[i] );
	}
	env[num_env] = NULL;

	ompi_unpack( buffer, &base_pid, 1, OMPI_INT32 );

	/* Create a buffer for the Return information:
	   child's pid, status.
	*/
	ompi_buffer_init( &return_msg, num_procs * sizeof(int) );
	    
	/* exec for all of the processes */
	for ( i=0; i < num_procs; i++ ){

	    if ( (pid = fork( )) < 0 ){
	        /* Send back something to let the mpirun, etc
		   that a process failed.
		*/
	        failure = OMPI_ERR_OUT_OF_RESOURCE;
	      ompi_pack( return_msg, &pid, 1, OMPI_INT32 );
	    }

	    /* Child process */
	    else if ( pid == 0 ){
	        /* Need to set up the file descriptors and locks here. */
	    
	      /* This is the child go off and exec things */
	      execve (argv[0], argv, env );
	    }

	    /* Parent process */
	    else if ( pid > 0 ) {
	        /* Save Child's Process ID */
	      ompi_pack( return_msg, &pid, 1, OMPI_INT32 );
	    }
	}

	/* If nothing bad happened, failure is OMPI_SUCCESS */
	ompi_pack ( return_msg, &failure, 1, OMPI_INT32 );

	mca_oob_send_packed( peer,
			     return_msg,
			     tag,
			     0);
        break;

    case OMPID_KILL_CMD:
        break;

    case OMPID_PING_CMD:
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

