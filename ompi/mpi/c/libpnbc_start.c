/*
 * Copyright (c)      2012 Oak Rigde National Laboratory. All rights reserved.
 * Copyright (c) 2015      Research Organization for Information Science
 *                         and Technology (RIST). All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */
#include "ompi_config.h"
#include <stdio.h>

#include "ompi/mpi/c/bindings.h"
#include "ompi/runtime/params.h"
#include "ompi/communicator/communicator.h"
#include "ompi/mca/pml/pml.h"
#include "ompi/errhandler/errhandler.h"
#include "ompi/datatype/ompi_datatype.h"
#include "ompi/memchecker.h"

#if OMPI_BUILD_MPI_PROFILING
#if OPAL_HAVE_WEAK_SYMBOLS
#pragma weak PNBC_Start = PMPI_PNBC_Start
#endif
#define PNBC_Start PMPI_PNBC_Start
#endif

static const char FUNC_NAME[] = "PNBC_Start";


int PNBC_Start(MPI_Comm comm,  MPI_Request *request) {

	int ret = OMPI_SUCCESS;
	int err;

	    MEMCHECKER(
	        memchecker_request(request);
	    );

	    if ( MPI_PARAM_CHECK ) {
	        int rc = MPI_SUCCESS;
	        OMPI_ERR_INIT_FINALIZE(FUNC_NAME);
	        if (request == NULL) {
	            rc = MPI_ERR_REQUEST;
	        }
	        OMPI_ERRHANDLER_CHECK(rc, MPI_COMM_WORLD, rc, FUNC_NAME);
	    }
	    /**
	     * Per definition of the handling of persistent request in the
	     * MPI standard 3.1 page 78 line 19: we must have the following
	     * sequence CREATE (START COMPLETE)* FREE. The upper level is
	     * responsible for handling any concurency. The PML must handle
	     * this case, as it is the only one knowing if the request can
	     * be reused or not (it is PML completed or not?).
	     */

	    switch((*request)->req_type) {
	    case OMPI_REQUEST_PML:
	    	//printf("start.c: req_type %s\n\n", (*request)->req_type);
	    	printf("start.c: case OMPI_REQUEST_PML entered.\n\n");
	        OPAL_CR_ENTER_LIBRARY();

	        ret = MCA_PML_CALL(start(1, request));

	        OPAL_CR_EXIT_LIBRARY();
	        return ret;

	    /* case OMPI_REQUEST_COLL:
	        printf("start.c: case OMPI_REQUEST_COLL entered.\n\n");
	    	OPAL_CR_ENTER_LIBRARY();

	        //ret = OMPI_SUCCESS;

	    	//ret = MCA_COLL_CALL(start(request));
	    	//ompi_coll_libpnbc_request_t *coll_request = (ompi_coll_libpnbc_request_t *) &request;

	    	//coll_request->comm->c_coll.coll_start(1, request);

	        OPAL_CR_EXIT_LIBRARY();
	        printf("start.c: case OMPI_REQUEST_COLL returning\n\n");
	        return ret; */

	    case OMPI_REQUEST_NOOP:
	        /**
	         * We deal with a MPI_PROC_NULL request. If the request is
	         * already active, fall back to the error case in the default.
	         * Otherwise, mark it active so we can correctly handle it in
	         * the wait*.
	         */
	    	printf("start.c: case OMPI_REQUEST_NOOP entered.\n\n");
	        if( OMPI_REQUEST_INACTIVE == (*request)->req_state ) {
	            (*request)->req_state = OMPI_REQUEST_ACTIVE;
	            return MPI_SUCCESS;
	        }

	    default:
	        //printf("firing OMPI_CR_ENTER_LIBRARY...\n\n");

	        OPAL_CR_ENTER_LIBRARY();

	        /* Invoke the coll component to perform the back-end operation */

	        //printf("firing libpnbc_start()...\n\n");
	        err = comm->c_coll->coll_libpnbc_start(request);
	        //printf("returning libpnbc_start()...\n\n");


	        OPAL_CR_EXIT_LIBRARY();

	        //return OMPI_ERRHANDLER_INVOKE(MPI_COMM_WORLD, MPI_ERR_REQUEST, FUNC_NAME);
	    }


    //OMPI_ERRHANDLER_RETURN(err, comm, err, FUNC_NAME);
}
