/* -*- Mode: C; c-basic-offset:4 ; indent-tabs-mode:nil -*- */
/*
 * Copyright (c) 2004-2006 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2010 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2006 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2006      QLogic Corporation. All rights reserved.
 * Copyright (c) 2013-2015 Intel, Inc. All rights reserved
 * Copyright (c) 2014      Los Alamos National Security, LLC. All rights
 *                         reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "ompi_config.h"

#include "opal/mca/pmix/pmix.h"
#include "ompi/mca/mtl/mtl.h"
#include "ompi/mca/mtl/base/mtl_base_datatype.h"
#include "opal/util/show_help.h"
#include "ompi/proc/proc.h"

#include "mtl_psm2.h"
#include "mtl_psm2_types.h"
#include "mtl_psm2_endpoint.h"
#include "mtl_psm2_request.h"

mca_mtl_psm2_module_t ompi_mtl_psm2 = {
    .super = {
        /* NTH: PSM supports 16 bit context ids */
        .mtl_max_contextid = (1UL << 16) - 1,
        .mtl_max_tag = (1UL << 30),  /* must allow negatives */

        .mtl_add_procs = ompi_mtl_psm2_add_procs,
        .mtl_del_procs = ompi_mtl_psm2_del_procs,
        .mtl_finalize = ompi_mtl_psm2_finalize,

        .mtl_send = ompi_mtl_psm2_send,
        .mtl_isend = ompi_mtl_psm2_isend,

        .mtl_irecv = ompi_mtl_psm2_irecv,
        .mtl_iprobe = ompi_mtl_psm2_iprobe,
        .mtl_imrecv = ompi_mtl_psm2_imrecv,
        .mtl_improbe = ompi_mtl_psm2_improbe,

        .mtl_cancel = ompi_mtl_psm2_cancel,
        .mtl_add_comm = ompi_mtl_psm2_add_comm,
        .mtl_del_comm = ompi_mtl_psm2_del_comm
    }
};

static
psm_error_t
ompi_mtl_psm2_errhandler(psm_ep_t ep, const psm_error_t error,
			const char *error_string, psm_error_token_t token)
{
    switch (error) {
	/* We don't want PSM to default to exiting when the following errors occur */
	case PSM_EP_DEVICE_FAILURE:
	case PSM_EP_NO_DEVICE:
	case PSM_EP_NO_PORTS_AVAIL:
	case PSM_EP_NO_NETWORK:
	case PSM_EP_INVALID_UUID_KEY:
	  opal_show_help("help-mtl-psm.txt",
			 "unable to open endpoint", true,
			 psm_error_get_string(error));
	    break;

	/* We can't handle any other errors than the ones above */
	default:
	    opal_output(0, "Open MPI detected an unexpected PSM error in opening "
			"an endpoint: %s\n", error_string);
	    return psm_error_defer(token);
	    break;
    }
    return error;
}

int ompi_mtl_psm2_progress( void );

int ompi_mtl_psm2_module_init(int local_rank, int num_local_procs) {
    psm_error_t err;
    psm_ep_t	ep; /* endpoint handle */
    psm_mq_t	mq;
    psm_epid_t	epid; /* unique lid+port identifier */
    psm_uuid_t  unique_job_key;
    struct psm_ep_open_opts ep_opt;
    unsigned long long *uu = (unsigned long long *) unique_job_key;
    char *generated_key;
    char env_string[256];
    int rc;

    generated_key = getenv("OMPI_MCA_orte_precondition_transports");
    memset(uu, 0, sizeof(psm_uuid_t));

    if (!generated_key || (strlen(generated_key) != 33) ||
        sscanf(generated_key, "%016llx-%016llx", &uu[0], &uu[1]) != 2)
    {
      opal_show_help("help-mtl-psm.txt",
		     "no uuid present", true,
		     generated_key ? "could not be parsed from" :
		     "not present in", ompi_process_info.nodename);
      return OMPI_ERROR;

    }

    /* Handle our own errors for opening endpoints */
    psm_error_register_handler(ompi_mtl_psm2.ep, ompi_mtl_psm2_errhandler);

    /* Setup MPI_LOCALRANKID and MPI_LOCALNRANKS so PSM can allocate hardware
     * contexts correctly.
     */
    snprintf(env_string, sizeof(env_string), "%d", local_rank);
    setenv("MPI_LOCALRANKID", env_string, 0);
    snprintf(env_string, sizeof(env_string), "%d", num_local_procs);
    setenv("MPI_LOCALNRANKS", env_string, 0);

    /* Setup the endpoint options. */
    psm_ep_open_opts_get_defaults(&ep_opt);
    ep_opt.timeout = ompi_mtl_psm2.connect_timeout * 1e9;
    ep_opt.affinity = PSM_EP_OPEN_AFFINITY_SKIP; /* do not let PSM set affinity */

    /* Open PSM endpoint */
    err = psm_ep_open(unique_job_key, &ep_opt, &ep, &epid);
    if (err) {
      opal_show_help("help-mtl-psm.txt",
		     "unable to open endpoint", true,
		     psm_error_get_string(err));
      return OMPI_ERROR;
    }

    /* Future errors are handled by the default error handler */
    psm_error_register_handler(ompi_mtl_psm2.ep, PSM_ERRHANDLER_DEFAULT);

    err = psm_mq_init(ep,
		      0xffff000000000000ULL,
		      NULL,
		      0,
		      &mq);
    if (err) {
      opal_show_help("help-mtl-psm.txt",
		     "psm init", true,
		     psm_error_get_string(err));
      return OMPI_ERROR;
    }

    ompi_mtl_psm2.ep   = ep;
    ompi_mtl_psm2.epid = epid;
    ompi_mtl_psm2.mq   = mq;

    OPAL_MODEX_SEND(rc, OPAL_PMIX_GLOBAL,
                    &mca_mtl_psm2_component.super.mtl_version,
                    &ompi_mtl_psm2.epid,
                    sizeof(psm_epid_t));

    if (OMPI_SUCCESS != rc) {
	opal_output(0, "Open MPI couldn't send PSM2 epid to head node process");
	return OMPI_ERROR;
    }


    /* register the psm progress function */
    opal_progress_register(ompi_mtl_psm2_progress);

    return OMPI_SUCCESS;
}

int
ompi_mtl_psm2_finalize(struct mca_mtl_base_module_t* mtl) {
    psm_error_t err;

    opal_progress_unregister(ompi_mtl_psm2_progress);

    /* free resources */
    err = psm_mq_finalize(ompi_mtl_psm2.mq);
    if (err) {
        opal_output(0, "Error in psm_mq_finalize (error %s)\n",
		    psm_error_get_string(err));
        return OMPI_ERROR;
    }

    err = psm_ep_close(ompi_mtl_psm2.ep, PSM_EP_CLOSE_GRACEFUL, 1*1e9);
    if (err) {
        opal_output(0, "Error in psm_ep_close (error %s)\n",
		    psm_error_get_string(err));
        return OMPI_ERROR;
    }

    err = psm_finalize();
    if (err) {
        opal_output(0, "Error in psm_finalize (error %s)\n",
		    psm_error_get_string(err));
        return OMPI_ERROR;
    }

    return OMPI_SUCCESS;
}

static
const char *
ompi_mtl_psm2_connect_error_msg(psm_error_t err)
{
    switch (err) { /* See if we expect the error */
	case PSM_EPID_UNREACHABLE:
	case PSM_EPID_INVALID_NODE:
	case PSM_EPID_INVALID_MTU:
	case PSM_EPID_INVALID_UUID_KEY:
	case PSM_EPID_INVALID_VERSION:
	case PSM_EPID_INVALID_CONNECT:
	    return psm_error_get_string(err);
	    break;
	case PSM_EPID_UNKNOWN:
	    return "Connect status could not be determined "
		   "because of other errors";
	default:
	    return NULL;
    }
}

#ifndef min
#  define min(a,b) ((a) < (b) ? (a) : (b))
#endif

#ifndef max
#  define max(a,b) ((a) > (b) ? (a) : (b))
#endif

int
ompi_mtl_psm2_add_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs)
{
    int i,j;
    int rc;
    psm_epid_t   *epids_in = NULL;
    int *mask_in = NULL;
    psm_epid_t	 *epid;
    psm_epaddr_t *epaddrs_out = NULL;
    psm_error_t  *errs_out = NULL, err;
    size_t size;
    int proc_errors[PSM_ERROR_LAST] = { 0 };
    int timeout_in_secs;

    assert(mtl == &ompi_mtl_psm2.super);
    rc = OMPI_ERR_OUT_OF_RESOURCE;

    errs_out = (psm_error_t *) malloc(nprocs * sizeof(psm_error_t));
    if (errs_out == NULL) {
	goto bail;
    }
    epids_in = (psm_epid_t *) malloc(nprocs * sizeof(psm_epid_t));
    if (epids_in == NULL) {
	goto bail;
    }
    mask_in = (int *) malloc(nprocs * sizeof(int));
    if (mask_in == NULL) {
	goto bail;
    }
    epaddrs_out = (psm_epaddr_t *) malloc(nprocs * sizeof(psm_epaddr_t));
    if (epaddrs_out == NULL) {
	goto bail;
    }
    rc = OMPI_SUCCESS;

    /* Get the epids for all the processes from modex */
    for (i = 0; i < (int) nprocs; i++) {
        if (NULL != procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL]) {
            /* Already connected: don't connect again */
            mask_in[i] = 0;
            continue;
        }

        OPAL_MODEX_RECV(rc, &mca_mtl_psm2_component.super.mtl_version,
                        &procs[i]->super.proc_name, (void**)&epid, &size);
	if (rc != OMPI_SUCCESS || size != sizeof(psm_epid_t)) {
	  return OMPI_ERROR;
	}
	epids_in[i] = *epid;
	mask_in[i] = 1;
    }

    timeout_in_secs = max(ompi_mtl_psm2.connect_timeout, 0.5 * nprocs);

    psm_error_register_handler(ompi_mtl_psm2.ep, PSM_ERRHANDLER_NOP);

    err = psm_ep_connect(ompi_mtl_psm2.ep,
			 nprocs,
			 epids_in,
			 mask_in,
			 errs_out,
			 epaddrs_out,
			 timeout_in_secs * 1e9);
    if (err) {
	char *errstr = (char *) ompi_mtl_psm2_connect_error_msg(err);
	if (errstr == NULL) {
	    opal_output(0, "PSM returned unhandled/unknown connect error: %s\n",
			psm_error_get_string(err));
	}
	for (i = 0; i < (int) nprocs; i++) {
            if (0 == mask_in[i]) {
                    continue;
            }

	    psm_error_t thiserr = errs_out[i];
	    errstr = (char *) ompi_mtl_psm2_connect_error_msg(thiserr);
	    if (proc_errors[thiserr] == 0) {
		proc_errors[thiserr] = 1;
		opal_output(0, "PSM EP connect error (%s):",
			    errstr ? errstr : "unknown connect error");
		for (j = 0; j < (int) nprocs; j++) {
		  if (errs_out[j] == thiserr) {
                      opal_output(0, " %s", (NULL == procs[j]->super.proc_hostname) ?
                                  "unknown" : procs[j]->super.proc_hostname);
		  }
		}
		opal_output(0, "\n");
	    }
	}

	rc = OMPI_ERROR;
    }
    else {
	/* Default error handling is enabled, errors will not be returned to
	 * user.  PSM prints the error and the offending endpoint's hostname
	 * and exits with -1 */
	psm_error_register_handler(ompi_mtl_psm2.ep, PSM_ERRHANDLER_DEFAULT);

	/* Fill in endpoint data */
	for (i = 0; i < (int) nprocs; i++) {
            if (0 == mask_in[i]) {
                    continue;
            }

            mca_mtl_psm2_endpoint_t *endpoint =
		(mca_mtl_psm2_endpoint_t *) OBJ_NEW(mca_mtl_psm2_endpoint_t);
	    endpoint->peer_epid = epids_in[i];
	    endpoint->peer_addr = epaddrs_out[i];
            procs[i]->proc_endpoints[OMPI_PROC_ENDPOINT_TAG_MTL] = endpoint;
	}

	rc = OMPI_SUCCESS;
    }

bail:
    if (epids_in != NULL) {
	free(epids_in);
    }
    if (mask_in != NULL) {
        free(mask_in);
    }
    if (errs_out != NULL) {
	free(errs_out);
    }
    if (epaddrs_out != NULL) {
	free(epaddrs_out);
    }

    return rc;
}

int
ompi_mtl_psm2_del_procs(struct mca_mtl_base_module_t *mtl,
                      size_t nprocs,
                      struct ompi_proc_t** procs)
{
    return OMPI_SUCCESS;
}


int
ompi_mtl_psm2_add_comm(struct mca_mtl_base_module_t *mtl,
                      struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}


int
ompi_mtl_psm2_del_comm(struct mca_mtl_base_module_t *mtl,
                      struct ompi_communicator_t *comm)
{
    return OMPI_SUCCESS;
}


int ompi_mtl_psm2_progress( void ) {
    psm_error_t err;
    mca_mtl_psm2_request_t* mtl_psm2_request;
    psm_mq_status2_t psm_status;
    psm_mq_req_t req;
    int completed = 1;

    do {
        err = psm_mq_ipeek2(ompi_mtl_psm2.mq, &req, NULL);
	if (err == PSM_MQ_INCOMPLETE) {
	    return completed;
	} else if (err != PSM_OK) {
	    goto error;
	}

	completed++;

	err = psm_mq_test2(&req, &psm_status);
	if (err != PSM_OK) {
	    goto error;
	}

        mtl_psm2_request = (mca_mtl_psm2_request_t*) psm_status.context;

	if (mtl_psm2_request->type == OMPI_mtl_psm2_IRECV) {

        mtl_psm2_request->super.ompi_req->req_status.MPI_SOURCE =
            psm_status.msg_tag.tag2;
	    mtl_psm2_request->super.ompi_req->req_status.MPI_TAG =
		    psm_status.msg_tag.tag1;
            mtl_psm2_request->super.ompi_req->req_status._ucount =
                psm_status.nbytes;

            ompi_mtl_datatype_unpack(mtl_psm2_request->convertor,
                                     mtl_psm2_request->buf,
                                     psm_status.msg_length);
	}

	if(mtl_psm2_request->type == OMPI_mtl_psm2_ISEND) {
	  if (mtl_psm2_request->free_after) {
	    free(mtl_psm2_request->buf);
	  }
	}

	switch (psm_status.error_code) {
	    case PSM_OK:
		mtl_psm2_request->super.ompi_req->req_status.MPI_ERROR =
		    OMPI_SUCCESS;
		break;
	    case PSM_MQ_TRUNCATION:
		mtl_psm2_request->super.ompi_req->req_status.MPI_ERROR =
		    MPI_ERR_TRUNCATE;
		break;
	    default:
		mtl_psm2_request->super.ompi_req->req_status.MPI_ERROR =
                        MPI_ERR_INTERN;
	}

	mtl_psm2_request->super.completion_callback(&mtl_psm2_request->super);

    }
    while (1);

 error:
    opal_show_help("help-mtl-psm.txt",
		   "error polling network", true,
		   psm_error_get_string(err));
    return 1;
}
