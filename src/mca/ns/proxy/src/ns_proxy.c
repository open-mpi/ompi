/*
 * $HEADER$
 */
/** @file:
 *
 */

#include "ompi_config.h"
#include "mca/mca.h"
#include "mca/oob/base/base.h"
#include "mca/ns/base/base.h"
#include "ns_proxy.h"

/**
 * globals
 */

/*
 * functions
 */

ompi_process_id_t ns_proxy_create_cellid(void)
{
    ompi_ns_msg_buffer_t cmd, *answer;
    struct iovec msg;
    ompi_process_id_t cell;

    cmd.command = OMPI_NS_CREATE_CELLID;
    cmd.buflen = 0;
    cmd.buf = NULL;

    msg.iov_base = (char*)&cmd;
    msg.iov_len = sizeof(cmd);

    if (0 > mca_oob_send(&mca_ns_my_replica, &msg, 1, 0)) { /* error on send */
	    return 0;
	}

    if (0 > mca_oob_recv(&mca_ns_my_replica, &msg, 1, 0)) { /* error on recv */
	    return 0;
	}

    answer = (ompi_ns_msg_buffer_t*)msg.iov_base;
    cell = (ompi_process_id_t)answer->buf;
    return cell;
}

ompi_process_id_t ns_proxy_create_jobid(void)
{
  /* JMS fill in here */
  return 0;
}


ompi_process_id_t ns_proxy_reserve_range(ompi_process_id_t job, ompi_process_id_t range)
{
  /* JMS fill in here */
  return 0;
}


int ns_proxy_free_name(ompi_process_name_t* name)
{
    return OMPI_SUCCESS;
}
