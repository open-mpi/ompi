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

/**
 * globals
 */

/*
 * functions
 */

mca_ns_base_cellid_t ns_base_create_cellid(void)
{
    ompi_ns_msg_buffer_t cmd, *answer;
    struct iovec msg;
    mca_ns_base_cellid_t cell;

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
    cell = (mca_ns_base_cellid_t)answer->buf;
    return cell;
}

mca_ns_base_jobid_t ns_base_create_jobid(void)
{
  /* JMS fill in here */
  return 0;
}


mca_ns_base_vpid_t ns_base_reserve_range(mca_ns_base_jobid_t job, mca_ns_base_vpid_t range)
{
  /* JMS fill in here */
  return 0;
}


int ns_base_free_name(ompi_process_name_t* name)
{
    return OMPI_SUCCESS;
}
