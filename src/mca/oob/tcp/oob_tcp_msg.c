#include "oob_tcp.h"
#include "oob_tcp_msg.h"


static void mca_oob_tcp_msg_construct(mca_oob_tcp_msg_t*);
static void mca_oob_tcp_msg_destruct(mca_oob_tcp_msg_t*);


OBJ_CLASS_INSTANCE(
    mca_oob_tcp_msg_t,
    ompi_list_item_t,
    mca_oob_tcp_msg_construct,
    mca_oob_tcp_msg_destruct);


static void mca_oob_tcp_msg_construct(mca_oob_tcp_msg_t* msg)
{

}


static void mca_oob_tcp_msg_destruct(mca_oob_tcp_msg_t* msg)
{

}


/**
 *  Wait for a msg to complete.
 *  @param  msg (IN)   Message to wait on.
 *  @param  size (OUT) Number of bytes delivered.
 *  @retval OMPI_SUCCESS or error code on failure.
 */

int mca_oob_tcp_msg_wait(mca_oob_tcp_msg_t* msg, int* size)
{
    return OMPI_SUCCESS;
}

/**
 *  Signal that a message has completed.
 *  @param  msg (IN)   Message to wait on.
 *  @retval OMPI_SUCCESS or error code on failure.
 */

int mca_oob_tcp_msg_complete(mca_oob_tcp_msg_t* msg)
{
    return OMPI_SUCCESS;
}

