/*
 * $HEADER$
 */

#include "ompi_config.h"
#include "include/constants.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "mca/pcmclient/pcmclient.h"
#include "mca/pcmclient/base/base.h"


int mca_oob_barrier(void)
{
    ompi_process_name_t* peers;
    ompi_process_name_t* self;
    size_t i, npeers;
    struct iovec iov;
    int foo = 0;

    int rc = mca_pcmclient.pcmclient_get_peers(&peers,&npeers);
    if(rc != OMPI_SUCCESS)
        return rc;

    self = mca_pcmclient.pcmclient_get_self();
    iov.iov_base = (void*)&foo;
    iov.iov_len = sizeof(foo);

    /* All non-root send & receive zero-length message. */
    if (self != peers) {
        int tag=-1;
        rc = mca_oob_send(&peers[0],&iov,1,tag,0);
        if (rc < 0) {
            return rc;
        }
 
        rc = mca_oob_recv(&peers[0],&iov,1,&tag,0);
        if (rc < 0) {
            return rc;
        }
    }

    /* The root collects and broadcasts the messages. */
    else {
        int tag=-1;
        for (i = 1; i < npeers; i++) {
            rc = mca_oob_recv(&peers[i],&iov,1,&tag,0);
            if (rc < 0) {
                return rc;
            }
        }

        for (i = 1; i < npeers; i++) {
            rc = mca_oob_send(&peers[i],&iov,1,tag,0);
            if (rc < 0) {
                return rc;
            }
        }
    }
    return OMPI_SUCCESS;
}


