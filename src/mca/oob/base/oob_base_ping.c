/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <string.h>
#include <sys/types.h>
#include <netinet/in.h>

#include "include/constants.h"
#include "mca/oob/oob.h"
#include "mca/oob/base/base.h"
#include "util/output.h"


int mca_oob_ping(ompi_process_name_t* peer, struct timeval* tv)
{
    return(mca_oob.oob_ping(peer, tv));
}

