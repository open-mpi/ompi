/* -*- C -*-
 * 
 * $HEADER$
 *
 */

#include "ompi_config.h"

#include "mca/pcm/pcm.h"
#include "include/types.h"

#include <sys/types.h>

#ifndef MCA_PCM_RSH_INTERNAL_H_
#define MCA_PCM_RSH_INTERNAL_H_


#ifdef __cplusplus
extern "C" {
#endif

static inline int get_first_child(int degree, int parent)
{
    return degree * parent + 1;
}


static inline int get_last_child(int degree, int parent)
{
    return degree * parent + degree;
}


int pcm_rsh_ioexecvp(char **cmdv, int showout, char *outbuff, 
		     int outbuffsize, int stderr_is_err, 
		     int sockfd, char *outmap, char *env,
		     int *num_launched);

#ifdef __cplusplus
}
#endif


#endif /* MCA_PCM_RSH_INTERNAL_H_ */
