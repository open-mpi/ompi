/*
 * $HEADER$
 */

#include "ompi_config.h"
#include <unistd.h>
#include <stdio.h>
#include <stdlib.h>
#include <stdarg.h>
#include <string.h>
#include <sys/param.h>
#include <sys/fcntl.h>
#include <sys/types.h>
#include <sys/time.h>

#include <elan/elan.h>
#include <elan/capability.h>
#include <qsnet/fence.h>

#include <elan4/library.h>
#define USE_BASEINIT 0
#define USE_ELANINIT (1-USE_BASEINIT)
        
ELAN_LOCATION     location;
ELAN_CAPABILITY   Cap;
ELAN4_CTX        *ctx   = NULL;
ELAN4_SDRAM      *sdram = NULL;
ELAN4_ALLOC      *alloc = NULL;
ELAN4_COOKIEPOOL *cpool = NULL;
ELAN_BASE        *elan_base;
        
int		  i, self;
int               nproc;

int
main (int argc, char *argv[])
{
#if USE_BASEINIT
    if (!(elan_base = elan_baseInit(0))) {
	perror( "elan_baseInit() failed" );
	exit(1);
    }

    nproc = elan_base->state->nvp;
    self = elan_base->state->vp;
    elan_gsync(elan_base->allGroup);
#elif USE_ELANINIT
    elan_base  = (ELAN_BASE*) malloc(sizeof(ELAN_BASE));
    if (!(elan_base->state = elan_init(0))) {
	perror( "elan_init() failed" );
	exit(1);
    }

    for (i = 0; i < 1; i++) {
	ELAN_STATE     *elan_state;
	ELAN_CAPABILITY *cap;
	ELAN_LOCATION loc;
	
	elan_state = elan_base->state;
	ctx = elan_state->ctx;
       	cap = elan_state->cap;

	if (elan4_add_p2pvp (ctx, i, cap) < 0)
	    fprintf(stderr, "elan_init() failed" );
	elan4_block_inputter (ctx, 1);
	if (elan4_attach (ctx, cap))
	    fprintf(stderr, "elan4_attach() failed" );
    }

    nproc = elan_base->state->nvp;
    self = elan_base->state->vp;
#endif
    return 0;
}

