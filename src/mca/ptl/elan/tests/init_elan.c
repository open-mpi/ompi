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

#define USE_BASEINIT 1

int		   self;
int                nproc;

int
main (int argc, char *argv[])
{
#if USE_BASEINIT
    ELAN_BASE         *elan_base;
    if (!(elan_base = elan_baseInit(0))) {
	perror( "elan_baseInit() failed" );
	exit(1);
    }

    nproc = elan_base->state->nvp;
    self = elan_base->state->vp;
    elan_gsync(elan_base->allGroup);
#else

#endif
    return 0;
}

