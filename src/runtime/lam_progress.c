#include "event/event.h"
#include "mca/pml/pml.h"
#include "runtime/lam_progress.h"


void lam_progress(void)
{
    lam_event_loop(LAM_EVLOOP_ONCE);
}

