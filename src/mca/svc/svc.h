/*
 * $HEADER$
 */

#ifndef MCA_SVC_H
#define MCA_SVC_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/base/base.h"


/**************************************************************************
 * Service component
 **************************************************************************/

struct mca_svc_base_module_t;
typedef struct mca_svc_base_module_t *(*mca_svc_base_component_init_fn_t) (void);

/*
 * Structure for svc v1.0.0 components
 * Chained to MCA v1.0.0
 */
struct mca_svc_base_component_t {
    mca_base_component_t svc_version;
    mca_base_component_data_1_0_0_t svc_data;

    /* Initialization functions */
    mca_svc_base_component_init_fn_t svc_init;
};
typedef struct mca_svc_base_component_t mca_svc_base_component_t;


/**************************************************************************
 * Service module
 **************************************************************************/

typedef int (*mca_svc_base_module_init_fn_t) (struct mca_svc_base_module_t *self);
typedef int (*mca_svc_base_module_fini_fn_t) (struct mca_svc_base_module_t *self);

struct mca_svc_base_module_t {
    mca_svc_base_module_init_fn_t svc_init;
    mca_svc_base_module_fini_fn_t svc_fini;
};
typedef struct mca_svc_base_module_t mca_svc_base_module_t;

/*
 * svc svcs (i.e., components), and the initial events that
 * will trigger scheduling them to run:
 *
 * Service              FD events    incoming OOB  timer  signals
 * ------------------ -------------  ------------  -----  -------
 * kenyad                 X (pipe)        X                  X
 * i/o relaying           X               X
 * inter-daemon comms     X
 * inter-daemon hbeats    X                          X
 *  ^^ may be same as inter-daemon comms? (a la lamd)
 * name server                            X
 * registry                               X
 * pushing files                          X                       (future)
 * routing oob                            X                       (future)
 * incoming oob           X
 * ------------------ -------------  ------------  -----  -------
 *
 * --> per converation w/ brian, do we need a pcm in here?  e.g.,
       spawn daemons in rsh and bproc kinds of environments, so that
       one daemon on bproc headnode acts as a proxy pcm...?
   --> open question
 *
 * ***Assumption*** oob will be based on libevent, perhaps tcp or
 *    perhaps only local named socket...is this a safe assumption?
 *
 * Initialization sequence:
 *
 * - find all svc components
 * - call init() on all component - if non-NULL reply, keep, otherwise close/unloads
 * - call init() on all modules
 *   - post non-blocking oob receive with callback
 *   - setup a fd and register with libevent with callback
 *   - indicate that it wants to run after receiving a specific signal
 *     (not in signal context -- will specify a callback that will be
 *     invoked after return from signal context)
 *   - request to be put on the end of the to-be-run queue
 *   - request to run sometime after the end of a timeout
 *   --> currently no other mechanisms for progress
 * - post a non-blocking oob receive on a specific tag for shutdown
 *
 * The callback posted by the svc for the shutdown message will
 * simply set a global "time_to_shutdown" flag that will be noticed in
 * the main event loop.
 *
 * Main event loop:
 *
 * Mainly a single threaded blocking progress on libevent (hence, it's
 * blocking/asleep most of the time).  Main loop spins on calling the
 * blocking libevent progress, checking for signal interrupts and
 * calling the appropriate callbacks, and checking the global shutdown
 * flag.  Something like this:
 *
 * while (1) {
 *   // make progress on fd (including OOB) and timer events in here
 *   err = blocking_libevent_progress();
 *   // if one of the callbacks set the time_to_shutdown flag, then quit
 *   if (time_to_shutdown) {
 *     break;
 *   } 
 *   // if we were interrupted by a signal, invoke the callback that
 *   // was registered for it
 *   else if (err == interrupted_by_signal) {
 *     call_handler_for_signal();
 *   }
 *   }
 * }
 *
 */

/*
 * Macro for use in modules that are of type svc v1.0.0
 */
#define MCA_SVC_BASE_VERSION_1_0_0 \
  /* svc v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* svc v1.0 */ \
  "svc", 1, 0, 0

#endif                          /* MCA_SVC_H */
