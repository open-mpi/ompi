/*
 * $HEADER$
 */

#ifndef MCA_SERVICE_H
#define MCA_SERVICE_H

#include "ompi_config.h"

#include "mpi.h"
#include "mca/mca.h"
#include "mca/base/base.h"


/**************************************************************************
 * Service component
 **************************************************************************/

typedef const struct mca_service_module_1_0_0_t *(*mca_service_base_select_fn_t) (bool am_seed);

/*
 * Structure for service v1.0.0 components
 * Chained to MCA v1.0.0
 */
struct mca_service_base_component_1_0_0_t {
    mca_base_component_t dc_version;
    mca_base_component_data_1_0_0_t dc_data;

    /* Initialization / querying functions */

    mca_service_base_select_fn_t dc_select;
};
typedef struct mca_service_base_component_1_0_0_t mca_service_base_component_1_0_0_t;


/**************************************************************************
 * Service module
 **************************************************************************/

typedef int (*mca_service_base_init_fn_t) (mca_service_base_module_t *self);
typedef int (*mca_service_base_finalize_t) (mca_service_base_module_t *self);

typedef int (*mca_service_base_poll_t) (mca_service_base_module_t *self);

struct mca_service_base_module_1_0_0_t {

    /* Init / finalize */

    mca_service_base_init_fn_t dm_init;
    mca_service_base_finalize_fn_t dm_finalize;

    /* Polling */

    mca_service_base_poll_fn_t dm_poll;
};
typedef struct mca_service_module_1_0_0_t mca_service_module_1_0_0_t;

/*
 * service services (i.e., components), and the initial events that
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
 * - find all service components
 * - call select(); if non-NULL reply, keep, otherwise close/unloads
 * - call init() on all modules
 * - call poll() on all modules once.  poll() can do the following:
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
 * The callback posted by the service for the shutdown message will
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
 * do_init_stuff();
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
 *   // see if there's anything on the to-be-run queue
 *   foreach service (@to_be_run) {
 *     service->poll();
 *   }
 * }
 * do_finalize_stuff();
 *
 * open q: how to do oob sending from within the services?
 */

/*
 * Macro for use in modules that are of type service v1.0.0
 */
#define MCA_SERVICE_BASE_VERSION_1_0_0 \
  /* service v1.0 is chained to MCA v1.0 */ \
  MCA_BASE_VERSION_1_0_0, \
  /* service v1.0 */ \
  "service", 1, 0, 0

#endif                          /* MCA_SERVICE_H */
