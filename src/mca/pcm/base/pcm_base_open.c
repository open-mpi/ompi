/*
 * $HEADER$
 */

#include "ompi_config.h"

#include "mca/mca.h"
#include "mca/base/base.h"
#include "mca/pcm/pcm.h"
#include "mca/pcm/base/base.h"
#include "util/output.h"
#include "event/event.h"
#include <signal.h>

/*
 * The following file was created by configure.  It contains extern
 * statements and the definition of an array of pointers to each
 * component's public mca_base_component_t struct.
 */

#include "mca/pcm/base/static-components.h"


void
signal_cb(int fd, short event, void *arg)
{
    struct ompi_event *signal = arg;

    printf("%s: got signal %d\n", "signal_cb", OMPI_EVENT_SIGNAL(signal));
}


/*
 * Global variables
 */
int mca_pcm_base_output = 0;
ompi_list_t mca_pcm_base_components_available;


/**
 * Function for finding and opening either all MCA components, or the one
 * that was specifically requested via a MCA parameter.
 */
int mca_pcm_base_open(void)
{
    /*
     * XXX: BEGIN DEBUGGING CODE BWB NEEDS TO REMOVE 
     */
    struct ompi_event *signal_int;
    signal_int = malloc(sizeof(struct ompi_event));

    /* Initalize one event */
    ompi_event_set(signal_int, SIGHUP, OMPI_EV_SIGNAL|OMPI_EV_PERSIST, signal_cb,
                   signal_int);

    ompi_event_add(signal_int, NULL);
    /*
     * XXX: END DEBUGGING CODE BWB NEEDS TO REMOVE 
     */


  /* Open up all available components */

  if (OMPI_SUCCESS != 
      mca_base_components_open("pcm", 0, mca_pcm_base_static_components, 
                               &mca_pcm_base_components_available)) {
      ompi_output_verbose(5, mca_pcm_base_output, 
                          "pcm: error opening components");
    return OMPI_ERROR;
  }

  /* All done */

  return OMPI_SUCCESS;
}
