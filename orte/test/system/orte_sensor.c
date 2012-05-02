/* -*- C -*-
 *
 * $HEADER$
 *
 */
#include <stdio.h>
#include <unistd.h>

#include "opal/util/output.h"
#include "opal/mca/event/event.h"

#include "orte/runtime/orte_globals.h"
#include "orte/runtime/runtime.h"
#include "orte/mca/sensor/sensor.h"

int main(int argc, char* argv[])
{
    int rc;
    char hostname[512];
    pid_t pid;
    
    if (0 > (rc = orte_init(&argc, &argv, ORTE_PROC_NON_MPI))) {
        fprintf(stderr, "orte_mcast: couldn't init orte - error code %d\n", rc);
        return rc;
    }
    
    gethostname(hostname, 512);
    pid = getpid();
    
    printf("orte_sensor: Node %s Name %s Pid %ld\n",
           hostname, ORTE_NAME_PRINT(ORTE_PROC_MY_NAME), (long)pid);
    
    /* open and select the sensor modules */
    orte_sensor_base_open();
    orte_sensor_base_select();

    /* start the sensors - note that we cannot monitor other
     * jobs as we are an application. So pass the invalid
     * jobid so the sensor modules can know
     */
    orte_sensor.start(ORTE_JOBID_INVALID);

    /* just sit here, letting the sensors run */
    while (orte_event_base_active) {
        opal_event_loop(orte_event_base, OPAL_EVLOOP_ONCE);
    }
    
    orte_finalize();
    return 0;
}
