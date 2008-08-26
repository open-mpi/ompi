#include <stdio.h>
#include <signal.h>
#include <math.h>

#include "orte/util/proc_info.h"
#include "orte/util/name_fns.h"
#include "orte/runtime/orte_globals.h"
#include "orte/mca/errmgr/errmgr.h"

#include "orte/runtime/runtime.h"

#define MAX_COUNT 3

int
main(int argc, char *argv[]){
    int count;
    int msgsize;
    uint8_t *msg;
    int i, j, rc;
    double maxpower;
    unsigned char chr;
    bool readstdin;
    
    /*
     * Init
     */
    orte_init(ORTE_NON_TOOL);

    if (argc >= 2) {
        count = atoi(argv[1]);
        if (count < 0) {
            count = INT_MAX-1;
        }
    } else {
        count = MAX_COUNT;
    }
    
    if (argc == 3) {
        /* read from stdin */
        readstdin = true;
    } else {
        readstdin = false;
    }
    
    for (j=1; j < count+1; j++) {
        
        maxpower = (double)(j%7);
        msgsize = (int)pow(10.0, maxpower);
        msg = (uint8_t*)malloc(msgsize);

        chr = (j % 26) + 65;
        memset(msg, chr, msgsize);
        msg[msgsize-1] = '\n';
        
        if (0 == ORTE_PROC_MY_NAME->vpid) {
            if (readstdin) {
                msgsize = read(0, msg, msgsize);
            }
            write(1, msg, msgsize);
        } else {
            write(1, msg, msgsize);
        }
        
        free(msg);
    }

    orte_finalize();

    return 0;
}
