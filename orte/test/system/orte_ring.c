#include <stdio.h>
#include <signal.h>


#include "orte/orte_types.h"
#include "orte/mca/ns/ns.h"
#include "orte/mca/rml/rml.h"
#include "orte/mca/oob/base/base.h"

#include "orte/runtime/runtime.h"

#define MY_TAG 12345
#define MAX_COUNT 3

#            define false 0
#            define true 1

int
main(int argc, char *argv[]){
    int counter = 0;
    char * my_name = NULL;
    char * my_right_peer = NULL;
    char * my_left_peer  = NULL;
    orte_process_name_t right_peer_orte_name;
    orte_process_name_t left_peer_orte_name;
    int num_peers = 0;
    struct iovec msg;

    /*
     * Init
     */
    orte_init(ORTE_NON_INFRASTRUCTURE);

    num_peers = orte_process_info.num_procs;

    /*
     * Construct Peer name in a ring
     */
    right_peer_orte_name.jobid  = orte_process_info.my_name->jobid;
    right_peer_orte_name.vpid   = orte_process_info.my_name->vpid + 1;
    if( right_peer_orte_name.vpid >= num_peers ) {
        right_peer_orte_name.vpid = 0;
    }

    left_peer_orte_name.jobid  = orte_process_info.my_name->jobid;
    left_peer_orte_name.vpid   = orte_process_info.my_name->vpid - 1;
    if( orte_process_info.my_name->vpid == 0 ) {
        left_peer_orte_name.vpid = num_peers - 1;
    }

    /*
     * Get the string version
     */
    orte_ns.get_proc_name_string(&my_name, orte_process_info.my_name);
    orte_ns.get_proc_name_string(&my_right_peer, &right_peer_orte_name);
    orte_ns.get_proc_name_string(&my_left_peer,  &left_peer_orte_name);

    printf("My name is: %s -- PID %d\tMy Left Peer is %s\tMy Right Peer is %s\n", my_name, getpid(), my_left_peer, my_right_peer);

    /*
     * Rank 0 starts the ring...
     */
    if( orte_process_info.my_name->vpid == 0) {
        /* update value */
        counter = 1;

        /* Send to right */
        msg.iov_base = (void *) &counter;
        msg.iov_len  = sizeof(counter);

        printf("%s) Send Counter (%d) to peer (%s)\n", my_name,  counter, my_right_peer);
        if( 0 > orte_rml.send(&right_peer_orte_name,
                                             &msg,
                                             1,
                                             MY_TAG,
                                             0) ) {
            printf("error... %d\n", __LINE__);;
        }
    }


    while (counter <= MAX_COUNT ) {
        int *cnt;

        /* Receive from left */
        printf("%s) Waiting to Recv Counter from peer (%s)\n", my_name,  my_left_peer);
        msg.iov_base = NULL;
        msg.iov_len  = 0;

        if( 0 > orte_rml.recv(&left_peer_orte_name,
                              &msg,
                              1,
                              MY_TAG,
                              ORTE_RML_ALLOC) ) {
            printf("error A... %d\n", __LINE__);
        }
        
        cnt = (int *) msg.iov_base;
        counter = *cnt;

        /* Update */
        printf("%s) Recv %d ... Send %d\n", my_name, counter, counter + 1);
        if( orte_process_info.my_name->vpid == 0 ) {
            sleep(2);
        }
        if(orte_process_info.my_name->vpid == 0) {
            counter++;
        }
        
        if(counter > MAX_COUNT && right_peer_orte_name.vpid == 0) {
            break;
        }

        /* Send to right */
        msg.iov_base = (void *) &counter;
        msg.iov_len  = sizeof(counter);

        printf("%s) Send Counter (%d) to peer (%s)\n", my_name,  counter, my_right_peer);
        if( 0 > orte_rml.send(&right_peer_orte_name,
                                             &msg,
                                             1,
                                             MY_TAG,
                                             0) ) {
            printf("error B... %d\n", __LINE__);;
        }
    }

    orte_finalize();

    return 0;
}
