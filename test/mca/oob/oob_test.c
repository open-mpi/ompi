/*
 * Basic test for the oob.
 * The idea is to try most combinations of sending and recieving
 * to run:
 * mpirun -np 2 -- oob_test
 */


#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include "support.h"
#include "mca/oob/oob.h"

#define MSG_TYPE_1   1
#define MSG_TYPE_2   2
#define NUM_TESTS    8
#define NUM_TIMES    1

int i;
bool testdone[NUM_TESTS * NUM_TIMES];
void do_sends(ompi_process_name_t * peer);
void do_recvs(ompi_process_name_t * peer);

bool compare_iovec(const struct iovec * msg1, const struct iovec * msg2, int n);

bool compare_iovec(const struct iovec * msg1, const struct iovec * msg2,
                   int n) {
    int i;
    for(i = 0; i < n; i++) {
        if(msg1[i].iov_len != msg2[i].iov_len) {
            return false;
        }
        if(0 != memcmp(msg1[i].iov_base, msg2[i].iov_base, msg1[i].iov_len)) {
            return false;
        }
    }
    return true;
}
    

void callback(int status, const ompi_process_name_t * peer,
              const struct iovec * msg, int count, int tag, void * cbdata);

void callback(int status, const ompi_process_name_t * peer,
              const struct iovec * msg, int count, int tag, void * cbdata)
{
    if(0 != tag) {
        test_failure("Bad tag.");
    }
    if(((int) cbdata) >= NUM_TESTS * NUM_TIMES) {
        test_failure("Bad value in callback function.");
    } else if (testdone[(int) cbdata]) {
        test_failure("Callback function called on an already completed test.");
    } else {
        testdone[(int) cbdata] = true;
        test_success();
    }
}

/* data */
/* setup message */
uint32_t msg_type_1 = MSG_TYPE_1;
uint32_t msg_type_2 = MSG_TYPE_2;
char send1[] = "hello";
uint32_t send2[] = {3, 5, 5, 9, 20};
uint16_t send3[] = {32, 4, 23};

/* now we set up the send iovect */
struct iovec send_msg1[4] = {{(void *) &msg_type_1, sizeof(msg_type_1)},
                             {(void *) &send1, sizeof(send1)},
                             {(void *) &send2, sizeof(send2)},
                             {(void *) &send3, sizeof(send3)}};

struct iovec send_msg2[3] = {{(void *) &msg_type_2, sizeof(msg_type_2)},
                             {(void *) &send2, sizeof(send2)},
                             {(void *) &send3, sizeof(send3)}};

/* if we want the send/ recieve functions to do the packing for us,
 * we have to provide an array that describes our data types
 */
mca_oob_base_type_t types[] = {MCA_OOB_BASE_INT32, MCA_OOB_BASE_BYTE,
MCA_OOB_BASE_INT32, MCA_OOB_BASE_INT16};

/* we'll pass the array an identical iovec */
uint32_t msg_type;
char     recv1[6];
uint32_t recv2[5];
uint16_t recv3[3];
struct iovec recv_msg1[4] = {{(void *) &msg_type, sizeof(msg_type)},
                             {(void *) &recv1, sizeof(recv1)},
                             {(void *) &recv2, sizeof(recv2)},
                             {(void *) &recv3, sizeof(recv3)}};

struct iovec recv_msg2[3] = {{(void *) &msg_type, sizeof(msg_type)},
                             {(void *) &recv2, sizeof(recv2)},
                             {(void *) &recv3, sizeof(recv3)}};


int main(int argc, char ** argv)
{
    ompi_process_name_t  peer;
    bool all_complete = false;
    int n;
    MPI_Init(&argc, &argv);
   
    for(i = 0; i < NUM_TESTS * NUM_TIMES; i++) {
       testdone[i] = false;
    } 
    /* setup peer address */
    peer = mca_oob_name_self;
    fprintf(stderr, "my vpid %d my jobid %d my cellid %d my pid %d\n",  
            peer.vpid, peer.jobid, peer.cellid, getpid());

    if(peer.vpid == 1) {
        test_init("oob send then recieve");
        /* local vpid is 1 - peer is 0 */
        peer.vpid = 0;
        for(i = 0; i < NUM_TIMES; i++) {
            do_sends(&peer);
            do_recvs(&peer);
        }
    } else {
        test_init("oob recieve then send");
        /* local vpid is 0 - peer is 1 */
        peer.vpid = 1;
        for(i = 0; i < NUM_TIMES; i++) {
            do_recvs(&peer);
            do_sends(&peer);
        }

    }
    /* now we want to make sure all the tests have completed */
    /* done */
    n = 0;
    while(!all_complete && n < 10) {
        all_complete = true;
        for(i = 0; i < NUM_TESTS * NUM_TIMES; i++) {
            if(!testdone[i]) {
                all_complete = false;
            }
        }
        if(!all_complete) {
            sleep(1);
        }
        n++;
    }
    if(!all_complete) {
        test_failure("not all sends or recieves were completed");
    }
    test_finalize();
    /* this is to give the oob time to finish all sends */
    MPI_Finalize();
    return 0;
}
 
void do_sends(ompi_process_name_t * peer) {
    /* non blocking send without doing any packing */
    if( 0 > mca_oob_send_nb(peer, send_msg1, 4, 0, 0, &callback, 
                            (void *) (0 +  (NUM_TESTS * i)))) {
        test_failure("mca_oob_send_nb.");
    } else {
        test_success();
    }
    if( 0 > mca_oob_send_nb(peer, send_msg1, 4, 0, 0, &callback, 
                            (void *) (1 +  (NUM_TESTS * i)))) {
        test_failure("mca_oob_send_nb.");
    } else {
        test_success();
    }

    /* nonblocking send with packing */
    if( 0 > mca_oob_send_hton_nb(peer, send_msg1, types, 4, 0, 0, &callback,
                                 (void *) (2 + (NUM_TESTS * i)))) {
        test_failure("mca_oob_send_hton_nb.");
    } else {
        test_success();
    }
    if( 0 > mca_oob_send_hton_nb(peer, send_msg1, types, 4, 0, 0, &callback,
                                 (void *) (3 + (NUM_TESTS * i)))) {
        test_failure("mca_oob_send_hton_nb.");
    } else {
        test_success();
    }

    /* blocking send  */
    if( 0 > mca_oob_send(peer, send_msg2, 3, 0, 0)) {
        test_failure("mca_oob_send.");
    } else {
        test_success();
    }
    if( 0 > mca_oob_send(peer, send_msg2, 3, 0, 0)) {
        test_failure("mca_oob_send.");
    } else {
        test_success();
    }

    /* blocking send with packing */
    if( 0 > mca_oob_send_hton(peer, send_msg1, types, 4, 0, 0)) {
        test_failure("mca_oob_send_hton.");
    } else {
        test_success();
    }
    if( 0 > mca_oob_send_hton(peer, send_msg1, types, 4, 0, 0)) {
        test_failure("mca_oob_send_hton.");
    } else {
        test_success();
    }
}

void do_recvs(ompi_process_name_t * peer) {
    /*first, we'll recieve the nonpacked send - assuming we know the
     *  message type */
    if( 0 > mca_oob_recv_nb(peer, recv_msg1, 4, 0, 0, &callback, 
                            (void *) (4 +  (NUM_TESTS * i)))) {
        test_failure("mca_oob_recv_nb.");
    } else {
        test_success();
    }
    if( 0 > mca_oob_recv(peer, recv_msg1, 4, 0, 0)) {
        test_failure("mca_oob_recv.");
    } else {
        test_success();
    }
    if(!compare_iovec(recv_msg1, send_msg1, 4)) {
        test_failure("compare  1 is wrong");
    }
    /* now we'll recieve the packed send  - assuming we know the message type */
    if( 0 > mca_oob_recv_ntoh(peer, recv_msg1, types, 4, 0,0)) {
        test_failure("mca_oob_recv_ntoh.");
    } else {
        test_success();
    }
    if(!compare_iovec(recv_msg1, send_msg1, 4)) {
        test_failure("compare 2 is wrong");
    }
    if( 0 > mca_oob_recv_ntoh_nb(peer, recv_msg1, types, 4, 0, 0, &callback, 
                                 (void *) (5 + (NUM_TESTS * i)))) {
        test_failure("mca_oob_recv_ntoh_nb.");
    } else {
        test_success();
    }

    /* now we'll do a blocking recv - waiting for the 3rd message to arrive 
     * - and peek the first element of the iovec array to determine 
     *   the message type.  */
    if( 0 > mca_oob_recv(peer, recv_msg2, 1, 0, MCA_OOB_PEEK)) {
        test_failure("mca_oob_recv w/peek.");
    } else {
        test_success();
    }
    /* check the type of message - before doing the actual receive */
    switch(msg_type) {
        case MSG_TYPE_1:
            if( 0 > mca_oob_recv(peer, recv_msg1, 4, 0, 0)) {
                test_failure("mca_oob_recv of peeked message.");
            } else {
                test_success();
            }
            if(!compare_iovec(recv_msg1, send_msg1, 4)) {
                test_failure("compare 3 is wrong");
            }
            break;
        case MSG_TYPE_2:
            if( 0 > mca_oob_recv(peer, recv_msg2, 3, 0, 0)) {
                test_failure("mca_oob_recv of peeked message.");
            } else {
                test_success();
            }
            if(!compare_iovec(recv_msg2, send_msg2, 3)) {
                test_failure("compare 4 is wrong");
            }
            break;
        default:
            test_failure("Message peek did not return a valid type number.");
            break;
    }
    if( 0 > mca_oob_recv_nb(peer, recv_msg2, 3, 0, 0, &callback, 
                            (void *) (6 + (NUM_TESTS * i)))) {
        test_failure("mca_oob_recv_nb.");
    } else {
        test_success();
    }

    /* now we will recieve the packed data */
    if( 0 > mca_oob_recv_ntoh(peer, recv_msg1, types, 4, 0, 0)) {
        test_failure("mca_oob_recv_ntoh.");
    } else {
        test_success();
    }
    if(!compare_iovec(recv_msg1, send_msg1, 4)) {
        test_failure("compare 5 is wrong");
    }
    if( 0 > mca_oob_recv_ntoh_nb(peer, recv_msg1, types, 4, 0, 0, &callback, 
                                 (void *) (7 + (NUM_TESTS * i)))) {
        test_failure("mca_oob_recv_ntoh_nb.");
    } else {
        test_success();
    }
}

