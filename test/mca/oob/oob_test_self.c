/**
 * In this test we try using oob to send messages to the same process.
 * to run: mpirun -np 1 -- oob_test_self
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
#define NUM_TESTS    5


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

bool testdone[NUM_TESTS];

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
    fprintf(stderr, "caqllback called on num %d.\n", (int) cbdata);
    if(0 != tag) {
        test_failure("Bad tag.");
    }
    if((int) cbdata >= NUM_TESTS) {
        test_failure("Bad value in callback function.");
    } else if (testdone[(int) cbdata]) {
        test_failure("Callback function called on an already completed test.");
    } else {
        testdone[(int) cbdata] = true;
        test_success();
    }
}

int main(int argc, char ** argv)
{
    ompi_process_name_t  peer;

    MPI_Init(&argc, &argv);

    /* setup peer address */
    peer = mca_oob_name_self;
    fprintf(stderr, "my vpid %d my jobid %d my cellid %d my pid %d\n",  
            peer.vpid, peer.jobid, peer.cellid, getpid());

    test_init("oob self");
    /* do a non blocking send without packing followed by a
     * non blocking recieve */
    if( 0 > mca_oob_send_nb(&peer, send_msg1, 4, 0, 0, &callback, (void *) 0)){
        test_failure("mca_oob_send_nb.");
    } else {
        test_success();
    }
    if( 0 > mca_oob_recv_nb(&peer, recv_msg1, 4, 0, 0, &callback, (void *) 2)) {
        test_failure("mca_oob_recv_nb.");
    } else {
        test_success();
    }

    /* Nonblocking send followed by a blocking recieve with packing */
    if( 0 > mca_oob_send_hton_nb(&peer, send_msg1, types, 4, 0, 0, &callback,
(void *) 1)) {
        test_failure("mca_oob_send_hton_nb.");
    } else {
        test_success();
    }
    if( 0 > mca_oob_recv_ntoh(&peer, recv_msg1, types, 4, 0, 0)) {
        test_failure("mca_oob_recv_ntoh.");
    } else {
        test_success();
    }
    if(!compare_iovec(recv_msg1, send_msg1, 4)) {
        test_failure("compare 1 is wrong");
    }
    
    /* non blocking send of message type 2  followed by blocking recieve*/
    if( 0 > mca_oob_send_nb(&peer, send_msg2, 3, 0, 0, &callback, 
                            (void *) 4)) {
        test_failure("mca_oob_send.");
    } else {
        test_success();
    }


    /* check the type of message - before doing the actual receive */
    if( 0 > mca_oob_recv(&peer, recv_msg1, 1, 0, MCA_OOB_PEEK)) {
        test_failure("mca_oob_recv w/peek.");
    } else {
        test_success();
    }

    switch(msg_type) {
        case MSG_TYPE_1:
            if( 0 > mca_oob_recv(&peer, recv_msg1, 4, 0, 0)) {
                test_failure("mca_oob_recv_nb of peeked message.");
            } else {
                test_success();
            }
            if(!compare_iovec(recv_msg1, send_msg1, 3)) {
                test_failure("compare 1 is wrong");
             }
            break;
        case MSG_TYPE_2:
            if( 0 > mca_oob_recv(&peer, recv_msg2, 3, 0, 0)) {
                test_failure("mca_oob_recv_nb of peeked message.");
            } else {
                test_success();
            }
            if(!compare_iovec(recv_msg1, send_msg1, 3)) {
                 test_failure("compare 1 is wrong");
            }
            break;
        default:
            test_failure("Message peek did not return a valid type number.");
            break;
    }
    test_finalize();
    MPI_Finalize();
    return 0;
}
 
