/*
 * Basic test for the oob when sending packed buffers.
 * The idea is to try most combinations of sending and recieving
 * to run:
 * mpirun -np 2 -- oob_test
 */


#include "ompi_config.h"
#include <sys/types.h>
#include <sys/uio.h>
#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include "mpi.h"
#include "support.h"
#include "mca/oob/oob.h"
#include "util/pack.h"

#define TAG 11011

int i;
void do_sends(ompi_process_name_t * peer);
void do_recvs(ompi_process_name_t * peer);
void do_compare ();

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
    
/*  */
/* void callback(int status, const ompi_process_name_t * peer, */
/*               const struct iovec * msg, int count, int tag, void * cbdata); */
/*  */
/* void callback(int status, const ompi_process_name_t * peer, */
/*               const struct iovec * msg, int count, int tag, void * cbdata) */
/* { */
/*     if(0 != tag) { */
/*         test_failure("Bad tag."); */
/*     } */
/*     if(((int) cbdata) >= NUM_TESTS * NUM_TIMES) { */
/*         test_failure("Bad value in callback function."); */
/*     } else if (testdone[(int) cbdata]) { */
/*         test_failure("Callback function called on an already completed test."); */
/*     } else { */
/*         testdone[(int) cbdata] = true; */
/*         test_success(); */
/*     } */
/* } */

/* data */
/* setup message */
char     data0[] = "hello";
uint32_t data1[] = {1};
uint16_t data2[] = {32};
char     data3[] = "world";
uint16_t data4[] = {1, 2, 3, 4, 5};
uint32_t data5[] = {101, 102, 103};

int      datalens[] = {6, 1, 1, 6, 5, 3};

/* recv data minibuffers */
char     rdata0[128];
uint32_t rdata1[128];
uint16_t rdata2[128]; 
char     rdata3[128];
uint16_t rdata4[128];
uint32_t rdata5[128];

int main(int argc, char ** argv)
{
    ompi_process_name_t  me;
    ompi_process_name_t  them;
    MPI_Init(&argc, &argv);
   
    /* setup peer address */
    me = mca_oob_name_self;
    fprintf(stderr, "my vpid %d my jobid %d my cellid %d my pid %d\n",  
            me.vpid, me.jobid, me.cellid, getpid());

    if(me.vpid == 1) {
        test_init("oob packed send then recieve");
        /* local vpid is 1 - peer is 0 */
	    them = me;
        them.vpid = 0;
        do_sends(&them);
        do_recvs(&them);
		do_compare();
    } else {
        test_init("oob packed recieve then send");
        /* local vpid is 0 - peer is 1 */
	    them = me;
        them.vpid = 1;
        do_recvs(&them);
		do_compare();
        do_sends(&them);
    }


    test_finalize();
    /* this is to give the oob time to finish all sends */
    MPI_Finalize();
    return 0;
}
 
void do_sends(ompi_process_name_t * peer) {
    /* non blocking send without doing any packing */
/*     if( 0 > mca_oob_send_nb(peer, send_msg1, 4, 0, 0, &callback,  */
/*                             (void *) (0 +  (NUM_TESTS * i)))) { */
/*         test_failure("mca_oob_send_nb."); */
/*     } else { */
/*         test_success(); */
/*     } */
/*     if( 0 > mca_oob_send_nb(peer, send_msg1, 4, 0, 0, &callback,  */
/*                             (void *) (1 +  (NUM_TESTS * i)))) { */
/*         test_failure("mca_oob_send_nb."); */
/*     } else { */
/*         test_success(); */
/*     } */

	ompi_buffer_t sendbuf;

	ompi_buffer_init (&sendbuf,0);

	/* pack complex data buffer */
	ompi_pack (sendbuf, data0, datalens[0], OMPI_STRING);
	ompi_pack (sendbuf, data1, datalens[1], OMPI_INT32);
	ompi_pack (sendbuf, data2, datalens[2], OMPI_INT16);
	ompi_pack (sendbuf, data3, datalens[3], OMPI_STRING);
	ompi_pack (sendbuf, data4, datalens[4], OMPI_INT16);
	ompi_pack (sendbuf, data5, datalens[5], OMPI_INT32);
	
    /* blocking packed send  */
    if( 0 > mca_oob_send_packed(peer, sendbuf, TAG, 0)) {
        test_failure("mca_oob_send_packed.");
		return;
    } 

	ompi_buffer_free (sendbuf);	/* can only free send buffer as a blocking send */

    test_success();
}

void do_recvs(ompi_process_name_t * peer) {
    /*first, we'll recieve the nonpacked send - assuming we know the
     *  message type */
/*     if( 0 > mca_oob_recv_nb(peer, recv_msg1, 4, 0, 0, &callback,  */
/*                             (void *) (4 +  (NUM_TESTS * i)))) { */
/*         test_failure("mca_oob_recv_nb."); */
/*     } else { */
/*         test_success(); */
/*     } */

	ompi_buffer_t recvbuf;
	int tag = TAG;

    if( 0 > mca_oob_recv_packed (peer, &recvbuf, &tag)) {
        test_failure("mca_oob_recv_packed.");
		return;
    } 

	ompi_unpack (recvbuf, rdata0, datalens[0], OMPI_STRING);
	ompi_unpack (recvbuf, rdata1, datalens[1], OMPI_INT32);
	ompi_unpack (recvbuf, rdata2, datalens[2], OMPI_INT16);
	ompi_unpack (recvbuf, rdata3, datalens[3], OMPI_STRING);
	ompi_unpack (recvbuf, rdata4, datalens[4], OMPI_INT16);
	ompi_unpack (recvbuf, rdata5, datalens[5], OMPI_INT32);

	ompi_buffer_free (recvbuf);

    test_success();
}

void do_compare () {
	int rc=0;
	int i;


	rc =  test_verify_str(data0, rdata0);
	for(i=0;i<datalens[1];i++) rc =  test_verify_int(data1[i], rdata1[i]);
	for(i=0;i<datalens[2];i++) rc =  test_verify_int(data2[i], rdata2[i]);
	rc =  test_verify_str(data3, rdata3);
	for(i=0;i<datalens[4];i++) rc =  test_verify_int(data4[i], rdata4[i]);
	for(i=0;i<datalens[5];i++) rc =  test_verify_int(data5[i], rdata5[i]);


    test_success();
}

