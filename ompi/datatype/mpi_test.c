/*-----------------------------------------------------------------------------
MESSAGE PASSING INTERFACE TEST CASE SUITE

Copyright - 1996 Intel Corporation

Intel Corporation hereby grants a non-exclusive license under Intel's
copyright to copy, modify and distribute this software for any purpose
and without fee, provided that the above copyright notice and the following
paragraphs appear on all copies.

Intel Corporation makes no representation that the test cases comprising
this suite are correct or are an accurate representation of any standard.

IN NO EVENT SHALL INTEL HAVE ANY LIABILITY FOR ANY DIRECT, INDIRECT OR
SPECULATIVE DAMAGES, (INCLUDING WITHOUT LIMITING THE FOREGOING, CONSEQUENTIAL,
INCIDENTAL AND SPECIAL DAMAGES) INCLUDING, BUT NOT LIMITED TO INFRINGEMENT,
LOSS OF USE, BUSINESS INTERRUPTIONS, AND LOSS OF PROFITS, IRRESPECTIVE OF
WHETHER INTEL HAS ADVANCE NOTICE OF THE POSSIBILITY OF ANY SUCH DAMAGES.

INTEL CORPORATION SPECIFICALLY DISCLAIMS ANY WARRANTIES INCLUDING, BUT NOT
LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY, FITNESS FOR A
PARTICULAR PURPOSE AND NON-INFRINGEMENT.  THE SOFTWARE PROVIDED HEREUNDER
IS ON AN "AS IS" BASIS AND INTEL CORPORATION HAS NO OBLIGATION TO PROVIDE
MAINTENANCE, SUPPORT, UPDATES, ENHANCEMENTS OR MODIFICATIONS.
-----------------------------------------------------------------------------*/
/******************************************************************************
          Test for MPI_Send():  All others Send TO Root
                       and MPI_Recv():  Any source/Any tag

Tests the basic blocking MPI_Send operation, for both Intra and Inter
communicators, as well as MPI_Recv using MPI_ANY_SOURCE and MPI_ANY_TAG.

For an INTRA-communicator, the program selects each node, in turn,
of the communicator to be the root.  the ROOT receives from all of the
other nodes (it does NOT receive from itself).

For an INTER-communicator, there are two groups of nodes; each group being
termed a sub-communicator.  The nodes in each sub-communicator are numbered
 0 to (k-1) for a k-node sub-communicator.  So,the MPITEST_current_rank will
return duplicate node numbers for nodes (0 to k-1) where k is the number of
nodes in the smaller communicator.

The program cycles through the nodes in one sub-communicator, with each
selected node receiving from all of the nodes in the other sub-communicator.
Then the program reverses the send and receive role of the
two sub-communicators, and repeats the process.
    
This test initializes the send buffer starting with the root's rank in the
communicator (or an appropriate value for the non-integer types.)
Once the receiving nodes have completed their message receive, they check to
make sure the current root's rank is in the received buffer.

This test may be run in any communicator, with any data type, and with
any non-negative message length.

The MPITEST environment provides looping over communicator size and
type, message length, and data type.  The properties of the loops are
encoded in configuration arrays in the file mpitest_cfg.h .  See the
MPITEST README for further details.
******************************************************************************/

#include "ompi_config.h"
#include <stdbool.h>
#include "datatype/datatype.h"
#include "datatype/datatype_internal.h"
#include "datatype/convertor.h"
#include "mpitest_cfg.h"
#include "mpitest.h"

/* Compile with:
mpicc -DHAVE_CONFIG_H -I. -I../../include -I../../../ompi-trunk/include  -I../.. -I../../include -I../../../ompi-trunk/opal -I../../../ompi-trunk/orte -I../../../ompi-trunk/ompi -g mpi_test.c -o mpi_test ~/ompi-tests/intel_tests/src/libmpitest.o -I../../../ompi-tests/intel_tests/src
*/
main(int argc, char *argv[])
{
    int
     berr,                      /* error flag for buffer ATTACH and DETACH       */
     byte_length,               /* The length of the current buffer in bytes     */
     cnt_len,                   /* received length returned by MPI_Get_Count     */
     comm_count,                /* loop counter for communicator loop            */
     comm_index,                /* the array index of the current comm           */
     comm_type,                 /* the index of the current communicator type    */
     error,                     /* errors from one MPI call                      */
     fail,                      /* counts total number of failures               */
     loop_fail,                 /* counts number of failures in one loop         */
     get_from,                  /* Number of nodes sending current message       */
     grp_lup,                   /* index for comm groups (1 to ntimes)           */
     i, j,                      /* utility loop index variables                  */
     ierr,                      /* return value from MPI calls                   */
     inter_comm,                /* flag, true, if intercommunicator              */
     left_recv_size,            /* Left Intercommunictor receive size            */
     left_send_size,            /* Left Intercommunicator send size              */
     length,                    /* The length of the current buffer              */
     length_count,              /* loop counter for message length loop          */
     loop_cnt,                  /* counts total number of loops through test     */
     max_byte_length,           /* maximum buffer length in bytes                */
     max_length,                /* max buffer length specified in config. file   */
     ntimes,                    /* # of communicator groups INTRA=1, INTER=2     */
     print_node_flag,           /* node 0 of INTRA, node 0 of left comm of INTER */
     receivers,                 /* INTER-comm #receivers for this send/rec choice */
     recv_group,                /* INTER-comm test receiving group(0 or 1)       */
     recv_size,                 /* INTER  receive size MPI_Comm_remote_size      */
     right_recv_size,           /* INTER: absolute count of right receive * nodes */
     right_send_size,           /* INTER: count of right sending nodes           */
     root,                      /* the root of the current broadcast             */
     send_group,                /* INTER-comm test sending group #(0 or 1)       */
     send_size,                 /* INTER: relative send size from MPI_Comm_size  */
     senders,                   /* INTER-comm #senders for this send/rec choice  */
     size,                      /* return size from MPI_Error_string             */
     test_nump,                 /* The number of nodes in current communicator   */
     test_type,                 /* the index of the current buffer type          */
     type_count;                /* loop counter for data type loop               */


    struct dataTemplate
     value;                     /* dataTemplate for initializing buffers         */

    void *recv_buffer;
    void *send_buffer;          /* message buffer                                */

    char
     info_buf[256],             /* buffer for passing mesages to MPITEST         */
     testname[64];              /* the name of this test                         */

    MPI_Comm comm,              /* MPI communicator                              */
        barrier_comm;           /* If intercommunicator, merged comm for Barrier */

    MPI_Status recv_stat;       /* MPI  status structure                         */

    /*-----------------------------  MPI_Init  ------------------------------*/
    ierr = MPI_Init(&argc, &argv);
    if (ierr != MPI_SUCCESS) {
        sprintf(info_buf, "Non-zero return code (%d) from MPI_Init()",
                ierr);
        MPITEST_message(MPITEST_FATAL, info_buf);
    }


    sprintf(testname, "MPI_convertor_c: Datatype convertor test");

    /*-----------------------------  MPITEST_init  --------------------------*/
    MPITEST_init(argc, argv);
    if (MPITEST_me == 0) {
        sprintf(info_buf, "Starting %s test", testname);
        MPITEST_message(MPITEST_INFO0, info_buf);
    }

    /* set the global error counter */
    fail = 0;
    loop_fail = 0;
    loop_cnt = 0;

    max_byte_length = MPITEST_get_max_message_length();

    /*--------------------------  Loop over Communicators  ------------------*/

    for (comm_count = 0; comm_count < 2 /*MPITEST_num_comm_sizes()*/;
         comm_count++) {
        comm_index = MPITEST_get_comm_index(comm_count);
        comm_type = MPITEST_get_comm_type(comm_count);

        /*
         * Reset a bunch of variables that will be set when we get our
         * communicator. These are mainly variables for determining
         * sub-communicator, left or right, when we get an INTER-communicator.
         */

        left_send_size = -1;
        left_recv_size = -1;
        right_send_size = -1;
        right_recv_size = -1;
        send_size = -1;
        recv_size = -1;

        inter_comm = -1;
        print_node_flag = 0;

        test_nump = MPITEST_get_communicator(comm_type, comm_index, &comm);

        /* Skip everything if not a member of this communicator */
        if (MPITEST_current_rank != MPI_UNDEFINED) {

            /*
             * Test if INTER-communicator:  inter_comm is true if this is an
             * inter-communicator
             */

            MPI_Comm_test_inter(comm, &inter_comm);
            /*
             * Variables, send_size, and recv_size  both default to the total 
             * nodes in the communicator for an INTRA-communicator For an
             * INTER-communicator: MPI_Comm_size returns the size of the
             * communicator for all of the nodes in each respective communicator
             * MPI_Comm_remote_size returns the size of the opposite
             * communicator for all nodes in each respective communicator
             *
             * Set the print_node_flag: This flag tells which node gets
             * to print.  For INTRA-communition it is simply node 0.  For
             * INTER-communication it is node zero of the left group.
             */

            if (inter_comm) {   /* Do this stuff for INTER-communicators */

                if (MPITEST_inter == 0) {       /* Do this stuff for left group of an INTER-communicator */
                    MPI_Comm_size(comm, &send_size);
                    MPI_Comm_remote_size(comm, &recv_size);

                    left_send_size = send_size;
                    left_recv_size = recv_size;
                    right_send_size = recv_size;
                    right_recv_size = send_size;

                    if (MPITEST_current_rank == 0) {    /* Do for node zero of left bank of INTER-communicator */
                        print_node_flag = 1;
                    }
                    /* End of INTER-communicator left group node zero */
                    ierr = MPI_Intercomm_merge(comm, FALSE, &barrier_comm);
                    if (ierr != MPI_SUCCESS) {
                        sprintf(info_buf,
                                "Non-zero return code (%d) from MPI_Intercomm_merge",
                                ierr);
                        MPITEST_message(MPITEST_NONFATAL, info_buf);
                        MPI_Error_string(ierr, &info_buf[0], &size);
                        MPITEST_message(MPITEST_FATAL, info_buf);
                    }

                }
                /* End of      INTER-communicator left group */
                if (MPITEST_inter == 1) {       /* Do this for right group of an INTER-communicator  */
                    MPI_Comm_size(comm, &send_size);
                    MPI_Comm_remote_size(comm, &recv_size);

                    left_send_size = recv_size;
                    left_recv_size = send_size;
                    right_send_size = send_size;
                    right_recv_size = recv_size;

                    ierr = MPI_Intercomm_merge(comm, FALSE, &barrier_comm);
                    if (ierr != MPI_SUCCESS) {
                        sprintf(info_buf,
                                "Non-zero return code (%d) from MPI_Intercomm_merge",
                                ierr);
                        MPITEST_message(MPITEST_NONFATAL, info_buf);
                        MPI_Error_string(ierr, &info_buf[0], &size);
                        MPITEST_message(MPITEST_FATAL, info_buf);
                    }
                }               /* End of    INTER_communicator right group */
            }

            /* End of        INTER_communicator  */
            /* Do  for INTRA-communicator  */
            if (!inter_comm) {
                if (MPITEST_current_rank == 0)
                    print_node_flag = 1;
                barrier_comm = comm;
            }


            /*------------------  Loop over Data Types  ---------------------*/

            for (type_count = 0; type_count < MPITEST_num_datatypes();
                 type_count++) {
                test_type = MPITEST_get_datatype(type_count);

                /* convert the number of bytes in the maximum length message */
                /* into the number of elements of the current type */

                max_length =
                    MPITEST_byte_to_element(test_type, max_byte_length);

                /* Allocate send and receive Buffers */
                MPITEST_get_buffer(test_type, max_length, &recv_buffer);
                MPITEST_get_buffer(test_type, max_length, &send_buffer);

                /*-------------  Loop over Message Lengths  ---------------*/

                for (length_count = 0;
                     length_count < MPITEST_num_message_lengths();
                     length_count++) {
                     byte_length = MPITEST_get_message_length(length_count);
                     length = MPITEST_byte_to_element(test_type, byte_length);
                     ompi_convertor_t *send_conv = ompi_convertor_create(0,0);
                     ompi_convertor_t *recv_conv = ompi_convertor_create(0,0);
                     char eager[1024];
                     char max_send[8192];
                     struct iovec iov;
                     uint32_t iov_count;
                     int32_t free_after;
                     size_t max_data_pack;
                     size_t max_data_unpack;
                     size_t bytes_remaining;

                     loop_cnt++;  /* increase the number of runned tests */

                     /* Initialize send buffer */
                     MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
                     MPITEST_init_buffer_inc(test_type, length + 1, value, send_buffer);

                     /* Initialize recv buffer */
                     MPITEST_dataTemplate_init(&value, -1);
                     MPITEST_init_buffer(test_type, length + 1, value, recv_buffer);

                     /* Initialize convertors */
                     ompi_convertor_prepare_for_send(send_conv, MPITEST_mpi_datatypes[test_type], 
                         length, send_buffer);
                     ompi_convertor_get_packed_size(send_conv, &bytes_remaining);
                     ompi_convertor_prepare_for_recv(recv_conv, MPITEST_mpi_datatypes[test_type], 
                         length, recv_buffer);

                     if(bytes_remaining > sizeof(eager))
                         max_data_pack = sizeof(eager);
                     else
                         max_data_pack = bytes_remaining;
 
                     iov.iov_base = eager;
                     iov.iov_len = max_data_pack;
                     iov_count = 1;
                     ompi_convertor_pack(send_conv, &iov, &iov_count, &max_data, &free_after);
                     bytes_remaining -= max_data_pack; /* sender schedules data */

                     iov.iov_base = eager;
                     iov.iov_len = max_data_pack;
                     max_data_unpack = max_data_pack;
                     ompi_convertor_unpack(recv_conv, &iov, &iov_count, &max_data, &free_after);

                     if (max_data_pack != max_data_unpack) {
                         fprintf(stderr, "pack/unpack count mismatch: %lu != !lu\n", max_data_pack, max_data_unpack);
                     }

                     while(bytes_remaining != 0) {
                         if(bytes_remaining > sizeof(max_send)) {
                             max_data = sizeof(max_send);
                         } else {
                             max_data = bytes_remaining;
                         }
 
                         iov.iov_base = max_send;
                         iov.iov_len = max_data;
                         iov_count = 1;

                         ompi_convertor_pack(send_conv, &iov, &iov_count, &max_data, &free_after);
                         bytes_remaining -= max_data;

                         iov.iov_base = max_send;
                         iov.iov_len = max_data_pack;
                         iov_count = 1;
                         max_data_unpack = max_data_pack;
                         ompi_convertor_unpack(recv_conv, &iov, &iov_count, &max_data_unpack, &free_after);

                         if (max_data_pack != max_data_unpack) {
                             fprintf(stderr, "pack/unpack count mismatch: %lu != !lu\n", max_data_pack, max_data_unpack);
                         }
                     }

                     /* Error Test  */
                     /*
                      * Set up the dataTemplate for checking the
                      * recv'd buffer.  Note that the sending
                      * node rank will be sent.
                      */
                     MPITEST_dataTemplate_init(&value, MPITEST_current_rank);
                     error = MPITEST_buffer_errors_inc
                            (test_type, length, value,
                             recv_buffer);

                     /* check for receive buffer overflow */
                     MPITEST_dataTemplate_init(&value, -1);

                     error += MPITEST_buffer_errors_ov(test_type,
                                                     length,
                                                     value,
                                                     recv_buffer);

                     if (error) {
                            sprintf(info_buf,
                                    "%d errors in buffer (%d,%d,%d) len %d commsize %d commtype %d data_type %d root %d",
                                    error, length_count,
                                    comm_count, type_count,
                                    length, test_nump,
                                    comm_type, test_type,
                                    root);
                            MPITEST_message(MPITEST_NONFATAL,
                                            info_buf);
                            loop_fail++;
                            fail += error;
                     }
                }               /* Loop over Message Lengths  */

                free(send_buffer);
                free(recv_buffer);

            }                   /* Loop over Data Types  */

            if (inter_comm) {
                ierr = MPI_Comm_free(&barrier_comm);
                if (ierr != MPI_SUCCESS) {
                    sprintf(info_buf,
                            "Non-zero return code (%d) from MPI_Comm_free",
                            ierr);
                    MPITEST_message(MPITEST_NONFATAL, info_buf);
                    MPI_Error_string(ierr, &info_buf[0], &size);
                    MPITEST_message(MPITEST_FATAL, info_buf);
                }
            }

        }

        /* node rank not defined for this communicator */
        MPITEST_free_communicator(comm_type, &comm);

        /*-------------------------------------------------------------------*/

        /*
         * With the current design of the program, we skip all the code for the
         * nodes that are not members of the communicator, so this is the first
         * point that we can place a barrier
         */
        MPI_Barrier(MPI_COMM_WORLD);

        /*-------------------------------------------------------------------*/

    }                           /* Loop over Communicators  */

    /* report overall results  */

    MPITEST_report(loop_cnt - loop_fail, loop_fail, 0, testname);

    MPI_Finalize();

    return fail;

}                               /* main() */

