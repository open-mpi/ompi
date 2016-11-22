/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2005 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart,
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2013      University of Houston. All rights reserved.
 * $COPYRIGHT$
 *
 * Additional copyrights may follow
 *
 * $HEADER$
 */

#include "sharedfp_addproc_control.h"

/* #define PRINT_TAG 1 */
void nodeDelete(node **front, node **rear)
{
    node *delNode;
    if ((*front) == NULL && (*rear)==NULL) {
        printf("The queue is empty\n");
    }
    else {
        delNode = *front;
        if (*front == *rear)  {
            *rear = NULL;
        }
        (*front) = (*front)->Next;

        free(delNode);
    }

    return;
}

void nodeInsert(node **front, node **rear, int procNo, long numBytesArrAddr)
{
    node *newNode;
    newNode = (node*)malloc(sizeof(node));

    newNode->Next = NULL;
    newNode->procNo = procNo;
    newNode->numBytesArrAddr = numBytesArrAddr;


    if ((*front == NULL) && (*rear == NULL))  {
        *front = newNode;
        *rear = newNode;
#if 0
	printf("Front and rear both NULL\n");
#endif
        fflush(stdout);
    }
    else {
        (*rear)->Next = newNode;
        *rear=newNode;
#if 0
            printf("Front and rear both not NULL\n");
#endif
	    fflush(stdout);
    }

    return;
}

int Check_Request_Offset(int tag_received)
{
#if 0
        printf("Tag received %d\n",tag_received);
#endif

    if (tag_received == REQUEST_TAG)  {
#if 0
	printf("Return from Check_Request_Offset\n");
#endif
	return 1;
    }


    return 0;
}

int Check_Acknowledgement(int tag_received)
{
    if (tag_received == ACK_TAG)
        return 1;

    return 0;
}

int End_control_shared_request(int tag_received)
{
    if (tag_received == END_TAG)
        return 1;


    return 0;
}


int main(int argc, char **argv)
{
    long recvBuff;
    long offsetValue;
    long endoffile;
    int size;
    int tag_received;
    int END_FLAG = 0;

    int recvcount = 1;
    MPI_Status status;
    MPI_Comm parentComm;
    static MPI_Offset offset = 0;

    /*statusStruct arr;*/

    node *rear, *front;
    rear = front = NULL;

#if 0
        printf("addproc_control: MPI_INIT\n"); fflush(stdout);
#endif
    MPI_Init(&argc,&argv);

#if 0
        printf("addproc_control: MPI_Comm_size\n"); fflush(stdout);
#endif
    MPI_Comm_size(MPI_COMM_WORLD,&size);


    endoffile = 0;

#if 0
        printf("addproc_control: start listening\n"); fflush(stdout);
#endif
    while(!END_FLAG)  {

        /* Receive request from other processes */
        MPI_Comm_get_parent(&parentComm);

        MPI_Recv(&recvBuff,recvcount,OMPI_OFFSET_DATATYPE,MPI_ANY_SOURCE,MPI_ANY_TAG,parentComm,&status);
        tag_received = status.MPI_TAG;

        switch (tag_received)
        {

        case REQUEST_TAG:
#if 0
                printf("addproc_control: Offset requested by the process %d\n",status.MPI_SOURCE); fflush(stdout);
#endif
            /* Insert the node into the linked list */
            nodeInsert(&front,&rear,status.MPI_SOURCE,recvBuff);
            break;
        case END_TAG:
#if 0
	    printf("addproc_control: End Control tag received\n"); fflush(stdout);
#endif
            END_FLAG = 1;
            break;
        case SEEK_SET_TAG:
            offset = recvBuff;
            MPI_Send(&offset,1,OMPI_OFFSET_DATATYPE,status.MPI_SOURCE,SEEK_SET_TAG,parentComm);
#if 0
	    printf("addproc_control: Seek set tag received\n"); fflush(stdout);
#endif
            break;
        case SEEK_CUR_TAG:
#if 0
	    printf("addproc_control: Seek CUR Tag received\n"); fflush(stdout);
#endif
            /*set the pointer to the offset*/
            offset += recvBuff;
            MPI_Send(&offset,1,OMPI_OFFSET_DATATYPE,status.MPI_SOURCE,SEEK_CUR_TAG,parentComm);
            break;
        case SEEK_END_TAG:
#if 0
	    printf("addproc_control: Seek END TAG received\n"); fflush(stdout);
#endif
            offset = endoffile;
            offset += recvBuff;
            MPI_Send(&offset,1,OMPI_OFFSET_DATATYPE,status.MPI_SOURCE,SEEK_END_TAG,parentComm);
            break;
        case GET_POSITION_TAG:
#if 0
	    printf("\naddproc_control: Get Position tag received\n"); fflush(stdout);
#endif
            /*Send the offset as requested*/
            MPI_Send(&offset,1,OMPI_OFFSET_DATATYPE,status.MPI_SOURCE,GET_POSITION_TAG,parentComm);
            break;
        default:
            printf("addproc_control: Unknown tag received\n"); fflush(stdout);
            break;
        }

        while (front != NULL)  {

            offsetValue = offset;

            offset += front->numBytesArrAddr;

            /* Store the end of file */
            if (endoffile < offset)
                endoffile = offset;


            /* MPI_Send to the correct process */

            MPI_Send(&offsetValue,1,OMPI_OFFSET_DATATYPE, front->procNo, OFFSET_TAG,
		     parentComm);
            nodeDelete(&front,&rear);

        }

    }		/* End of while(1) loop */

#if 0
    printf("addproc_control: finalizing mpi...\n"); fflush(stdout);
#endif
    MPI_Finalize();

#if 0
    printf("addproc_control: Exiting...\n");
#endif
    return 0;
}
