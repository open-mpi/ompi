#include "adio.h"
#include "adio_extern.h"
#include "../ad_gpfs/ad_gpfs_tuning.h"

#include <pthread.h>

/* #define p2pcontigtrace 1 */

void ADIOI_P2PContigWriteAggregation(ADIO_File fd,
	const void *buf,
	int *error_code,
	ADIO_Offset *st_offsets,
	ADIO_Offset *end_offsets,
	ADIO_Offset *fd_start,
	ADIO_Offset* fd_end)
{

    *error_code = MPI_SUCCESS; /* initialize to success */

#ifdef ROMIO_GPFS
    double startTimeBase,endTimeBase;
#endif

    MPI_Status status;
    pthread_t io_thread;
    void *thread_ret;
    ADIOI_IO_ThreadFuncData io_thread_args;

    int nprocs,myrank;
    MPI_Comm_size(fd->comm, &nprocs);
    MPI_Comm_rank(fd->comm, &myrank);

	ADIO_Offset myOffsetStart = st_offsets[myrank], myOffsetEnd = end_offsets[myrank];

    int myAggRank = -1; /* if I am an aggregor this is my index into fd->hints->ranklist */
    int iAmUsedAgg = 0;

#ifdef ROMIO_GPFS
    startTimeBase = MPI_Wtime();
#endif

    int naggs = fd->hints->cb_nodes;
    int coll_bufsize = fd->hints->cb_buffer_size;
#ifdef ROMIO_GPFS
    if (gpfsmpio_pthreadio == 1) {
	/* split buffer in half for a kind of double buffering with the threads*/
	coll_bufsize = fd->hints->cb_buffer_size/2;
    }
#endif

    int j;
    for (j=0;j<naggs;j++) {
	if (fd->hints->ranklist[j] == myrank) {
	    myAggRank = j;
	    if (fd_end[j] > fd_start[j]) {
		iAmUsedAgg = 1;
	    }
	}
    }

    /* Determine how much data and to whom I need to send.  For source proc
     * targets, also determine the target file domain offsets locally to
     * reduce communication overhead */
    int *targetAggsForMyData = (int *)ADIOI_Malloc(naggs * sizeof(int));
    ADIO_Offset *targetAggsForMyDataFDStart = (ADIO_Offset *)ADIOI_Malloc(naggs * sizeof(ADIO_Offset));
    ADIO_Offset *targetAggsForMyDataFDEnd = (ADIO_Offset *)ADIOI_Malloc(naggs * sizeof(ADIO_Offset));
    int numTargetAggs = 0;
    int i;
    for (i=0;i<naggs;i++) {
	if ( ((myOffsetStart >= fd_start[i]) &&  (myOffsetStart <= fd_end[i])) || ((myOffsetEnd >= fd_start[i]) &&  (myOffsetEnd <= fd_end[i]))) {
	    targetAggsForMyData[numTargetAggs] = fd->hints->ranklist[i];
	    targetAggsForMyDataFDStart[numTargetAggs] = fd_start[i];
	    targetAggsForMyDataFDEnd[numTargetAggs] = fd_end[i];
	    numTargetAggs++;
	}
    }

    /* these 3 arrays track info on the procs that feed an aggregtor */
    int *sourceProcsForMyData=NULL;
    int *remainingDataAmountToGetPerProc=NULL;
    ADIO_Offset *remainingDataOffsetToGetPerProc=NULL;

    int numSourceProcs = 0;

    if (iAmUsedAgg) { /* for the used aggregators figure out how much data I
			 need from what procs */

	/* count numSourceProcs so we know how large to make the arrays */
	for (i=0;i<nprocs;i++)
	    if ( ((st_offsets[i] >= fd_start[myAggRank]) &&  (st_offsets[i] <= fd_end[myAggRank])) || ((end_offsets[i] >= fd_start[myAggRank]) &&  (end_offsets[i] <= fd_end[myAggRank])))
		numSourceProcs++;

	sourceProcsForMyData = (int *)ADIOI_Malloc(numSourceProcs * sizeof(int));
	remainingDataAmountToGetPerProc = (int *)ADIOI_Malloc(numSourceProcs * sizeof(int));
	remainingDataOffsetToGetPerProc = (ADIO_Offset *)ADIOI_Malloc(numSourceProcs * sizeof(ADIO_Offset));

	/* everybody has the st_offsets and end_offsets for all ranks so if I am a
	 * used aggregator go thru them and figure out which ranks have data that
	 * falls into my file domain assigned to me */
	numSourceProcs = 0;
	for (i=0;i<nprocs;i++) {
	    if ( ((st_offsets[i] >= fd_start[myAggRank]) &&  (st_offsets[i] <= fd_end[myAggRank])) || ((end_offsets[i] >= fd_start[myAggRank]) &&  (end_offsets[i] <= fd_end[myAggRank]))) {
		sourceProcsForMyData[numSourceProcs] = i;
		if ( ((st_offsets[i] >= fd_start[myAggRank]) &&  (st_offsets[i] <= fd_end[myAggRank])) && ((end_offsets[i] >= fd_start[myAggRank]) &&  (end_offsets[i] <= fd_end[myAggRank]))) {
		    remainingDataAmountToGetPerProc[numSourceProcs] = (end_offsets[i] - st_offsets[i])+1;
		    remainingDataOffsetToGetPerProc[numSourceProcs] = st_offsets[i];
		}
		else if ((st_offsets[i] >= fd_start[myAggRank]) &&  (st_offsets[i] <= fd_end[myAggRank])) {/* starts in this fd and goes past it */
		    remainingDataAmountToGetPerProc[numSourceProcs] = (fd_end[myAggRank] - st_offsets[i]) +1;
		    remainingDataOffsetToGetPerProc[numSourceProcs] = st_offsets[i];
		}
		else { /* starts in fd before this and ends in it */
		    remainingDataAmountToGetPerProc[numSourceProcs] = (end_offsets[i] - fd_start[myAggRank]) +1;
		    remainingDataOffsetToGetPerProc[numSourceProcs] = fd_start[myAggRank];
		}
#ifdef p2pcontigtrace
		printf("getting %ld bytes from source proc %d in fd rank %d with borders %ld to %ld\n",remainingDataAmountToGetPerProc[numSourceProcs],i,fd->hints->ranklist[myAggRank],fd_start[myAggRank],fd_end[myAggRank]);
#endif
		numSourceProcs++;
	    }
	}
    }

    int *amountOfDataReqestedByTargetAgg = (int *)ADIOI_Malloc(naggs * sizeof(int));
    for (i=0;i<numTargetAggs;i++) {
	amountOfDataReqestedByTargetAgg[i] = 0;
    }

    int totalAmountDataReceived = 0;
    MPI_Request *mpiSizeToSendRequest = (MPI_Request *) ADIOI_Malloc(numTargetAggs * sizeof(MPI_Request));
    MPI_Request *mpiRecvDataRequest = (MPI_Request *) ADIOI_Malloc(numSourceProcs * sizeof(MPI_Request));
    MPI_Request *mpiSendDataSizeRequest = (MPI_Request *) ADIOI_Malloc(numSourceProcs * sizeof(MPI_Request));

    MPI_Request *mpiSendDataToTargetAggRequest = (MPI_Request *) ADIOI_Malloc(numTargetAggs * sizeof(MPI_Request));
    MPI_Status mpiWaitAnyStatusFromTargetAggs,mpiWaitAnyStatusFromSourceProcs;
    MPI_Status mpiIsendStatusForSize,  mpiIsendStatusForData;

    /* use the write buffer allocated in the file_open */
    char *write_buf0 = fd->io_buf;
    char *write_buf1 = fd->io_buf + coll_bufsize;

    /* start off pointing to the first buffer. If we use the 2nd buffer (threaded
     * case) we'll swap later */
    char *write_buf = write_buf0;

    /* compute number of rounds */
    ADIO_Offset numberOfRounds = (ADIO_Offset)((((ADIO_Offset)(end_offsets[nprocs-1]-st_offsets[0]))/((ADIO_Offset)((ADIO_Offset)coll_bufsize*(ADIO_Offset)naggs)))) + 1;

    int currentWriteBuf = 0;
    int useIOBuffer = 0;
#ifdef ROMIO_GPFS
    if (gpfsmpio_pthreadio && (numberOfRounds>1)) {
	useIOBuffer = 1;
	io_thread = pthread_self();
    }
#endif

    ADIO_Offset currentRoundFDStart = 0;
    ADIO_Offset currentRoundFDEnd = 0;

    if (iAmUsedAgg) {
	currentRoundFDStart = fd_start[myAggRank];
    }

    int *dataSizeGottenThisRoundPerProc = (int *)ADIOI_Malloc(numSourceProcs * sizeof(int));
    int *mpiRequestMapPerProc = (int *)ADIOI_Malloc(numSourceProcs * sizeof(int));
    int *targetAggIndexesForMyDataThisRound = (int *)ADIOI_Malloc(numTargetAggs * sizeof(int));
    int *sendBufferOffsetsThisRound = (int *)ADIOI_Malloc(numTargetAggs * sizeof(int));
    int *bufferAmountsToSendThisRound = (int *)ADIOI_Malloc(numTargetAggs * sizeof(int));

#ifdef ROMIO_GPFS
    endTimeBase = MPI_Wtime();
    gpfsmpio_prof_cw[GPFSMPIO_CIO_T_MYREQ] += (endTimeBase-startTimeBase);
    startTimeBase = MPI_Wtime();
#endif

    /* each iteration of this loop writes a coll_bufsize portion of the file
     * domain */
    int roundIter;
    for (roundIter=0;roundIter<numberOfRounds;roundIter++) {

	/* determine what target aggs I need to send data to this round */
	int numTargetAggsThisRound = 0;
	for (i=0;i<numTargetAggs;i++) {
	    if ( ((myOffsetStart >= targetAggsForMyDataFDStart[i]) && (myOffsetStart <= targetAggsForMyDataFDEnd[i])) ||
		    ((myOffsetEnd >= targetAggsForMyDataFDStart[i]) && (myOffsetEnd <= targetAggsForMyDataFDEnd[i]))) {
		/* we know that we need to send data to this target agg at some point, now need to figure out how much this round */

		/* here are the offsets currently being collected by the aggregator during this round */
		ADIO_Offset currentRoundFDStartForMyTargetAgg = (ADIO_Offset)((ADIO_Offset)targetAggsForMyDataFDStart[i] + (ADIO_Offset)((ADIO_Offset)roundIter*(ADIO_Offset)coll_bufsize));
		ADIO_Offset currentRoundFDEndForMyTargetAgg = (ADIO_Offset)((ADIO_Offset)targetAggsForMyDataFDStart[i] + (ADIO_Offset)((ADIO_Offset)(roundIter+1)*(ADIO_Offset)coll_bufsize) - (ADIO_Offset)1);
		if (currentRoundFDEndForMyTargetAgg > targetAggsForMyDataFDEnd[i])
		    currentRoundFDEndForMyTargetAgg = targetAggsForMyDataFDEnd[i];

#ifdef p2pcontigtrace
		printf("roundIter %d target iter %d targetAggsForMyData is %d myOffsetStart is %ld myOffsetEnd is %ld targetAggsForMyDataFDStart is %ld targetAggsForMyDataFDEnd is %ld currentRoundFDStartForMyTargetAgg is %ld currentRoundFDEndForMyTargetAgg is %ld\n",
			roundIter,i,targetAggsForMyData[i],myOffsetStart,myOffsetEnd,
			targetAggsForMyDataFDStart[i],targetAggsForMyDataFDEnd[i],
			currentRoundFDStartForMyTargetAgg,currentRoundFDEndForMyTargetAgg);
#endif

		/* send the portion of my data that is within
		 * currentRoundFDStartForMyTargetAgg to
		 * currentRoundFDEndForMyTargetAgg */
		/* find the offset into the send buffer and the amount
		 * of data to send */
		int sendBufferOffset = 0;
		int bufferAmountToSend = 0;

		if ((myOffsetStart >= currentRoundFDStartForMyTargetAgg) && (myOffsetStart <= currentRoundFDEndForMyTargetAgg)) {
		    if (myOffsetEnd > currentRoundFDEndForMyTargetAgg)
			bufferAmountToSend = (currentRoundFDEndForMyTargetAgg - myOffsetStart) +1;
		    else
			bufferAmountToSend = (myOffsetEnd - myOffsetStart) +1;
		}
		else if ((myOffsetEnd >= currentRoundFDStartForMyTargetAgg) && (myOffsetEnd <= currentRoundFDEndForMyTargetAgg)) {
		    sendBufferOffset = (int) (currentRoundFDStartForMyTargetAgg - myOffsetStart);
		    if (myOffsetEnd > currentRoundFDEndForMyTargetAgg)
			bufferAmountToSend = (currentRoundFDEndForMyTargetAgg - currentRoundFDStartForMyTargetAgg) +1;
		    else
			bufferAmountToSend = (myOffsetEnd - currentRoundFDStartForMyTargetAgg) +1;
		}
		else if ((myOffsetStart <= currentRoundFDStartForMyTargetAgg) && (myOffsetEnd >= currentRoundFDEndForMyTargetAgg)) {
		    sendBufferOffset = (int) (currentRoundFDStartForMyTargetAgg - myOffsetStart);
		    bufferAmountToSend = (currentRoundFDEndForMyTargetAgg - currentRoundFDStartForMyTargetAgg) +1;
		}

		if (bufferAmountToSend > 0) { /* we have data to send this round */
		    targetAggIndexesForMyDataThisRound[numTargetAggsThisRound] = i;
		    sendBufferOffsetsThisRound[numTargetAggsThisRound] = sendBufferOffset;
		    bufferAmountsToSendThisRound[numTargetAggsThisRound] = bufferAmountToSend;
#ifdef p2pcontigtrace
		    printf("bufferAmountToSend is %d sendBufferOffset is %d\n",bufferAmountToSend,sendBufferOffset);
#endif
		    /* only need to be pinged by the agg for rounds after the first one - for the first one just
		     * send the data without being pinged */
		    if (roundIter > 0)
			MPI_Irecv(&amountOfDataReqestedByTargetAgg[numTargetAggsThisRound],1,
				MPI_INT,targetAggsForMyData[i],0,
				fd->comm,&mpiSizeToSendRequest[numTargetAggsThisRound]);
		    numTargetAggsThisRound++;

		}
	    }
	}

	/* determine what offsets define the portion of the file domain the agg is writing this round */
	if (iAmUsedAgg) {
	    if ((fd_end[myAggRank] - currentRoundFDStart) < coll_bufsize) {
		currentRoundFDEnd = fd_end[myAggRank];
	    }
	    else
		currentRoundFDEnd = currentRoundFDStart + coll_bufsize - 1;
#ifdef p2pcontigtrace
	    printf("currentRoundFDStart is %ld currentRoundFDEnd is %ld within file domeain %ld to %ld\n",currentRoundFDStart,currentRoundFDEnd,fd_start[myAggRank],fd_end[myAggRank]);
#endif
	}

	int irecv,isend;
	int numSourceProcsSentData = 0;

	/* the aggs send the amount of data they need to their source procs */
	for (i=0;i<numSourceProcs;i++) {
	    if ((remainingDataOffsetToGetPerProc[i] >= currentRoundFDStart) && (remainingDataOffsetToGetPerProc[i] <= currentRoundFDEnd)) {
		if ((remainingDataOffsetToGetPerProc[i] + remainingDataAmountToGetPerProc[i]) <= currentRoundFDEnd)
		    dataSizeGottenThisRoundPerProc[i] = remainingDataAmountToGetPerProc[i];
		else
		    dataSizeGottenThisRoundPerProc[i] = (currentRoundFDEnd - remainingDataOffsetToGetPerProc[i]) +1;
	    }
	    else if (((remainingDataOffsetToGetPerProc[i]+remainingDataAmountToGetPerProc[i]) >= currentRoundFDStart) && ((remainingDataOffsetToGetPerProc[i]+remainingDataAmountToGetPerProc[i]) <= currentRoundFDEnd)) {
		if ((remainingDataOffsetToGetPerProc[i]) >= currentRoundFDStart)
		    dataSizeGottenThisRoundPerProc[i] = remainingDataAmountToGetPerProc[i];
		else
		    dataSizeGottenThisRoundPerProc[i] = (remainingDataOffsetToGetPerProc[i]-currentRoundFDStart) +1;
	    }
	    else
		dataSizeGottenThisRoundPerProc[i] = 0;

#ifdef p2pcontigtrace
	    printf("dataSizeGottenThisRoundPerProc[%d] set to %d - remainingDataOffsetToGetPerProc is %d remainingDataAmountToGetPerProc is %d currentRoundFDStart is %d currentRoundFDEnd is %d\n",i,dataSizeGottenThisRoundPerProc[i],remainingDataOffsetToGetPerProc[i],remainingDataAmountToGetPerProc[i],currentRoundFDStart,currentRoundFDEnd);
#endif
	    if (dataSizeGottenThisRoundPerProc[i] > 0) {
		if (roundIter > 0) {
		    MPI_Isend(&dataSizeGottenThisRoundPerProc[i],1,MPI_INT,
			    sourceProcsForMyData[i],0,fd->comm,
			    &mpiSendDataSizeRequest[numSourceProcsSentData]);
		    numSourceProcsSentData++;
		}
	    }
	}

	int numDataSendToWaitFor = 0;
	/* the source procs send the requested data to the aggs */
	for (i = 0; i < numTargetAggsThisRound; i++) {

		/* the source procs aren't pinged by the target aggs on the first round */
	    if (roundIter > 0) {

		MPI_Waitany(numTargetAggsThisRound,mpiSizeToSendRequest,
			&irecv,&mpiWaitAnyStatusFromTargetAggs);

#ifdef p2pcontigtrace
		printf("irecv is %d amountOfDataReqestedByTargetAgg is %d bufferAmountsToSendThisRound is %d sendBufferOffsetsThisRound is %d targetAggsForMyData is %d\n",irecv,amountOfDataReqestedByTargetAgg[irecv], bufferAmountsToSendThisRound[irecv], sendBufferOffsetsThisRound[irecv],targetAggsForMyData[targetAggIndexesForMyDataThisRound[irecv]]);
#endif
		ADIOI_Assert(amountOfDataReqestedByTargetAgg[irecv] == bufferAmountsToSendThisRound[irecv]);
		MPI_Isend(&((char*)buf)[sendBufferOffsetsThisRound[irecv]],
			bufferAmountsToSendThisRound[irecv],MPI_BYTE,
			targetAggsForMyData[targetAggIndexesForMyDataThisRound[irecv]],
			0,fd->comm,&mpiSendDataToTargetAggRequest[irecv]);

	    }
	    else {
#ifdef p2pcontigtrace
		printf("i is %d bufferAmountsToSendThisRound is %d sendBufferOffsetsThisRound is %d targetAggsForMyData is %d\n",i, bufferAmountsToSendThisRound[i], sendBufferOffsetsThisRound[i],targetAggsForMyData[targetAggIndexesForMyDataThisRound[i]]);
#endif
		MPI_Isend(&((char*)buf)[sendBufferOffsetsThisRound[i]],bufferAmountsToSendThisRound[i],MPI_BYTE,
			targetAggsForMyData[targetAggIndexesForMyDataThisRound[i]],0,fd->comm,&mpiSendDataToTargetAggRequest[i]);
	    }
        numDataSendToWaitFor++;
	}

#ifdef ROMIO_GPFS
	gpfsmpio_prof_cw[GPFSMPIO_CIO_T_DEXCH_SETUP] += (endTimeBase-startTimeBase);
	startTimeBase = MPI_Wtime();
#endif

	/* the aggs receive the data from the source procs */
	int numDataRecvToWaitFor = 0;
	for (i=0;i<numSourceProcs;i++) {

	    int currentWBOffset = 0;
	    for (j=0;j<i;j++)
		currentWBOffset += dataSizeGottenThisRoundPerProc[j];

	    /* only receive from source procs that will send > 0 count data */
	    if (dataSizeGottenThisRoundPerProc[i] > 0) {
#ifdef p2pcontigtrace
		printf("receiving data from rank %d dataSizeGottenThisRoundPerProc is %d currentWBOffset is %d\n",sourceProcsForMyData[i],dataSizeGottenThisRoundPerProc[i],currentWBOffset);
#endif
		MPI_Irecv(&((char*)write_buf)[currentWBOffset],dataSizeGottenThisRoundPerProc[i],
			MPI_BYTE,sourceProcsForMyData[i],0,
			fd->comm,&mpiRecvDataRequest[numDataRecvToWaitFor]);
		mpiRequestMapPerProc[numDataRecvToWaitFor] = i;
		numDataRecvToWaitFor++;
	    }

#ifdef p2pcontigtrace
	    printf("MPI_Irecv from rank %d\n",targetAggsForMyData[i]);
#endif
	}

	int totalDataReceivedThisRound = 0;
	for (i = 0; i < numDataRecvToWaitFor; i++) {
	    MPI_Waitany(numDataRecvToWaitFor,mpiRecvDataRequest,
		    &irecv,&mpiWaitAnyStatusFromSourceProcs);
	    totalDataReceivedThisRound +=
		dataSizeGottenThisRoundPerProc[mpiRequestMapPerProc[irecv]];
	    totalAmountDataReceived +=
		dataSizeGottenThisRoundPerProc[mpiRequestMapPerProc[irecv]];

#ifdef p2pcontigtrace
	    printf("numDataRecvToWaitFor is %d was sent %d bytes data for %d remaining bytes from rank %d irecv index %d\n",numDataRecvToWaitFor,dataSizeGottenThisRoundPerProc[mpiRequestMapPerProc[irecv]],remainingDataAmountToGetPerProc[mpiRequestMapPerProc[irecv]],sourceProcsForMyData[mpiRequestMapPerProc[irecv]],irecv);
#endif
	    remainingDataAmountToGetPerProc[mpiRequestMapPerProc[irecv]] -=
		dataSizeGottenThisRoundPerProc[mpiRequestMapPerProc[irecv]];
	    remainingDataOffsetToGetPerProc[mpiRequestMapPerProc[irecv]] +=
		dataSizeGottenThisRoundPerProc[mpiRequestMapPerProc[irecv]];

	}

	/* clean up the MPI_Request object for the MPI_Isend which told the
	 * source procs how much data to send */
        for (i=0;i<numSourceProcsSentData;i++) {
           MPI_Waitany(numSourceProcsSentData,mpiSendDataSizeRequest,
		   &isend,&mpiIsendStatusForSize);
        }


#ifdef ROMIO_GPFS
        endTimeBase = MPI_Wtime();
	gpfsmpio_prof_cw[GPFSMPIO_CIO_T_DEXCH_NET] += (endTimeBase-startTimeBase);
#endif
	/* the aggs now write the data */
	if (numDataRecvToWaitFor > 0) {

#ifdef p2pcontigtrace
	    printf("totalDataReceivedThisRound is %d\n",totalDataReceivedThisRound);
#endif
	    if (!useIOBuffer) {

		ADIO_WriteContig(fd, write_buf, (int)totalDataReceivedThisRound,
			MPI_BYTE, ADIO_EXPLICIT_OFFSET,
			currentRoundFDStart, &status, error_code);
	    } else { /* use the thread writer */

		if(!pthread_equal(io_thread, pthread_self())) {
		    pthread_join(io_thread, &thread_ret);
		    *error_code = *(int *)thread_ret;
		    if (*error_code != MPI_SUCCESS) return;
		    io_thread = pthread_self();

		}
		io_thread_args.fd = fd;
		/* do a little pointer shuffling: background I/O works from one
		 * buffer while two-phase machinery fills up another */

		if (currentWriteBuf == 0) {
		    io_thread_args.buf = write_buf0;
		    currentWriteBuf = 1;
		    write_buf = write_buf1;
		}
		else {
		    io_thread_args.buf = write_buf1;
		    currentWriteBuf = 0;
		    write_buf = write_buf0;
		}
		io_thread_args.io_kind = ADIOI_WRITE;
		io_thread_args.size = totalDataReceivedThisRound;
		io_thread_args.offset = currentRoundFDStart;
		io_thread_args.status = status;
		io_thread_args.error_code = *error_code;
		if ( (pthread_create(&io_thread, NULL,
				ADIOI_IO_Thread_Func, &(io_thread_args))) != 0)
		    io_thread = pthread_self();

	    }

	} /* numDataRecvToWaitFor > 0 */

	if (iAmUsedAgg)
	    currentRoundFDStart += coll_bufsize;
        for (i = 0; i < numDataSendToWaitFor; i++) {
          MPI_Wait(&mpiSendDataToTargetAggRequest[i],
		  &mpiIsendStatusForData);
        }

    } /* for-loop roundIter */

#ifdef ROMIO_GPFS
    endTimeBase = MPI_Wtime();
    gpfsmpio_prof_cw[GPFSMPIO_CIO_T_DEXCH] += (endTimeBase-startTimeBase);
#endif

    if (useIOBuffer) { /* thread writer cleanup */

	if ( !pthread_equal(io_thread, pthread_self()) ) {
	    pthread_join(io_thread, &thread_ret);
	    *error_code = *(int *)thread_ret;
	}

    }



    if (iAmUsedAgg) {
	ADIOI_Free(sourceProcsForMyData);
	ADIOI_Free(remainingDataAmountToGetPerProc);
	ADIOI_Free(remainingDataOffsetToGetPerProc);
    }

    ADIOI_Free(targetAggsForMyData);
    ADIOI_Free(targetAggsForMyDataFDStart);
    ADIOI_Free(targetAggsForMyDataFDEnd);
    ADIOI_Free(targetAggIndexesForMyDataThisRound);
    ADIOI_Free(sendBufferOffsetsThisRound);
    ADIOI_Free(bufferAmountsToSendThisRound);
    ADIOI_Free(amountOfDataReqestedByTargetAgg);
    ADIOI_Free(mpiSizeToSendRequest);
    ADIOI_Free(mpiRecvDataRequest);
    ADIOI_Free(mpiSendDataSizeRequest);
    ADIOI_Free(mpiSendDataToTargetAggRequest);
    ADIOI_Free(dataSizeGottenThisRoundPerProc);
    ADIOI_Free(mpiRequestMapPerProc);

    /* TODO: still need a barrier here? */
    MPI_Barrier(fd->comm);
    return;
}

void ADIOI_P2PContigReadAggregation(ADIO_File fd,
	const void *buf,
	int *error_code,
	ADIO_Offset *st_offsets,
	ADIO_Offset *end_offsets,
	ADIO_Offset *fd_start,
	ADIO_Offset* fd_end)
{

    *error_code = MPI_SUCCESS; /* initialize to success */

#ifdef ROMIO_GPFS
    double startTimeBase,endTimeBase;
#endif

    MPI_Status status;
    pthread_t io_thread;
    void *thread_ret;
    ADIOI_IO_ThreadFuncData io_thread_args;

#ifdef ROMIO_GPFS
    startTimeBase = MPI_Wtime();
#endif

    int nprocs,myrank;
    MPI_Comm_size(fd->comm, &nprocs);
    MPI_Comm_rank(fd->comm, &myrank);

    ADIO_Offset myOffsetStart = st_offsets[myrank], myOffsetEnd = end_offsets[myrank];

    int myAggRank = -1; /* if I am an aggregor this is my index into fd->hints->ranklist */
    int iAmUsedAgg = 0;

    int naggs = fd->hints->cb_nodes;
    int coll_bufsize = fd->hints->cb_buffer_size;
#ifdef ROMIO_GPFS
    if (gpfsmpio_pthreadio == 1)
	/* share buffer between working threads */
	coll_bufsize = coll_bufsize/2;
#endif

    int j;
    for (j=0;j<naggs;j++) {
	if (fd->hints->ranklist[j] == myrank) {
	    myAggRank = j;
	    if (fd_end[j] > fd_start[j]) {
		iAmUsedAgg = 1;
	    }
	}
    }

    /* for my offset range determine how much data and from whom I need to get
     * it.  For source ag targets, also determine the source file domain
     * offsets locally to reduce communication overhead */
    int *sourceAggsForMyData = (int *)ADIOI_Malloc(naggs * sizeof(int));
    ADIO_Offset *sourceAggsForMyDataFDStart = (ADIO_Offset *)ADIOI_Malloc(naggs * sizeof(ADIO_Offset));
    ADIO_Offset *sourceAggsForMyDataFDEnd = (ADIO_Offset *)ADIOI_Malloc(naggs * sizeof(ADIO_Offset));
    int numSourceAggs = 0;
    int i;
    for (i=0;i<naggs;i++) {
	if ( ((myOffsetStart >= fd_start[i]) && (myOffsetStart <= fd_end[i])) ||
		((myOffsetEnd >= fd_start[i]) &&  (myOffsetEnd <= fd_end[i]))) {
	    sourceAggsForMyData[numSourceAggs] = fd->hints->ranklist[i];
	    sourceAggsForMyDataFDStart[numSourceAggs] = fd_start[i];
	    sourceAggsForMyDataFDEnd[numSourceAggs] = fd_end[i];
	    numSourceAggs++;
	}
    }

    /* these 3 arrays track info on the procs that are fed from an aggregtor -
     * to sacrifice some performance at setup to save on memory instead of
     * using max size of nprocs for the arrays could determine exact size first
     * and then allocate that size */
    int *targetProcsForMyData=NULL;
    int *remainingDataAmountToSendPerProc=NULL;
    ADIO_Offset *remainingDataOffsetToSendPerProc=NULL;

    int numTargetProcs = 0;

    if (iAmUsedAgg) {
	/* for the used aggregators figure out how much data I need from what procs */

	/* count numTargetProcs so we know how large to make the arrays */
	for (i=0;i<nprocs;i++)
	    if ( ((st_offsets[i] >= fd_start[myAggRank]) &&
			(st_offsets[i] <= fd_end[myAggRank])) ||
		    ((end_offsets[i] >= fd_start[myAggRank]) &&
		     (end_offsets[i] <= fd_end[myAggRank]))  )
		numTargetProcs++;

	targetProcsForMyData =
	    (int *)ADIOI_Malloc(numTargetProcs * sizeof(int));
	remainingDataAmountToSendPerProc =
	    (int *)ADIOI_Malloc(numTargetProcs * sizeof(int));
	remainingDataOffsetToSendPerProc =
	    (ADIO_Offset *)ADIOI_Malloc(numTargetProcs * sizeof(ADIO_Offset));

	/* everybody has the st_offsets and end_offsets for all ranks so if I am a
	 * used aggregator go thru them and figure out which ranks have data that
	 * falls into my file domain assigned to me */
	numTargetProcs = 0;
	for (i=0;i<nprocs;i++) {
	    if ( ((st_offsets[i] >= fd_start[myAggRank]) &&  (st_offsets[i] <= fd_end[myAggRank])) || ((end_offsets[i] >= fd_start[myAggRank]) &&  (end_offsets[i] <= fd_end[myAggRank]))) {
		targetProcsForMyData[numTargetProcs] = i;
		if ( ((st_offsets[i] >= fd_start[myAggRank]) &&  (st_offsets[i] <= fd_end[myAggRank])) && ((end_offsets[i] >= fd_start[myAggRank]) &&  (end_offsets[i] <= fd_end[myAggRank]))) {
		    remainingDataAmountToSendPerProc[numTargetProcs] = (end_offsets[i] - st_offsets[i])+1;
		    remainingDataOffsetToSendPerProc[numTargetProcs] = st_offsets[i];
		}
		else if ((st_offsets[i] >= fd_start[myAggRank]) &&  (st_offsets[i] <= fd_end[myAggRank])) {/* starts in this fd and goes past it */
		    remainingDataAmountToSendPerProc[numTargetProcs] = (fd_end[myAggRank] - st_offsets[i]) +1;
		    remainingDataOffsetToSendPerProc[numTargetProcs] = st_offsets[i];
		}
		else { /* starts in fd before this and ends in it */
		    remainingDataAmountToSendPerProc[numTargetProcs] = (end_offsets[i] - fd_start[myAggRank]) +1;
		    remainingDataOffsetToSendPerProc[numTargetProcs] = fd_start[myAggRank];
		}
		numTargetProcs++;
	    }
	}
    }


    MPI_Request *mpiRecvDataFromSourceAggsRequest = (MPI_Request *) ADIOI_Malloc(numSourceAggs * sizeof(MPI_Request));
    MPI_Request *mpiSendDataToTargetProcRequest = (MPI_Request *) ADIOI_Malloc(numTargetProcs * sizeof(MPI_Request));
    MPI_Status mpiWaitAnyStatusFromSourceProcs,mpiIsendStatusForData;

    /* use the two-phase buffer allocated in the file_open - no app should ever
     * be both reading and writing at the same time */
    char *read_buf0 = fd->io_buf;
    char *read_buf1 = fd->io_buf + coll_bufsize;
    /* if threaded i/o selected, we'll do a kind of double buffering */
    char *read_buf = read_buf0;

    /* compute number of rounds */
    ADIO_Offset numberOfRounds = (ADIO_Offset)((((ADIO_Offset)(end_offsets[nprocs-1]-st_offsets[0]))/((ADIO_Offset)((ADIO_Offset)coll_bufsize*(ADIO_Offset)naggs)))) + 1;

    ADIO_Offset currentRoundFDStart = 0, nextRoundFDStart = 0;
    ADIO_Offset currentRoundFDEnd = 0, nextRoundFDEnd = 0;

    if (iAmUsedAgg) {
	currentRoundFDStart = fd_start[myAggRank];
	nextRoundFDStart = fd_start[myAggRank];
    }

    int *dataSizeSentThisRoundPerProc = (int *)ADIOI_Malloc(numTargetProcs * sizeof(int));
    int *sourceAggIndexesForMyDataThisRound = (int *)ADIOI_Malloc(numSourceAggs * sizeof(int));
    int *recvBufferOffsetsThisRound = (int *)ADIOI_Malloc(numSourceAggs * sizeof(int));
    int *bufferAmountsToGetThisRound = (int *)ADIOI_Malloc(numSourceAggs * sizeof(int));
    *error_code = MPI_SUCCESS;

    int currentReadBuf = 0;
    int useIOBuffer = 0;
#ifdef ROMIO_GPFS
    if (gpfsmpio_pthreadio && (numberOfRounds>1)) {
	useIOBuffer = 1;
	io_thread = pthread_self();
    }
#endif

#ifdef ROMIO_GPFS
    endTimeBase = MPI_Wtime();
    gpfsmpio_prof_cw[GPFSMPIO_CIO_T_MYREQ] += (endTimeBase-startTimeBase);
#endif


    /* each iteration of this loop reads a coll_bufsize portion of the file domain */
    int roundIter;
    for (roundIter=0;roundIter<numberOfRounds;roundIter++) {

	int irecv,isend;
	/* determine what offsets define the portion of the file domain the agg is reading this round */
	if (iAmUsedAgg) {

	    currentRoundFDStart = nextRoundFDStart;

	    if (!useIOBuffer || (roundIter == 0)) {
		int amountDataToReadThisRound;
		if ((fd_end[myAggRank] - currentRoundFDStart) < coll_bufsize) {
		    currentRoundFDEnd = fd_end[myAggRank];
		    amountDataToReadThisRound = ((currentRoundFDEnd-currentRoundFDStart)+1);
		}
		else {
		    currentRoundFDEnd = currentRoundFDStart + coll_bufsize - 1;
		    amountDataToReadThisRound = coll_bufsize;
		}

		/* read currentRoundFDEnd bytes */
		ADIO_ReadContig(fd, read_buf,amountDataToReadThisRound,
			MPI_BYTE, ADIO_EXPLICIT_OFFSET, currentRoundFDStart,
			&status, error_code);
        currentReadBuf = 1;

#ifdef ROMIO_GPFS
		endTimeBase = MPI_Wtime();
#endif
	    }

	    if (useIOBuffer) { /* use the thread reader for the next round */
		/* switch back and forth between the read buffers so that the data aggregation code is diseminating 1 buffer while the thread is reading into the other */

		if (roundIter > 0)
		    currentRoundFDEnd = nextRoundFDEnd;

		if (roundIter < (numberOfRounds-1)) {
		    nextRoundFDStart += coll_bufsize;
		    int amountDataToReadNextRound;
		    if ((fd_end[myAggRank] - nextRoundFDStart) < coll_bufsize) {
			nextRoundFDEnd = fd_end[myAggRank];
			amountDataToReadNextRound = ((nextRoundFDEnd-nextRoundFDStart)+1);
		    }
		    else {
			nextRoundFDEnd = nextRoundFDStart + coll_bufsize - 1;
			amountDataToReadNextRound = coll_bufsize;
		    }

		    if(!pthread_equal(io_thread, pthread_self())) {
			pthread_join(io_thread, &thread_ret);
			*error_code = *(int *)thread_ret;
			if (*error_code != MPI_SUCCESS) return;
			io_thread = pthread_self();

		    }
		    io_thread_args.fd = fd;
		    /* do a little pointer shuffling: background I/O works from one
		     * buffer while two-phase machinery fills up another */

		    if (currentReadBuf == 0) {
			io_thread_args.buf = read_buf0;
			currentReadBuf = 1;
			read_buf = read_buf1;
		    }
		    else {
			io_thread_args.buf = read_buf1;
			currentReadBuf = 0;
			read_buf = read_buf0;
		    }
		    io_thread_args.io_kind = ADIOI_READ;
		    io_thread_args.size = amountDataToReadNextRound;
		    io_thread_args.offset = nextRoundFDStart;
		    io_thread_args.status = status;
		    io_thread_args.error_code = *error_code;
		    if ( (pthread_create(&io_thread, NULL,
				    ADIOI_IO_Thread_Func, &(io_thread_args))) != 0)
			io_thread = pthread_self();

		}
		else { /* last round */

		    if(!pthread_equal(io_thread, pthread_self())) {
			pthread_join(io_thread, &thread_ret);
			*error_code = *(int *)thread_ret;
			if (*error_code != MPI_SUCCESS) return;
			io_thread = pthread_self();

		    }
		    if (currentReadBuf == 0) {
			read_buf = read_buf1;
		    }
		    else {
			read_buf = read_buf0;
		    }

		}
	    } /* useIOBuffer */
	} /* IAmUsedAgg */

	/* determine what source aggs I need to get data from this round and
	 * recv only from them */
	int numSourceAggsThisRound = 0;
	for (i=0;i<numSourceAggs;i++) {
	    if ( ((myOffsetStart >= sourceAggsForMyDataFDStart[i]) && (myOffsetStart <= sourceAggsForMyDataFDEnd[i]))
		    || ((myOffsetEnd >= sourceAggsForMyDataFDStart[i]) && (myOffsetEnd <= sourceAggsForMyDataFDEnd[i])) ) {
		/* we know that we need to get data from this source agg at
		 * some point, now need to figure out how much this round */

		/* here are the offsets currently being sent by the aggregator
		 * during this round */
		ADIO_Offset currentRoundFDStartForMySourceAgg =
		    (ADIO_Offset)((ADIO_Offset)sourceAggsForMyDataFDStart[i] +
			    (ADIO_Offset)((ADIO_Offset)roundIter*(ADIO_Offset)coll_bufsize));
		ADIO_Offset currentRoundFDEndForMySourceAgg =
		    (ADIO_Offset)((ADIO_Offset)sourceAggsForMyDataFDStart[i] +
			    (ADIO_Offset)((ADIO_Offset)(roundIter+1)*(ADIO_Offset)coll_bufsize) - (ADIO_Offset)1);
		if (currentRoundFDEndForMySourceAgg > sourceAggsForMyDataFDEnd[i])
		    currentRoundFDEndForMySourceAgg = sourceAggsForMyDataFDEnd[i];

#ifdef p2pcontigtrace
		printf("roundIter %d source iter %d sourceAggsForMyData is %d myOffsetStart is %ld myOffsetEnd is %ld sourceAggsForMyDataFDStart is %ld sourceAggsForMyDataFDEnd is %ld currentRoundFDStartForMySourceAgg is %ld currentRoundFDEndForMySourceAgg is %ld\n",roundIter,i,sourceAggsForMyData[i],myOffsetStart,myOffsetEnd,sourceAggsForMyDataFDStart[i],sourceAggsForMyDataFDEnd[i],currentRoundFDStartForMySourceAgg,currentRoundFDEndForMySourceAgg);
#endif

		/* get the portion of my data that is within currentRoundFDStartForMySourceAgg to currentRoundFDEndForMySourceAgg */
		/* find the offset into the recv buffer and the amount of data to get */
		int recvBufferOffset = 0;
		int bufferAmountToGet = 0;

		if ((myOffsetStart >= currentRoundFDStartForMySourceAgg) && (myOffsetStart <= currentRoundFDEndForMySourceAgg)) {
		    if (myOffsetEnd > currentRoundFDEndForMySourceAgg)
			bufferAmountToGet = (currentRoundFDEndForMySourceAgg - myOffsetStart) +1;
		    else
			bufferAmountToGet = (myOffsetEnd - myOffsetStart) +1;
		}
		else if ((myOffsetEnd >= currentRoundFDStartForMySourceAgg) && (myOffsetEnd <= currentRoundFDEndForMySourceAgg)) {
		    recvBufferOffset = (int) (currentRoundFDStartForMySourceAgg - myOffsetStart);
		    if (myOffsetEnd > currentRoundFDEndForMySourceAgg)
			bufferAmountToGet = (currentRoundFDEndForMySourceAgg - currentRoundFDStartForMySourceAgg) +1;
		    else
			bufferAmountToGet = (myOffsetEnd - currentRoundFDStartForMySourceAgg) +1;
		}
		else if ((myOffsetStart <= currentRoundFDStartForMySourceAgg) && (myOffsetEnd >= currentRoundFDEndForMySourceAgg)) {
		    recvBufferOffset = (int) (currentRoundFDStartForMySourceAgg - myOffsetStart);
		    bufferAmountToGet = (currentRoundFDEndForMySourceAgg - currentRoundFDStartForMySourceAgg) +1;
		}


		if (bufferAmountToGet > 0) { /* we have data to get this round */
		    sourceAggIndexesForMyDataThisRound[numSourceAggsThisRound] = i;
		    recvBufferOffsetsThisRound[numSourceAggsThisRound] = recvBufferOffset;
		    bufferAmountsToGetThisRound[numSourceAggsThisRound] = bufferAmountToGet;
#ifdef p2pcontigtrace
		    printf("bufferAmountToGet is %d recvBufferOffset is %d\n",bufferAmountToGet,recvBufferOffset);
#endif
		    numSourceAggsThisRound++;
		}
	    }
	}

	/* the aggs determine the amount of data they will be sending to their
	 * source procs */
	for (i=0;i<numTargetProcs;i++) {
	    if ((remainingDataOffsetToSendPerProc[i] >= currentRoundFDStart) &&
		    (remainingDataOffsetToSendPerProc[i] <= currentRoundFDEnd)) {
		if ((remainingDataOffsetToSendPerProc[i] +
			    remainingDataAmountToSendPerProc[i]) <= currentRoundFDEnd)
		    dataSizeSentThisRoundPerProc[i] = remainingDataAmountToSendPerProc[i];
		else
		    dataSizeSentThisRoundPerProc[i] =
			(currentRoundFDEnd - remainingDataOffsetToSendPerProc[i]) +1;
	    }
	    else if (((remainingDataOffsetToSendPerProc[i]+
			    remainingDataAmountToSendPerProc[i]) >=
			currentRoundFDStart) &&
		    ((remainingDataOffsetToSendPerProc[i]+
		      remainingDataAmountToSendPerProc[i]) <= currentRoundFDEnd)) {
		if ((remainingDataOffsetToSendPerProc[i]) >= currentRoundFDStart)
		    dataSizeSentThisRoundPerProc[i] = remainingDataAmountToSendPerProc[i];
		else
		    dataSizeSentThisRoundPerProc[i] =
			(remainingDataOffsetToSendPerProc[i]-currentRoundFDStart) +1;
	    }
	    else
		dataSizeSentThisRoundPerProc[i] = 0;

	}

	/* the target procs get the data from the source aggs */
	for (i = 0; i < numSourceAggsThisRound; i++) {
	    MPI_Irecv(&((char*)buf)[recvBufferOffsetsThisRound[i]],
		    bufferAmountsToGetThisRound[i],MPI_BYTE,
		    sourceAggsForMyData[sourceAggIndexesForMyDataThisRound[i]],0,fd->comm,
		    &mpiRecvDataFromSourceAggsRequest[i]);
	}

	/* the source aggs send the data to the target procs */
	int numTargetProcsSentThisRound = 0;
	for (i=0;i<numTargetProcs;i++) {

	    int currentWBOffset = 0;
	    for (j=0;j<i;j++)
		currentWBOffset += dataSizeSentThisRoundPerProc[j];

	    /* only send to target procs that will recv > 0 count data */
	    if (dataSizeSentThisRoundPerProc[i] > 0) {
		MPI_Isend(&((char*)read_buf)[currentWBOffset],
			dataSizeSentThisRoundPerProc[i],
			MPI_BYTE,targetProcsForMyData[i],0,
			fd->comm,&mpiSendDataToTargetProcRequest[numTargetProcsSentThisRound]);
		numTargetProcsSentThisRound++;
		remainingDataAmountToSendPerProc[i] -= dataSizeSentThisRoundPerProc[i];
		remainingDataOffsetToSendPerProc[i] += dataSizeSentThisRoundPerProc[i];
	    }
	}

	/* wait for the target procs to get their data */
	for (i = 0; i < numSourceAggsThisRound; i++) {
	    MPI_Waitany(numSourceAggsThisRound,mpiRecvDataFromSourceAggsRequest,
		    &irecv,&mpiWaitAnyStatusFromSourceProcs);
	}

	nextRoundFDStart = currentRoundFDStart + coll_bufsize;

        /* clean up the MPI_Isend MPI_Requests */
        for (i=0;i<numTargetProcsSentThisRound;i++) {
          MPI_Waitany(numTargetProcsSentThisRound,mpiSendDataToTargetProcRequest,
		  &isend,&mpiIsendStatusForData);
        }

	MPI_Barrier(fd->comm); /* need to sync up the source aggs which did the isend with the target procs which did the irecvs to give the target procs time to get the data before overwriting with next round readcontig */

    } /* for-loop roundIter */

    if (useIOBuffer) { /* thread reader cleanup */

	if ( !pthread_equal(io_thread, pthread_self()) ) {
	    pthread_join(io_thread, &thread_ret);
	    *error_code = *(int *)thread_ret;
	}
    }

    if (iAmUsedAgg) {
	ADIOI_Free(targetProcsForMyData);
	ADIOI_Free(remainingDataAmountToSendPerProc);
	ADIOI_Free(remainingDataOffsetToSendPerProc);
    }

    ADIOI_Free(sourceAggsForMyData);
    ADIOI_Free(sourceAggsForMyDataFDStart);
    ADIOI_Free(sourceAggsForMyDataFDEnd);

    ADIOI_Free(mpiRecvDataFromSourceAggsRequest);
    ADIOI_Free(mpiSendDataToTargetProcRequest);
    ADIOI_Free(dataSizeSentThisRoundPerProc);
    ADIOI_Free(sourceAggIndexesForMyDataThisRound);
    ADIOI_Free(recvBufferOffsetsThisRound);
    ADIOI_Free(bufferAmountsToGetThisRound);

    /* TODO: is Barrier here needed? */
    MPI_Barrier(fd->comm);

    return;

}
