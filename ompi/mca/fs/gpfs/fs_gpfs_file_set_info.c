/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2005 High Performance Computing Center Stuttgart, 
 *                         University of Stuttgart.  All rights reserved.
 * Copyright (c) 2004-2005 The Regents of the University of California.
 *                         All rights reserved.
 * Copyright (c) 2008-2011 University of Houston. All rights reserved.
 * $COPYRIGHT$
 * 
 * Additional copyrights may follow
 * 
 * $HEADER$
 */

#include "ompi_config.h"
#include "fs_gpfs.h"
#include "mpi.h"
#include "ompi/constants.h"
#include "ompi/mca/fs/fs.h"

#include <unistd.h>
#include <string.h>

#include <gpfs.h>
#include <fcntl.h>
#include <errno.h>
#include <gpfs_fcntl.h>

/*
 *	file_set_info_gpfs
 *
 *	Function:	- set_info of a file
 *	Accepts:	- same arguments as MPI_File_set_info()
 *	Returns:	- Success if info is set
 */

/*struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsAccessRange_t gpfsAccessRange;
	gpfsFreeRange_t gpfsFreeRange;
	gpfsRangeArray_t gpfsRangeArray;
	gpfsMultipleAccessRange_t gpfsMultipleAccessRange;
	gpfsClearFileCache_t gpfsClearFileCache;
	gpfsCancelHints_t gpfsCancelHints;
	gpfsDataShipStart_t gpfsDataShipStart;
	gpfsDataShipMap_t gpfsDataShipMap;
	gpfsDataShipMapVariable_t gpfsDataShipMapVariable;
	gpfsDataShipStop_t gpfsDataShipStop;
	gpfsSetReplication_t gpfsSetReplication;
	gpfsSetStoragePool_t gpfsSetStoragePool;
	gpfsByteRange_t gpfsByteRange;
	gpfsRestripeData_t gpfsRestripeData;
	gpfsRestripeRange_t gpfsRestripeRange;
	gpfsGetReplication_t gpfsGetReplication;
	gpfsGetStoragePool_t gpfsGetStoragePool;
	gpfsGetFilesetName_t gpfsGetFilesetName;
	gpfsGetSnapshotName_t gpfsGetSnapshotName;
	gpfsSetImmutable_t gpfsSetImmutable;
	gpfsGetImmutable_t gpfsGetImmutable;
	gpfsSetExpTime_t gpfsSetExpTime;
	gpfsGetExpTime_t gpfsGetExpTime;
	gpfsSetAppendOnly_t gpfsSetAppendOnly;
	gpfsGetAppendOnly_t gpfsGetAppendOnly;
} all_gpfs_hints;*/

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsAccessRange_t gpfsAccessRange;
} gpfs_hints_1;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsFreeRange_t gpfsFreeRange;
} gpfs_hints_2;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsRangeArray_t gpfsRangeArray;
} gpfs_hints_3;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsMultipleAccessRange_t gpfsMultipleAccessRange;
} gpfs_hints_4;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsClearFileCache_t gpfsClearFileCache;
} gpfs_hints_5;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsCancelHints_t gpfsCancelHints;
} gpfs_hints_6;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsDataShipStart_t gpfsDataShipStart;
} gpfs_hints_7;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsDataShipMap_t gpfsDataShipMap;
} gpfs_hints_8;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsDataShipMapVariable_t gpfsDataShipMapVariable;
} gpfs_hints_9;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsDataShipStop_t gpfsDataShipStop;
} gpfs_hints_10;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsSetReplication_t gpfsSetReplication;
} gpfs_hints_11;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsSetStoragePool_t gpfsSetStoragePool;
} gpfs_hints_12;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsByteRange_t gpfsByteRange;
} gpfs_hints_13;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsRestripeData_t gpfsRestripeData;
} gpfs_hints_14;

//TODO
/*struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsRestripeRange_t gpfsRestripeRange;
} gpfs_hints_15;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsGetReplication_t gpfsGetReplication;
} gpfs_hints_16;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsGetStoragePool_t gpfsGetStoragePool;
} gpfs_hints_17;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsGetFilesetName_t gpfsGetFilesetName;
} gpfs_hints_18;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsGetSnapshotName_t gpfsGetSnapshotName;
} gpfs_hints_19;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsSetImmutable_t gpfsSetImmutable;
} gpfs_hints_20;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsGetImmutable_t gpfsGetImmutable;
} gpfs_hints_21;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsSetExpTime_t gpfsSetExpTime;
} gpfs_hints_22;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsGetExpTime_t gpfsGetExpTime;
} gpfs_hints_23;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsSetAppendOnly_t gpfsSetAppendOnly;
} gpfs_hints_24;

struct {
	gpfsFcntlHeader_t gpfsFcntlHeader;
	gpfsGetAppendOnly_t gpfsGetAppendOnly;
} gpfs_hints_25;*/

int mca_fs_gpfs_file_set_info(mca_io_ompio_file_t *fh, struct ompi_info_t *info) {
	printf("GPFS SET INFO\n");
	int ret;
	ret = mca_fs_gpfs_prefetch_hints(fh->f_amode, fh, info);
	return ret;
}

int mca_fs_gpfs_prefetch_hints(int access_mode,
		mca_io_ompio_file_t *fh, struct ompi_info_t *info) {

	if (! (access_mode & MPI_MODE_RDONLY | access_mode & MPI_MODE_WRONLY
			| access_mode & MPI_MODE_RDWR)) {
		return OMPI_SUCCESS;
	}

	gpfs_file_t gpfs_file_handle = fh->fd;

	int rc = 0, valueLen = MPI_MAX_INFO_VAL, flag;
	char value[MPI_MAX_INFO_VAL + 1], gpfsHintsKey[50];
	const char* split = ",";
	char* token;
	int ret = OMPI_SUCCESS;
	ompi_info_t *info_selected;
	info_selected = info;

	strcpy(gpfsHintsKey, "useSIOXLib");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag && strcmp(value, "true") == 0) {
		//using the SIOX lib and the I/O pattern selection
		ret = mca_fs_gpfs_io_selection(fh, info, info_selected);
		if (ret != OMPI_SUCCESS)
			return ret;
	}
	else {
		//TODO Sending the MPI_INFO to SIOX for knowledgebase
	}

	//Setting GPFS Hints 1 - gpfsAccessRange
	strcpy(gpfsHintsKey, "sioxAccessRange");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Access Range is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_1.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_1);
		gpfs_hints_1.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_1.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_1.gpfsAccessRange.structLen =
				sizeof(gpfs_hints_1.gpfsAccessRange);
		gpfs_hints_1.gpfsAccessRange.structType = GPFS_ACCESS_RANGE;
		token = strtok(value, split);
		gpfs_hints_1.gpfsAccessRange.start = atol(token);
		token = strtok(NULL, split);
		gpfs_hints_1.gpfsAccessRange.length = atol(token);
		token = strtok(NULL, split);
		gpfs_hints_1.gpfsAccessRange.isWrite = atoi(token);

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_1);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_1 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 2 - gpfsFreeRange
	strcpy(gpfsHintsKey, "sioxFreeRange");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Free Range is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_2.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_2);
		gpfs_hints_2.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_2.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_2.gpfsFreeRange.structLen =
				sizeof(gpfs_hints_2.gpfsFreeRange);
		gpfs_hints_2.gpfsFreeRange.structType = GPFS_FREE_RANGE;
		token = strtok(value, split);
		gpfs_hints_2.gpfsFreeRange.start = atol(token);
		token = strtok(NULL, split);
		gpfs_hints_2.gpfsFreeRange.length = atol(token);

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_2);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_2 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 3 - gpfsRangeArray
	/*strcpy(gpfsHintsKey, "sioxFreeRange");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Free Range is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_2.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_2);
		gpfs_hints_2.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_2.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_2.gpfsFreeRange.structLen =
				sizeof(gpfs_hints_2.gpfsFreeRange);
		gpfs_hints_2.gpfsFreeRange.structType = GPFS_FREE_RANGE;
		token = strtok(value, split);
		gpfs_hints_2.gpfsFreeRange.start = atoi(token);
		token = strtok(NULL, split);
		gpfs_hints_2.gpfsFreeRange.length = atoi(token);

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_2);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_2 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 4 - gpfsMultipleAccessRange
	/*strcpy(gpfsHintsKey, "sioxFreeRange");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Free Range is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_2.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_2);
		gpfs_hints_2.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_2.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_2.gpfsFreeRange.structLen =
				sizeof(gpfs_hints_2.gpfsFreeRange);
		gpfs_hints_2.gpfsFreeRange.structType = GPFS_FREE_RANGE;
		token = strtok(value, split);
		gpfs_hints_2.gpfsFreeRange.start = atoi(token);
		token = strtok(NULL, split);
		gpfs_hints_2.gpfsFreeRange.length = atoi(token);

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_2);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_2 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 5 - gpfsClearFileCache
	strcpy(gpfsHintsKey, "sioxClearFileCache");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag & strcmp(value, "true") == 0) {
		printf("GPFS Clear File Cache is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_5.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_5);
		gpfs_hints_5.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_5.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_5.gpfsClearFileCache.structLen =
				sizeof(gpfs_hints_5.gpfsClearFileCache);
		gpfs_hints_5.gpfsClearFileCache.structType = GPFS_CLEAR_FILE_CACHE;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_5);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_5 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 6 - gpfsCancelHints
	strcpy(gpfsHintsKey, "sioxCancelHints");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag & strcmp(value, "true") == 0) {
		printf("GPFS Cancel Hints is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_6.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_6);
		gpfs_hints_6.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_6.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_6.gpfsCancelHints.structLen =
				sizeof(gpfs_hints_6.gpfsCancelHints);
		gpfs_hints_6.gpfsCancelHints.structType = GPFS_CANCEL_HINTS;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_6);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_6 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 7 - gpfsDataShipStart
	strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 8 - gpfsDataShipMap
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 9 - gpfsDataShipMapVariable
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 10 - gpfsDataShipStop
	strcpy(gpfsHintsKey, "sioxDataShipStop");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag & strcmp(value, "true") == 0) {
		printf("GPFS Data Ship Stop is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_10.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_10);
		gpfs_hints_10.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_10.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_10.gpfsDataShipStop.structLen =
				sizeof(gpfs_hints_10.gpfsDataShipStop);
		gpfs_hints_10.gpfsDataShipStop.structType = GPFS_DATA_SHIP_STOP;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_10);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_10 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 11 - gpfsSetReplication
	strcpy(gpfsHintsKey, "sioxSetReplication");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Set Replication is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_11.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_11);
		gpfs_hints_11.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_11.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_11.gpfsSetReplication.structLen =
				sizeof(gpfs_hints_11.gpfsSetReplication);
		gpfs_hints_11.gpfsSetReplication.structType = GPFS_FCNTL_SET_REPLICATION;
		token = strtok(value, split);
		gpfs_hints_11.gpfsSetReplication.metadataReplicas = atoi(token);
		gpfs_hints_11.gpfsSetReplication.maxMetadataReplicas = atoi(token);
		gpfs_hints_11.gpfsSetReplication.dataReplicas = atoi(token);
		gpfs_hints_11.gpfsSetReplication.maxDataReplicas = atoi(token);
		gpfs_hints_11.gpfsSetReplication.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_11);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_11 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 12 - gpfsSetStoragePool
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 13 - gpfsByteRange
	strcpy(gpfsHintsKey, "sioxByteRange");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Byte Range is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_13.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_13);
		gpfs_hints_13.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_13.gpfsFcntlHeader.fcntlReserved = 0;

		token = strtok(value, split);
		gpfs_hints_13.gpfsByteRange.startOffset = atol(token);
		token = strtok(value, split);
		gpfs_hints_13.gpfsByteRange.numOfBlks = atol(token);

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_13);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_13 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 14 - gpfsRestripeData
	strcpy(gpfsHintsKey, "sioxRestripeData");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Restripe Data is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_14.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_14);
		gpfs_hints_14.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_14.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_14.gpfsRestripeData.structLen =
				sizeof(gpfs_hints_14.gpfsRestripeData);
		gpfs_hints_14.gpfsRestripeData.structType = GPFS_FCNTL_RESTRIPE_DATA;
		token = strtok(value, split);
		gpfs_hints_14.gpfsRestripeData.options = atoi(token);
		gpfs_hints_14.gpfsRestripeData.reserved1 = 0;
		gpfs_hints_14.gpfsRestripeData.reserved2 = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_14);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_14 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}

	//Setting GPFS Hints 15 - gpfsRestripeRange
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 16 - gpfsGetReplication
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 17 - gpfsGetStoragePool
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 18 - gpfsGetFilesetName
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 19 - gpfsGetSnapshotName
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 20 - gpfsSetImmutable
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 21 - gpfsGetImmutable
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 22 - gpfsSetExpTime
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 23 - gpfsGetExpTime
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	//Setting GPFS Hints 24 - gpfsSetAppendOnly
	//Setting GPFS Hints 25 - gpfsGetAppendOnly
	/*strcpy(gpfsHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("GPFS Data Ship Start is set: %s: %s\n", gpfsHintsKey, value);
		gpfs_hints_7.gpfsFcntlHeader.totalLength = sizeof(gpfs_hints_7);
		gpfs_hints_7.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
		gpfs_hints_7.gpfsFcntlHeader.fcntlReserved = 0;

		gpfs_hints_7.gpfsDataShipStart.structLen =
				sizeof(gpfs_hints_7.gpfsDataShipStart);
		gpfs_hints_7.gpfsDataShipStart.structType = GPFS_DATA_SHIP_START;
		token = strtok(value, split);
		gpfs_hints_7.gpfsDataShipStart.numInstances = atoi(token);
		gpfs_hints_7.gpfsDataShipStart.reserved = 0;

		rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hints_7);
		if (rc != 0) {
			rc = errno;
			printf(
					"gpfs_hints_7 gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
					gpfs_file_handle, rc, strerror(rc));
			ret = OMPI_ERROR;
		}
	}*/

	return ret;
}

int mca_fs_gpfs_io_selection(mca_io_ompio_file_t *fh,
		struct ompi_info_t *info, struct ompi_info_t *info_selected) {

#ifdef HAVE_C_SIOX_H
	char value[MPI_MAX_INFO_VAL + 1], sioxHintsKey[50], optimal_value_str[MPI_MAX_INFO_VAL + 1];
	int rc = 0, valueLen = MPI_MAX_INFO_VAL, flag;
	//START SIOX initialization
	if (siox_gpfs_uiid == SIOX_INVALID_ID){
		siox_gpfs_uiid = siox_system_information_lookup_interface_id("MPI",
			"Generic");
		if (!siox_component_is_registered(siox_gpfs_uiid)){
			printf("SIOX Component MPI Generic is NOT registered!\n");
			siox_gpfs_component = siox_component_register(siox_gpfs_uiid, "GPFS");
		}
		siox_gpfs_component_activity = siox_component_register_activity(
			siox_gpfs_uiid, "MPI_File_open");
	}
	printf("Beginning the SIOX_activity in mca_fs_gpfs_siox_io_selection()\n");

	fh->f_siox_component = siox_gpfs_component;
	fh->f_siox_activity = siox_activity_begin(siox_gpfs_component,
			siox_gpfs_component_activity);
	siox_activity_start(fh->f_siox_activity);
	//END SIOX initialization

	info_selected = info;

	printf("Starting setting the SIOX_activity_attribute\n");
	siox_attribute **siox_attribute_array;
	//START Registering the SIOX activities' attributes
	//Make sure how many SIOX activities' attributes should be registered
	int i = 0;
	int number_of_info = opal_list_get_size(&(info_selected->super));
	printf("The size of number_of_info is: %d\n", number_of_info);
	siox_attribute_array = (siox_attribute **) malloc(
	                       sizeof(siox_attribute*) * number_of_info);
	if (siox_attribute_array == 0) {
		printf("assign siox_attribute_array fail, out of memory!\n");
		exit(0);
	}
	//END Registering the SIOX activities' attributes
	// Setting the fileNameAttribute
	siox_attribute_array[i] = siox_ontology_register_attribute("MPI",
				"descriptor/filename", SIOX_STORAGE_STRING);
	siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], fh->f_filename);
	i++;

	//START setting the siox activity attributes
	strcpy(sioxHintsKey, "sioxAccessRange");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxAccessRange hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxAccessRange", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxAccessRange hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxFreeRange");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxFreeRange hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxFreeRange", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxFreeRange hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxClearFileCache");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxClearFileCache hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxClearFileCache", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxClearFileCache hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxCancelHints");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxCancelHints hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxCancelHints", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxCancelHints hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxDataShipStart");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxDataShipStart hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxDataShipStart", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxDataShipStart hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxDataShipStop");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxDataShipStop hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxDataShipStop", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxDataShipStop hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxSetReplication");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxSetReplication hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxSetReplication", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxSetReplication hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxByteRange");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxByteRange hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxByteRange", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxByteRange hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}

	strcpy(sioxHintsKey, "sioxRestripeData");
	ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
	if (flag) {
		printf("Setting sioxRestripeData hints to SIOX activity attribute.\n");
		siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
				"sioxRestripeData", SIOX_STORAGE_STRING);
		siox_activity_set_attribute(fh->f_siox_activity,
				siox_attribute_array[i], &value);
		if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
			printf("Getting optimal value of sioxRestripeData hints from SIOX: %s \n", optimal_value_str);
			ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
		}
		i++;
	}
	
	printf("Stopping and ending the SIOX activity in mca_fs_gpfs_siox_io_selection()\n");
	siox_activity_stop(fh->f_siox_activity);
	siox_activity_end(fh->f_siox_activity);
#else
	info_selected = info;
#endif
	return OMPI_SUCCESS;
}
