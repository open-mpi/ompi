/*
 * Copyright (c) 2004-2005 The Trustees of Indiana University and Indiana
 *                         University Research and Technology
 *                         Corporation.  All rights reserved.
 * Copyright (c) 2004-2011 The University of Tennessee and The University
 *                         of Tennessee Research Foundation.  All rights
 *                         reserved.
 * Copyright (c) 2004-2015 High Performance Computing Center Stuttgart,
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
#include "ompi/mca/fs/base/base.h"

#include <unistd.h>
#include <string.h>

#include <gpfs.h>
#include <fcntl.h>
#include <errno.h>
#include <gpfs_fcntl.h>

/*
 *  file_set_info_gpfs
 *
 *  Function:   - set_info of a file
 *  Accepts:    - same arguments as MPI_File_set_info()
 *  Returns:    - Success if info is set
 */

int mca_fs_gpfs_file_set_info(ompio_file_t *fh, struct ompi_info_t *info)
{
    int rc = 0;
    int flag;
    int valueLen = MPI_MAX_INFO_VAL;
    char value[MPI_MAX_INFO_VAL + 1];
    char gpfsHintsKey[50];
    const char* split = ",";
    char* token;
    int ret = OMPI_SUCCESS;
    ompi_info_t *info_selected;
    info_selected = info;
    gpfs_file_t gpfs_file_handle = fh->fd;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsAccessRange_t gpfsAccessRange;
    } gpfs_hint_AccessRange;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsFreeRange_t gpfsFreeRange;
    } gpfs_hint_FreeRange;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsClearFileCache_t gpfsClearFileCache;
    } gpfs_hint_ClearFileCache;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsCancelHints_t gpfsCancelHints;
    } gpfs_hint_CancelHints;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsSetReplication_t gpfsSetReplication;
    } gpfs_hint_SetReplication;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsByteRange_t gpfsByteRange;
    } gpfs_hint_ByteRange;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsRestripeData_t gpfsRestripeData;
    } gpfs_hint_RestripeData;

    //CN: TODO: Implement the following currently unused GPFS hints
    /*
    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsRestripeRange_t gpfsRestripeRange;
    } gpfs_hint_RestripeRange;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsGetReplication_t gpfsGetReplication;
    } gpfs_hint_GetReplication;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsGetStoragePool_t gpfsGetStoragePool;
    } gpfs_hint_GetStoragePool;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsGetFilesetName_t gpfsGetFilesetName;
    } gpfs_hint_GetFilesetName;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsGetSnapshotName_t gpfsGetSnapshotName;
    } gpfs_hint_GetSnapshotName;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsSetImmutable_t gpfsSetImmutable;
    } gpfs_hint_SetImmutable;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsGetImmutable_t gpfsGetImmutable;
    } gpfs_hint_GetImmutable;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsSetExpTime_t gpfsSetExpTime;
    } gpfs_hint_SetExpTime;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsGetExpTime_t gpfsGetExpTime;
    } gpfs_hint_GetExpTime;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsSetAppendOnly_t gpfsSetAppendOnly;
    } gpfs_hint_SetAppendOnly;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsGetAppendOnly_t gpfsGetAppendOnly;
    } gpfs_hint_GetAppendOnly;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsSetStoragePool_t gpfsSetStoragePool;
    } gpfs_hint_SetStoragePool;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsRangeArray_t gpfsRangeArray;
    } gpfs_hint_RangeArray;

    struct {
        gpfsFcntlHeader_t gpfsFcntlHeader;
        gpfsMultipleAccessRange_t gpfsMultipleAccessRange;
    } gpfs_hint_MultipleAccessRange;
    */

    strcpy(gpfsHintsKey, "useSIOXLib");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag) {
        if(strcmp(value, "true") == 0) {
            //using the SIOX lib and the I/O pattern selection
            ret = mca_fs_gpfs_io_selection(fh, info, info_selected);
            if (ret != OMPI_SUCCESS)
                return ret;
        }
        else {
            //CN: Is there something left to do here?
            //TODO Sending the MPI_INFO to SIOX for knowledgebase
        }
    }

    //Setting GPFS Hint - gpfsAccessRange
    strcpy(gpfsHintsKey, "gpfsAccessRange");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag) {
        opal_output(ompi_fs_base_framework.framework_output,
                    "GPFS Access Range is set: %s: %s\n", gpfsHintsKey, value);
        gpfs_hint_AccessRange.gpfsFcntlHeader.totalLength = sizeof(gpfs_hint_AccessRange);
        gpfs_hint_AccessRange.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        gpfs_hint_AccessRange.gpfsFcntlHeader.fcntlReserved = 0;

        gpfs_hint_AccessRange.gpfsAccessRange.structLen =
                sizeof(gpfs_hint_AccessRange.gpfsAccessRange);
        gpfs_hint_AccessRange.gpfsAccessRange.structType = GPFS_ACCESS_RANGE;
        token = strtok(value, split);
        gpfs_hint_AccessRange.gpfsAccessRange.start = atol(token);
        token = strtok(NULL, split);
        gpfs_hint_AccessRange.gpfsAccessRange.length = atol(token);
        token = strtok(NULL, split);
        gpfs_hint_AccessRange.gpfsAccessRange.isWrite = atoi(token);

        rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hint_AccessRange);
        if (rc != 0) {
            rc = errno;
            opal_output(ompi_fs_base_framework.framework_output,
                    "gpfs_hint_AccessRange gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
                    gpfs_file_handle, rc, strerror(rc));
            ret = OMPI_ERROR;
        }
    }

    //Setting GPFS Hint - gpfsFreeRange
    strcpy(gpfsHintsKey, "gpfsFreeRange");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag) {
        opal_output(ompi_fs_base_framework.framework_output,
                    "GPFS Free Range is set: %s: %s\n", gpfsHintsKey, value);
        gpfs_hint_FreeRange.gpfsFcntlHeader.totalLength = sizeof(gpfs_hint_FreeRange);
        gpfs_hint_FreeRange.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        gpfs_hint_FreeRange.gpfsFcntlHeader.fcntlReserved = 0;

        gpfs_hint_FreeRange.gpfsFreeRange.structLen =
                sizeof(gpfs_hint_FreeRange.gpfsFreeRange);
        gpfs_hint_FreeRange.gpfsFreeRange.structType = GPFS_FREE_RANGE;
        token = strtok(value, split);
        gpfs_hint_FreeRange.gpfsFreeRange.start = atol(token);
        token = strtok(NULL, split);
        gpfs_hint_FreeRange.gpfsFreeRange.length = atol(token);

        rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hint_FreeRange);
        if (rc != 0) {
            rc = errno;
            opal_output(ompi_fs_base_framework.framework_output,
                    "gpfs_hint_FreeRange gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
                    gpfs_file_handle, rc, strerror(rc));
            ret = OMPI_ERROR;
        }
    }

    //CN: TODO
    //Setting GPFS Hint - gpfsRangeArray
    //Setting GPFS Hint - gpfsMultipleAccessRange

    //Setting GPFS Hint - gpfsClearFileCache
    strcpy(gpfsHintsKey, "gpfsClearFileCache");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag & (strcmp(value, "true") == 0)) {
        opal_output(ompi_fs_base_framework.framework_output,
                    "GPFS Clear File Cache is set: %s: %s\n", gpfsHintsKey, value);
        gpfs_hint_ClearFileCache.gpfsFcntlHeader.totalLength = sizeof(gpfs_hint_ClearFileCache);
        gpfs_hint_ClearFileCache.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        gpfs_hint_ClearFileCache.gpfsFcntlHeader.fcntlReserved = 0;

        gpfs_hint_ClearFileCache.gpfsClearFileCache.structLen =
                sizeof(gpfs_hint_ClearFileCache.gpfsClearFileCache);
        gpfs_hint_ClearFileCache.gpfsClearFileCache.structType = GPFS_CLEAR_FILE_CACHE;

        rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hint_ClearFileCache);
        if (rc != 0) {
            rc = errno;
            opal_output(ompi_fs_base_framework.framework_output,
                    "gpfs_hint_ClearFileCache gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
                    gpfs_file_handle, rc, strerror(rc));
            ret = OMPI_ERROR;
        }
    }

    //Setting GPFS Hint - gpfsCancelHints
    strcpy(gpfsHintsKey, "gpfsCancelHints");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag & (strcmp(value, "true") == 0)) {
        opal_output(ompi_fs_base_framework.framework_output,
                    "GPFS Cancel Hints is set: %s: %s\n", gpfsHintsKey, value);
        gpfs_hint_CancelHints.gpfsFcntlHeader.totalLength = sizeof(gpfs_hint_CancelHints);
        gpfs_hint_CancelHints.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        gpfs_hint_CancelHints.gpfsFcntlHeader.fcntlReserved = 0;

        gpfs_hint_CancelHints.gpfsCancelHints.structLen =
                sizeof(gpfs_hint_CancelHints.gpfsCancelHints);
        gpfs_hint_CancelHints.gpfsCancelHints.structType = GPFS_CANCEL_HINTS;

        rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hint_CancelHints);
        if (rc != 0) {
            rc = errno;
            opal_output(ompi_fs_base_framework.framework_output,
                    "gpfs_hint_CancelHints gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
                    gpfs_file_handle, rc, strerror(rc));
            ret = OMPI_ERROR;
        }
    }

    //Setting GPFS Hint - gpfsSetReplication
    strcpy(gpfsHintsKey, "gpfsSetReplication");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag) {
        opal_output(ompi_fs_base_framework.framework_output,
                    "GPFS Set Replication is set: %s: %s\n", gpfsHintsKey, value);
        gpfs_hint_SetReplication.gpfsFcntlHeader.totalLength = sizeof(gpfs_hint_SetReplication);
        gpfs_hint_SetReplication.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        gpfs_hint_SetReplication.gpfsFcntlHeader.fcntlReserved = 0;

        gpfs_hint_SetReplication.gpfsSetReplication.structLen =
                sizeof(gpfs_hint_SetReplication.gpfsSetReplication);
        gpfs_hint_SetReplication.gpfsSetReplication.structType = GPFS_FCNTL_SET_REPLICATION;
        token = strtok(value, split);
        gpfs_hint_SetReplication.gpfsSetReplication.metadataReplicas = atoi(token);
        gpfs_hint_SetReplication.gpfsSetReplication.maxMetadataReplicas = atoi(token);
        gpfs_hint_SetReplication.gpfsSetReplication.dataReplicas = atoi(token);
        gpfs_hint_SetReplication.gpfsSetReplication.maxDataReplicas = atoi(token);
        gpfs_hint_SetReplication.gpfsSetReplication.reserved = 0;

        rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hint_SetReplication);
        if (rc != 0) {
            rc = errno;
            opal_output(ompi_fs_base_framework.framework_output,
                    "gpfs_hint_SetReplication gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
                    gpfs_file_handle, rc, strerror(rc));
            ret = OMPI_ERROR;
        }
    }

    //CN: TODO
    //Setting GPFS Hint - gpfsSetStoragePool

    //Setting GPFS Hint - gpfsByteRange
    strcpy(gpfsHintsKey, "gpfsByteRange");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag) {
        opal_output(ompi_fs_base_framework.framework_output,
                    "GPFS Byte Range is set: %s: %s\n", gpfsHintsKey, value);
        gpfs_hint_ByteRange.gpfsFcntlHeader.totalLength = sizeof(gpfs_hint_ByteRange);
        gpfs_hint_ByteRange.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        gpfs_hint_ByteRange.gpfsFcntlHeader.fcntlReserved = 0;

        token = strtok(value, split);
        gpfs_hint_ByteRange.gpfsByteRange.startOffset = atol(token);
        token = strtok(value, split);
        gpfs_hint_ByteRange.gpfsByteRange.numOfBlks = atol(token);

        rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hint_ByteRange);
        if (rc != 0) {
            rc = errno;
            opal_output(ompi_fs_base_framework.framework_output,
                    "gpfs_hint_ByteRange gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
                    gpfs_file_handle, rc, strerror(rc));
            ret = OMPI_ERROR;
        }
    }

    //Setting GPFS Hint - gpfsRestripeData
    strcpy(gpfsHintsKey, "gpfsRestripeData");
    ompi_info_get(info_selected, gpfsHintsKey, valueLen, value, &flag);
    if (flag) {
        opal_output(ompi_fs_base_framework.framework_output,
                    "GPFS Restripe Data is set: %s: %s\n", gpfsHintsKey, value);
        gpfs_hint_RestripeData.gpfsFcntlHeader.totalLength = sizeof(gpfs_hint_RestripeData);
        gpfs_hint_RestripeData.gpfsFcntlHeader.fcntlVersion = GPFS_FCNTL_CURRENT_VERSION;
        gpfs_hint_RestripeData.gpfsFcntlHeader.fcntlReserved = 0;

        gpfs_hint_RestripeData.gpfsRestripeData.structLen =
                sizeof(gpfs_hint_RestripeData.gpfsRestripeData);
        gpfs_hint_RestripeData.gpfsRestripeData.structType = GPFS_FCNTL_RESTRIPE_DATA;
        token = strtok(value, split);
        gpfs_hint_RestripeData.gpfsRestripeData.options = atoi(token);
        gpfs_hint_RestripeData.gpfsRestripeData.reserved1 = 0;
        gpfs_hint_RestripeData.gpfsRestripeData.reserved2 = 0;

        rc = gpfs_fcntl(gpfs_file_handle, &gpfs_hint_RestripeData);
        if (rc != 0) {
            rc = errno;
            opal_output(ompi_fs_base_framework.framework_output,
                    "gpfs_hint_RestripeData gpfs_fcntl(file handle: %d): Error number is %d, %s\n",
                    gpfs_file_handle, rc, strerror(rc));
            ret = OMPI_ERROR;
        }
    }

    //CN: TODO
    //Setting GPFS Hint - gpfsRestripeRange
    //Setting GPFS Hint - gpfsGetReplication
    //Setting GPFS Hint - gpfsGetStoragePool
    //Setting GPFS Hint - gpfsGetFilesetName
    //Setting GPFS Hint - gpfsGetSnapshotName
    //Setting GPFS Hint - gpfsSetImmutable
    //Setting GPFS Hint - gpfsGetImmutable
    //Setting GPFS Hint - gpfsSetExpTime
    //Setting GPFS Hint - gpfsGetExpTime
    //Setting GPFS Hint - gpfsSetAppendOnly
    //Setting GPFS Hint - gpfsGetAppendOnly

    return ret;
}

//CN: Will this function set info keys with siox prefix?
//CN: Where shall the knowledge of the optimization of GPFS hints go? Into Open MPI or into SIOX?
//CN: Never ever exit! Open MPI requires error propagation.
int mca_fs_gpfs_io_selection(ompio_file_t *fh,
        struct ompi_info_t *info, struct ompi_info_t *info_selected) {

//CN: configure option to enable/disable SIOX support?
#ifdef HAVE_C_SIOX_H
    char value[MPI_MAX_INFO_VAL + 1], sioxHintsKey[50], optimal_value_str[MPI_MAX_INFO_VAL + 1];
    int rc = 0, valueLen = MPI_MAX_INFO_VAL, flag;
    //START SIOX initialization
    if (siox_gpfs_uiid == SIOX_INVALID_ID){
        siox_gpfs_uiid = siox_system_information_lookup_interface_id("MPI",
            "Generic");
        if (!siox_component_is_registered(siox_gpfs_uiid)){
            fprintf(stderr, "SIOX Component MPI Generic is NOT registered!\n");
            siox_gpfs_component = siox_component_register(siox_gpfs_uiid, "GPFS");
        }
        siox_gpfs_component_activity = siox_component_register_activity(
            siox_gpfs_uiid, "MPI_File_open");
    }
    //DEBUG: fprintf(stderr, "Beginning the SIOX_activity in mca_fs_gpfs_siox_io_selection()\n");

    fh->f_siox_component = siox_gpfs_component;
    fh->f_siox_activity = siox_activity_begin(siox_gpfs_component,
            siox_gpfs_component_activity);
    siox_activity_start(fh->f_siox_activity);
    //END SIOX initialization

    info_selected = info;

    //DEBUG: fprintf(stderr, "Starting setting the SIOX_activity_attribute\n");
//CN: Why we need all attributes as an dynamic array?
//CN: Only one element used at a time to be added to mpi_info object at a time
    siox_attribute **siox_attribute_array;
    //START Registering the SIOX activities' attributes
    //Make sure how many SIOX activities' attributes should be registered
    int i = 0;
    int number_of_info = opal_list_get_size(&(info_selected->super));
    //DEBUG: fprintf(stderr, "The size of number_of_info is: %d\n", number_of_info);

//CN: Why we need all attributes as an dynamic array?
//CN: Where is the corresponding free?
    siox_attribute_array = (siox_attribute **) malloc(
                           sizeof(siox_attribute*) * number_of_info);
    if (siox_attribute_array == 0) {
        //DEBUG: fprintf(stderr, "assign siox_attribute_array fail, out of memory!\n");
        return OMPI_ERROR;
    }
    //END Registering the SIOX activities' attributes
    // Setting the fileNameAttribute
    siox_attribute_array[i] = siox_ontology_register_attribute("MPI",
                "descriptor/filename", SIOX_STORAGE_STRING);
    siox_activity_set_attribute(fh->f_siox_activity,
                siox_attribute_array[i], fh->f_filename);
    i++;

//CN: Code duplication en mass (9 times same code block wich changing key!)
//CN: do this with a loop over a list of sioxHintsKeys
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
        //DEBUG: fprintf(stderr, "Setting sioxFreeRange hints to SIOX activity attribute.\n");
        siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
                "sioxFreeRange", SIOX_STORAGE_STRING);
        siox_activity_set_attribute(fh->f_siox_activity,
                siox_attribute_array[i], &value);
        if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
            //DEBUG: fprintf(stderr, "Getting optimal value of sioxFreeRange hints from SIOX: %s \n", optimal_value_str);
            ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
        }
        i++;
    }

    strcpy(sioxHintsKey, "sioxClearFileCache");
    ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
    if (flag) {
        //DEBUG: fprintf(stderr, "Setting sioxClearFileCache hints to SIOX activity attribute.\n");
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
        //DEBUG: fprintf(stderr, "Setting sioxCancelHints hints to SIOX activity attribute.\n");
        siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
                "sioxCancelHints", SIOX_STORAGE_STRING);
        siox_activity_set_attribute(fh->f_siox_activity,
                siox_attribute_array[i], &value);
        if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
            //DEBUG: fprintf(stderr, "Getting optimal value of sioxCancelHints hints from SIOX: %s \n", optimal_value_str);
            ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
        }
        i++;
    }

    strcpy(sioxHintsKey, "sioxDataShipStart");
    ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
    if (flag) {
        //DEBUG: fprintf(stderr, "Setting sioxDataShipStart hints to SIOX activity attribute.\n");
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
            //DEBUG: fprintf(stderr, "Getting optimal value of sioxDataShipStop hints from SIOX: %s \n", optimal_value_str);
            ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
        }
        i++;
    }

    strcpy(sioxHintsKey, "sioxSetReplication");
    ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
    if (flag) {
        //DEBUG: fprintf(stderr, "Setting sioxSetReplication hints to SIOX activity attribute.\n");
        siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
                "sioxSetReplication", SIOX_STORAGE_STRING);
        siox_activity_set_attribute(fh->f_siox_activity,
                siox_attribute_array[i], &value);
        if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
            //DEBUG: fprintf(stderr, "Getting optimal value of sioxSetReplication hints from SIOX: %s \n", optimal_value_str);
            ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
        }
        i++;
    }

    strcpy(sioxHintsKey, "sioxByteRange");
    ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
    if (flag) {
        //DEBUG: fprintf(stderr, "Setting sioxByteRange hints to SIOX activity attribute.\n");
        siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
                "sioxByteRange", SIOX_STORAGE_STRING);
        siox_activity_set_attribute(fh->f_siox_activity,
                siox_attribute_array[i], &value);
        if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
            //DEBUG: fprintf(stderr, "Getting optimal value of sioxByteRange hints from SIOX: %s \n", optimal_value_str);
            ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
        }
        i++;
    }

    strcpy(sioxHintsKey, "sioxRestripeData");
    ompi_info_get(info_selected, sioxHintsKey, valueLen, value, &flag);
    if (flag) {
        //DEBUG: fprintf(stderr, "Setting sioxRestripeData hints to SIOX activity attribute.\n");
        siox_attribute_array[i] = siox_ontology_register_attribute("GPFS",
                "sioxRestripeData", SIOX_STORAGE_STRING);
        siox_activity_set_attribute(fh->f_siox_activity,
                siox_attribute_array[i], &value);
        if(siox_suggest_optimal_value_for_str(fh->f_siox_component, siox_attribute_array[i], fh->f_siox_activity, optimal_value_str, valueLen)) {
            //DEBUG: fprintf(stderr, "Getting optimal value of sioxRestripeData hints from SIOX: %s \n", optimal_value_str);
            ompi_info_set(info_selected, sioxHintsKey, optimal_value_str);
        }
        i++;
    }

    //DEBUG: fprintf(stderr, "Stopping and ending the SIOX activity in mca_fs_gpfs_siox_io_selection()\n");
    siox_activity_stop(fh->f_siox_activity);
    siox_activity_end(fh->f_siox_activity);
#else
    info_selected = info;
#endif /* HAVE_C_SIOX_H */
    return OMPI_SUCCESS;
}
