/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_File.h
 * 
 *  @brief Provides a low-level API for accessing files.
 *
 *  \ingroup internal
 */


#ifndef OTF_FILE_H
#define OTF_FILE_H


#include "OTF_FileManager.h"
#include "OTF_Filenames.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/** mode determining what to do with a file */
enum enum_OTF_FileMode {

	OTF_FILEMODE_NOTHING = 0,
	OTF_FILEMODE_READ= 1, 
	OTF_FILEMODE_WRITE= 2, 
	OTF_FILEMODE_SEEK= 3
};
typedef enum enum_OTF_FileMode OTF_FileMode;

/** status of a file */
enum enum_OTF_FileStatus {

	OTF_FILESTATUS_ACTIVE= 1, 
	OTF_FILESTATUS_SUSPENDED= 2,
	OTF_FILESTATUS_CLOSED= 3
};
typedef enum enum_OTF_FileStatus OTF_FileStatus;

/** initialize a OTF_File object */
void OTF_File_init( OTF_File* o );

/** finalize a OTF_File object */
void OTF_File_finalize( OTF_File* o );

/** open an OTF_File */
OTF_File* OTF_File_open( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode );

/** OTF_File to an OTF_File */
size_t OTF_File_write( OTF_File* file, const void* ptr, size_t size );

/** read from an OTF_File */
size_t OTF_File_read( OTF_File* file, void* ptr, size_t size );

/** seek absolute position in an OTF_File */
int OTF_File_seek( OTF_File* file, uint64_t pos );

/** get absolut position from an OTF_File */
uint64_t OTF_File_tell( OTF_File* file );

/** return the file size in bytes*/
uint64_t OTF_File_size( OTF_File* file );

/** close OTF_File */
int OTF_File_close( OTF_File* file );

/** return OTF_File status */
OTF_FileStatus OTF_File_status( OTF_File* file );


/** suspend OTF_File - internal use only. */
void OTF_File_suspend( OTF_File* file );

/** re-open the file when closed or suspended - internal use only. 
return 1 on success, 0 otherwise */
int OTF_File_revive( OTF_File* file, OTF_FileMode mode );

void OTF_File_setZBufferSize( OTF_File* file, uint32_t size );


/** internal use */
OTF_File* OTF_File_open_zlevel( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode, OTF_FileCompression compression );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_FILE_H */
