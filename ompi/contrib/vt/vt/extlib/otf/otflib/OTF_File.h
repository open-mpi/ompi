/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2012.
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

#include <stdio.h>

#ifdef HAVE_ZLIB

/* We cannot include the zlib.h due to possibly missing path to it
(e.g. zlib.h isn't located in a default compiler search path) */
/*#include <zlib.h>*/

/* macro to access the zlib object of struct_OTF_File with the correct type */
#define OTF_FILE_Z(file) ((z_stream*) (file)->z)


#endif /* HAVE_ZLIB */

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

	OTF_FILESTATUS_UNKNOWN= 0,
	OTF_FILESTATUS_ACTIVE= 1,
	OTF_FILESTATUS_SUSPENDED= 2,
	OTF_FILESTATUS_CLOSED= 3
};
typedef enum enum_OTF_FileStatus OTF_FileStatus;

/* Needs to be in the header so we can use it from OTF_File_iofsl */
struct struct_OTF_File {

	/** own copy of filename */
	char* filename;

	/** actual file handle, it is NULL if file is currently closed,
	!= NULL otherwise */
	FILE* file;

#ifdef HAVE_ZLIB

	/** zlib object,
	The actual type z_stream* cannot be used here (see notes above).
	Use the macro OTF_FILE_Z() to access it with the correct type. */
	void* z;

	/** zlib entry buffer ... what a nice wordplay */
	unsigned char* zbuffer;

	uint32_t zbuffersize;

#endif /* HAVE_ZLIB */

	/** keep file pos when the real file is closed,
	undefined while file is open, == 0 before opened for the first time */
	uint64_t pos;

	OTF_FileMode mode;

	OTF_FileManager* manager;


	/** Reference to external buffer to read from instead of a real file.
	This is for reading of definitions only and has some limitations. */
	const char* externalbuffer;

	/** the current position in the 'externalbuffer' */
	uint64_t externalpos;
	/** the total length of the 'externalbuffer' */
	uint64_t externallen;

	OTF_File_iofsl *iofsl;
};


#ifdef HAVE_ZOIDFS
/** external variable indicating whether zoidfs was initialized */
extern uint8_t zoidfs_initialized;
#endif

/** initialize a OTF_File object */
void OTF_File_init( OTF_File* o );

/** finalize a OTF_File object */
void OTF_File_finalize( OTF_File* o );

/** open an OTF_File */
OTF_File* OTF_File_open( const char* filename, OTF_FileManager* manager,
	OTF_FileMode mode );

/** open a pseudo OTF_File that actually reads from the given memory buffer. 
The buffer is not copied, you need to manage it yourself! 
Don't touch it during access operations. */
OTF_File* OTF_File_open_with_external_buffer( uint32_t len, const char* buffer, uint8_t is_compressed, 
    OTF_FileMode mode );

/** Rename file pointed to by 'from' to file 'to'.
 *  If the filename describes a zoidfs file the zoidfs API is used.
 *  Otherwise standard POSIX rename is used.*/
int OTF_File_rename(const char* from, const char* to);

int OTF_File_access(const char* filename, int mode);

/** Remove the file according to the stream id encoded in the filename */
int OTF_File_remove(const char* filename);

/** Clean up everything -- relevant only for multifile use to remove the data and index file */
int OTF_File_clean(const char* filename);

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


/* internal function */
/** read 'length' bytes from underlying file or from special memory buffer */
size_t OTF_File_read_internal( OTF_File* file, void* dest, size_t length );


/** Wrapper around fwrite to issue calls to zoidfs_write if needed */
size_t OTF_File_write_internal(OTF_File* file, const void* src, size_t length);


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_FILE_H */
