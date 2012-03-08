/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2012.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_FileManager.h
 * 
 *  @brief Manages file handles.
 *
 *  i.e. Opens, closes and suspends files, if there are not enough
 *  filehandles available.
 *
 *  \ingroup fm
 */

/**
 * \defgroup fm File Manager Interface
 *
 * The file manager schedules an unlimited number OTF_Files to a limited
 * number of actual open OS files. Therefore all open are registered with
 * this manager. When a file is requested while no more OS files are
 * available any of the other files are suspended, i.i. the OS file is
 * closed.
 */


#ifndef OTF_FILEMANAGER_H
#define OTF_FILEMANAGER_H


#include "OTF_inttypes.h"


#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

struct struct_OTF_File;
typedef struct struct_OTF_File OTF_File;


struct struct_OTF_FileManager;
/** file manager object \ingroup fm */
typedef struct struct_OTF_FileManager OTF_FileManager;


/** Generates a new file manager with a maximum number of files that are allowed
to be open simultaneously. \ingroup fm */
OTF_FileManager* OTF_FileManager_open( uint32_t number );

/** Closes the file manager \ingroup fm */
void OTF_FileManager_close( OTF_FileManager* m );

/** Returns the number of files currently open. */
uint32_t OTF_FileManager_getCount( OTF_FileManager* m );

/** Returns the numbner of files allowed to be open simultaneously. */
uint32_t OTF_FileManager_getNumber( OTF_FileManager* m );

/** Sets the number of files allowed to be open simultaneously. */
uint32_t OTF_FileManager_setNumber( OTF_FileManager* m, uint32_t number );

/** Ensure there is a free file handle available after this call. 
return 1 on success, 0 otherwise (which is not supposed to happen) */
int OTF_FileManager_guaranteeFile( OTF_FileManager* m );

/** Registers the 'file' as open. Return 1 on success, 0 otherwise. */
int OTF_FileManager_registerFile( OTF_FileManager* m, OTF_File* file );

/** Marks currently opened 'file' as used which is important for the 
scheduling strategy, i.e. the internal decision which file to suspend next.
return 1 on success or 0 for an suspended file. */
int OTF_FileManager_touchFile( OTF_FileManager* m, OTF_File* file );

/** Suspend an open file explicitly. this may be called externaly or 
internally. Return 1 on success, 0 otherwise. */
int OTF_FileManager_suspendFile( OTF_FileManager* m, OTF_File* file );

#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTF_FILEMANAGER_H */

