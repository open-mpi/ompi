#ifndef OTFAUX_THUMBNAIL_H
#define OTFAUX_THUMBNAIL_H

#include <otf.h>

/**
 *  @file otfauxlib/OTFAUX_Thumbnail.h
 *
 *  @brief Provides a module to collect data for thumbnail generation.
 */

#ifdef __cplusplus
extern "C" {
#endif /* __cplusplus */

/**
 * @defgroup thumbnail Module for thumbnail generation.
 *
 * @usage:
 *
 *  ctx = OTFAUX_Thumbnail_Create(minTime, maxTime, 1024);
 *
 *  announce all interesting processes:
 *      OTFAUX_Thumbnail_declareProcess(ctx, ...);
 *
 *  repeatedly call for interesting processes:
 *      OTFAUX_Thumbnail_handleEnter(ctx, ...);
 *      OTFAUX_Thumbnail_handleLeave(ctx, ...);
 *
 *  at end, for all processes:
 *      OTFAUX_ThumbnailData td;
 *      OTFAUX_Thumbnail_getData(ctx, process, &td);
 *      .. do something with td.start_pixel and td.function ..
 *
 *  OTFAUX_Thumbnail_Destroy(ctx);
 *
 * @{
 */

/** Opaque type for using the thumbnail module. */
typedef struct OTFAUX_Thumbnail_Context OTFAUX_Thumbnail_Context;

/**
 * Create a context for thumbnail generation.
 *
 * @param minTime   Minimum timestamp of the trace file.
 * @param maxTime   Maximum timestamp of the trace file.
 * @param width     The width in pixels of the thumbnail.
 *
 * @return          The context.
 */
OTFAUX_Thumbnail_Context*
OTFAUX_Thumbnail_create( uint64_t minTime,
                         uint64_t maxTime,
                         uint32_t width );

/**
 * Destroy a context previously created with @a OTFAUX_Thumbnail_Create.
 *
 * @param tn_context   The context.
 */
void
OTFAUX_Thumbnail_destroy( OTFAUX_Thumbnail_Context* tn_context );

/**
 * Declares that the process @a process should be handled by this context.
 *
 * @param tn_context   The context.
 */
void
OTFAUX_Thumbnail_declareProcess( OTFAUX_Thumbnail_Context* tn_context,
                                 uint64_t                  process );

/**
 * Declare that the process @a process has entered the fucntion @a function
 * at timestamp @a timestamp.
 *
 * This function needs to be called in monotonically increasing timestamp order.
 *
 * @param tn_context   The context.
 * @param timestamp    The timestamp.
 * @param process      The process.
 * @param function     The function.
 */
void
OTFAUX_Thumbnail_handleEnter( OTFAUX_Thumbnail_Context* tn_context,
                              uint64_t                  timestamp,
                              uint64_t                  process,
                              uint32_t                  function );

/**
 * Declare that the process @a process has left the current fucntion at
 * timestamp @a timestamp.
 *
 * This function needs to be called in monotonically increasing timestamp order.
 *
 * @param tn_context   The context.
 * @param timestamp    The timestamp.
 * @param process      The process.
 */
void
OTFAUX_Thumbnail_handleLeave( OTFAUX_Thumbnail_Context* tn_context,
                               uint64_t                  timestamp,
                               uint64_t                  process );

/**
 * Get the number of entries for the process @a process.
 *
 * @param tn_context   The context.
 *
 * @param              The size.
 */
uint32_t
OTFAUX_Thumbnail_getSize( OTFAUX_Thumbnail_Context* context,
                          uint64_t                  process );

typedef struct {
    uint32_t* start_pixel;
    uint32_t* function;
    uint32_t  size;
} OTFAUX_Thumbnail_Data;

/**
 * Get the collected thumbnail data for process @a process.
 *
 * @param tn_context   The context.
 * @param process      The process.
 * @param data         Pointer to storage where the data will be stored into.
 * @param size         Pointer to storage where the size will be stored into.
 *
 * @param              1 on success.
 */
int
OTFAUX_Thumbnail_getData( OTFAUX_Thumbnail_Context* context,
                          uint64_t                  process,
                          OTFAUX_Thumbnail_Data*    data );

/**
 * @}
 */

char*
OTFAUX_Thumbnail_getFilename( const char* namestub,
                              size_t length,
                              char* name_buffer );

/**
 * @defgroup thumbnailwriter Module to write a thumbnail.
 *
 * @usage:
 *
 *  writer = OTFAUX_ThumbnailWriter_create("foo.otf", 512, 1024, ...);
 *
 *  for each process:
 *      OTFAUX_ThumbnailData td;
 *      OTFAUX_Thumbnail_getData( ctx, process, &td );
 *      OTFAUX_ThumbnailWriter_writeProcess( writer, process, &td );
 *
 *  OTFAUX_ThumbnailWriter_destroy( writer );
 */

typedef struct OTFAUX_ThumbnailWriter OTFAUX_ThumbnailWriter;

OTFAUX_ThumbnailWriter*
OTFAUX_ThumbnailWriter_create( const char* filename,
                               uint32_t height,
                               uint32_t width,
                               OTF_FileManager* manager );

int
OTFAUX_ThumbnailWriter_destroy( OTFAUX_ThumbnailWriter* tn_writer );

int
OTFAUX_ThumbnailWriter_close( OTFAUX_ThumbnailWriter* tn_writer );

int
OTFAUX_ThumbnailWriter_writeProcess( OTFAUX_ThumbnailWriter* tn_writer,
                                     uint64_t process,
                                     OTFAUX_Thumbnail_Data* data );

/**
 * @}
 */


/**
 * @defgroup thumbnailreader Module to read a thumbnail.
 */

typedef struct OTFAUX_ThumbnailReader OTFAUX_ThumbnailReader;

OTFAUX_ThumbnailReader*
OTFAUX_ThumbnailReader_create( const char* filename,
                               OTF_FileManager* manager );

int
OTFAUX_ThumbnailReader_destroy( OTFAUX_ThumbnailReader* tn_reader );

int
OTFAUX_ThumbnailReader_close( OTFAUX_ThumbnailReader* tn_reader );

int
OTFAUX_ThumbnailReader_getDimension( OTFAUX_ThumbnailReader* tn_reader,
                                     uint32_t* height,
                                     uint32_t* width );

int
OTFAUX_ThumbnailReader_read( OTFAUX_ThumbnailReader* tn_reader,
                             void ( *process_handler )( void*,
                                                        uint64_t,
                                                        uint32_t,
                                                        uint32_t ),
                             void* data );


/**
 * @}
 */


#ifdef __cplusplus
}
#endif /* __cplusplus */

#endif /* OTFAUX_THUMBNAIL_H */
