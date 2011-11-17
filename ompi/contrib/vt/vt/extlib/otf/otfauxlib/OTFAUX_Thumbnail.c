#include <config.h>

#include <stdlib.h>
#include <stddef.h>
#include <stdio.h>

#include <otf.h>

/* for OTF_Error */
#include <OTF_Errno.h>

#include <jenkins_hash.h>

#include "otfaux.h"

#define FUNCTION_STACK_INCREMENT 16

typedef struct OTFAUX_Thumbail_Process {
    /** next in hash chain */
    struct OTFAUX_Thumbail_Process* next;

    /** the id of this process */
    uint64_t token;

    /** current function stack */
    uint32_t* function_stack;
    uint32_t stack_pos;
    uint32_t stack_size;

    uint32_t pos, alloc;
    uint32_t* start_pixel;
    uint32_t* function;
} OTFAUX_Thumbail_Process;

#define PROCESSES_HASH_SHIFT 10
#define PROCESSES_HASH_SIZE  (1 << PROCESSES_HASH_SHIFT)
#define PROCESSES_HASH_MASK  (PROCESSES_HASH_SIZE - 1)

struct OTFAUX_Thumbnail_Context {
    /** The processes */
    OTFAUX_Thumbail_Process* processes[ PROCESSES_HASH_SIZE ];

    /* timestamps */
    uint32_t* timestamps;
};

OTFAUX_Thumbnail_Context*
OTFAUX_Thumbnail_create( uint64_t minTime,
                         uint64_t maxTime,
                         uint32_t width )
{
    OTFAUX_Thumbnail_Context* new_context = calloc( 1, sizeof( *new_context ) );

    /* TODO: callculate sample time stamps */

    return new_context;
}

void
OTFAUX_Thumbnail_destroy( OTFAUX_Thumbnail_Context* tn_context )
{
    int i;
    for ( i = 0; i < PROCESSES_HASH_SIZE; i++ ) {
        while ( tn_context->processes[ i ] ) {
            OTFAUX_Thumbail_Process* next = tn_context->processes[ i ]->next;
            free( tn_context->processes[ i ]->function_stack );
            free( tn_context->processes[ i ] );
            tn_context->processes[ i ] = next;
        }
    }

    free( tn_context );
}

static OTFAUX_Thumbail_Process*
get_process( OTFAUX_Thumbnail_Context* tn_context,
             uint64_t                  process_token,
             int                       create )
{
    uint32_t process_hash = hash( &process_token, sizeof( process_token ), 0 );
    OTFAUX_Thumbail_Process** process_bucket = &tn_context->processes[ process_hash & PROCESSES_HASH_MASK ];
    OTFAUX_Thumbail_Process* process = *process_bucket;

    /* search in hash chain */
    while ( process ) {
        if ( process->token == process_token ) {
            /* found, is this an error? */
            return process;
        }

        process = process->next;
    }

    if ( !create ) {
        return process;
    }

    /*  create new process */
    process = calloc( 1, sizeof( *process ) );
    if ( !process ) {
        return NULL;
    }

    process->token = process_token;
    process->function_stack = calloc( FUNCTION_STACK_INCREMENT,
                                      sizeof( *process->function_stack ) );
    if ( !process->function_stack ) {
        free( process );
        return NULL;
    }
    process->stack_size = FUNCTION_STACK_INCREMENT;

    /* TODO: init arrays */

    /* chain into hash table */
    process->next = *process_bucket;
    *process_bucket = process;

    return process;
}

void
OTFAUX_Thumbnail_declareProcess( OTFAUX_Thumbnail_Context* tn_context,
                                 uint64_t                  process_token )
{
    OTFAUX_Thumbail_Process* process = get_process( tn_context,
                                                    process_token,
                                                    1 );

    if ( !process ) {
        return;
    }

    /* TODO: enter the invalid */
}

void
OTFAUX_Thumbnail_handleEnter( OTFAUX_Thumbnail_Context* tn_context,
                              uint64_t                  timestamp,
                              uint64_t                  process_token,
                              uint32_t                  function_token )
{
    OTFAUX_Thumbail_Process* process;

    process = get_process( tn_context, process_token, 0 );

    if ( !process ) {
        return;
    }

    /* need to increase stack size? */
    if ( process->stack_pos == process->stack_size ) {
        uint32_t new_stack_size = process->stack_size + FUNCTION_STACK_INCREMENT;
        uint32_t* new_function_stack = realloc( process->function_stack,
                                                new_stack_size * sizeof( *process->function_stack ) );
        if ( !new_function_stack ) {
            return;
        }
        process->function_stack = new_function_stack;
        process->stack_size = new_stack_size;
    }

    process->function_stack[ process->stack_pos++ ] = function_token;

    /* TODO: check for pipxel */
}

void
OTFAUX_Thumbnail_handleLeave( OTFAUX_Thumbnail_Context* tn_context,
                              uint64_t                  timestamp,
                              uint64_t                  process_token )
{
    OTFAUX_Thumbail_Process* process;

    process = get_process( tn_context, process_token, 0 );

    if ( !process || process->stack_pos == 0 ) {
        return;
    }

    /* pop from function stack */
    process->stack_pos--;

    /* TODO: check for pipxel */
}

uint32_t
OTFAUX_Thumbnail_getSize( OTFAUX_Thumbnail_Context* tn_context,
                          uint64_t                  process_token )
{
    OTFAUX_Thumbail_Process* process;

    process = get_process( tn_context, process_token, 0 );

    if ( !process ) {
        return 0;
    }

    /* TODO */
    return 0;
}

int
OTFAUX_Thumbnail_getData( OTFAUX_Thumbnail_Context* tn_context,
                          uint64_t                  process_token,
                          OTFAUX_Thumbnail_Data*    data )
{
    OTFAUX_Thumbail_Process* process;

    process = get_process( tn_context, process_token, 0 );

    if ( !process ) {
        return 0;
    }

    /* TODO */
    return 0;
}

char*
OTFAUX_Thumbnail_getFilename( const char* namestub,
                              size_t length,
                              char* name_buffer )
{
    if ( !namestub ) {
        return NULL;
    }

    if ( ( NULL == name_buffer ) || ( 0 == length ) ) {
        length = strlen( namestub ) + strlen( ".thumb" ) + 1;
        name_buffer = (char*)malloc( length * sizeof( char ) );
    }

    strcpy( name_buffer, namestub );
    strcat( name_buffer, ".thumb" );

    return name_buffer;
}

struct OTFAUX_ThumbnailWriter {
    char* namestub;
    OTF_FileManager* manager;

    uint32_t height, width;

    OTF_WBuffer* buffer;
};


OTFAUX_ThumbnailWriter*
OTFAUX_ThumbnailWriter_create( const char* filename,
                               uint32_t height,
                               uint32_t width,
                               OTF_FileManager* manager )
{
    OTFAUX_ThumbnailWriter* new_writer;

    if ( !filename || !manager ) {
        return NULL;
    }

    new_writer = calloc( 1, sizeof( *new_writer) );
    if ( !new_writer ) {
        return NULL;
    }

    new_writer->namestub = OTF_stripFilename( filename );
    if ( !new_writer->namestub ) {
        free( new_writer );
        return NULL;
    }

    new_writer->height  = height;
    new_writer->width   = width;
    new_writer->manager = manager;

    return new_writer;
}

int
OTFAUX_ThumbnailWriter_destroy( OTFAUX_ThumbnailWriter* tn_writer )
{
    int ret;

    if ( !tn_writer ) {
        return 0;
    }

    ret = OTFAUX_ThumbnailWriter_close( tn_writer );

    free( tn_writer->namestub );
    free( tn_writer );

    return ret;
}

int
OTFAUX_ThumbnailWriter_close( OTFAUX_ThumbnailWriter* tn_writer )
{
    if ( !tn_writer ) {
        return 0;
    }

    if ( tn_writer->buffer ) {
        OTF_WBuffer_close( tn_writer->buffer );
    }
    tn_writer->buffer = NULL;

    return 1;
}

int
OTFAUX_ThumbnailWriter_writeProcess( OTFAUX_ThumbnailWriter* tn_writer,
                                     uint64_t process,
                                     OTFAUX_Thumbnail_Data* data )
{
    size_t i;
    char sep = ':';

    if ( !tn_writer || !data ) {
        return 0;
    }

    if ( !tn_writer->buffer ) {
        char* filename = OTFAUX_Thumbnail_getFilename( tn_writer->namestub,
                                                       0, NULL );

        if ( !filename ) {
            OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
                    "OTFAUX_Thumbnail_getFilename() failed.\n",
                    __FUNCTION__, __FILE__, __LINE__ );

            return 0;
        }

        tn_writer->buffer = OTF_WBuffer_open( filename, tn_writer->manager );
        if ( !tn_writer->buffer ) {
            OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
                    "OTF_WBuffer_open( %s ) failed.\n",
                    __FUNCTION__, __FILE__, __LINE__, filename );

            free( filename );

            return  0;
        }

        OTF_WBuffer_setSize( tn_writer->buffer, tn_writer->width * 16 );

        free( filename );

        /* write header */
        OTF_WBuffer_writeUint32( tn_writer->buffer, 0 );
        OTF_WBuffer_writeChar( tn_writer->buffer, ':' );
        OTF_WBuffer_writeUint32( tn_writer->buffer, tn_writer->height );
        OTF_WBuffer_writeChar( tn_writer->buffer, ',' );
        OTF_WBuffer_writeUint32( tn_writer->buffer, tn_writer->width );
        OTF_WBuffer_writeNewline( tn_writer->buffer );
    }

    OTF_WBuffer_writeUint64( tn_writer->buffer, process );

    for ( i = 0; i < data->size; i++ ) {
        OTF_WBuffer_writeChar( tn_writer->buffer, sep );
        sep = ';';

        OTF_WBuffer_writeUint32( tn_writer->buffer, data->start_pixel[ i ] );
        OTF_WBuffer_writeChar( tn_writer->buffer, ',' );
        OTF_WBuffer_writeUint32( tn_writer->buffer, data->function[ i ] );
    }

    OTF_WBuffer_writeNewline( tn_writer->buffer );

    return 1;
}

struct OTFAUX_ThumbnailReader
{
    char* namestub;
    OTF_FileManager* manager;

    uint32_t height, width;

    OTF_RBuffer* buffer;
};

OTFAUX_ThumbnailReader*
OTFAUX_ThumbnailReader_create( const char* filename,
                               OTF_FileManager* manager )
{
    OTFAUX_ThumbnailReader* new_reader;

    if ( !filename || !manager ) {
        return NULL;
    }

    new_reader = calloc( 1, sizeof( *new_reader) );
    if ( !new_reader ) {
        return NULL;
    }

    new_reader->namestub = OTF_stripFilename( filename );
    if ( !new_reader->namestub ) {
        free( new_reader );
        return NULL;
    }

    new_reader->manager = manager;

    return new_reader;
}

int
OTFAUX_ThumbnailReader_destroy( OTFAUX_ThumbnailReader* tn_reader )
{
    int ret;

    if ( !tn_reader ) {
        return 0;
    }

    ret = OTFAUX_ThumbnailReader_close( tn_reader );

    free( tn_reader->namestub );
    free( tn_reader );

    return ret;
}

int
OTFAUX_ThumbnailReader_close( OTFAUX_ThumbnailReader* tn_reader )
{
    if ( !tn_reader ) {
        return 0;
    }

    if ( tn_reader->buffer ) {
        OTF_RBuffer_close( tn_reader->buffer );
    }
    tn_reader->buffer = NULL;

    return 1;
}

int
OTFAUX_ThumbnailReader_getDimension( OTFAUX_ThumbnailReader* tn_reader,
                                     uint32_t* height,
                                     uint32_t* width )
{
    if ( !tn_reader ) {
        return 0;
    }

    if ( !tn_reader->buffer ) {
        uint32_t val;

        char* filename = OTFAUX_Thumbnail_getFilename( tn_reader->namestub,
                                                       0, NULL );
        if ( !filename ) {
            OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
                    "OTF_getFilename() failed.\n",
                    __FUNCTION__, __FILE__, __LINE__ );

            return 0;
        }

        tn_reader->buffer = OTF_RBuffer_open( filename, tn_reader->manager );
        if ( !tn_reader->buffer ) {
            OTF_Error( "ERROR in function %s, file: %s, line: %i:\n "
                    "OTF_RBuffer_open( %s ) failed.\n",
                    __FUNCTION__, __FILE__, __LINE__, filename );

            free( filename );

            return  0;
        }

        OTF_RBuffer_setSize( tn_reader->buffer, 1024 );

        free( filename );

        /* read header */

        if ( !OTF_RBuffer_guaranteeRecord( tn_reader->buffer ) ) {
            OTF_RBuffer_close( tn_reader->buffer );
            tn_reader->buffer = NULL;
            return 0;
        }

        val = OTF_RBuffer_readUint32( tn_reader->buffer );
        if ( val != 0 || !OTF_RBuffer_testChar( tn_reader->buffer, ':' ) ) {
            OTF_RBuffer_close( tn_reader->buffer );
            tn_reader->buffer = NULL;
            return 0;
        }

        tn_reader->height = OTF_RBuffer_readUint32( tn_reader->buffer );

        if ( !OTF_RBuffer_testChar( tn_reader->buffer, ',' ) ) {
            OTF_RBuffer_close( tn_reader->buffer );
            tn_reader->buffer = NULL;
            return 0;
        }

        tn_reader->width = OTF_RBuffer_readUint32( tn_reader->buffer );

        OTF_RBuffer_readNewline( tn_reader->buffer );
    }

    if ( height ) {
        *height = tn_reader->height;
    }

    if ( width ) {
        *width = tn_reader->width;
    }

    return 1;
}

int
OTFAUX_ThumbnailReader_read( OTFAUX_ThumbnailReader* tn_reader,
                             void ( *handler )( void*,
                                                uint64_t,
                                                uint32_t,
                                                uint32_t ),
                             void* data )
{
    uint64_t process;
    uint32_t start_pixel, function;

    if ( !tn_reader ) {
        return 0;
    }

    if ( !tn_reader->buffer ) {
        int ret;

        ret = OTFAUX_ThumbnailReader_getDimension( tn_reader, NULL, NULL );
        if ( !ret ) {
            return ret;
        }
    }

    while ( OTF_RBuffer_guaranteeRecord( tn_reader->buffer ) ) {
        /* read process */
        process = OTF_RBuffer_readUint64( tn_reader->buffer );

        if ( !OTF_RBuffer_testChar( tn_reader->buffer, ':' ) ) {
            OTF_RBuffer_readNewline( tn_reader->buffer );
            continue;
        }

        do {

            start_pixel = OTF_RBuffer_readUint32( tn_reader->buffer );

            if ( !OTF_RBuffer_testChar( tn_reader->buffer, ',' ) ) {
                OTF_RBuffer_readNewline( tn_reader->buffer );
                break;
            }

            function = OTF_RBuffer_readUint32( tn_reader->buffer );

            if ( handler ) {
                handler( data, process, start_pixel, function );
            }

        } while ( OTF_RBuffer_testChar( tn_reader->buffer, ';' ) );

        OTF_RBuffer_readNewline( tn_reader->buffer );
    }

    return 1;
}
