/* -*- c -*- */

#include <unistd.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <stdint.h>
#include <math.h>

#include <otfaux.h>

static void
process_handler( void* data,
                 uint64_t process,
                 uint32_t start_pixel,
                 uint32_t function )
{
    printf( "%lx: %x/%x\n", process, start_pixel, function );
}

int
main(int ac, char *av[])
{
    OTF_FileManager* manager;
    OTFAUX_ThumbnailWriter* writer;
    OTFAUX_ThumbnailReader* reader;
    OTFAUX_Thumbnail_Data data;
    uint32_t start_pixels[7] = { 2, 5, 23, 49, 63, 76, 80 };
    uint32_t functions[7] = { 1, 2, 3, 2, 3, 2, 1 };
    uint32_t height, width;
    int i;

    manager = OTF_FileManager_open( 16 );

    writer = OTFAUX_ThumbnailWriter_create( "test_thumbnail.otf",
                                            4,
                                            16,
                                            manager );

    for ( i = 1; i <= 4; i++ )
    {
        data.start_pixel = start_pixels;
        data.function    = functions;
        data.size        = 7;

        OTFAUX_ThumbnailWriter_writeProcess( writer, i, &data );
    }
    OTFAUX_ThumbnailWriter_destroy( writer );

    reader = OTFAUX_ThumbnailReader_create( "test_thumbnail.otf",
                                            manager );
    OTFAUX_ThumbnailReader_getDimension( reader, &height, &width );
    printf( "%xx%x\n", height, width );

    OTFAUX_ThumbnailReader_read( reader, process_handler, NULL );

    OTFAUX_ThumbnailReader_destroy( reader );

    OTF_FileManager_close( manager );

    return 0;
}
