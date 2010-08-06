/**
 * VampirTrace
 * http://www.tu-dresden.de/zih/vampirtrace
 *
 * Copyright (c) 2005-2010, ZIH, TU Dresden, Federal Republic of Germany
 *
 * Copyright (c) 1998-2005, Forschungszentrum Juelich GmbH, Federal
 * Republic of Germany
 *
 * See the file COPYRIGHT in the package base directory for details
 **/

#include "vt_unify.h"
#include "vt_unify_markers.h"
#include "vt_unify_markers_hdlr.h"

#include "otf.h"

#include <iostream>
#include <string>
#include <vector>

#include <assert.h>

Markers * theMarkers; // instance of class Markers

//////////////////// class Markers ////////////////////

// public methods
//

Markers::Markers()
{
   // Empty
}

Markers::~Markers()
{
   // Empty
}

bool
Markers::run()
{
   VPrint( 1, "Unifying markers\n" );

   bool error = false;

   // allocate vector for marker definitions
   std::vector<DefRec_Marker_struct*> * p_vec_marker_defs =
      new std::vector<DefRec_Marker_struct*>();
   assert( p_vec_marker_defs );

   // read local marker definitions
   if( !readLocalMarkerDefs( p_vec_marker_defs ) )
     error = true;

   if( !error && p_vec_marker_defs->size() > 0 )
   {
      std::string tmp_out_file_prefix =
	 Params.out_file_prefix + TmpFileSuffix;

      // open file manager for writer stream
      OTF_FileManager * p_uni_markers_manager 
	 = OTF_FileManager_open( 1 );
      assert( p_uni_markers_manager );

      // open stream for writing (stream id = 0)
      OTF_WStream * p_uni_markers_wstream =
	 OTF_WStream_open( tmp_out_file_prefix.c_str(), 0, p_uni_markers_manager );
      assert( p_uni_markers_wstream );

      // set file compression
      if( Params.docompress )
      {
	 OTF_WStream_setCompression( p_uni_markers_wstream,
				     OTF_FILECOMPRESSION_COMPRESSED );
      }

      // try to get buffer
      if( !OTF_WStream_getMarkerBuffer( p_uni_markers_wstream ) )
      {
	 std::cerr << ExeName << ": Error: "
		   << "Could not open OTF writer stream [namestub "
		   << tmp_out_file_prefix.c_str() << " id 0]" << std::endl;
	 error = true;
      }
      else
      {
	 VPrint( 2, " Opened OTF writer stream [namestub %s id 0]\n",
		 tmp_out_file_prefix.c_str() );

	 // write global marker definitions
	 error = !writeGlobalMarkerDefs( p_uni_markers_wstream,
					 p_vec_marker_defs );

	 // read/write marker
	 if( !error && !unifyMarkerSpots( p_uni_markers_wstream ) )
	    error = true;
      }

      // close writer stream
      OTF_WStream_close( p_uni_markers_wstream );
      // close file manager for writer stream
      OTF_FileManager_close( p_uni_markers_manager );
	 
      VPrint( 2, " Closed OTF writer stream [namestub %s id 0]\n",
	      tmp_out_file_prefix.c_str() );
   }

   // free definition record vector
   //
   for( uint32_t i = 0; i < p_vec_marker_defs->size(); i++ )
      delete (*p_vec_marker_defs)[i];
   delete p_vec_marker_defs;
   
   return !error;
}

// private methods
//

bool
Markers::readLocalMarkerDefs( std::vector<DefRec_Marker_struct*> * p_vecMarkerDefs )
{
   VPrint( 1, " Reading local marker definitions\n" );

   bool error = false;

   // create record handler and set the definition
   // record vector as first handler argument for ...
   //
   OTF_HandlerArray * p_handler_array =
      OTF_HandlerArray_open();
   assert( p_handler_array );

   // ... OTF_DEFMARKER_RECORD
   OTF_HandlerArray_setHandler( p_handler_array,
       (OTF_FunctionPointer*)Handle_DefMarker,
       OTF_DEFMARKER_RECORD );
   OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
      p_vecMarkerDefs,
      OTF_DEFMARKER_RECORD );

   // read local marker definitions
   //
   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      // open file manager for reader stream
      OTF_FileManager * p_loc_defmarkers_manager 
	 = OTF_FileManager_open( 1 );
      assert( p_loc_defmarkers_manager );

      // open stream for reading
      OTF_RStream * p_loc_defmarkers_rstream =
	 OTF_RStream_open( Params.in_file_prefix.c_str(),
			   g_vecUnifyCtls[i]->streamid,
			   p_loc_defmarkers_manager );
      assert( p_loc_defmarkers_rstream );

      VPrint( 2, "  Opened OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( !OTF_RStream_getMarkerBuffer( p_loc_defmarkers_rstream ) )
      {
	 VPrint( 2, "   No markers found in this OTF reader stream "
		    "- Ignored\n" );
      }
      else
      {
	 // close marker buffer
	 OTF_RStream_closeMarkerBuffer( p_loc_defmarkers_rstream );

	 // read marker definitions
	 if( OTF_RStream_readMarker( p_loc_defmarkers_rstream, p_handler_array )
	     == OTF_READ_ERROR )
	 {
	    std::cerr << ExeName << ": Error: "
		      << "Could not read markers of OTF stream [namestub "
		      << Params.in_file_prefix << " id "
		      << std::hex << g_vecUnifyCtls[i]->streamid << "]"
		      << std::dec << std::endl;
	    error = true;
	 }
      }

      // close reader stream 
      OTF_RStream_close( p_loc_defmarkers_rstream );
      // close file manager for reader stream
      OTF_FileManager_close( p_loc_defmarkers_manager );

      VPrint( 2, "  Closed OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( error ) break;
   }

   // close record handler
   OTF_HandlerArray_close( p_handler_array );

   if( error )
   {
      std::cerr << ExeName << ": "
		<< "An error occurred during unifying marker definitions. Aborting"
		<< std::endl;
   }

   return !error;
}

bool
Markers::writeGlobalMarkerDefs( OTF_WStream * p_uniMarkersWstream,
				const std::vector<DefRec_Marker_struct*> *
				p_vecMarkerDefs )
{
   VPrint( 1, " Writing global marker definitions\n" );

   assert( p_uniMarkersWstream );
   assert( p_vecMarkerDefs->size() > 0 );

   bool error = false;

   // write global marker definition records
   //
   for( uint32_t i = 0; i < p_vecMarkerDefs->size(); i++ )
   {
      OTF_WStream_writeDefMarker( p_uniMarkersWstream,
				  (*p_vecMarkerDefs)[i]->deftoken,
				  (*p_vecMarkerDefs)[i]->name.c_str(),
				  (*p_vecMarkerDefs)[i]->type );
   }

   return !error;
}

bool
Markers::unifyMarkerSpots( OTF_WStream * p_uniMarkersWstream )
{
   VPrint( 1, " Unifying marker spots\n" );

   assert( p_uniMarkersWstream );

   bool error = false;

   for( uint32_t i = 0; i < g_vecUnifyCtls.size(); i++ )
   {
      // open file manager for reader stream
      OTF_FileManager * p_org_markers_manager =
	 OTF_FileManager_open( 1 );
      assert( p_org_markers_manager );
      
      // open stream for reading
      OTF_RStream * p_org_markers_rstream =
	 OTF_RStream_open( Params.in_file_prefix.c_str(),
			   g_vecUnifyCtls[i]->streamid,
			   p_org_markers_manager );
      assert( p_org_markers_rstream );

      VPrint( 2, "  Opened OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( !OTF_RStream_getMarkerBuffer( p_org_markers_rstream ) )
      {
	 VPrint( 2, "   No markers found in this OTF reader stream "
		    "- Ignored\n" );
      }
      else
      {
	 // close markers buffer
	 OTF_RStream_closeMarkerBuffer( p_org_markers_rstream );

	 // create record handler
	 OTF_HandlerArray * p_handler_array =
	    OTF_HandlerArray_open();
	 assert( p_handler_array );

	 // set record handler and first handler argument for ...
	 //

	 // ... OTF_MARKER_RECORD
	 OTF_HandlerArray_setHandler( p_handler_array,
            (OTF_FunctionPointer*)Handle_Marker,
				      OTF_MARKER_RECORD );
	 OTF_HandlerArray_setFirstHandlerArg( p_handler_array,
	    p_uniMarkersWstream, OTF_MARKER_RECORD );

	 // read marker spots
	 if( OTF_RStream_readMarker( p_org_markers_rstream, p_handler_array )
	     == OTF_READ_ERROR )
	 {
	    std::cerr << ExeName << ": Error: "
		      << "Could not read marker spots of OTF stream [namestub "
		      << Params.in_file_prefix << " id "
		      << std::hex << g_vecUnifyCtls[i]->streamid << "]"
		      << std::dec << std::endl;
	    error = true;
	 }

	 // close record handler
	 OTF_HandlerArray_close( p_handler_array );
      }

      // close reader stream
      OTF_RStream_close( p_org_markers_rstream );
      // close file manager for reader stream
      OTF_FileManager_close( p_org_markers_manager );

      VPrint( 2, "  Closed OTF reader stream [namestub %s id %x]\n",
	      Params.in_file_prefix.c_str(),
	      g_vecUnifyCtls[i]->streamid );

      if( error ) break;
   }

   if( error )
   {
      std::cerr << ExeName << ": "
		<< "An error occurred during unifying marker spots. Aborting"
		<< std::endl;
   }

   return !error;
}
