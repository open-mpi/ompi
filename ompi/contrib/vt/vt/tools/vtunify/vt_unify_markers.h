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

#ifndef _VT_UNIFY_MARKERS_H_
#define _VT_UNIFY_MARKERS_H_

#include "vt_inttypes.h"

#include "otf.h"

#include <string>
#include <vector>

//
// Markers class
//
class Markers
{
public:

   //
   // DefRec_Marker_struct
   //
   struct DefRec_Marker_struct
   {
      DefRec_Marker_struct()
	 : loccpuid(0), deftoken(0), type(0) {}
      DefRec_Marker_struct(uint32_t _loccpuid, uint32_t _deftoken,
			   std::string _name, uint32_t _type)
	 : loccpuid(_loccpuid), deftoken(_deftoken), name(_name), type(_type) {}

      uint32_t    loccpuid;
      uint32_t    deftoken;
      std::string name;
      uint32_t    type;
   };

   // contructor
   Markers();

   // destructor
   ~Markers();

   bool run();

private:

   bool readLocalMarkerDefs( std::vector<DefRec_Marker_struct*> *
			     p_vecMarkerDefs );
   bool writeGlobalMarkerDefs( OTF_WStream * p_uniMarkersWstream,
			       const std::vector<DefRec_Marker_struct*> *
			       p_vecMarkerDefs );

   bool unifyMarkerSpots( OTF_WStream * p_uniMarkersWstream );

};

// instance of class Markers
extern Markers * theMarkers;

#endif // _VT_UNIFY_MARKERS_H_
