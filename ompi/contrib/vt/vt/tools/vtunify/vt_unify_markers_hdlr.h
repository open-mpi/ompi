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

#ifndef _VT_UNIFY_MARKERS_HDLR_H_
#define _VT_UNIFY_MARKERS_HDLR_H_

#include "vt_unify_markers.h"

#include "vt_inttypes.h"

#include <vector>

int Handle_DefMarker(
   std::vector<Markers::DefRec_Marker_struct*>* p_vecLocDefRecs,
   uint32_t streamid, uint32_t deftoken, const char* name, uint32_t type );

int Handle_Marker(
   OTF_WStream* wstream,
   uint64_t time, uint32_t process, uint32_t token, const char* text );

#endif // _VT_UNIFY_MARKER_HDLR_H_
