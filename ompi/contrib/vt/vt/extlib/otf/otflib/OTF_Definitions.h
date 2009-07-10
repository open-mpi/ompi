/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Definitions.h
 * 
 *  @brief Provides many many macros for different purposes.
 *
 *  \ingroup misc
 */


#ifndef OTF_DEFINITIONS_H
#define OTF_DEFINITIONS_H


/* version information */

#define OTF_VERSION_MAYOR	1
#define OTF_VERSION_MINOR	3
#define OTF_VERSION_SUB 	12
#define OTF_VERSION_STRING	"jellyfish"

/* version history:

0.2.1 	"octopussy"
0.3.1 	"starfish"
0.3.2 	"starfish"
0.4.0 	"starfish"
0.4.1 	"starfish"
0.5.0	"starfish"
1.0.*	"starfish"
1.1.*	"starfish"
1.2.*	"pufferfish"
1.3.*   "jellyfish"

*/


/* definitions of record type identifiers */


/* Event records*/

#define OTF_EVENTCOMMENT_RECORD					0
#define OTF_COUNTER_RECORD						1
#define OTF_ENTER_RECORD						2

#define OTF_COLLOP_RECORD						5

#define OTF_RECEIVE_RECORD						10
#define OTF_SEND_RECORD							11
#define OTF_LEAVE_RECORD						12

#define OTF_BEGINPROCESS_RECORD					35
#define OTF_ENDPROCESS_RECORD					36

#define OTF_FILEOPERATION_RECORD				42

/* Definition records*/

#define OTF_DEFTIMERRESOLUTION_RECORD			13
#define OTF_DEFPROCESS_RECORD					14
#define OTF_DEFPROCESSGROUP_RECORD				15
#define OTF_DEFFUNCTION_RECORD					16
#define OTF_DEFFUNCTIONGROUP_RECORD				17
#define OTF_DEFCOUNTER_RECORD					18
#define OTF_DEFCOUNTERGROUP_RECORD				19
#define OTF_DEFCOLLOP_RECORD					20
#define OTF_DEFSCL_RECORD						21
#define OTF_DEFSCLFILE_RECORD					22
#define OTF_DEFVERSION_RECORD					23
#define OTF_DEFCREATOR_RECORD					24
#define OTF_DEFFILE_RECORD						25
#define OTF_DEFFILEGROUP_RECORD					26


#define OTF_FUNCTIONSUMMARY_RECORD				28
#define OTF_FUNCTIONGROUPSUMMARY_RECORD			29
#define OTF_MESSAGESUMMARY_RECORD				30
#define OTF_FILEOPERATIONSUMMARY_RECORD			31
#define OTF_FILEGROUPOPERATIONSUMMARY_RECORD	32

#define OTF_DEFINITIONCOMMENT_RECORD			34

#define OTF_ENTERSNAPSHOT_RECORD				37
#define OTF_SENDSNAPSHOT_RECORD					38

#define OTF_SUMMARYCOMMENT_RECORD				39
#define OTF_SNAPSHOTCOMMENT_RECORD				40
#define OTF_OPENFILESNAPSHOT_RECORD				43

#define OTF_UNKNOWN_RECORD						41


/* Number of records */
#define OTF_NRECORDS							44

/* Stream format definition */

#define OTF_WSTREAM_FORMAT_SHORT		0
#define OTF_WSTREAM_FORMAT_LONG			1


/* Counter properties */

/* 1st-2nd bit */
#define OTF_COUNTER_TYPE_BITS		3
#define OTF_COUNTER_TYPE_ACC		0
#define OTF_COUNTER_TYPE_ABS		1

/* 3rd-4th bit */
#define OTF_COUNTER_SCOPE_BITS 		12
#define OTF_COUNTER_SCOPE_START 	0
#define OTF_COUNTER_SCOPE_POINT 	4
#define OTF_COUNTER_SCOPE_LAST		8
#define OTF_COUNTER_SCOPE_NEXT		12

/* 6th-9th bit */
#define OTF_COUNTER_VARTYPE_ISINTEGER(x) (x < 256)
#define OTF_COUNTER_VARTYPE_ISSIGNED(x) ((x&32) == 32)
#define OTF_COUNTER_VARTYPE_ISUNSIGNED(x) ((x&32) == 0)

#define OTF_COUNTER_VARTYPE_BITS				480 /* 1111xxxxx */
#define OTF_COUNTER_VARTYPE_UNSIGNED8			0 /* 0000 */
#define OTF_COUNTER_VARTYPE_SIGNED8				32 /* 00001 */
#define OTF_COUNTER_VARTYPE_UNSIGNED4			64 /* 0010 */
#define OTF_COUNTER_VARTYPE_SIGNED4				96 /* 0011 */
#define OTF_COUNTER_VARTYPE_UNSIGNED2			128 /* 0100 */
#define OTF_COUNTER_VARTYPE_SIGNED2				160 /* 0101 */
#define OTF_COUNTER_VARTYPE_FLOAT				256 /* 1000 */
#define OTF_COUNTER_VARTYPE_DOUBLE				288 /* 1001 */



#define OTF_COUNTER_PROP_DEFAULT	0


/* Types of collective operations */

#define OTF_COLLECTIVE_TYPE_UNKNOWN 	0
#define OTF_COLLECTIVE_TYPE_BARRIER 	1
#define OTF_COLLECTIVE_TYPE_ONE2ALL 	2
#define OTF_COLLECTIVE_TYPE_ALL2ONE 	3
#define OTF_COLLECTIVE_TYPE_ALL2ALL 	4


/* File Operations */
#define OTF_FILEOP_OPEN		0
#define OTF_FILEOP_CLOSE	1
#define OTF_FILEOP_READ		2
#define OTF_FILEOP_WRITE	3
#define OTF_FILEOP_SEEK		4


/* return values for handlers. they are not yet evaluated!!! */

/** When writing an own handler, use these macros to tell OTF, what to do.
if you return OTF_RETURN_OK OTF continues reading the trace and calling the
appropriate handlers. If you return OTF_RETURN_BREAK or OTF_RETURN_ABORT OTF
stops reading immediately */
#define OTF_RETURN_OK					0
/** @see OTF_RETURN_OK */
#define OTF_RETURN_BREAK				1
/** @see OTF_RETURN_OK */
#define OTF_RETURN_ABORT				1

#define OTF_READ_ERROR					(uint64_t)-1
#define OTF_READ_MAXRECORDS				(uint64_t)-2

#endif /* OTF_DEFINITIONS_H */
