/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2008.
 Authors: Andreas Knuepfer, Holger Brunst, Ronny Brendel, Thomas Kriebitzsch
*/

/** 
 *  @file OTF_Keywords.h
 *
 *  @brief All record format keywords are located in this file.
 *
 *  \ingroup internal
 */


#ifndef OTF_KEYWORDS_H
#define OTF_KEYWORDS_H

/* 

define all record format keywords here in order to have a single 
spot for them.

the keywords identifying record types must follow some special rules:

1) all keywords are UPPER CASE

2) no keyword must be equal to another keyword, prefixes are allowed though

3) all definition records and only them have the prefix "DEF" or "D"

4) all summary records and only them have the prefix "SUM" or "S"

5) all status records and only them have the prefix "T" (better ideas?)

7) the prefixes for definition, summary resp. status record types are 
	extra, the don't go into the specific definitions - see below 

8) there is always a long and a short version for every keyword marked with
	_L_ and _S_. both versions might be equal, but always they have the 
	same first letter given by an additional macro marked with _F_

- white spaces after the keyword should be included in the keyword

*/


/* *** general keywords *** */


#define OTF_KEYWORD_PROCESS_CHAR 				'*'


/* *** definition record keywords *** */

#define OTF_KEYWORD_L_DEF_PREFIX 				"DEF"
#define OTF_KEYWORD_S_DEF_PREFIX 				"D"

#define OTF_KEYWORD_L_DEFTIMERRESOLUTION 		"TIMERRESOLUTION"
#define OTF_KEYWORD_S_DEFTIMERRESOLUTION 		"TR"
#define OTF_KEYWORD_F_DEFTIMERRESOLUTION 		'T'

#define OTF_KEYWORD_L_DEFPROCESS 				"PROCESS"
#define OTF_KEYWORD_S_DEFPROCESS 				"P"
#define OTF_KEYWORD_F_DEFPROCESS 				'P'

#define OTF_KEYWORD_L_DEFPROCESSGROUP 			"PROCESSGROUP"
#define OTF_KEYWORD_S_DEFPROCESSGROUP 			"PG"
#define OTF_KEYWORD_F_DEFPROCESSGROUP 			'P'

#define OTF_KEYWORD_L_DEFFUNCTION 				"FUNCTION"
#define OTF_KEYWORD_S_DEFFUNCTION 				"F"
#define OTF_KEYWORD_F_DEFFUNCTION 				'F'

#define OTF_KEYWORD_L_DEFFUNCTIONGROUP  		"FUNCTIONGROUP"
#define OTF_KEYWORD_S_DEFFUNCTIONGROUP  		"FG"
#define OTF_KEYWORD_F_DEFFUNCTIONGROUP  		'F'

#define OTF_KEYWORD_L_DEFCOLLOP 				"COLLOP"
#define OTF_KEYWORD_S_DEFCOLLOP 				"CO"
#define OTF_KEYWORD_F_DEFCOLLOP 				'C'

#define OTF_KEYWORD_L_DEFCOUNTER 				"COUNTER"
#define OTF_KEYWORD_S_DEFCOUNTER 				"CNT"
#define OTF_KEYWORD_F_DEFCOUNTER 				'C'

#define OTF_KEYWORD_L_DEFCOUNTERGROUP 			"COUNTERGROUP"
#define OTF_KEYWORD_S_DEFCOUNTERGROUP 			"CG"
#define OTF_KEYWORD_F_DEFCOUNTERGROUP 			'C'

#define OTF_KEYWORD_L_DEFINITIONCOMMENT 		"COMMENT"
#define OTF_KEYWORD_S_DEFINITIONCOMMENT 		"CMT"
#define OTF_KEYWORD_F_DEFINITIONCOMMENT 		'C'

#define OTF_KEYWORD_L_DEFSCL 					"SCL"
#define OTF_KEYWORD_S_DEFSCL 					"S"
#define OTF_KEYWORD_F_DEFSCL 					'S'

#define OTF_KEYWORD_L_DEFSCLFILE 				"SCLFILE"
#define OTF_KEYWORD_S_DEFSCLFILE 				"SF"
#define OTF_KEYWORD_F_DEFSCLFILE 				'S'

#define OTF_KEYWORD_L_DEFVERSION 				"VERSION"
#define OTF_KEYWORD_S_DEFVERSION 				"V"
#define OTF_KEYWORD_F_DEFVERSION 				'V'

#define OTF_KEYWORD_L_DEFCREATOR 				"CREATOR"
#define OTF_KEYWORD_S_DEFCREATOR 				"CR"
#define OTF_KEYWORD_F_DEFCREATOR 				'C'

#define OTF_KEYWORD_L_DEFFILE 					"FILE"
#define OTF_KEYWORD_S_DEFFILE 					"FL"
#define OTF_KEYWORD_F_DEFFILE	 				'F'

#define OTF_KEYWORD_L_DEFFILEGROUP 				"FILEGROUP"
#define OTF_KEYWORD_S_DEFFILEGROUP 				"FLG"
#define OTF_KEYWORD_F_DEFFILEGROUP 				'F'


/* *** event record keywords *** */


#define OTF_KEYWORD_L_ENTER 					"ENTER"
#define OTF_KEYWORD_S_ENTER 					"E"
#define OTF_KEYWORD_F_ENTER 					'E'

#define OTF_KEYWORD_L_LEAVE 					"LEAVE"
#define OTF_KEYWORD_S_LEAVE 					"L"
#define OTF_KEYWORD_F_LEAVE 					'L'

#define OTF_KEYWORD_L_RECEIVE 					"RECEIVE"
#define OTF_KEYWORD_S_RECEIVE 					"R"
#define OTF_KEYWORD_F_RECEIVE 					'R'

#define OTF_KEYWORD_L_SEND  					"SEND"
#define OTF_KEYWORD_S_SEND  					"S"
#define OTF_KEYWORD_F_SEND  					'S'

#define OTF_KEYWORD_L_COUNTER 					"COUNTER"
#define OTF_KEYWORD_S_COUNTER 					"CNT"
#define OTF_KEYWORD_F_COUNTER 					'C'

#define OTF_KEYWORD_L_COLLECTIVEOPERATION 		"COLLOP"
#define OTF_KEYWORD_S_COLLECTIVEOPERATION 		"COP"
#define OTF_KEYWORD_F_COLLECTIVEOPERATION 		'C'

#define OTF_KEYWORD_L_EVENTCOMMENT 				"#EVTCOMMENT"
#define OTF_KEYWORD_S_EVENTCOMMENT 				"#"
#define OTF_KEYWORD_F_EVENTCOMMENT 				'#'

#define OTF_KEYWORD_L_BEGINPROCESS 				"PROCESSBEGIN"
#define OTF_KEYWORD_S_BEGINPROCESS 				"PB"
#define OTF_KEYWORD_F_BEGINPROCESS 				'P'

#define OTF_KEYWORD_L_ENDPROCESS 				"PROCESSEND"
#define OTF_KEYWORD_S_ENDPROCESS 				"PE"
#define OTF_KEYWORD_F_ENDPROCESS 				'P'

#define OTF_KEYWORD_L_FILEOPERATION				"FILEOP"
#define OTF_KEYWORD_S_FILEOPERATION				"F"
#define OTF_KEYWORD_F_FILEOPERATION				'F'


/* *** summary record keywords *** */


#define OTF_KEYWORD_L_SUM_PREFIX 				"SUM"
#define OTF_KEYWORD_S_SUM_PREFIX 				"S"
#define OTF_KEYWORD_F_SUM_PREFIX 				'S'

#define OTF_KEYWORD_L_SUMFUNCTION 				"FUNCTION"
#define OTF_KEYWORD_S_SUMFUNCTION 				"F"
#define OTF_KEYWORD_F_SUMFUNCTION 				'F'

#define OTF_KEYWORD_L_SUMFUNCTIONGROUP  		"FUNCTIONGROUP"
#define OTF_KEYWORD_S_SUMFUNCTIONGROUP  		"G"
#define OTF_KEYWORD_F_SUMFUNCTIONGROUP  		'G'

#define OTF_KEYWORD_L_SUMMESSAGE 				"MESSAGE"
#define OTF_KEYWORD_S_SUMMESSAGE 				"M"
#define OTF_KEYWORD_F_SUMMESSAGE 				'M'

#define OTF_KEYWORD_L_SUMCOMMENT 				"COMMENT"
#define OTF_KEYWORD_S_SUMCOMMENT 				"C"
#define OTF_KEYWORD_F_SUMCOMMENT 				'C'

#define OTF_KEYWORD_L_SUMFILEOPERATION			"FILEOPERATION"
#define OTF_KEYWORD_S_SUMFILEOPERATION			"FL"
#define OTF_KEYWORD_F_SUMFILEOPERATION			'F'

#define OTF_KEYWORD_L_SUMFILEGROUPOPERATION		"FILEGROUPOPERATION"
#define OTF_KEYWORD_S_SUMFILEGROUPOPERATION		"FLG"
#define OTF_KEYWORD_F_SUMFILEGROUPOPERATION		'F'

/* *** status record keywords *** */


#define OTF_KEYWORD_L_STATUS_PREFIX 			"T"
#define OTF_KEYWORD_S_STATUS_PREFIX 			"T"


/* *** snapshot keywords */


#define OTF_KEYWORD_L_SNAPSHOT_PREFIX 			"T"
#define OTF_KEYWORD_S_SNAPSHOT_PREFIX 			"T"
#define OTF_KEYWORD_F_SNAPSHOT_PREFIX 			'T'

#define OTF_KEYWORD_L_SNAPSHOT_ENTER			"ENTER"
#define OTF_KEYWORD_S_SNAPSHOT_ENTER			"E"
#define OTF_KEYWORD_F_SNAPSHOT_ENTER			'E'

#define OTF_KEYWORD_L_SNAPSHOT_SEND				"SEND"
#define OTF_KEYWORD_S_SNAPSHOT_SEND				"S"
#define OTF_KEYWORD_F_SNAPSHOT_SEND				'S'

#define OTF_KEYWORD_L_SNAPSHOT_COMMENT			"COMMENT"
#define OTF_KEYWORD_S_SNAPSHOT_COMMENT			"C"
#define OTF_KEYWORD_F_SNAPSHOT_COMMENT			'C'

#define OTF_KEYWORD_L_SNAPSHOT_OPENFILE			"OPENFILE"
#define OTF_KEYWORD_S_SNAPSHOT_OPENFILE			"OF"
#define OTF_KEYWORD_F_SNAPSHOT_OPENFILE			'O'


/* *** local keywords *** */


#define OTF_KEYWORD_L_LOCAL_BYTES				"BYTES"
#define OTF_KEYWORD_S_LOCAL_BYTES				"B"

#define OTF_KEYWORD_L_LOCAL_BYTESREAD			"BYTESREAD"
#define OTF_KEYWORD_S_LOCAL_BYTESREAD			"BR"

#define OTF_KEYWORD_L_LOCAL_BYTESWRITE			"BYTESWRITE"
#define OTF_KEYWORD_S_LOCAL_BYTESWRITE			"BW"

#define OTF_KEYWORD_L_LOCAL_COMMUNICATOR		"COMM"
#define OTF_KEYWORD_S_LOCAL_COMMUNICATOR		"C"

#define OTF_KEYWORD_L_LOCAL_COUNT				"COUNT"
#define OTF_KEYWORD_S_LOCAL_COUNT				"N"

#define OTF_KEYWORD_L_LOCAL_DURATION			"DUR"
#define OTF_KEYWORD_S_LOCAL_DURATION			"D"

#define OTF_KEYWORD_L_LOCAL_EXCLTIME			"EXCL"
#define OTF_KEYWORD_S_LOCAL_EXCLTIME			"E"

#define OTF_KEYWORD_L_LOCAL_FILE				"FILE"
#define OTF_KEYWORD_S_LOCAL_FILE				"F"

#define OTF_KEYWORD_L_LOCAL_GROUP				"GROUP"
#define OTF_KEYWORD_S_LOCAL_GROUP				"G"

#define OTF_KEYWORD_L_LOCAL_HANDLEID			"HANDLEID"
#define OTF_KEYWORD_S_LOCAL_HANDLEID			"H"

#define OTF_KEYWORD_L_LOCAL_INCLTIME			"INCL"
#define OTF_KEYWORD_S_LOCAL_INCLTIME			"I"

#define OTF_KEYWORD_L_LOCAL_LENGTH				"LEN"
#define OTF_KEYWORD_S_LOCAL_LENGTH				"L"

#define OTF_KEYWORD_L_LOCAL_LINE				"LINE"
#define OTF_KEYWORD_S_LOCAL_LINE				"LN"

#define OTF_KEYWORD_L_LOCAL_MEMBERS				"MEMBERS"
#define OTF_KEYWORD_S_LOCAL_MEMBERS				"M"

#define OTF_KEYWORD_L_LOCAL_NAME				"NAME"
#define OTF_KEYWORD_S_LOCAL_NAME				"NM"

#define OTF_KEYWORD_L_LOCAL_NUMBERSENT			"NUMSENT"
#define OTF_KEYWORD_S_LOCAL_NUMBERSENT			"NS"

#define OTF_KEYWORD_L_LOCAL_NUMBERCLOSE			"NUMCLOSE"
#define OTF_KEYWORD_S_LOCAL_NUMBERCLOSE			"NC"

#define OTF_KEYWORD_L_LOCAL_NUMBERREAD			"NUMREAD"
#define OTF_KEYWORD_S_LOCAL_NUMBERREAD			"NRD"

#define OTF_KEYWORD_L_LOCAL_NUMBERRECVD			"NUMRECVD"
#define OTF_KEYWORD_S_LOCAL_NUMBERRECVD			"NR"

#define OTF_KEYWORD_L_LOCAL_NUMBERSEEK			"NUMSEEK"
#define OTF_KEYWORD_S_LOCAL_NUMBERSEEK			"NSK"

#define OTF_KEYWORD_L_LOCAL_NUMBEROPEN			"NUMOPEN"
#define OTF_KEYWORD_S_LOCAL_NUMBEROPEN			"NO"

#define OTF_KEYWORD_L_LOCAL_NUMBERWRITE			"NUMWRITTEN"
#define OTF_KEYWORD_S_LOCAL_NUMBERWRITE			"NW"

#define OTF_KEYWORD_L_LOCAL_OPERATION			"OPERATION"
#define OTF_KEYWORD_S_LOCAL_OPERATION			"OP"

#define OTF_KEYWORD_L_LOCAL_OTIME				"OTIME"
#define OTF_KEYWORD_S_LOCAL_OTIME				"O"

#define OTF_KEYWORD_L_LOCAL_PARENT				"PARENT"
#define OTF_KEYWORD_S_LOCAL_PARENT				"PT"

#define OTF_KEYWORD_L_LOCAL_PROPERTIES			"PROPERTIES"
#define OTF_KEYWORD_S_LOCAL_PROPERTIES			"P"

#define OTF_KEYWORD_L_LOCAL_RECVD				"RECVD"
#define OTF_KEYWORD_S_LOCAL_RECVD				"R"

#define OTF_KEYWORD_L_LOCAL_ROOT				"ROOT"
#define OTF_KEYWORD_S_LOCAL_ROOT				"RT"

#define OTF_KEYWORD_L_LOCAL_SCL					"SCL"
#define OTF_KEYWORD_S_LOCAL_SCL					"X"

#define OTF_KEYWORD_L_LOCAL_SENT				"SENT"
#define OTF_KEYWORD_S_LOCAL_SENT				"S"

#define OTF_KEYWORD_L_LOCAL_TAG					"TAG"
#define OTF_KEYWORD_S_LOCAL_TAG					"T"

#define OTF_KEYWORD_L_LOCAL_TYPE				"TYPE"
#define OTF_KEYWORD_S_LOCAL_TYPE				"Y"

#define OTF_KEYWORD_L_LOCAL_UNIT				"UNIT"
#define OTF_KEYWORD_S_LOCAL_UNIT				"U"

#define OTF_KEYWORD_L_LOCAL_VALUE				"VALUE"
#define OTF_KEYWORD_S_LOCAL_VALUE				"V"



#endif /* OTF_KEYWORDS_H */
