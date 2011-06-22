/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2011.
 Authors: Andreas Knuepfer, Denis Huenich, Johannes Spazier
*/

#ifndef DEFINITIONS_H
#define DEFINITIONS_H

#define _BYTE 1.0
#define KBYTE 1024.0
#define MBYTE 1048576.0
#define GBYTE 1073741824.0
#define KILO  1000.0
#define MEGA  1000000.0
#define GIGA  1000000000.0

/* scale units */

#define SECOND             " sec"
#define K_SECOND           " K*sec"
#define M_SECOND           " M*sec"
#define G_SECOND           " G*sec"

#define INVOC	            " \\#"
#define K_INVOC	         " K*\\#"
#define M_INVOC	         " M*\\#"
#define G_INVOC	         " G*\\#"
#define BYTE_SEC	         " Byte/s"
#define KBYTE_SEC	         " KByte/s"
#define MBYTE_SEC	         " MByte/s"
#define GBYTE_SEC	         " GByte/s"
#define BYTE_TEXT          " Byte"
#define KBYTE_TEXT         " KByte"
#define MBYTE_TEXT         " MByte"
#define GBYTE_TEXT         " GByte"

/* specify which global summary should be printed */

#define TEX_OFF           -1
#define TEX_ALL           0
#define TEX_ALLPLOT       1
#define TEX_FUNC          2
#define TEX_P2P           3
#define TEX_COLLOP        4

/* definitions for prodtex */

#define TINY_TEX     6   //range of color (6 different colors)
#define SMALL_TEX    11  //range of color (11 different colors)
#define MIDDLE_TEX   16  //range of color (16 different colors)
#define LARGE_TEX    21  //range of color (21 different colors)
#define HUGE_TEX     26  //range of color (26 different colors)

#define P2P_AV_RAT  1 // Average Rate P2P
#define P2P_AV_DUR  2 // Average Duration P2P
#define P2P_AV_LEN  3 // Average Message Length P2P
#define P2P_SUM_DUR 4 // Sum of Duration P2P
#define P2P_SUM_LEN 5 // Sum of Message Length P2P
#define P2P_ALL     6 // All plots

#define MAXIMUM(x , y) (((x) > (y)) ? (x) : (y))
#define MINIMUM(x , y) (((x) < (y)) ? (x) : (y))

#endif /* DEFINITIONS_H */
