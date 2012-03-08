/*
 This is part of the OTF library. Copyright by ZIH, TU Dresden 2005-2012.
 Authors: Michael Heyde
*/

#ifndef OTFINFO_ERROR_H
#define OTFINFO_ERROR_H

#define otfinfo_assert(expr) if(!(expr)) otfinfo_assert_impl(__FILE__, __LINE__, #expr);
void otfinfo_assert_impl(const char* f, int l, const char* expr);

#endif /* OTFINFO_ERROR_H */
