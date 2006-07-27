/*
	File:		MoreBacktraceTestCommon.h

	Contains:	Common code for testing backtraces.

	Written by:	DTS

	Copyright:	Copyright (c) 2006 by Apple Computer, Inc., All Rights Reserved.

	Disclaimer:	IMPORTANT:  This Apple software is supplied to you by Apple Computer, Inc.
				("Apple") in consideration of your agreement to the following terms, and your
				use, installation, modification or redistribution of this Apple software
				constitutes acceptance of these terms.  If you do not agree with these terms,
				please do not use, install, modify or redistribute this Apple software.

				In consideration of your agreement to abide by the following terms, and subject
				to these terms, Apple grants you a personal, non-exclusive license, under Apple’s
				copyrights in this original Apple software (the "Apple Software"), to use,
				reproduce, modify and redistribute the Apple Software, with or without
				modifications, in source and/or binary forms; provided that if you redistribute
				the Apple Software in its entirety and without modifications, you must retain
				this notice and the following text and disclaimers in all such redistributions of
				the Apple Software.  Neither the name, trademarks, service marks or logos of
				Apple Computer, Inc. may be used to endorse or promote products derived from the
				Apple Software without specific prior written permission from Apple.  Except as
				expressly stated in this notice, no other rights or licenses, express or implied,
				are granted by Apple herein, including but not limited to any patent rights that
				may be infringed by your derivative works or by other works in which the Apple
				Software may be incorporated.

				The Apple Software is provided by Apple on an "AS IS" basis.  APPLE MAKES NO
				WARRANTIES, EXPRESS OR IMPLIED, INCLUDING WITHOUT LIMITATION THE IMPLIED
				WARRANTIES OF NON-INFRINGEMENT, MERCHANTABILITY AND FITNESS FOR A PARTICULAR
				PURPOSE, REGARDING THE APPLE SOFTWARE OR ITS USE AND OPERATION ALONE OR IN
				COMBINATION WITH YOUR PRODUCTS.

				IN NO EVENT SHALL APPLE BE LIABLE FOR ANY SPECIAL, INDIRECT, INCIDENTAL OR
				CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE
				GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION)
				ARISING IN ANY WAY OUT OF THE USE, REPRODUCTION, MODIFICATION AND/OR DISTRIBUTION
				OF THE APPLE SOFTWARE, HOWEVER CAUSED AND WHETHER UNDER THEORY OF CONTRACT, TORT
				(INCLUDING NEGLIGENCE), STRICT LIABILITY OR OTHERWISE, EVEN IF APPLE HAS BEEN
				ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

	Change History (most recent first):

$Log: MoreBacktraceTestCommon.h,v $
Revision 1.1  2006/01/22 22:47:51  eskimo1
A new module that abstracts the test code out of the GUI test app so that it can also be used by the command line tool.


*/

#pragma once

/////////////////////////////////////////////////////////////////

// MoreIsBetter Setup

#include "MoreSetup.h"

// Mac OS Interfaces

#include <stdbool.h>
#include <unistd.h>

// MIB Interfaces

#include "MoreBacktrace.h"

/////////////////////////////////////////////////////////////////

#ifdef __cplusplus
extern "C" {
#endif

/////////////////////////////////////////////////////////////////

// This module contains common test code that's shared between the application 
// and the command line tool.

extern int TestMoreBacktraceFramesToText(
	const MoreBTFrame	frames[], 
	size_t				frameCount, 
	bool				lookupSymbolNames, 
	char **				btPtr
);
	// Converts a backtrace (specified by frames and frameCount) 
	// to textual form, returning a pointer to the text in *btPtr. 
	// Each line is separated by a '\n' and the entire string 
	// is null terminated.  The caller is responsible for disposing 
	// of this result using free.
	//
	// If lookupSymbolNames is true, 
	// the textual representation will contain symbols generated 
	// using the MoreAddrToSym module.  Given that this module 
	// only supports looking up symbols in the current task 
	// (at least currently), you should only pass true to this 
	// parameter if the backtrace came from the current task.
	//
	// On entry, frames must not be NULL.
	// On entry, btPtr must not be NULL.
	// On entry, *btPtr must be NULL.
	// Returns an errno-style error code.
	// On success, *btPtr will not be NULL.
	// On error, *btPtr will be NULL.
	
// The following routines run a variety of different backtrace tests. 
// This each return an errno-style error code, and set *btPtr as 
// specified by TestMoreBacktraceFramesToText.

extern int TestMoreBacktraceMachSelf(char **btPtr);
	// Backtrace the current thread.
	
extern int TestMoreBacktraceMachThread(pid_t pid, char **btPtr);
	// Backtrace a random thread in the specified task.

extern int TestMoreBacktraceMachSelfSignal(bool nonLeaf, char **btPtr);
	// Backtrace across a signal handler.  If nonLeaf is set, 
	// this requires user intervention to deliver the signal.
	
extern int TestMoreBacktraceMachThreadStackSmash(char **btPtr);
	// Backtrace a thread with a smashed stack.

#ifdef __cplusplus
}
#endif
