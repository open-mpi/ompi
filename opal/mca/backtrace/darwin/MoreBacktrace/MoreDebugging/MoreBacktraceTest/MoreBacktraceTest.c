/*
	File:		MoreBacktraceTest.c

	Contains:	A simple program to test MoreBacktrace.

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

*/

/////////////////////////////////////////////////////////////////

#include "MoreSetup.h"

// System prototypes

#include <Carbon/Carbon.h>

// Our interfaces

#include "MoreBacktraceTestCommon.h"

/////////////////////////////////////////////////////////////////
#pragma mark ***** Utilities

static OSStatus GetControlByIDQ(WindowRef inWindow, OSType signature, SInt32 id, ControlRef *outControl)
	// A simple wrapper around GetControlByID.
{
	ControlID theID;

	theID.signature = signature;
	theID.id = id;	
	return GetControlByID(inWindow, &theID, outControl);
}

static OSStatus FindFinderProcess(ProcessSerialNumber *psnPtr)
	// Find the ProcessSerialNumber of the Finder process.
{
	OSStatus		err;
	ProcessInfoRec	info;
	Boolean			found;
	
	psnPtr->lowLongOfPSN	= kNoProcess;
	psnPtr->highLongOfPSN	= kNoProcess;
	
	found = false;
	do {
		err = GetNextProcess(psnPtr);
		if (err == noErr) {
			memset(&info, 0, sizeof(info));
			
			err = GetProcessInformation(psnPtr, &info);
			if (err == noErr) {
				found = (info.processSignature == 'MACS');
			}
		}
	} while ( (err == noErr) && ! found );

	return err;
}

/////////////////////////////////////////////////////////////////
#pragma mark ***** Compile-Time Parameters

// We hard-code our output to Monaco 10.  This is bad programming, 
// but it suffices for a simple hack like this.

static const UInt8  kOutputFont[] = "\pMonaco";
static const SInt16 kOutputSize = 10;

// The main text field will display about 70 frames, so that's all that 
// we ask for.  This is more than enough for our tests.

enum {
	kFrameCount = 70
};

/////////////////////////////////////////////////////////////////
#pragma mark ***** GUI Infrastructure

static void DoAbout(void)
	// Displays the about box.
{
	SInt16 junkHit;
	
	(void) StandardAlert(kAlertPlainAlert, "\pMoreBacktraceTest", "\pA simple program to test MoreBacktrace.\r\rDTS\r\r© 2006 Apple Computer, Inc.", NULL, &junkHit);
}

static void DisplayError(OSStatus errNum)
{
	OSStatus	junk;
	Str255 		errStr;
	SInt16 		junkHit;
	
	if ( (errNum != noErr) && (errNum != userCanceledErr) ) {
		NumToString(errNum, errStr);

		junk = StandardAlert(kAlertStopAlert, "\pError.", errStr, NULL, &junkHit);
		assert(junk == noErr);
	}
}

static ControlRef gOutputText = NULL;
static ControlRef gPIDText = NULL;

static OSStatus SetOutputText(CFStringRef newText)
	// Put newText into the output edit text control.
	// We use an edit text control because it's 
	// a) easy, b) handles multiple lines of text, and 
	// c) allows the user to copy the results.
{
	OSStatus err;
	
	assert(newText != NULL);
	
	err = SetControlData(gOutputText, kControlEntireControl, kControlEditTextCFStringTag, sizeof(newText), &newText);
	Draw1Control(gOutputText);

	return err;
}

static void ClearOutput(void)
	// Clear the output text.
{
	OSStatus junk;
	
	junk = SetOutputText(CFSTR(""));
	assert(junk == noErr);
}

static OSStatus OutputFrames(char *bt)
	// Output a textual description of frameCount frames from frameArray 
	// into the output text control.  Actually generating the text from 
	// the frame array is handled by the common test code; all we have 
	// to do here is reflow the text into two columns so it fits in 
	// our text field.
{
	OSStatus			err;
	Boolean				removedOne;
	size_t				row;
	size_t				col;
	CFStringRef			btStr;
	CFArrayRef			frameStrings;
	size_t				frameCount;
	size_t				frameIndex;
	CFMutableStringRef	result;

	btStr = NULL;
	frameStrings = NULL;
	result = NULL;
	
	// Remove any trailing newlines.
	
	removedOne = false;
	do {
		size_t	len;
		
		len = strlen(bt);
		if (len > 0 && bt[len] == '\n') {
			bt[len] = 0;
			removedOne = true;
		}
	} while ( removedOne );
	
	// Break the string up into lines.
	
	err = noErr;
	btStr = CFStringCreateWithCString(NULL, bt, kCFStringEncodingUTF8);
	if (btStr == NULL) {
		err = coreFoundationUnknownErr;
	}

	if (err == noErr) {
		frameStrings = CFStringCreateArrayBySeparatingStrings(NULL, btStr, CFSTR("\n"));
		if (frameStrings == NULL) {
			err = coreFoundationUnknownErr;
		}
		assert(frameStrings != NULL);
	}

	// Count the frames and then place it into the output text field.
	
	if (err == noErr) {
		frameCount = (size_t) CFArrayGetCount(frameStrings);
		
		result = CFStringCreateMutable(NULL, 0);
		if (result == NULL) {
			err = coreFoundationUnknownErr;
		}
	}
	if (err == noErr) {
		if ( frameCount > (kFrameCount / 2) ) {

			// We need two columns, so reflow.  Nasty.

			for (row = 0; row < (kFrameCount / 2); row++) {
				for (col = 0; col < 2; col++) {
					frameIndex = (col * (kFrameCount / 2)) + row;
					
					if (frameIndex < frameCount) {
						CFStringRef		frameStr;
						CFStringRef		frameStrTrimmed;
						CFStringRef		columnPad;
						
						columnPad = NULL;
						frameStrTrimmed = NULL;
						
						frameStr = CFArrayGetValueAtIndex(frameStrings, frameIndex);
						assert(frameStr != NULL);
						
						if ( CFStringGetLength(frameStr) > 56 ) {
							frameStrTrimmed = CFStringCreateWithSubstring(NULL, frameStr, CFRangeMake(0, 56));
							columnPad = CFStringCreateWithCString(NULL, "", kCFStringEncodingUTF8);
						} else {
							frameStrTrimmed = CFRetain(frameStr);
							if (col == 0) {
								columnPad = CFStringCreateWithFormat(NULL, NULL, CFSTR("%*.*s"), 56 - CFStringGetLength(frameStr), 56 - CFStringGetLength(frameStr), "");
							} else {
								columnPad = CFStringCreateWithCString(NULL, "", kCFStringEncodingUTF8);
							}
						}
						assert(frameStrTrimmed != NULL);
						assert(columnPad != NULL);
						
						if ( (frameStrTrimmed != NULL) && (columnPad != NULL) ) {
							CFStringAppendFormat(result, NULL, CFSTR("%@%@"), frameStrTrimmed, columnPad);	
							if (col == 0) {
								CFStringAppend(result, CFSTR("  "));
							}
						}
						
						if (columnPad != NULL) {
							CFRelease(columnPad);
						}
						if (frameStrTrimmed != NULL) {
							CFRelease(frameStrTrimmed);
						}
					}
				}
				CFStringAppend(result, CFSTR("\r"));
			}
		} else {

			// We can get away with a single column.
			
			for (frameIndex = 0; frameIndex < frameCount; frameIndex++) {
				CFStringAppendFormat(result, NULL, CFSTR("%@\r"), CFArrayGetValueAtIndex(frameStrings, frameIndex));
			}
		}
	}

	// Put the string into the control.
	
	if (err == noErr) {
		err = SetOutputText(result);
	}

	// Clean up.
	
	if (btStr != NULL) {
		CFRelease(btStr);
	}
	if (frameStrings != NULL) {
		CFRelease(frameStrings);
	}
	if (result != NULL) {
		CFRelease(result);
	}
			
	return err;
}

/////////////////////////////////////////////////////////////////
#pragma mark ***** Test Handlers

static void GUITestMoreBacktraceMachSelf(void)
	// Run in response to a click of the "MoreBacktraceMachSelf"
	// button.  It does the test and displays the result.
{
	OSStatus		err;
	char *			bt;

	ClearOutput();

	bt = NULL;
	err = TestMoreBacktraceMachSelf(&bt);
	if (err == noErr) {
		err = OutputFrames(bt);
	}
	DisplayError(err);
	free(bt);
}

static void GUITestMoreBacktraceMachThread(void)
	// Run in response to a click of the "MoreBacktraceMachThread"
	// button.  It does the test and displays the result.  It gets 
	// the target PID from the "PID" field.
{
	OSStatus		err;
	CFStringRef		pidStr;
	pid_t			pid;
	char *			bt;

	ClearOutput();
	
	bt = NULL;
	pidStr = NULL;
	
	// Get the PID of the process to sample from the 
	// gPIDText edit text control and convert that to 
	// the Mach task control port.
	
	err = GetControlData(gPIDText, kControlEntireControl, kControlEditTextCFStringTag, sizeof(pidStr), &pidStr, NULL);
	if (err == noErr) {
		pid = (pid_t) CFStringGetIntValue(pidStr);
	}
	if (err == noErr) {
		err = TestMoreBacktraceMachThread(pid, &bt);
	}
	if (err == noErr) {
		err = OutputFrames(bt);
	}
	DisplayError(err);
	
	// Clean up.
	
	free(bt);
	if (pidStr != NULL) {
		CFRelease(pidStr);
	}
}

static void GUITestSignalBacktrace(void)
	// Run in response to a click of the "Signal Test"
	// button.  It does the test and displays the result.
{
	OSStatus 			err;
	char *				bt;

	bt = NULL;

	ClearOutput();
	
	// Call a routine that sends a signal to us.
	
	err = TestMoreBacktraceMachSelfSignal(false, &bt);
	if (err == noErr) {
		err = OutputFrames(bt);
	}
	DisplayError(err);
	
	free(bt);
}

static void GUITestStackSmash(void)
	// Run in response to a click of the "Stack Smash Test"
	// button.  It does the test and displays the result.
{
	OSStatus		err;
	char *			bt;

	ClearOutput();

	bt = NULL;
	
	err = TestMoreBacktraceMachThreadStackSmash(&bt);
	if (err == noErr) {
		err = OutputFrames(bt);
	}
	DisplayError(err);
	
	free(bt);
}

/////////////////////////////////////////////////////////////////
#pragma mark ***** Boilerplate Application Stuff

static EventHandlerUPP gApplicationEventHandlerUPP;		// -> ApplicationEventHandler

static const EventTypeSpec kApplicationEvents[] = { {kEventClassCommand, kEventCommandProcess} };

static pascal OSStatus ApplicationEventHandler(EventHandlerCallRef inHandlerCallRef, 
											   EventRef inEvent, void *inUserData)
	// Dispatches HICommands to their implementations.
{
	OSStatus 	err;
	HICommand 	command;
	#pragma unused(inHandlerCallRef)
	#pragma unused(inUserData)
	
	assert( GetEventClass(inEvent) == kEventClassCommand  );
	assert( GetEventKind(inEvent)  == kEventCommandProcess);
	
	err = GetEventParameter(inEvent, kEventParamDirectObject, typeHICommand, NULL, sizeof(command), NULL, &command);
	if (err == noErr) {
		switch (command.commandID) {
			case kHICommandAbout:
				DoAbout();
				break;
			case 'btMS':
				GUITestMoreBacktraceMachSelf();
				break;
			case 'btMT':
				GUITestMoreBacktraceMachThread();
				break;
			case 'btST':
				GUITestSignalBacktrace();
				break;
			case 'btSS':
				GUITestStackSmash();
				break;
			default:
				err = eventNotHandledErr;
				break;
		}
	}
	
	return err;
}

int main(int argc, char* argv[])
{
	OSStatus		err;
	IBNibRef 		nibRef;
	WindowRef 		window;
	#pragma unused(argc)
	#pragma unused(argv)
	
	// DebugStr("\pmain");
	
	nibRef = NULL;
	
	// Create menu bar from NIB.
	
	err = CreateNibReference(CFSTR("main"), &nibRef);
	if (err == noErr) {
		err = SetMenuBarFromNib(nibRef, CFSTR("MenuBar"));
	}
	if (nibRef != NULL) {
		DisposeNibReference(nibRef);
		nibRef = NULL;
	}

	// Create main window from NIB.
	
	if (err == noErr) {
		err = CreateNibReference(CFSTR("MainWindow"), &nibRef);
	}
	if (err == noErr) {
		err = CreateWindowFromNib(nibRef, CFSTR("MainWindow"), &window);
	}
	if (nibRef != NULL) {
		DisposeNibReference(nibRef);
		nibRef = NULL;
	}

	// Install our HICommand handler.
	
	if (err == noErr) {
		gApplicationEventHandlerUPP = NewEventHandlerUPP(ApplicationEventHandler);
		assert(gApplicationEventHandlerUPP != NULL);

		err = InstallApplicationEventHandler(gApplicationEventHandlerUPP, 
											 GetEventTypeCount(kApplicationEvents), 
											 kApplicationEvents, NULL, NULL);
	}

	// Get the output static text control and set it to Monaco 9.
	
	if (err == noErr) {
		err = GetControlByIDQ(window, 'ETXT', 0, &gOutputText);
	}
	if (err == noErr) {
		ControlFontStyleRec styleRec;
		SInt16				monacoFontNum;

		GetFNum(kOutputFont, &monacoFontNum);
		
		styleRec.flags = kControlUseFontMask | kControlUseSizeMask;
		styleRec.font = monacoFontNum;
		styleRec.size = kOutputSize;

		err = SetControlData(gOutputText, kControlEntireControl, kControlStaticTextStyleTag,
							 sizeof(styleRec), &styleRec);
	}

	// Get the PID edit text field and initialise it to the Finder's PID.
	
	if (err == noErr) {
		err = GetControlByIDQ(window, 'PIDT', 0, &gPIDText);
	}
	if (err == noErr) {
		ProcessSerialNumber finderPSN;
		pid_t				finderPID;
		CFStringRef			finderPIDStr;
		
		finderPIDStr = NULL;
		
		err = FindFinderProcess(&finderPSN);
		if (err == noErr) {
			err = GetProcessPID(&finderPSN, &finderPID);
		}
		if (err == noErr) {
			finderPIDStr = CFStringCreateWithFormat(NULL, NULL, CFSTR("%ld"), (long) finderPID);
			if (finderPIDStr == NULL) {
				err = coreFoundationUnknownErr;
			}
		}
		if (err == noErr) {
			err = SetControlData(gPIDText, kControlEntireControl, kControlEditTextCFStringTag, sizeof(finderPIDStr), &finderPIDStr);
		}
		
		if (finderPIDStr) {
			CFRelease(finderPIDStr);
		}
		
		// I don't want to refuse to start up just because this code failed, 
		// so swallow any error.
		
		assert(err == noErr);
		err = noErr;
	}

	if (err == noErr) {
		// The window was created hidden so show it.
	
		ShowWindow( window );
	
		// Call the event loop

		RunApplicationEventLoop();
	}

	return (err == noErr) ? EXIT_SUCCESS : EXIT_FAILURE;
}
