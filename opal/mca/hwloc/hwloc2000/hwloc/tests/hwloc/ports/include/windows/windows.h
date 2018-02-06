/*
 * Copyright © 2009-2016 Inria.  All rights reserved.
 * Copyright © 2009-2012 Université Bordeaux
 * See COPYING in top-level directory.
 */

#ifndef HWLOC_PORT_WINDOWS_WINDOWS_H
#define HWLOC_PORT_WINDOWS_WINDOWS_H

#include <inttypes.h>

#define CALLBACK
#define FALSE 0

#define DECLARE_HANDLE(n) typedef struct n##__ {int i;} *n
DECLARE_HANDLE(HINSTANCE);
typedef HINSTANCE HMODULE;
typedef int WINBOOL, BOOL;
typedef int64_t LONGLONG;
typedef uint64_t DWORDLONG;
typedef DWORDLONG ULONGLONG, *PULONGLONG;
typedef unsigned char BYTE, UCHAR;
typedef unsigned short WORD, USHORT;
typedef unsigned int UINT, UINT_PTR, DWORD, *PDWORD, *LPDWORD;
typedef unsigned long ULONG_PTR, DWORD_PTR, *PDWORD_PTR;
typedef long LONG, LONG_PTR;
typedef const char *LPCSTR;
typedef int (*FARPROC)(void);
typedef void *PVOID,*LPVOID;
typedef char CHAR;
typedef CHAR *LPSTR;
typedef LPSTR LPTSTR;
typedef const void *LPCVOID;
typedef ULONG_PTR SIZE_T;
typedef LONG_PTR LRESULT;
typedef UINT_PTR WPARAM;
typedef LONG_PTR LPARAM;
typedef void *HGDIOBJ;
typedef void *HWND;
typedef struct tagMSG {
  void *dummy;
} MSG;
typedef struct tagMSG *LPMSG;
typedef void VOID;
typedef WORD ATOM;
typedef char TCHAR;
typedef const TCHAR *LPCTSTR;
typedef void *HBRUSH;
typedef void *HICON;
typedef void *HMENU;
typedef void *HFONT;
typedef HICON HCURSOR;
typedef DWORD COLORREF;
typedef LRESULT CALLBACK (*WNDPROC)(HWND hwnd, UINT message, WPARAM wparam, LPARAM lparam);
typedef struct tagWNDCLASS {
  UINT      style;
  WNDPROC   lpfnWndProc;
  int       cbClsExtra;
  int       cbWndExtra;
  HINSTANCE hInstance;
  HICON     hIcon;
  HCURSOR   hCursor;
  HBRUSH    hbrBackground;
  LPCTSTR   lpszMenuName;
  LPCTSTR   lpszClassName;
} WNDCLASS;
typedef void *HDC;
typedef struct tagPAINTSTRUCT {
  HDC hdc;
} PAINTSTRUCT, *LPPAINTSTRUCT;
typedef struct _RECT {
  LONG left;
  LONG top;
  LONG right;
  LONG bottom;
} RECT, *LPRECT;
typedef void *HRGN;
typedef struct tagPOINT {
  void *dummy;
} *LPPOINT;
typedef struct _SIZE {
  unsigned cx;
  unsigned cy;
} SIZE, *LPSIZE;

/* This is to cope with linux using integers for hwloc_pid_t and hwloc_thread_t
typedef PVOID HANDLE; */
typedef int HANDLE;

#ifdef __GNUC__
#define _ANONYMOUS_UNION __extension__
#define _ANONYMOUS_STRUCT __extension__
#else
#define _ANONYMOUS_UNION
#define _ANONYMOUS_STRUCT
#endif /* __GNUC__ */
#define DUMMYUNIONNAME
#define WINAPI

#define ANYSIZE_ARRAY 1

#define ERROR_INSUFFICIENT_BUFFER 122L

#define MEM_COMMIT	0x1000
#define MEM_RESERVE	0x2000
#define MEM_RELEASE	0x8000

#define PAGE_EXECUTE_READWRITE	0x0040

WINAPI HINSTANCE LoadLibrary(LPCSTR);
WINAPI FARPROC GetProcAddress(HINSTANCE, LPCSTR);
WINAPI DWORD GetLastError(void);

DWORD_PTR WINAPI SetThreadAffinityMask(HANDLE hThread, DWORD_PTR dwThreadAffinityMask);
BOOL WINAPI SetProcessAffinityMask(HANDLE hProcess, DWORD_PTR dwProcessAffinityMask);
BOOL WINAPI GetProcessAffinityMask(HANDLE hProcess, PDWORD_PTR lpProcessAffinityMask, PDWORD_PTR lpSystemAffinityMask);

HANDLE WINAPI GetCurrentThread(void);
HANDLE WINAPI GetCurrentProcess(void);

PVOID WINAPI VirtualAlloc(PVOID,DWORD,DWORD,DWORD);

BOOL GetNumaAvailableMemoryNode(UCHAR Node, PULONGLONG AvailableBytes);

typedef struct _SYSTEM_INFO {
  DWORD dwPageSize;
  DWORD_PTR dwActiveProcessorMask;
  DWORD dwNumberOfProcessors;
} SYSTEM_INFO, *LPSYSTEM_INFO;

void WINAPI GetSystemInfo(LPSYSTEM_INFO lpSystemInfo);

HANDLE WINAPI OpenProcess(DWORD dwDesiredAccess, BOOL bInheritHandle, DWORD dwProcessId);
#define PROCESS_SET_INFORMATION 0x0200
#define PROCESS_QUERY_INFORMATION 0x0400

DWORD WINAPI FormatMessage(DWORD dwFlags, LPCVOID lpSource, DWORD dwMessageId, DWORD dwLanguageId, LPTSTR lpBuffer, DWORD nSize, va_list *Arguments);
#define FORMAT_MESSAGE_ALLOCATE_BUFFER 0x00000100
#define FORMAT_MESSAGE_FROM_SYSTEM 0x00001000

WORD MAKELANGID(USHORT usPrimaryLanguage, USHORT usSubLanguage);
#define LANG_NEUTRAL 0x00
#define SUBLANG_DEFAULT 0x01

HGDIOBJ GetStockObject(int fnObject);
BOOL DeleteObject(HGDIOBJ hObject);
HGDIOBJ SelectObject(HDC hdc, HGDIOBJ hgdiobj);

HWND WINAPI CreateWindow(LPCTSTR lpClassName, LPCTSTR lpWindowName, DWORD dwStyle, int x, int y, int nWidth, int nHeight, HWND hWndParent, HMENU hMenu, HINSTANCE hInstance, LPVOID lpParam);
BOOL WINAPI ShowWindow(HWND hWnd, int nCmdShow);
BOOL UpdateWindow(HWND hWnd);
BOOL RedrawWindow(HWND hWnd, const RECT *lprcUpdate, HRGN hrgnUpdate, UINT flags);
BOOL DestroyWindow(HWND hWnd);
COLORREF RGB(BYTE byRed, BYTE byGreen, BYTE byBlue);
HBRUSH CreateSolidBrush(COLORREF crColor);
COLORREF SetBkColor(HDC hdc, COLORREF crColor);
COLORREF SetTextColor(HDC hdc, COLORREF crColor);
BOOL Rectangle(HDC hdc, int nLeftRect, int nTopRect, int nRightRect, int nBottomRect);
BOOL MoveToEx(HDC hdc, int X, int Y, LPPOINT lpPoint);
BOOL LineTo(HDC hdc, int nXEnd, int nYEnd);
HFONT CreateFont(int nHeight, int nWidth, int nEscapement, int nOrientation, int fnWeight, DWORD fdwItalic, DWORD fdwUnderline, DWORD fdwStrikeOut, DWORD fdwCharSet, DWORD fdwOutputPrecision, DWORD fdwClipPrecision, DWORD fdwQuality, DWORD fdwPitchAndFamily, LPCTSTR lpszFace);
BOOL TextOut(HDC hdc, int nXStart, int nYStart, LPCTSTR lpString, int cchString);
BOOL GetTextExtentPoint32(HDC hdc, LPCTSTR lpString, int c, LPSIZE lpSize);

LRESULT DispatchMessage(const MSG *lpmsg);
BOOL TranslateMessage(const MSG *lpMsg);
BOOL GetMessage(LPMSG lpMsg, HWND hWnd, UINT wMsgFilterMin, UINT wMsgFilterMax);
VOID WINAPI PostQuitMessage(int nExitCode);

#define WM_DESTROY 2
#define WM_SIZE 5
#define WM_PAINT 15
#define WM_KEYDOWN 256
#define WM_KEYUP 257
#define WM_CHAR 258
#define WM_MOUSEMOVE 512
#define WM_LBUTTONDOWN 513
#define WM_LBUTTONUP 514

#define MK_LBUTTON 1

#define VK_ESCAPE 0x1B
#define VK_CONTROL 17
#define VK_PRIOR 33
#define VK_NEXT 34
#define VK_END 35
#define VK_HOME 36
#define VK_LEFT	37
#define VK_UP 38
#define VK_RIGHT 39
#define VK_DOWN 40

#define RDW_INVALIDATE 1

#define SM_CYCAPTION 4
#define SM_CXFULLSCREEN 16
#define SM_CYFULLSCREEN 17
#define SM_CXSIZEFRAME 32
#define SM_CYSIZEFRAME 33

#define CW_USEDEFAULT	0x80000000

#define SW_SHOWDEFAULT 10

#define WS_OVERLAPPEDWINDOW	0xcf0000

#define MAKEINTRESOURCE(x) (LPCTSTR)(x)
#define IDC_SIZEALL MAKEINTRESOURCE(32646)
#define IDI_APPLICATION MAKEINTRESOURCE(32512)

#define DEFAULT_CHARSET 43
#define DEFAULT_QUALITY 43
#define DEFAULT_PITCH 43
#define OUT_DEFAULT_PRECIS 43
#define CLIP_DEFAULT_PRECIS 43

HDC BeginPaint(HWND hwnd, LPPAINTSTRUCT lpPaint);
BOOL EndPaint(HWND hWnd, const PAINTSTRUCT *lpPaint);

int GET_X_LPARAM(LPARAM lParam);
int GET_Y_LPARAM(LPARAM lParam);

WORD LOWORD(DWORD dwValue);
WORD HIWORD(DWORD dwValue);

LRESULT WINAPI DefWindowProc(HWND hWnd, UINT Msg, WPARAM wParam, LPARAM lParam);

#define WHITE_BRUSH 26

HCURSOR WINAPI LoadCursor(HINSTANCE hInstance, LPCTSTR lpCursorName);
HICON WINAPI LoadIcon(HINSTANCE hInstance, LPCTSTR lpIconName);

int WINAPI GetSystemMetrics(int nIndex);

ATOM WINAPI RegisterClass(const WNDCLASS *lpWndClass);

/* hide Linux' host disabling _SC_LARGE_PAGESIZE */
#undef HAVE_DECL__SC_LARGE_PAGESIZE
#define HAVE_DECL__SC_LARGE_PAGESIZE 1
#undef _SC_LARGE_PAGESIZE
#define _SC_LARGE_PAGESIZE 33

#endif /* HWLOC_PORT_WINDOWS_WINDOWS_H */
