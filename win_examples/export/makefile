all: hello main

hello: hello.c hello.h
	cl /TC /LD /DEXPORTING hello.c

main: main.cpp
	cl main.cpp hello.lib

clean:
	rm -Rf *.obj *.lib *.dll *.exp *.i *.exe
