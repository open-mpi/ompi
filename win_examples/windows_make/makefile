all: dll load

dll: component.c component.h
	cl /LD component.c

load: loading.c component.h
	cl loading.c

clean:
	rm -Rf *.dll *.obj core* *.lib *.exp *.exe
