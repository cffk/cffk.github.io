# $Id: Makefile 6143 2006-04-18 20:35:44Z ckarney $

TARGET=RandomExample

all: $(TARGET)

Random.o RandomExample.o: Random.hpp
RandomExample: Random.o

clean:
	rm -f *.o

CXXFLAGS=-O2 -funroll-loops # -mtune=i686 -march=i686
CC=g++
