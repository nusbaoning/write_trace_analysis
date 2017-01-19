
# This is an example Makefile for a countwords program.  This
# program uses both the scanner module and a counter module.
# Typing 'make' or 'make count' will create the executable file.
#

# define some Makefile variables for the compiler and compiler flags
# to use Makefile variables later in the Makefile: $()
#
#  -g    adds debugging information to the executable file
#  -Wall turns on most, but not all, compiler warnings
#
# for C++ define  CC = g++
CC = gcc
CFLAGS  = -g -Wall -mcmodel=large

# typing 'make' will invoke the first target entry in the file 
# (in this case the default target entry)
# you can name this target entry anything, but "default" or "all"
# are the most commonly used names by convention
#
default: simulator

# To create the executable file count we need the object files
# countwords.o, counter.o, and scanner.o:
#
simulator:  filemap.o linked_list.o page_cache.o 
	$(CC) $(CFLAGS) -o simulator filemap.o linked_list.o page_cache.o 

# To create the object file countwords.o, we need the source
# files countwords.c, scanner.h, and counter.h:
#
filemap.o:  filemap.c page_cache.h
	$(CC) $(CFLAGS) -c filemap.c

# To create the object file counter.o, we need the source files
# counter.c and counter.h:
#
linked_list.o:  linked_list.c linked_list.h 
	$(CC) $(CFLAGS) -c linked_list.c

# To create the object file scanner.o, we need the source files
# scanner.c and scanner.h:
#
page_cache.o:  page_cache.c page_cache.h 
	$(CC) $(CFLAGS) -c page_cache.c



# To start over from scratch, type 'make clean'.  This
# removes the executable file, as well as old .o object
# files and *~ backup files:
#
clean: 
	$(RM) count *.o *~
