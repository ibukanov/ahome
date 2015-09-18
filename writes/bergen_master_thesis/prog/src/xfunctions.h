#ifndef _XFUNCTIONS_H
#define _XFUNCTIONS_H

#include <stdio.h>
#include <strings.h>
#include <stdlib.h>
#include <errno.h>
#include <setjmp.h>

#ifdef __GNUC__
#define no_return __attribute__ ((noreturn))
#else
#define no_return
#endif


void * xmalloc(size_t size);
void * xcalloc(size_t nelem, size_t elsize);
char * xstrdup(const char * str);

FILE * xfopen(const char *pathname, const char *type);

int yfclose(FILE * stream);
int yfflush(FILE * stream);

long xatol(const char *str);
int  xatoi(const char *str);
unsigned long xatoul(const char *str);
unsigned int xatoui(const char *str);
double xatof(const char *str);

long yatol(const char *str);
int  yatoi(const char *str);
unsigned long yatoul(const char *str);
unsigned int yatoui(const char *str);
double yatof(const char *str);

bool xatob(const char * str);
bool yatob(const char * str);

void xexit(int exitCode) no_return;
// The program should define xexit somewhere, i.e.
// void xexit(int exitCode) { exit(exitCode); }
//


#endif /* _XFUNCTIONS_H */





