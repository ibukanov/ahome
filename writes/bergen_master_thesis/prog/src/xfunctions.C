#include <stdio.h>
#include <string.h>
#include "xfunctions.h"
#include "strtonum.h"

void * xmalloc(size_t size)
{
   void * ptr = malloc(size);
   if ( ptr != NULL ) return ptr;
   perror("xmalloc()");
   xexit(1);
}

void * xcalloc(size_t nelem, size_t elsize)
{
   void * ptr = calloc(nelem, elsize);
   if ( ptr != NULL ) return ptr;
   perror("xcalloc()");
   xexit(1);
}

char * xstrdup(const char * str)
{
   char * ptr = strdup(str);
   if ( ptr != NULL ) return ptr;
   perror("xstrdup()");
   xexit(1);
}


FILE * xfopen(const char *pathname, const char *type)
{
   FILE *  stream = fopen(pathname, type);
   if ( stream != NULL ) return stream;
   perror(pathname);
   xexit(1);
}

int yfclose(FILE * stream)
{
   int status = fclose(stream);
   if (status) perror("xfclose()");
   return status;
}

int yfflush(FILE * stream)
{
   int status = fflush(stream);
   if (status) perror("xfflush()");
   return status;
}

long xatol(const char *str)
{
  StrNum conv;
  long n = conv.tol(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "xatol(\"%s\") : bad string-to-integer conversion.\n", str);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "xatol(\"%s\")",str);
      perror(" ");
    }
    xexit(1);
  }   
  return n;
}

long yatol(const char *str)
{
  StrNum conv;
  long n = conv.tol(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "yatol(\"%s\") : bad string-to-integer conversion; %ld is returned.\n", str,n);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "yatol(\"%s\")", str);
      perror(" ");
      fprintf(stderr, "\t%ld is returned.\n",n);
    }
  }
  return n;
}

unsigned long xatoul(const char *str)
{
  StrNum conv;
  unsigned long n = conv.toul(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "xatoul(\"%s\") : bad string-to-integer conversion.\n", str);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "xatoul(\"%s\")",str);
      perror(" ");
    }
    xexit(1);
  }   
  return n;
}

unsigned long yatoul(const char *str)
{
  StrNum conv;
  unsigned long n = conv.toul(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "yatoul(\"%s\") : bad string-to-integer conversion; %lu is returned.\n", str,n);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "yatoul(\"%s\")",str);
      perror(" ");
      fprintf(stderr,"\t%lu is returned.\n", n);
    }
  }
  return n;
}

int xatoi(const char *str)
{
  StrNum conv;
  int n = conv.toi(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "xatoi(\"%s\") : bad string-to-integer conversion.\n", str);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "xatoi(\"%s\") : Value out of range.\n",str);
    }
    xexit(1);
  }
  return n;
}

int yatoi(const char *str)
{
  StrNum conv;
  int n = conv.toi(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "yatoi(\"%s\") : bad string-to-integer conversion; %d is returned.\n", str,n);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "yatoi(\"%s\") : Value out of range.\n",str);
      fprintf(stderr,"\t%d is returned.\n", n);
    }
  }
  return n;
}

unsigned int xatoui(const char *str)
{
  StrNum conv;
  int n = conv.toui(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "xatoui(\"%s\") : bad string-to-integer conversion.\n", str);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "xatoui(\"%s\") : Value out of range.\n",str);
    }
    perror(" ");
    xexit(1);
  }   
  return n;
}

unsigned int yatoui(const char *str)
{
  StrNum conv;
  int n = conv.toui(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "yatoui(\"%s\") : bad string-to-integer conversion; %u is returned.\n", str,n);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "yatoui(\"%s\") : Value out of range.\n",str);
      fprintf(stderr,"\t%u is returned.\n", n);
    }
  }   
  return n;
}

double xatof(const char *str)
{
  StrNum conv;
  double x = conv.tof(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "xatof(\"%s\") : Bad string-to-double conversion.\n", str);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "xatof(\"%s\")",str);
      perror(" ");
    }
    xexit(1);
  }   
  return x;
}


double yatof(const char *str)
{
  StrNum conv;
  double x = conv.tof(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "yatof(\"%s\") : Bad string-to-double conversion; %g is returned.\n", str, x);
    }
    else if (conv.getStatus() == StrNum::OUT_OF_RANGE) {
      fprintf(stderr, "yatof(\"%s\")",str);
      perror(" ");
      fprintf(stderr,"\t%g is returned.\n", x);
    }
  }   
  return x;
}


bool xatob(const char * str)
{
  StrNum conv;
  bool b = conv.tob(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "xatob(\"%s\") : bad string-to-boolean conversion.\n", str);
    }
    xexit(1);
  }   
  return b;   
}

bool yatob(const char * str)
{
  StrNum conv;
  bool b = conv.tob(str);
  if (!conv.ok()) {
    if (conv.getStatus() == StrNum::BAD_STRING) {
      fprintf(stderr, "yatob(\"%s\") : bad string-to-boolean conversion; \"False\" is returned.\n", str);
    }
  }   
  return b;   
}



