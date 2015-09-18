#include <ctype.h>
#include <stdio.h>
#include <string.h>
#include <stdlib.h>
#include <limits.h>
#include <math.h>

#include "strtonum.h"
#include "xfunctions.h"


static const char* TrueStrings[] = { 
  "1", "true",  "t", "yes", "y", "on",  NULL 
};
static const char* FalseStrings[] = { 
  "0", "false", "f", "no",  "n", "off", NULL 
};

long StrNum::tol(const char *str)
{
  char * c;
  long n = strtol(str, &c, 10);
  setStatus((c == str) ? 
    BAD_STRING : (n == LONG_MIN || n == LONG_MAX) ? OUT_OF_RANGE : OK);
  return n;
}

unsigned long StrNum::toul(const char *str)
{
  char * c;
  unsigned long n = strtoul(str, &c, 10);
  setStatus((c == str) ? 
    BAD_STRING : (n == ULONG_MAX) ? OUT_OF_RANGE : OK);
  return n;
}

int StrNum::toi(const char *str)
{
  char * c;
  long n = strtol(str, &c, 10);
  setStatus((c == str) ? 
    BAD_STRING : (n <= INT_MIN || n >= INT_MAX) ? OUT_OF_RANGE : OK);
  return int(n);
}

unsigned int StrNum::toui(const char *str)
{
  char * c;
  unsigned long n = strtoul(str, &c, 10);
  setStatus((c == str) ? 
    BAD_STRING : (n >= UINT_MAX) ? OUT_OF_RANGE : OK);
  return (unsigned int)n;
}

double StrNum::tof(const char *str)
{
  char* c;
  double x = strtod(str, &c);
  setStatus((c == str) ? 
    BAD_STRING : (x == HUGE_VAL || x == -HUGE_VAL) ? OUT_OF_RANGE : OK);
  return x;
}

inline bool testSubStr(const char* first, const char* last, const char* str)
{
  while (first <= last) {
    if (tolower(*first) != *str) {
      return false;
    }
    ++first;
    ++str;
  }
  return *str == '\0';
}

bool StrNum::tob(const char * str)
{
  const char* last = strchr(str, '\0') - 1;
  while (last >= str && isspace(*last)) {
    --last;
  }  
  setStatus(OK);
  const char** ptr;
  for (ptr = TrueStrings; *ptr != NULL; ++ptr) {
    if (testSubStr(str, last, *ptr)) {
      return true;
    }   
  }  
  for (ptr = FalseStrings; *ptr != NULL ; ++ptr) {
    if (testSubStr(str, last, *ptr)) {
      return false;
    }   
  }
  setStatus(BAD_STRING);
  return false;
}

int getInt(ResourceSource& r, const char *name, const char *argList) {
  static const char type[] = "int";
  const char *str = r.accessStr(name, argList);
  char* c;
  long n = strtol(str, &c, 10);
  if (c == str) { 
    r.onParseError(name, str, type); 
  }
  else if (n < INT_MIN || n > INT_MAX) {
    r.onOutOfRangeNumber(name, str, c, type); 
  }
  return (int)n;
}             

unsigned int getUnsignedInt(
  ResourceSource& r, const char *name, const char *argList) 
{
  static const char type[] = "unsigned int";
  const char *str = r.accessStr(name, argList);
  char* c;
  unsigned long n = strtoul(str, &c, 10);
  if (c == str) { 
    r.onParseError(name, str, type); 
  }
  else if (n > UINT_MAX) {
    r.onOutOfRangeNumber(name, str, c, type); 
  }
  return (unsigned int)n;
}

long getLong(ResourceSource& r, const char *name, const char *argList) {
  static const char type[] = "long";
  const char *str = r.accessStr(name, argList);
  char* c;
  long n = strtol(str, &c, 10);
  if (c == str) { 
    r.onParseError(name, str, type); 
  }
  else if (n == LONG_MIN || n == LONG_MAX) {
    r.onOutOfRangeNumber(name, str, c, type); 
  }
  return n;
}

unsigned long getUnsignedLong(
  ResourceSource& r, const char *name, const char *argList) 
{
  static const char type[] = "unsigned long";
  const char *str = r.accessStr(name, argList);
  char* c;
  unsigned long n = strtoul(str, &c, 10);
  if (c == str) { 
    r.onParseError(name, str, type); 
  }
  else if (n == ULONG_MAX) {
    r.onOutOfRangeNumber(name, str, c, type); 
  }
  return n;
}             

double getDouble(ResourceSource& r, const char *name, const char *argList) {
  static const char type[] = "double";
  const char *str = r.accessStr(name, argList);
  char* c;
  double n = strtod(str, &c);
  if (c == str) { 
    r.onParseError(name, str, type); 
  }
  else if (n == HUGE_VAL || n == -HUGE_VAL) {
    r.onOutOfRangeNumber(name, str, c, type); 
  }
  return n;
}             

bool getBool(ResourceSource& r, const char *name, const char *argList) {
  static const char type[] = "bool";
  const char *str = r.accessStr(name, argList);
  StrNum conv;
  bool n = conv.tob(str);
  if (!conv.ok()) {
    r.onParseError(name, str, type); 
  }
  return n;
}
             
char* getString(ResourceSource& r, const char *name, const char *argList) 
{
  char *retStr;
  const char *s = r.accessStr(name, argList);
  size_t len = strlen(s);
  while (len > 0 && isspace(s[len  - 1])) {
    --len;
  }
  retStr = new char[len + 1];
  memcpy(retStr, s, len);
  retStr[len] = '\0';
  return retStr;
}

unsigned int getStringToBuffer(
  char *buffer, size_t size, 
  ResourceSource& r, const char *name, const char *argList) 
{
  const char *s = r.accessStr(name, argList);
  size_t len = strlen(s);
  while (len > 0 && isspace(s[len  - 1])) {
    --len;
  }
  if (size < len + 1) {
    if (size > 0) {
      memcpy(buffer, s, size - 1);
      buffer[size - 1] = '\0';
    }
  }
  else {
    memcpy(buffer, s, len);
    buffer[len] = '\0';
  }
  return len + 1;
}
             
inline const char *nextStrToken(const char *s) {
  while (isspace(*s)) {
    ++s;
  }
  while(*s != '\0' && !isspace(*s)) {
    ++s;
  }
  while (isspace(*s)) {
    ++s;
  }
  return s;
}

ClosedInterval getInterval(
  ResourceSource& r, const char *name, const char *argList) 
{
  static const char type[] = "ClosedInterval";
  StrNum conv;
  ClosedInterval i;  
  const char *s = r.accessStr(name, argList);
  i.begin = conv.tof(s);
  if (conv.ok()) {
    i.end = i.begin;
    s = nextStrToken(s);
    if (*s != '\0') {
      i.end = conv.tof(s);
      if (conv.ok()) {
        if (i.end < i.begin) {
          double temp = i.begin;
          i.begin = i.end;
          i.end = temp;
        }
        if (i.end != i.begin) {
          i.pointCnt = 2;  
          i.step = i.end - i.begin;
        }
        s = nextStrToken(s);
        if (*s != '\0') {
          i.pointCnt = conv.toui(s);
          if (conv.ok()) {
            if (i.pointCnt >= 2) {
	      i.step = i.step / double(i.pointCnt - 1);
            }
            else if (i.pointCnt == 1) {
	      i.end = i.begin;
	      i.step = 0;
            }
          }  
        }   
      }
    }  
  }  
  if (!conv.ok()) {
    r.onParseError(name, s, type); 
  }  
  return i;
}







