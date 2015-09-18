#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <ctype.h>

#include "resources.h"
#include "xfunctions.h"

static const char ENV_PREFIX_VAR[] = "XPRM_prefix";

struct CrResources : ResourceSource {

  int& argc;
  char** argv;
  char* prefix;
  bool shouldExitOnFail;

  CrResources(int& _argc, char** _argv, bool _shouldExitOnFail)
    : argc(_argc), argv(_argv), shouldExitOnFail(_shouldExitOnFail) 
  {
  	prefix = getenv(ENV_PREFIX_VAR);
	if (true && prefix == NULL) { prefix = ""; }
  }
   
  const char* accessStrFromArgs(const char *argList);

  virtual ~CrResources() {
  }

  virtual const char* accessStr(const char *name, const char *argList);
  virtual bool onParseError(
    const char* name, const char* badString, const char* typeName);
  virtual bool onOutOfRangeNumber(
    const char* name, const char* numberBegin, const char* numberEnd, 
    const char* typeName);
};

inline int optionsEnd(const char *str)
{
  return 0 == strcmp(str, "--");
}

const char* CrResources::accessStrFromArgs(const char *argList)
{
  int i;
  const char *result = NULL;
  if (argList == NULL) {
    return result;
  }
  for (i = 1; i < argc; ) {
    const char *cursor = argv[i];
    if (optionsEnd(cursor)) {
      break;
    }
    else {
      const char *begin = argList, *end;
      int length = strlen(cursor);
      for (;;) {
	while (isspace(*begin)) {
	  ++begin;
	}
	end = begin;
	while (!isspace(*end) && *end != '\0') {
	  ++end;
	}
	if (length == end - begin && 0 == strncmp(begin, cursor, length)) {
	  break;
	}
	if (*end == '\0') {
	  goto optionNotFound;
	}
	begin = end;
      }
      if (i + 1 == argc || optionsEnd(argv[i + 1])) {
	fprintf(stderr, 
		"Invalid option %s usage, a value should be given\n", cursor);
	if (shouldExitOnFail) {
	  xexit(1);
	}
	--argc;
	if (argc > i) {
	  memmove(argv + i, argv + i + 1, sizeof(argv[0]) * (argc - i));
	}   
      }
      else {
	result = argv[i + 1];
	argc -= 2;
	if (argc > i) {
	  memmove(argv + i, argv + i + 2, sizeof(argv[0]) * (argc - i));
	}   
      }
      continue;
    optionNotFound:
      ++i;    
    }
  }
  return result;
}


const char* CrResources::accessStr(const char *name, const char *argList)
{
  const char* result = accessStrFromArgs(argList);    
  if (result == NULL) {
  	size_t prefix_len = strlen(prefix);
  	size_t name_len = strlen(name);
  	char* env_name = new char[prefix_len + name_len + 1];
	memcpy(env_name, prefix, prefix_len);
	memcpy(env_name + prefix_len, name, name_len + 1);
    result = getenv(env_name);
	delete env_name;
	if (result == NULL) {
      fprintf(stderr,"The resource \"%s\" does not set.\n", name);
      xexit(1);
    }  	   
  }
  return result;   
}

bool CrResources::onParseError(
  const char* name, const char* badString, const char* typeName) 
{
  fprintf(stderr, 
    "Can not convert the value \"%s\" of resource \"%s\" to \"%s\"",
    badString, name, typeName);
  xexit(1);  
}
    
bool CrResources::onOutOfRangeNumber(
  const char* name, const char* numberBegin, const char* numberEnd, 
  const char* typeName)
{  
  size_t size = numberEnd - numberBegin;
  char* tmp = new char[size + 1];
  memcpy(tmp, numberBegin, size);
  tmp[size] = '\0';
  fprintf(stderr, 
    "The value \"%s\" of resource \"%s\" is not representable as \"%s\"",
    tmp, name, typeName);
  delete[] tmp;       
  xexit(1);  
}

char defaultConfigFileName[] = "Config";

static int testForStrings(char *test, char *first, char *second)
{
  return (NULL != first && 0 == strcmp(test, first))
    ||  (NULL != second && 0 == strcmp(test, second));
}

bool preprocessCommands(
  int *pargc, char *argv[], 
  void (*usage)(const char * programName), 
  void (*version)(const char * programName)) {

  int i;
  int argc = *pargc;
  char *name = strrchr(argv[0], '/'), *config = defaultConfigFileName, *cursor;
  if (name == NULL) {
    name = argv[0];
  }
  for (i = 1; i < argc; ) {
    cursor = argv[i];
    if (optionsEnd(cursor)) {
      return config;
    }
    if (testForStrings(cursor, "-h", "--help")) {
      if (usage != NULL) {
	usage(name);
      }
      else {
	fprintf(stderr, "Why don't write usage function for %s ?\n", name);
      }   
      return false;
    }
    else if (testForStrings(cursor, NULL, "--version")) {
      if (version != NULL) {
	version(name);
      }   
      return false;
    }
    else {
      ++i;
    }
  }
  *pargc = argc;
  return true;
}

ResourceSource* setupParameters(bool shouldExitOnFail, 
                                int* argc, char *argv[]) 
{
  return new CrResources(*argc, argv, shouldExitOnFail);
}  
