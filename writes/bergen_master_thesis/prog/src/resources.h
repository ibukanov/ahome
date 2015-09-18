#ifndef _RESOURCES_H_
#define _RESOURCES_H_

#include <stdlib.h>
#include <math.h>

#include "strtonum.h"

/*
 * CrPreprocessCommands recognizes:
 *  -h, --help options to show help message in which case usage() is called, 
 *  --version in in which case version() is called
 * On return:
 * NULL is returned if usage or version were called or 
 *    there is an error in the options, 
 * otherwise pointer to the configure file name is returned,
 */
bool preprocessCommands(int *argc, char *argv[], 
                        void (*usage)(const char * programName), 
                        void (*version)(const char * programName));

ResourceSource* setupParameters(bool shouldExitOnFail, 
                                int* argc, char *argv[]);

#endif













