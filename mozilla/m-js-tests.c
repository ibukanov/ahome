#define _GNU_SOURCE

#include <assert.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#include <unistd.h>
#include <sys/resource.h>

char *program_name = NULL;

void perror_and_abort()
    __attribute__ ((noreturn));

void perror_and_abort()
{
    perror(program_name);
    abort();
}

void *xmalloc(size_t nbytes)
{
    assert(nbytes);
    void *p = malloc(nbytes);
    if (!p)
        perror_and_abort();
    return p;
}

char *xstrdup(const char *str)
{
    size_t n = strlen(str) + 1;
    char *copy = xmalloc(n);
    memcpy(copy, str, n);
    return copy;
}

char *xasprintf(const char *format, ...)
    __attribute__ ((format (printf, 1, 2)));

char *xasprintf(const char *format, ...)
{
    char *str;
    va_list ap;

    va_start(ap, format);
    if (-1 == vasprintf(&str, format, ap))
        perror_and_abort();
    va_end(ap);
    return str;
}

char *concat_path(const char *dir, const char *rel_path)
{
    size_t length1 = strlen(dir);
    size_t length2 = strlen(rel_path);
    size_t length_result = length1 + 1 + length2;
    char *s = xmalloc(length_result + 1);
    sprintf(s, "%s/%s", dir, rel_path);
    assert(strlen(s) == length_result);
    return s;
}

char *home_path(const char *rel_path)
{
    static char *home = NULL;
    if (!home) {
        home = getenv("HOME");
        if (!home)
            home = "/";
    }
    return concat_path(home, rel_path);
}

void bad_usage(const char *message)
{
    if (message)
        fprintf(stderr, "%s: %s\n", program_name, message);
    fprintf(stderr, "See '%s -h' for usage\n.", program_name);
    exit(EXIT_FAILURE);
}

const char test_dir_path[] = "m/mc/js/src/tests";

void usage()
{
    fprintf(stderr,
"Usage %s [OPTION]... BUILD_DIR\n"
"Run js test suite against a build of tm shell.\n"
"\n"
"  -b  generate list of base failures\n"            
"  -f  test only the failures from the previous run\n"
"Only one of -b or -f can be specified\n"
"  -h  show this help and exit\n"
"  -o  show js command and output\n"
"  -t <test source dir>\n"
"     use tests from the given tree\n",
            program_name);
    exit(EXIT_SUCCESS);
}

int main(int argc, char **argv)
{
    program_name = argv[0];
    char *test_dir = NULL;

    bool base_failures = false;
    bool retest_failures = false;
    bool show_output = false;
    char opt;
    while ((opt = getopt(argc, argv, "bfhot:")) != -1) {
        switch (opt) {
          case 'b':
            base_failures = true;
            break;
          case 'f':
            retest_failures = true;
            break;
          case 'o':
            show_output = true;
            break;
          case 'h':
            usage();
          case 't':
            free(test_dir);
            test_dir = xstrdup(optarg);
          default:
            bad_usage(NULL);
        }
    }
    if (optind == argc)
        bad_usage("missing BUILD_DIR argument");
    char *build_dir = argv[optind];
        
    if (optind + 1 < argc)
        bad_usage("too many arguments");
        
    if (base_failures && retest_failures)
        bad_usage("only one of -b or -f can be given");

    if (!test_dir)
        test_dir = home_path(test_dir_path);
    
    if (chdir(build_dir))
        perror_and_abort();

    char *exec_args[9];
    size_t n = 0;
    char *test_driver = concat_path(test_dir, "jstests.py");
    exec_args[n++] = test_driver;
    exec_args[n++] = test_driver;
    exec_args[n++] = "--timeout=20";
    if (retest_failures) {
        exec_args[n++] = xasprintf("--file=%s/%s", build_dir, "failures.txt");
    } else if (base_failures){
        exec_args[n++] = xasprintf("--failure-file=%s/%s", build_dir, "base_failures.txt");
    } else {
        exec_args[n++] = xasprintf("--failure-file=%s/%s", build_dir, "failures.txt");
        exec_args[n++] = xasprintf("--exclude-file=%s/%s", build_dir, "base_failures.txt");
    }
    if (show_output) {
        exec_args[n++] = "-s";
        exec_args[n++] = "-o";
    }
    exec_args[n++] = xasprintf("%s/%s", build_dir, "js");
    exec_args[n++] = NULL;

    assert(n <= sizeof(exec_args) / sizeof(exec_args[0]));

    execvp("python", exec_args);
    perror_and_abort();
}
