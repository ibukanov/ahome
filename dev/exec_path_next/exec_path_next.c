#include <stdlib.h>
#include <stdbool.h>
#include <stdio.h>
#include <string.h>
#include <stdarg.h>
#include <errno.h>
#include <assert.h>

#include <sys/types.h>
#include <sys/stat.h>
#include <unistd.h>

const char *program_name;

static void 
fatal_error(char *format, ...)
{
	va_list ap;
	
	fprintf(stderr, "%s: error: ", program_name);

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
	
	fputc('\n', stderr);

	exit(EXIT_FAILURE);
}

static void 
warning(char *format, ...)
{
	va_list ap;
	
	fprintf(stderr, "%s: warning: ", program_name);

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
	
	fputc('\n', stderr);
}

static void *
xmalloc(size_t size)
{
	void* p = malloc(size);
	if (p == NULL) {
		fatal_error("Failed malloc(%lu)", (unsigned long)size);
	}
	return p;
}

struct buffer 
{
	char* ptr;
	size_t capacity;	
};

static void 
buffer_init(struct buffer *buf)
{
	buf->ptr = NULL;
	buf->capacity = 0;
}

static void
buffer_ensure(struct buffer *buf, size_t min_capacity)
{
	if (buf->capacity >= min_capacity) {
		return;
	}
	if (buf->capacity != 0) {
		assert(buf->ptr != NULL);
		free(buf->ptr);
	} else {
		assert(buf->ptr == NULL);
	}
	buf->ptr = xmalloc(min_capacity);
	buf->capacity = min_capacity;
}

static char*
buffer_forget(struct buffer *buf, size_t return_size)
{
	char* ptr;
	assert(buf->capacity >= return_size);
	if (return_size == 0) {
		ptr = NULL;
		if (buf->capacity != 0) {
			assert(buf->ptr != NULL);
			free(buf->ptr);
		}
		
	} else if (buf->capacity == return_size) {
		ptr = buf->ptr;

	} else {
		ptr = realloc(buf->ptr, return_size);
		if (ptr == NULL) {
			ptr = buf->ptr;
		}
	}
	buf->ptr = NULL;
	buf->capacity = 0;

	return ptr;
}


static void 
build_location(const char* dir_start, size_t dir_len, const char* name, struct buffer *buf)
{
	size_t name_len, file_len;
	
	if (name != NULL) {
		name_len = strlen(name);
		file_len = dir_len + 1 + name_len;
	} else {
		name_len = 0; /* or GCC complains */
		file_len = dir_len;
	}
	buffer_ensure(buf, file_len + 1);	

	memcpy(buf->ptr, dir_start, dir_len);
	if (name != NULL) {
		buf->ptr[dir_len] = '/';
		memcpy(buf->ptr + dir_len + 1, name, name_len);
	}
	buf->ptr[file_len] = '\0';
}

static bool 
is_executable(const char* location)
{
	struct stat st;

	if (stat(location, &st)) {
		if (errno != ENOENT) {
			warning("Failed to stat '%s': %s", 
				location, strerror(errno));
		}
		return false;
	}
	
	if (S_ISREG(st.st_mode) 
	    && (st.st_mode & (S_IXUSR | S_IXGRP | S_IXOTH)) != 0)
	{
		return true;
	}
	
	warning("File in path with executable name is not regular executable: %s", location);
	
	return false;
}	      


static char *
locate_next_exec(const char *path, const char *first_dir, const char *name, const char* ignore_prefix)
{
	struct buffer buf[1];
	size_t first_dir_len, ignore_prefix_len;
	const char *dir_start;
	bool more, found;
	enum { BEFORE_FIRST_DIR, SEARCH_SECOND_DIR } state;
	
	first_dir_len = strlen(first_dir);
	if (ignore_prefix != NULL) {
		ignore_prefix_len = strlen(ignore_prefix);
	} else {
		ignore_prefix_len = 0;
	}
	buffer_init(buf);

	dir_start = path;
	more = true;
	found = false;
	state = BEFORE_FIRST_DIR;
	for (;;) {
		const char *dir_end = strchr(dir_start, ':');
		size_t dir_len;
		if (dir_end == NULL) {
			more = false;
			dir_end = strchr(path, '\0');
		}
		dir_len = dir_end - dir_start;

		if (dir_start[0] != '/' 
		    && !(dir_len == 1 && *dir_start == '.')) 
		{
			build_location(dir_start, dir_len, NULL, buf);
			warning("PATH component is not absolute path: %s",
				buf->ptr);
				
		} else if (state == BEFORE_FIRST_DIR) {
			bool found = false;
			if (first_dir_len == dir_len) {
				if (0 == memcmp(dir_start, first_dir, 
				                first_dir_len)) 
				{
					found = true;
				}
			}
			if (found) {
				build_location(dir_start, dir_len, name, buf);
				if (!is_executable(buf->ptr)) {
					fatal_error("Failed to locate executable regular file '%s' in %s", name, first_dir);
				}
				state = SEARCH_SECOND_DIR;
			}
			
		} else if (state == SEARCH_SECOND_DIR) {
			bool skip = false;
			if (ignore_prefix != NULL 
			    && dir_len >= ignore_prefix_len)
			{
				if (0 == memcmp(dir_start, ignore_prefix, 
				                ignore_prefix_len))
				{
					skip = (dir_len == ignore_prefix_len)
						|| (dir_start[ignore_prefix_len]
					            == '/');
				}
			}
			if (!skip) {
				build_location(dir_start, dir_len, name, buf);
				if (is_executable(buf->ptr)) {
					found = true;
					break;
				}
			}
		
		} else {
			assert(false);
		}
		
		if (more) {
			dir_start = dir_end + 1;
			continue;
		}
		break;
	}
	
	if (!found) {
		fatal_error("Failed to locate real executable '%s' in %s", name, path);
	}
	
	return buffer_forget(buf, strlen(buf->ptr) + 1);
}

static void 
bad_usage(char *format, ...)
{
	va_list ap;
	
	fprintf(stderr, "%s: invalid usage: ", program_name);

	va_start(ap, format);
	vfprintf(stderr, format, ap);
	va_end(ap);
	
	fprintf(stderr, "\nTry '%s --help' for more information.\n", program_name);

	exit(EXIT_FAILURE);
}

static void 
show_usage(const char* replace_dir)
{
	fprintf(stdout, 
"Usage: %s [OPTION]... NAME [NAME_ARGUMENT]...\n"
"Execute executable NAME that comes in $PATH after one located in '%s'.\n"
"\n"
"All arguments after NAME will be passed as NAME's arguments\n"
"  --ignore-home   ignore $PATH components that starts with $HOME when\n"
"                    searching for NAME\n"
"  --use-basename  use only base name part of NAME as a name of executable\n"
"  --help          print this help to stdout and exit\n"
"  --version       print version and build information to stdout and exit\n"
"  --              stop option processing and assume that the next argument\n"
"                    is NAME\n"
"\n"
"Report bugs to igor@fastmail.fm\n", 
		program_name, replace_dir);
		
	exit(EXIT_SUCCESS);
}

static void 
show_version(const char* replace_dir)
{
	fprintf(stdout, 
"%s 0.1\n"
"\n"
"Directory with initial executables: %s\n",
		program_name, replace_dir);
		
	exit(EXIT_SUCCESS);
}

int main(int argc, char** argv)
{
	char *executable, *path;
	char *resolved_location;
	char **real_argv;
	size_t name_arg;
	const char *ignore_prefix = NULL;
	const char *home;
	char *replace_dir;
	bool use_basename = false;
	
	program_name = strrchr(argv[0], '/');
	if (program_name == NULL) {
		program_name = argv[0];
	} else {
		++program_name;
	}
	
	home = getenv("HOME");
	if (home == NULL) {
		fatal_error("HOME is not defined");
	}
	
	{
		size_t home_len = strlen(home);
		size_t replace_len = home_len + 1 + strlen(REPLACE_COMMAND_DIR);
		replace_dir = xmalloc(replace_len + 1);
		memcpy(replace_dir, home, home_len);
		replace_dir[home_len] = '/';
		memcpy(replace_dir + home_len + 1, REPLACE_COMMAND_DIR,
		       strlen(REPLACE_COMMAND_DIR));
		replace_dir[replace_len] = '\0';
	}
	
	for (name_arg = 1; name_arg != argc; ++name_arg) {
		const char *arg = argv[name_arg];
		if (arg[0] == '\0') {
			bad_usage("Empty executable name argument");
		}
		if (arg[0] == '-') {
			if (0 == strcmp(arg, "--ignore-home")) {
				ignore_prefix = home;

			} else if (0 == strcmp(arg, "--use-basename")) {
				use_basename = true;

			} else if (0 == strcmp(arg, "--help")) {
				show_usage(replace_dir);

			} else if (0 == strcmp(arg, "--version")) {
				show_version(replace_dir);

			} else if (0 == strcmp(arg, "--")) {
				++name_arg;
				break;
			
			} else {
				bad_usage("Unknown option '%s'", arg);
			}
			 
			continue;
		}
		break;
	}

	if (name_arg == argc) {
		bad_usage("Missed executable name argument");
	}
	
	executable = argv[name_arg];
	if (!use_basename) {
		if (strchr(executable, '/') != NULL) {
			bad_usage("Execute name should not contain '/': %s",
				executable);
		}
	} else {
		char* last_slash = strrchr(executable, '/');
		if (last_slash != NULL) {
			executable = last_slash + 1;
		}
	}
	
	path = getenv("PATH");
	if (path == NULL || path[0] == '\0') {
		fatal_error("$PATH is not defined");
	}
	
	resolved_location = locate_next_exec(
		path, replace_dir, executable, ignore_prefix);
		
	real_argv = xmalloc(sizeof(real_argv[0]) * (argc - name_arg + 1));
	real_argv[0] = executable;
	memcpy(real_argv + 1, argv + name_arg + 1, 
		sizeof(real_argv[0]) * (argc - name_arg - 1));
	real_argv[argc - name_arg] = NULL;

	execv(resolved_location, real_argv);
	fatal_error("Failed to execute '%s': %s", resolved_location, strerror(errno));

	return 0;
}
