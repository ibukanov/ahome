#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <unistd.h>
#include <signal.h>
#include <fcntl.h>
#include <sys/socket.h>
#include <sys/select.h>
#include <sys/un.h>
#include <sys/wait.h>

#define UNUSED_ARG(arg) arg __attribute__((unused))
#define LIKE_PRINTF(string_index, first_to_check) \
    __attribute__((format(printf, string_index, first_to_check)))

#define LOG_ERR(x) log_error x
#define LOG_ERRNO(x) log_errno x

#define DBG(condition, x) do { if (condition) print_debug x; } while (0)

static char *program_name;

static unsigned debug_level;
static char *stderr_destination;
static char *socket_path;
static char **exec_cmd_and_args;

static char *path_to_unlink;

static void 
print_debug(const char *format, ...) LIKE_PRINTF(1, 2);

static void 
log_error(const char *format, ...) LIKE_PRINTF(1, 2);

static void 
log_errno(const char *format, ...) LIKE_PRINTF(1, 2);

static void 
log_error(const char *format, ...)
{
    va_list ap;
    
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    putc('\n', stderr);
    fflush(stderr);
}

static void 
log_errno(const char *format, ...)
{
    char buf[256];
    va_list ap;
    int saved_errno = errno;

    va_start(ap, format);
    vsnprintf(buf, sizeof(buf), format, ap);
    va_end(ap);
    log_error("%s: %s, errno=%d", buf, strerror(saved_errno), saved_errno);
}

static void 
print_debug(const char *format, ...)
{
    va_list ap;
    
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    putc('\n', stderr);
}

static bool 
set_path_to_unlink(const char *path)
{
    bool ok = true;
    
    assert(path);
    path_to_unlink = strdup(path);
    if (!path_to_unlink) {
        LOG_ERR(("strdup"));
        ok = false;
    }
    return ok;
}

static void 
do_cleanup()
{
    sigset_t all_blocked_sigmask, orig_sigmask;

    sigfillset(&all_blocked_sigmask);
    sigprocmask(SIG_SETMASK, &all_blocked_sigmask, &orig_sigmask);

    if (path_to_unlink) {
        unlink(path_to_unlink);
        free(path_to_unlink);
        path_to_unlink = NULL;
    }

    sigprocmask(SIG_SETMASK, &orig_sigmask, NULL);
}

static void 
cleanup_handler(int UNUSED_ARG(sig))
{
    do_cleanup();
    /* Assume signal means exit with fail. */
    _exit(EXIT_FAILURE);
}

static bool 
start_new_client(int client_fd)
{
    int child_pid;
    const char *client_errors;
    
    child_pid = fork();
    if (child_pid == 0) 
        goto fork_child;

    /* Parent */
    if (child_pid < 0) {
        LOG_ERRNO(("fork"));
        return false;
    }
    assert(child_pid > 0);
    return true;

  fork_child:
    /* Redirect stdin and stdout to the socket. */
    if (dup2(client_fd, 0) < 0) {
        LOG_ERRNO(("dup2 stdin"));
        goto child_failed;
    }
    if (dup2(client_fd, 1) < 0) {
        LOG_ERRNO(("dup2 stdout"));
        goto child_failed;
    }
    if (stderr_destination) {
        client_errors = stderr_destination;
    } else if (debug_level == 0) {
        client_errors = "/dev/null";
    } else {
        client_errors = NULL;
    }
    if (client_errors) {
        /* Redirect stderr*/
        if (strcmp(client_errors, "-") == 0) {
            /* Redirect to socket to mix with stdout. */
            if (dup2(client_fd, 2) < 0) {
                LOG_ERRNO(("dup2 stderr"));
                goto child_failed;
            }
        } else {
            int fd_errors;
            int dup_status;
        
            fd_errors = open(client_errors, O_WRONLY | O_CREAT | O_APPEND,
                             0660);
            if (fd_errors < 0) {
                LOG_ERRNO(("open(%s, O_WRONLY)", client_errors));
                goto child_failed;
            }
            dup_status = dup2(fd_errors, 2);
            if (dup_status < 0) {
                LOG_ERRNO(("dup2 stderr"));
            }
            close(fd_errors);
            if (dup_status < 0) {
                goto child_failed;
            }
        }
    }
    close(client_fd);

    execvp(exec_cmd_and_args[0], exec_cmd_and_args);
    LOG_ERRNO(("execvp %s", exec_cmd_and_args[0]));

  child_failed:
    _exit(EXIT_FAILURE);
}

static bool 
do_listen(int socket_fd)
{
    bool ok;
    struct sockaddr addr;
    socklen_t addrlen;
    
    assert(socket_fd >= 0);
    
    ok = true;
    for (;;) {
        int client_fd = accept(socket_fd, &addr, &addrlen);
        if (client_fd < 0) {
            if (errno == EAGAIN || errno == EINTR) {
                continue;
            }
            LOG_ERRNO(("accept"));
            goto failed;
        }
        DBG(debug_level >= 1, ("accept()=>%d", client_fd));
        ok &= start_new_client(client_fd);
        close(client_fd);
    }
    
  failed:
    return false;  
}

bool 
create_local_listen_socket(const char *filename, int *socket_fd)
{
    size_t name_length;
    struct sockaddr_un sock_addr;    
    int fd = -1;
    size_t sock_addr_size;
    
    name_length = strlen(filename);
    if (name_length > sizeof(sock_addr.sun_path) - 1) {
        LOG_ERR(("Length of filename exceeded max limit: %zu > %zu\n",
                 name_length, sizeof(sock_addr.sun_path) - 1));
        goto failed;
    }

    sock_addr.sun_family = AF_LOCAL;
    memcpy(sock_addr.sun_path, filename, name_length);
    sock_addr.sun_path[name_length] = '\0';
    sock_addr_size = offsetof(struct sockaddr_un, sun_path) + name_length + 1;

    fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (fd < 0) {
        LOG_ERRNO(("socket(PF_LOCAL)"));
        goto failed;
    }
    
    /* 
     * Block signals during bind to avoid race when signal after successful
     * bind but before call to set_path_to_unlink would leave socket unremoved.
     */
    {
        sigset_t all_blocked_sigmask, orig_sigmask;
        int bind_status;
        bool ok = false;
        sigfillset(&all_blocked_sigmask);
        sigprocmask(SIG_SETMASK, &all_blocked_sigmask, &orig_sigmask);
        bind_status = bind(fd, (struct sockaddr *)&sock_addr, sock_addr_size);
        if (bind_status == 0) {
            ok = set_path_to_unlink(filename);
        }
        sigprocmask(SIG_SETMASK, &orig_sigmask, NULL);
        if (bind_status != 0) {
            LOG_ERRNO(("bind(%s)", filename));
            goto failed;
        }
        if (!ok) goto failed;
    }
    
    if (listen(fd, 5) < 0) {
        LOG_ERRNO(("listen(%s)", filename));
        goto failed;
    }
    if (-1 == fcntl(fd, F_SETFD, FD_CLOEXEC)) {
        LOG_ERRNO(("fcntl"));
        goto failed;
    }
    
    *socket_fd = fd;
    return true;

  failed:
    if (fd >= 0) close(fd);
    *socket_fd = -1;
    return false;  
}

static void
child_terminated_handler(int UNUSED_ARG(sig))
{
    int status, child_pid;
    
    for (;;) {
        child_pid = waitpid(-1, &status, WNOHANG);
        if (child_pid <= 0) {
            if (child_pid == -1) {
                if (errno == EINTR) continue;
                /* Do not log: waitpid may report error for terminated child */
            }
            break;
        }
    }
}

static bool
setup_child_zombie_killer()
{
    struct sigaction action;
    
    action.sa_handler = child_terminated_handler;
    sigemptyset(&action.sa_mask);
    action.sa_flags = SA_NOCLDSTOP;
    if (sigaction(SIGCHLD, &action, NULL) < 0) {
        LOG_ERRNO(("sigaction(SIGCHLD)"));
        goto failed;
    }
    return true;
    
  failed:
    return false;    
}

static int 
run_server()
{
    int exit_code;
    int socket_fd = -1;
    
    if (!setup_child_zombie_killer()) goto failed;
    
    /* SIGINT should be SIG_IGN when properly forking. */
    signal(SIGINT, cleanup_handler);
    signal(SIGPIPE, SIG_IGN);
    signal(SIGHUP, cleanup_handler);
    signal(SIGTERM, cleanup_handler);
    
    if (!create_local_listen_socket(socket_path, &socket_fd)) goto failed;
    
    if (!do_listen(socket_fd)) goto failed;

    exit_code = EXIT_SUCCESS;
    goto finish;

  failed:
    exit_code = EXIT_FAILURE;
    
  finish:  
    if (socket_fd >= 0) close(socket_fd);
    do_cleanup();
    return exit_code;
}

static void
parse_args(int argc, char **argv)
{
    int c;
    char msg[200];
    
    program_name = strrchr(argv[0], '/');
    if (program_name) {
        ++program_name;
    } else {
        program_name = argv[0];
    }
        
    debug_level = 0;
    stderr_destination = NULL;
    
    /* Do own error message printing */
    opterr = 0;
    while ((c = getopt (argc, argv, ":e:dh")) != -1) {
        switch (c) {
          case 'd':
            ++debug_level;
            continue;
          case 'e':
            stderr_destination = optarg;
            continue;
          case 'h':
            goto show_usage;
          case '?':
            if (isprint(optopt))
                snprintf(msg, sizeof(msg), 
                         "Unknown option '-%c'.", optopt);
            else
                snprintf(msg, sizeof(msg), 
                        "Unknown option character '\\x%x'.", optopt);
            goto bad_usage;
          case ':':
            snprintf(msg, sizeof(msg), 
                     "Missing required argument for option -%c.", optopt);
            goto bad_usage;
          default:
            abort();  
        }
    }
    argc -= optind;
    argv += optind;

    if (argc < 2) {
        if (argc == 0) {
            snprintf(msg, sizeof(msg), 
                     "Missing required file socket argument");
        } else {
            snprintf(msg, sizeof(msg), 
                     "Missing required command name argument");
        }
        goto bad_usage;
    }
    socket_path = argv[0];
    
    /* Move args to remove socket_path to make room for NULL for execvp. */
    memmove(argv, argv + 1, (argc - 1) * sizeof(*argv));
    argv[argc - 1] = NULL;
    exec_cmd_and_args = argv;
    return;    
    
  bad_usage:
    fprintf(stderr, 
"%s: %s\n"
"Try '%s -h' for more information.\n",
        program_name, msg, program_name);
    exit(1);
  
  show_usage:
    fprintf(stdout, 
"Usage: %s [options] FILESYSTEM-SOCKET COMMAND [ARGS]...\n"
"Options:\n"
"  -d      Debug mode. Additional -d increases level of verboseness.\n"
"  -e errorlog\n"
"           Specifies the stderr redirection for COMMAND instead of\n"
"           default /dev/null. If errorlog is '-', it is redirected to\n"
"           the socket together with stdin.\n"
"  -h       Print this help and exit.\n",
        program_name);
    exit(0);
}

int 
main(int argc, char *argv[])
{
    int pid;
    
    parse_args(argc, argv);
    
    if (debug_level == 0) {
        if (0 != daemon(1, 1)) {
            LOG_ERRNO(daemon);
            exit(1);
        }
    }

    return run_server();
}


