#include <assert.h>
#include <ctype.h>
#include <errno.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stddef.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <signal.h>
#include <unistd.h>
#include <sys/socket.h>
#include <sys/stat.h>
#include <sys/select.h>
#include <sys/un.h>
#include <netinet/in.h>
#include <netdb.h>
#include <netinet/tcp.h>
 
#define LIKE_PRINTF(string_index, first_to_check) \
    __attribute__((format(printf, string_index, first_to_check)))

#define DBG(condition, x) do { if (condition) print_debug x; } while (0)

static void 
print_debug(const char *format, ...) LIKE_PRINTF(1, 2);
static void 
log_err(const char *format, ...) LIKE_PRINTF(1, 2);
static void 
log_errno(const char *format, ...) LIKE_PRINTF(1, 2);

#define DEFAULT_BUFFER_SIZE (32U * 1024U)

static unsigned debug_level = 0;
static size_t buffer_size = 0;
static char *program_name;

static void 
print_debug(const char *format, ...)
{
    va_list ap;
    
    fprintf(stderr, "[%s] ", program_name);
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    putc('\n', stderr);
}

static void 
log_err(const char *format, ...)
{
    va_list ap;
    
    fprintf(stderr, "%s: ", program_name);
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    putc('\n', stderr);
}

static void 
log_errno(const char *format, ...)
{
    int saved_errno;
    va_list ap;
    
    saved_errno = errno;
    fprintf(stderr, "%s: ", program_name);
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    fprintf(stderr, ": %s, errno=%d\n", strerror(saved_errno), saved_errno);
}


struct FDCell
{
    int fd;
    bool closed;
};

enum SimplexPipeState
{
    SPS_NORMAL,
    SPS_READ_END,
    SPS_READ_WRITE_END
};

struct SimplexPipe
{
    struct FDCell *reading;
    struct FDCell *writing;
    enum SimplexPipeState state;
    size_t buf_size;
    size_t buf_start;
    size_t buf_end;
    unsigned char *buf_area;
};

void fd_cell_close(struct FDCell *cell)
{
    DBG(debug_level > 0, ("close(%d)", cell->fd));
    if (close(cell->fd) < 0) log_errno("close");
    cell->closed = true;
}

bool spipe_init(struct FDCell *reading_cell, struct FDCell *writing_cell,
                size_t buf_size, struct SimplexPipe *spipe)
{
    assert(reading_cell->fd >= 0);
    assert(!reading_cell->closed);
    assert(writing_cell->fd >= 0);
    assert(!writing_cell->closed);
    assert(buf_size >= 1);
    
    spipe->reading = reading_cell;
    spipe->writing = writing_cell;
    spipe->state = SPS_NORMAL;
    spipe->buf_size = buf_size;
    spipe->buf_start = 0;
    spipe->buf_end = 0;
    spipe->buf_area = malloc(buf_size);
    if (!spipe->buf_area) {
        log_err("malloc %zu", buf_size);
        goto failed;
    }
    return true;
    
  failed:
    return false;  
}

void spipe_release(struct SimplexPipe *spipe)
{
    if (spipe->buf_area) {
        free(spipe->buf_area);
        spipe->buf_area = NULL;
    }
}

bool spipe_can_read(struct SimplexPipe *spipe)
{
    return spipe->buf_size - spipe->buf_end;
}

bool spipe_can_write(const struct SimplexPipe *spipe)
{
    return spipe->buf_end - spipe->buf_start;
}

static bool is_socket(struct FDCell *fd_cell)
{
    struct stat stat_buf;
    
    if (fstat(fd_cell->fd, &stat_buf) < 0) {
        log_errno("fstat");
        return false;
    }
    return S_ISSOCK(stat_buf.st_mode);
}

static void 
spipe_stop_read_impl(struct SimplexPipe *spipe)
{
    assert(spipe->state == SPS_NORMAL);
    if (!spipe->reading->closed) {
        if (is_socket(spipe->reading)) {
            DBG(debug_level > 0, 
                ("shutdown(%d, SHUT_RD)", spipe->reading->fd));
            if (shutdown(spipe->reading->fd, SHUT_RD) < 0) {
                /* 
                 * It is OK to receive ENOTCONN as socket can be already 
                 * closed by the other end.
                 */
                if (errno != ENOTCONN) {
                    log_errno("shutdown SHUT_RD");
                }
                fd_cell_close(spipe->reading);
            }
        } else {
            fd_cell_close(spipe->reading);
        }
    }
    spipe->state = SPS_READ_END;
}

void spipe_stop_read_write(struct SimplexPipe *spipe)
{
    if (spipe->state == SPS_NORMAL) {
        spipe_stop_read_impl(spipe);
    }
    assert(spipe->state == SPS_READ_END);
    
    if (!spipe->writing->closed) {
        if (is_socket(spipe->writing)) {
            DBG(debug_level > 0, 
                ("shutdown(%d, SHUT_WR)", spipe->writing->fd));
            if (shutdown(spipe->writing->fd, SHUT_WR) < 0) {
                /* 
                 * It is OK to receive ENOTCONN as socket can be already 
                 * closed by the other end.
                 */
                if (errno != ENOTCONN) {
                    log_errno("shutdown SHUT_WR");
                }
                fd_cell_close(spipe->writing);
            }
        } else {
            fd_cell_close(spipe->writing);
        }
    }
    spipe->state = SPS_READ_WRITE_END;
}

void spipe_stop_read(struct SimplexPipe *spipe)
{
    spipe_stop_read_impl(spipe);
    /* 
     * Close write end as well if buffer empty to avoid deadlock when
     * select loop ignore spipe->writing->fd on empty buffer and 
     * spipe_write would never call spipe_stop_read_write(spipe);
     * causing clients to wait for the input from spipe->writing->fd forever. 
     */
    if (!spipe_can_write(spipe)) {
        spipe_stop_read_write(spipe);
    }
}

/*
 * Read out-of-band byte from source and sent it to sink. 
 */
bool spipe_transfer_oob_byte(struct SimplexPipe *spipe)
{
    bool ok;
    char c;
    int n;
    
    assert(spipe->state == SPS_NORMAL);
    
    if (spipe->reading->closed || spipe->writing->closed) {
        return true;
    }
    
    DBG(debug_level > 1,
        ("Transfering OOB from %d to %d", 
         spipe->reading->fd, spipe->writing->fd));
    n = recv(spipe->reading->fd, &c, 1, MSG_OOB);
    if (n == 1) {
        n = send(spipe->writing->fd, &c, 1, MSG_OOB);
        if (n == 1) {
            ok = true;
        } else if (n == 0) {
            log_err("Failed to send MSG_OOB byte\n");
            ok = false;
        } else {
            if (errno == ECONNRESET || errno == EPIPE) {
                /* write end was closed */
                fd_cell_close(spipe->writing);
                spipe_stop_read_write(spipe);
                ok = true;
            } else {
                log_errno("send");
                ok = false;
            }
        }
    } else if (n == 0) {
        /* source was shutdown. */
        spipe_stop_read(spipe);
        ok = true;
    } else  {
        if (errno == EINTR || errno == EAGAIN) {
            ok = true;
        } else {
            fd_cell_close(spipe->reading);
            spipe_stop_read(spipe);
            ok = false;
        }
    }
    return ok;
}

bool spipe_read(struct SimplexPipe *spipe)
{
    bool        ok;
    void        *read_area;
    size_t      read_capacity;
    ssize_t     n;
    
    assert(spipe->state == SPS_NORMAL);
    
    if (spipe->reading->closed) {
        return true;
    }

    read_capacity = spipe->buf_size - spipe->buf_end;
    assert(read_capacity != 0);
    read_area = spipe->buf_area + spipe->buf_end;
    
    n = read(spipe->reading->fd, read_area, read_capacity);
    DBG(debug_level > 1, 
        ("read(%d,,%zu)=>%zd", spipe->reading->fd, read_capacity, n));
    if (n >= 1) {
        spipe->buf_end += n;
        ok = true;
    } else if (n == 0) {
        /* End-of-file */
        spipe_stop_read(spipe);
        ok = true;
    } else {
        if (errno == EINTR || errno == EAGAIN) {
            ok = true;            
        } else if (errno == ECONNRESET || errno == EPIPE) {
            fd_cell_close(spipe->reading);
            spipe_stop_read(spipe);
            ok = true;
        } else {
            log_errno("read");
            fd_cell_close(spipe->reading);
            spipe_stop_read(spipe);
            ok = false;
        }
    }
    
    return ok;
}

static void 
spipe_finalize_write(struct SimplexPipe *spipe)
{
    size_t used_size;
    
    /* Merge head and tail space. */
    used_size = spipe->buf_end - spipe->buf_start;
    if (used_size == 0) {
        spipe->buf_start = 0;
        spipe->buf_end = 0;
    } else if (used_size <= spipe->buf_size / 4) {
        memmove(spipe->buf_area, spipe->buf_area + spipe->buf_start, used_size);
        spipe->buf_start = 0; 
        spipe->buf_end = used_size;
    }

    if (spipe->state == SPS_READ_END && !spipe_can_write(spipe)) {
        spipe_stop_read_write(spipe);
    }
}

bool spipe_write(struct SimplexPipe *spipe)
{
    bool        ok;
    const void  *write_area;
    size_t      write_size;
    ssize_t     n;

    assert(spipe->state != SPS_READ_WRITE_END);

    if (spipe->writing->closed) {
        return true;
    }

    write_size = spipe->buf_end - spipe->buf_start;
    assert(write_size != 0);
    write_area = spipe->buf_area + spipe->buf_start;
    
    n = write(spipe->writing->fd, write_area, write_size);
    DBG(debug_level > 1, 
        ("write(%d,,%zu)=>%zd", spipe->writing->fd, write_size, n));
    if (n >= 0) {
        spipe->buf_start += n;
        spipe_finalize_write(spipe);
        ok = true;
    } else {
        if (errno == EINTR || errno == EAGAIN) {
            ok = true;
        } else if (errno == ECONNRESET || errno == EPIPE) {
            fd_cell_close(spipe->writing);
            spipe_stop_read_write(spipe);
            ok = true;
        } else {
            log_errno("write");
            fd_cell_close(spipe->writing);
            spipe_stop_read_write(spipe);
            ok = false;
        }
    }
    
    return ok;
}

static bool do_select_loop(struct SimplexPipe *spipe_start, 
                           struct SimplexPipe *spipe_end)
{
    bool ok;
    fd_set rd, wr, er;
    int fd_max, nselected, max_select_checks;
    struct SimplexPipe *spipe;
    
    ok = true;
    
  restart:
    FD_ZERO(&rd);
    FD_ZERO(&wr);
    FD_ZERO(&er);
    
    for (;;) {
        
        fd_max = -1;

        for (spipe = spipe_start; spipe != spipe_end; ++spipe) {
            if (spipe->state != SPS_READ_WRITE_END
                && !spipe->writing->closed) 
            {
                if (spipe->state == SPS_NORMAL && !spipe->reading->closed) {
                    FD_SET(spipe->reading->fd, &er);
                    DBG(debug_level > 2, 
                        ("FD_SET(%d, er)", spipe->reading->fd));
                    if (spipe->reading->fd > fd_max) 
                        fd_max = spipe->reading->fd;
                    if (spipe_can_read(spipe)) {
                        FD_SET(spipe->reading->fd, &rd);
                        DBG(debug_level > 2, 
                            ("FD_SET(%d, rd)", spipe->reading->fd));
                    }
                }
                if (spipe_can_write(spipe)) {
                    FD_SET(spipe->writing->fd, &wr);
                    DBG(debug_level > 2, 
                        ("FD_SET(%d, wr)", spipe->writing->fd));
                    if (spipe->writing->fd > fd_max) 
                        fd_max = spipe->writing->fd;
                }
            }
        }
        
        if (fd_max < 0) {
            break;
        }
        
        DBG(debug_level > 1, ("select with fd_max=%d", fd_max));
        nselected = select(fd_max + 1, &rd, &wr, &er, NULL);
        DBG(debug_level > 1, ("select=>%d", nselected));

        if (nselected < 0) {
            if (nselected == -1 && errno == EINTR) {
                goto restart;
            }
            log_errno("select");
            ok = false;
            break;
        } else if (nselected == 0) {
            /* Should not happen with 0 timeout */
            continue;
        }
        
        /* NB: pipe out-of-band data before normal reads/writes. */
        max_select_checks = nselected;
        for (spipe = spipe_start; spipe != spipe_end; ++spipe) {
            if (spipe->state == SPS_NORMAL) {
                if (FD_ISSET(spipe->reading->fd, &er)) {
                    ok &= spipe_transfer_oob_byte(spipe);
                    FD_CLR(spipe->reading->fd, &er);
                    --max_select_checks;
                    if (max_select_checks == 0) break;
                }
            }
        }

        /* Do reads before writes to fill write buffers as much as possible*/
        max_select_checks = nselected;
        for (spipe = spipe_start; spipe != spipe_end; ++spipe) {
            if (spipe->state == SPS_NORMAL) {
                if (FD_ISSET(spipe->reading->fd, &rd)) {
                    ok &= spipe_read(spipe);
                    FD_CLR(spipe->reading->fd, &rd);
                    --max_select_checks;
                    if (max_select_checks == 0) break;
                }
            }
        }

        /* Write the buffers. */
        max_select_checks = nselected;
        for (spipe = spipe_start; spipe != spipe_end; ++spipe) {
            if (spipe->state != SPS_READ_WRITE_END) {
                if (FD_ISSET(spipe->writing->fd, &wr)) {
                    ok &= spipe_write(spipe);
                    FD_CLR(spipe->writing->fd, &wr);
                    --max_select_checks;
                    if (max_select_checks == 0) break;
                }
            }
        }
    }
        
    return ok;
}

static bool do_transfer(int source_fd, int sink_fd, int socket_fd)
{
    bool ok;
    struct FDCell fd_cells[3] = { 
        { source_fd, false },
        { sink_fd, false },
        { socket_fd, false }
    };
    struct SimplexPipe spipes[2];
    
    ok = spipe_init(&fd_cells[0], &fd_cells[2], buffer_size, &spipes[0]);
    if (ok) {
        ok = spipe_init(&fd_cells[2], &fd_cells[1], buffer_size, &spipes[1]);
        if (ok) {
            ok = do_select_loop(spipes, spipes + 2);
            spipe_release(&spipes[1]);
        }
        spipe_release(&spipes[0]);
    }
    
    return ok;
}


static int connect_local_socket(const char *socket_path)
{
    size_t name_length;
    struct sockaddr_un sock_addr;    
    int fd = -1;
    size_t sock_addr_size;
    
    name_length = strlen(socket_path);
    if (name_length > sizeof(sock_addr.sun_path) - 1) {
        log_err("Length of socket_path exceeded max limit: %zu > %zu\n",
                name_length, sizeof(sock_addr.sun_path) - 1);
        goto failed;
    }

    sock_addr.sun_family = AF_LOCAL;
    memcpy(sock_addr.sun_path, socket_path, name_length);
    sock_addr.sun_path[name_length] = '\0';
    sock_addr_size = offsetof(struct sockaddr_un, sun_path) + name_length + 1;

    fd = socket(PF_LOCAL, SOCK_STREAM, 0);
    if (fd < 0) {
        log_errno("socket(PF_LOCAL, SOCK_STREAM, 0)");
        goto failed;
    }

    if (connect(fd, (struct sockaddr *)&sock_addr, sock_addr_size) < 0) {
        log_errno("connect");
        goto failed;
    }
    
    return fd;

  failed:
    if (fd >= 0) close(fd);
    return -1;
}

static int connect_tcp_socket(const char *tcp_name_port)
{
    struct sockaddr_in inet_sock_addr;
    int fd = -1;
    unsigned long port;
    size_t host_name_length;
    const char *host_name, *port_start;
    char *port_end;
    
    port_start = strchr(tcp_name_port, '/');
    if (!port_start) {
        port_start = tcp_name_port;
        host_name = "localhost";
        host_name_length = strlen(host_name);
    } else {
        host_name = tcp_name_port;
        host_name_length = port_start - tcp_name_port; 
        ++port_start; /* Skip '/'. */
    }
    
    port = strtoul(port_start, &port_end, 0);
    if (*port_end != '\0') {
        log_err("Bad tcp socket port: %s", port_start);
        goto failed;
    }
    if (port > 0xFFFF) {
        log_err("Tcp socket port exceeds 64K limit: %s", port_start);
        goto failed;
    }
    
    inet_sock_addr.sin_family = AF_INET;
    inet_sock_addr.sin_port = htons((uint16_t)port);
    
    /* Find the IP4 address. */
    {
        struct hostent *hostinfo;
        char *tmp;
        
        tmp = malloc(host_name_length + 1);
        if (!tmp) {
            log_err("malloc(%zu)", host_name_length + 1);
            goto failed;
        }
        memcpy(tmp, host_name, host_name_length);
        tmp[host_name_length] = '\0';
        hostinfo = gethostbyname(tmp);
        free(tmp);
        if (!hostinfo) {
            log_err("Failed to locate %s", host_name);
            goto failed;
        }
        if (hostinfo->h_length != sizeof(struct in_addr)) {
            log_err("Failed to locate INET4 address for %s", host_name);
            goto failed;
        }
        inet_sock_addr.sin_addr = *(struct in_addr *)hostinfo->h_addr;
    }

    fd = socket(PF_INET, SOCK_STREAM, 0);
    if (fd < 0) {
        log_errno("socket(PF_INET, SOCK_STREAM, 0)");
        goto failed;
    }

    if (connect(fd, (struct sockaddr *)&inet_sock_addr, 
                sizeof(struct sockaddr_in)) < 0) 
    {
        log_errno("connect");
        goto failed;
    }
    
    /* Disable delay. */
    /* TODO: can this be avoided if other end does not need it ? */
    {
        int opt_value = 1;
        DBG(debug_level > 1, ("TCP_NODELAY for fd=%d", fd));
        if (setsockopt(fd, IPPROTO_TCP, TCP_NODELAY,
		       &opt_value, sizeof(opt_value)) < 0) {
            log_errno("setsockopt(%d, IPPROTO_TCP, TCP_NODELAY...)", fd);
            goto failed;
        }
    }
    return fd;

  failed:
    if (fd >= 0) close(fd);
    return -1;
}


int connect_socket(const char *socket_spec)
{
    int fd;
    const char *s;
    size_t prefix_size;
    int socket_type;
    
    s = strchr(socket_spec, ':');
    if (!s) {
        socket_type = PF_LOCAL;
        s = socket_spec;
    } else {
        ++s;
        prefix_size = s - socket_spec;
        if (strncmp("local:", socket_spec, prefix_size) == 0
            || strncmp("unix:", socket_spec, prefix_size) == 0)
        {
            socket_type = PF_LOCAL;
        } else if (strncmp("tcp:", socket_spec, prefix_size) == 0) {
            socket_type = PF_INET;
        } else {
            log_err("Unknown socket protocol: %s\n", socket_spec);
            goto failed;
        }
    }
    
    if (socket_type == PF_LOCAL) {
        fd = connect_local_socket(s);
    } else {
        fd = connect_tcp_socket(s);
    }
    if (fd < 0) goto failed;
    
    return fd;
    
  failed:
    return -1;  
} 

static char *
parse_args(int argc, char *argv[])
{
    char *s;
    int c;
    char msg[200];
    
    program_name = strrchr(argv[0], '/');
    if (program_name) {
        ++program_name;
    } else {
        program_name = argv[0];
    }
        
    buffer_size = DEFAULT_BUFFER_SIZE;
    debug_level = 0;
    
    /* Do own error message printing */
    opterr = 0;
    while ((c = getopt (argc, argv, ":b:dh")) != -1) {
        switch (c) {
          case 'b':
            buffer_size = strtoul(optarg, &s, 0);
            if (*s != '\0') {
                snprintf(msg, sizeof(msg), 
                        "Can not parse -b argument '%s' as number.", optarg);
                goto bad_usage;
            }
            break;
          case 'd':
            ++debug_level;
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
    if (argc == 0) {
        snprintf(msg, sizeof(msg), 
                 "Missing required file socket arhument");
        goto bad_usage;
    } else if (argc > 1) {
        snprintf(msg, sizeof(msg), 
                 "Too many arguments");
        goto bad_usage;
    }
    return argv[0];    
    
  bad_usage:
    fprintf(stderr, 
"%s: %s\n"
"Try '%s -h' for more information.\n",
        program_name, msg, program_name);
    exit(1);
  
  show_usage:
    fprintf(stdout, 
"Usage: %s [options] SOCKET_SPEC\n"
"where SOCKET_SPEC can have the following form:\n"
"  local:path   Filesystem socket.\n"
"  unix:path    Alias for local:path.\n"
"  tcp:host_name/port\n"    
"               Connect using tcp to the given host and port.\n"
"  tcp:port     Alias for tcp:localhost/port.\n"  
"If SOCKET_SPEC does not contain ':', 'local:' prefix is assumed.\n"  
"\n"
"Options:\n"
"  -b size   Change buffer size to 'size' bytes from default 0x%X.\n"
"  -d        Debug mode. Additional -d increases level of verboseness.\n"
"  -h        Print this help and exit.\n"
"\n",
        program_name, DEFAULT_BUFFER_SIZE);
    exit(0);
}

int main(int argc, char *argv[])
{
    char *socket_spec;
    int exit_code;
    int socket_fd = -1;

    socket_spec = parse_args(argc, argv);    
    
    signal(SIGPIPE, SIG_IGN);

    socket_fd = connect_socket(socket_spec);
    if (socket_fd < 0)
        goto failed;
    
    DBG(debug_level > 0, 
        ("Connected to: %s, socket_fd=%d", socket_spec, socket_fd));
    if (!do_transfer(STDIN_FILENO, STDOUT_FILENO, socket_fd))
        goto failed;
    DBG(debug_level > 0, ("exit, code=0"));

    exit_code = EXIT_SUCCESS;
    goto finish;
  
  failed:
    exit_code = EXIT_FAILURE;
    
  finish:  
    if (socket_fd >= 0) close(socket_fd);
    return exit_code;
}
