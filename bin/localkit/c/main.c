#include <assert.h>
#include <errno.h>
#include <fcntl.h>
#include <poll.h>
#include <signal.h>
#include <stdarg.h>
#include <stdbool.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/random.h>
#include <termios.h>
#include <unistd.h>

#include "vendor/crypt_blowfish/ow-crypt.h"

enum {
    USER_CANCEL_EXIT = 2,

    BCRYPT_HASHSIZE = 64,
    BCRYPT_PASSWORD_COST = 11,
};

struct {
    const char *arg0;
    const char *command_name;
    const char *hash_path;
    const char *gui_title;

    int forced_exit;

    int quit_read_fd;
    int quit_write_fd;

} ds = {
    .quit_read_fd = -1,
    .quit_write_fd = -1,
};

char *g_buffer;
size_t g_buffer_capacity;
size_t g_buffer_end;

typedef struct {
    size_t pos;
    size_t len;
} str_loc_t;

static inline char *bufptr(size_t offset)
{
    assert(offset <= g_buffer_end);
    return g_buffer + offset;
}

static inline char bufchar(size_t offset)
{
    assert(offset < g_buffer_end);
    return g_buffer[offset];
}

static inline void set_bufchar(size_t offset, char c)
{
    assert(offset < g_buffer_end);
    g_buffer[offset] = c;
}

__attribute__ ((noreturn))
void cleanup_and_exit(int code) {
    fflush(stderr);
    _Exit(code);
}

const char *get_program_name()
{
    const char *slash = strrchr(ds.arg0, '/');
    return slash ? slash + 1 : ds.arg0;
}

#define log_bad_io(...) log_bad_io_impl(__FILE__, __LINE__, __func__, __VA_ARGS__)

#define bad_io(...) do { \
    log_bad_io(__VA_ARGS__); \
    cleanup_and_exit(1); \
} while (0)


__attribute__ ((format (printf, 4, 5)))
void log_bad_io_impl(const char *file, int line, const char *func, const char *format, ...)
{
    ds.forced_exit = 1;
    int saved_errno = errno;
    fprintf(stderr, "%s:%s:%d:%s: failed ", get_program_name(), file, line, func);
    va_list ap;
    va_start(ap, format);
    vfprintf(stderr,  format, ap);
    va_end(ap);
    if (saved_errno) {
        fprintf(stderr, " errno=%d - %s", errno, strerror(saved_errno));
    }
    putc('\n', stderr);
}

#define fail(...) do { \
    info(__VA_ARGS__); \
    cleanup_and_exit(1); \
} while (0)

__attribute__ ((format (printf, 1, 2)))
void info(const char *format, ...)
{
    va_list ap;
    fprintf(stderr, "%s: ", get_program_name());
    va_start(ap, format);
    vfprintf(stderr, format, ap);
    va_end(ap);
    putc('\n', stderr);
}

void check_forced_exit()
{
    if (ds.forced_exit)
        cleanup_and_exit(ds.forced_exit);
}

void ensure_buffer(size_t extra)
{
    assert(g_buffer_end <= g_buffer_capacity);
    if (extra >= (1 << 30) - g_buffer_end)
        bad_io("Too big buffer requested: preserve=%zu extra=%zu", g_buffer_end, extra);

    size_t new_end = g_buffer_end + extra;
    if (new_end > g_buffer_capacity) {
        size_t new_capacity = g_buffer_capacity;
        if (new_capacity == 0) {
            new_capacity = 64;
        }
        do {
            new_capacity *= 2;
        } while (new_capacity < new_end);
        if (g_buffer_end < g_buffer_capacity) {
            // Try to shrink if possible
            void *tmp = realloc(g_buffer, g_buffer_end);
            if (tmp) {
                g_buffer = tmp;
                g_buffer_capacity = g_buffer_end;
            }
        }
        g_buffer = realloc(g_buffer, new_capacity);
        if (!g_buffer)
            bad_io("realloc size=%zu", new_capacity);
        g_buffer_capacity = new_capacity;
    }
}

size_t alloc_buffer(size_t size)
{
    ensure_buffer(size);
    size_t offset = g_buffer_end;
    g_buffer_end += size;
    memset(bufptr(offset), 0, size);
    return offset;
}

__attribute__ ((format (printf, 1, 2)))
str_loc_t sprintf_buffer(const char *format, ...)
{
    assert(g_buffer_end <= g_buffer_capacity);
    size_t available = g_buffer_capacity - g_buffer_end;
    va_list ap;
    va_start(ap, format);
    int n = vsnprintf(bufptr(g_buffer_end), available, format, ap);
    va_end(ap);
    if (n < 0)
        bad_io("vsnprintf");
    if ((size_t) n >= available) {
        ensure_buffer((size_t) n + 1);
        va_start(ap, format);
        (void) vsnprintf(bufptr(g_buffer_end), n + 1, format, ap);
        va_end(ap);
    }

    size_t offset = g_buffer_end;

    // Include \0 into the buffer
    g_buffer_end += (size_t) n + 1;

    str_loc_t loc = { offset, (size_t) n };
    return loc;
}

enum {
    WAIT_READ = 1,
    WAIT_WRITE = 2,
    DELAY_ERROR_EXIT = 4,
};

void wait_fd(int fd, int flags)
{
    assert((flags & ~(WAIT_READ | WAIT_WRITE | DELAY_ERROR_EXIT)) == 0);
    assert(
        (flags & (WAIT_READ | WAIT_WRITE)) == WAIT_READ
        || (flags & (WAIT_READ | WAIT_WRITE)) == WAIT_WRITE
    );
    struct pollfd p[2];
    p[0].fd = fd;
    p[0].events = (flags & WAIT_READ) ? POLLIN : POLLOUT;
    p[1].fd = ds.quit_read_fd;
    p[1].events = POLLIN;

    for (;;) {
        int n = poll(p, 2, -1);
        if (n < 0) {
            if (errno == EINTR)
                continue;
            log_bad_io("poll");
            break;
        }
        if (n == 0) {
            log_bad_io("poll timeout");
            break;
        }

        // First check for any error on any fd
        for (size_t i = 0; i != 2; ++i) {
            if (p[i].revents & (POLLERR | POLLNVAL)) {
                log_bad_io("poll fd=%d revents=%d", p[i].fd, p[i].revents);
                break;
            }
        }
        if (p[1].revents & POLLIN) {
            if (!ds.forced_exit) {
                info("quitting on signal");
                ds.forced_exit = 2;
            }
            break;
        }
        if (flags & WAIT_READ) {
            if (p[0].revents & POLLIN)
                return;
        } else {
            if (p[0].revents & (POLLOUT | POLLHUP))
                return;
        }
    }

    if (!(flags & DELAY_ERROR_EXIT)) {
        check_forced_exit();
    }
}

void poll_write_all(int fd, size_t offset, size_t length)
{
    assert(offset <= g_buffer_capacity);
    assert(length <= g_buffer_capacity - offset);

    while (length != 0) {
        wait_fd(fd, WAIT_WRITE);
        ssize_t n = write(fd, bufptr(offset), length);
        if (n < 0) {
            if (errno == EINTR || errno == EAGAIN || EWOULDBLOCK)
                continue;
            bad_io("write fd=%d length=%zu", fd, length);
        }
        offset += (size_t) n;
        length -= (size_t) n;
    }
}

int open_tty()
{
    int fd = open("/dev/tty", O_RDWR | O_NONBLOCK | O_CLOEXEC);
    if (fd < 0) {
        bad_io("open path=/dev/tty");
    }
    return fd;
}

static const char *get_password_title()
{
    if (ds.gui_title)
        return ds.gui_title;
    if (ds.hash_path) {
        const char *name = strrchr(ds.hash_path, '/');
        if (name) {
            ++name;
        } else {
            name = ds.hash_path;
        }
        return name;
    }
    return get_program_name();
}

str_loc_t ask_tty_password(int tty_fd)
{
    // getpass in glibc disables both ECHO and ISIG.
    // ssh.terminal.ReadPassword in Go disables ECHO and
    // sets ICANON, ISIG, ICRNL. That seems more resonable,
    // so follow it, but also reset VEOL.
    struct termios tio, saved_tio;
    int tc_status = tcgetattr(tty_fd, &tio);
    if (tc_status != 0)
        bad_io("tcgetattr");
    saved_tio = tio;
    tio.c_lflag &= ~ECHO;
    tio.c_lflag |= ICANON | ISIG;
    tio.c_iflag |= ICRNL;
    tio.c_cc[VEOL] = '\0';
    tc_status = tcsetattr(tty_fd, TCSAFLUSH, &tio);
    if (tc_status != 0)
        bad_io("tcsetattr");

    const size_t MAX_PASSWORD_LENGTH = 80;

    size_t offset = alloc_buffer(MAX_PASSWORD_LENGTH + 1);

    size_t n = 0;
    for (;;) {
        wait_fd(tty_fd, WAIT_READ | DELAY_ERROR_EXIT);
        if (ds.forced_exit)
            break;
        ssize_t nread = read(tty_fd, bufptr(offset), MAX_PASSWORD_LENGTH + 1);
        if (nread < 0) {
            if (errno == EINTR || errno == EAGAIN || EWOULDBLOCK)
                continue;
            log_bad_io("read");
            break;
        }
        if (nread != 0) {
            n = (size_t) nread;
            break;
        }
    }
    tc_status = tcsetattr(tty_fd, TCSAFLUSH, &saved_tio);
    if (tc_status != 0)
        bad_io("tcsetattr");
    check_forced_exit();

    if (bufchar(offset + n - 1) == '\n') {
        --n;

        // Write \n that was not echoed
        poll_write_all(tty_fd, offset + n, 1);
    } else if (n == MAX_PASSWORD_LENGTH + 1) {
        fail("Too long password, its length must not exceed %zu bytes",
            MAX_PASSWORD_LENGTH);
    }

    set_bufchar(offset + n, '\0');
    str_loc_t password = { offset, n };
    return password;
}

str_loc_t get_gui_ask_password_prog()
{
    const char *env = "SSH_ASKPASS";
    char *ask = getenv(env);
    if (!ask || !*ask) {
        fail("failed to find program to ask for password - %s is unset or empty", env);
    }
    return sprintf_buffer("%s", ask);
}

str_loc_t ask_gui_password(str_loc_t gui_prog, str_loc_t message)
{
    (void) gui_prog;
    (void) message;
    fail("not implemented");
}

void show_gui_message(str_loc_t gui_prog, str_loc_t message)
{
    (void) ask_gui_password(gui_prog, message);
}

void ask_password()
{
    str_loc_t password;
    str_loc_t gui_prog = {0, 0};

    int tty_fd = open_tty();
    if (tty_fd < 0) {
        gui_prog = get_gui_ask_password_prog();
    }

    if (tty_fd >= 0) {
        str_loc_t prompt = sprintf_buffer("Enter %s password: ", get_password_title());
        poll_write_all(tty_fd, prompt.pos, prompt.len);
        password = ask_tty_password(tty_fd);
    } else {
        str_loc_t prompt = sprintf_buffer("Enter %s password", get_password_title());
        password = ask_gui_password(gui_prog, prompt);
    }

    size_t stored_hash = alloc_buffer(BCRYPT_HASHSIZE);
    int fd = open(ds.hash_path, O_RDONLY);
    if (fd < 0) {
        bad_io("open path=%s", ds.hash_path);
    }

    size_t hash_length;
    for (size_t cursor = 0;;) {
        ssize_t n = read(fd, bufptr(stored_hash + cursor), BCRYPT_HASHSIZE - cursor);
        if (n < 0) {
            if (errno == EINTR)
                continue;
            bad_io("read path=%s", ds.hash_path);
        }
        if (n == 0) {
            set_bufchar(stored_hash + cursor, '\0');
            hash_length = cursor;
            break;
        }
        cursor += n;
        if (cursor == BCRYPT_HASHSIZE) {
            fail("size of %s exceeds max supported length %d",
                 ds.hash_path, BCRYPT_HASHSIZE);
        }
    }
    int status = close(fd);
    if (status != 0) {
        bad_io("close fd=%d", fd);
    }

    size_t computed_hash = alloc_buffer(BCRYPT_HASHSIZE);

    const char *crypt_status = crypt_rn(
        bufptr(password.pos), bufptr(stored_hash),
        bufptr(computed_hash), BCRYPT_HASHSIZE
    );
    if (!crypt_status) {
        fail("error when computing password hash using data from %s", ds.hash_path);
    }
    if (hash_length != strlen(bufptr(computed_hash))) {
        info("\n'%s'\n'%s'", bufptr(stored_hash), bufptr(computed_hash));
        fail(
            "computed password length mismatch, expected=%zu actual=%zu",
            hash_length, strlen(bufptr(computed_hash))
        );
    }

    // Use context-independent compare to avoid timing attacks.
    unsigned diff = 0;
    for (size_t i = 0; i != hash_length; ++i) {
        diff |= bufchar(computed_hash + i) ^ bufchar(stored_hash + i);
    }
    if (diff) {
        if (tty_fd >= 0) {
            str_loc_t msg = sprintf_buffer("Password mismatch\n");
            poll_write_all(tty_fd, msg.pos, msg.len);
        } else {
            str_loc_t msg = sprintf_buffer("Password mismatch");
            show_gui_message(gui_prog, msg);
        }
        cleanup_and_exit(USER_CANCEL_EXIT);
    }
    poll_write_all(1, password.pos, password.len);

    if (tty_fd >= 0) {
        status = close(tty_fd);
        if (status != 0)
            bad_io("close fd=%d", tty_fd);
    }
}

void set_password_hash()
{
    str_loc_t gui_prog = {0, 0};
    int tty_fd = open_tty();
    if (tty_fd < 0) {
        gui_prog = get_gui_ask_password_prog();
    }

    str_loc_t password;
    if (tty_fd >= 0) {
        str_loc_t prompt = sprintf_buffer(
            "Enter a new password for %s: ", get_password_title()
        );
        poll_write_all(tty_fd, prompt.pos, prompt.len);
        password = ask_tty_password(tty_fd);
        if (!password.len) {
            // Write on tty as this is a GUI message for the user.
            str_loc_t msg = sprintf_buffer("Password cannot be empty\n");
            poll_write_all(tty_fd, msg.pos, msg.len);
            cleanup_and_exit(1);
        }

        str_loc_t prompt2 = sprintf_buffer(
            "Repeat to confirm the new value for %s: ", get_password_title()
        );
        poll_write_all(tty_fd, prompt2.pos, prompt2.len);
        str_loc_t password2 = ask_tty_password(tty_fd);
        if (!password2.len) {
            str_loc_t msg = sprintf_buffer("Canceled\n");
            poll_write_all(tty_fd, msg.pos, msg.len);
            cleanup_and_exit(USER_CANCEL_EXIT);
        }
        if (password2.len != password.len
            || memcmp(bufptr(password2.pos), bufptr(password.pos), password.len) != 0) {
            str_loc_t msg = sprintf_buffer("Password mismatch\n");
            poll_write_all(tty_fd, msg.pos, msg.len);
            cleanup_and_exit(1);
        }

        int status = close(tty_fd);
        if (status != 0)
            bad_io("close fd=%d", tty_fd);

    } else {
        str_loc_t prompt = sprintf_buffer(
            "Enter a new password for %s", get_password_title()
        );
        password = ask_gui_password(gui_prog, prompt);
        if (!password.len) {
            str_loc_t msg = sprintf_buffer("Password cannot be empty");
            show_gui_message(gui_prog, msg);
            cleanup_and_exit(1);
        }
        str_loc_t prompt2 = sprintf_buffer(
            "Repeat to confirm the new value for %s", get_password_title()
        );
        str_loc_t password2 = ask_gui_password(gui_prog, prompt2);
        if (!password2.len) {
            cleanup_and_exit(USER_CANCEL_EXIT);
        }
        if (password2.len != password.len
            || memcmp(bufptr(password2.pos), bufptr(password.pos), password.len) != 0) {
            str_loc_t msg = sprintf_buffer("Password mismatch");
            show_gui_message(gui_prog, msg);
            cleanup_and_exit(1);
        }
    }

    enum {
        RANDOM_BYTE_COUNT = 16,
    };
    size_t random_input = alloc_buffer(RANDOM_BYTE_COUNT);
    for (size_t cursor = 0; cursor != RANDOM_BYTE_COUNT;) {
        int n = getrandom(bufptr(random_input + cursor), RANDOM_BYTE_COUNT - cursor, 0);
        if (n < 0) {
            if (errno == EAGAIN)
                continue;
            bad_io("getrandom");
        }
        cursor += n;
    }

    size_t salt = alloc_buffer(BCRYPT_HASHSIZE);
    const char *crypt_status = crypt_gensalt_rn(
        "$2y$", BCRYPT_PASSWORD_COST,
        bufptr(random_input), RANDOM_BYTE_COUNT,
        bufptr(salt), BCRYPT_HASHSIZE
    );
    if (!crypt_status) {
        bad_io("crypt_gensalt_rn");
    }
    size_t computed_hash = alloc_buffer(BCRYPT_HASHSIZE);
    crypt_status = crypt_rn(
        bufptr(password.pos), bufptr(salt),
        bufptr(computed_hash), BCRYPT_HASHSIZE
    );
    if (!crypt_status) {
        bad_io("crypt_rn");
    }
    size_t hash_length = strlen(bufptr(computed_hash));

    int fd = open(ds.hash_path, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR);
    if (fd < 0) {
        bad_io("open path=%s", ds.hash_path);
    }
    for (size_t cursor = 0; cursor != hash_length;) {
        ssize_t n = write(fd, bufptr(computed_hash + cursor), hash_length - cursor);
        if (n < 0) {
            if (errno == EINTR)
                continue;
            bad_io("write path=%s", ds.hash_path);
        }
        cursor += n;
    }
    int status = close(fd);
    if (status != 0) {
        bad_io("close fd=%d", fd);
    }
}

__attribute__ ((format (printf, 1, 2)))
void usage_error(const char *format, ...)
{
    va_list ap;
    va_start(ap, format);
    fprintf(stderr, "%s: ", get_program_name());
    vfprintf(stderr, format, ap);
    fprintf(stderr, "\nTry '%s help' for more information.\n", get_program_name());
    va_end(ap);
    cleanup_and_exit(1);
}

void show_usage()
{
    printf(
"Usage: %s command [ARGS]...\n"
"Perform one of the following commands:\n"
"\n"
"  help\n"
"    print this help and exit\n"
"\n"
"  ask-password [-t TITLE] -p HASH_PATH\n"
"    ask for a password and print it on stdout if its hash matches the\n"
"    value in HASH_PATH. The GUI uses TITLE when asking for a password.\n"
"    The default value for TITLE is the name of HASH_PATH.\n"
"\n"
"  set-password-hash [-t TITLE] -p HASH_PATH\n"
"    ask for a new password and updates HASH_PATH with the new hash\n"
"    value of the password. The GUI uses TITLE when asking for a password.\n"
"    The default value for TITLE is the name of HASH_PATH.\n"
"\n"
"Exit codes:\n"
"  0 - success\n"
"  1 - generic error\n"
"  2 - user canceled the action\n"
"\n",
        get_program_name());
    cleanup_and_exit(0);
}

typedef enum {
    CMD_HELP,
    CMD_ASK_PASSWORD,
    CMD_SET_PASSWORD_HASH,
} CommandId;

static void quit_signal_handler(int signal)
{
    uint8_t kind = (uint8_t) signal;

    // XXX - handle write errors somehow
    (void) write(ds.quit_write_fd, &kind, 1);
}

int main(int argc, char **argv)
{
    ds.arg0 = argv[0];

    if (argc == 1) {
        usage_error("no command was given");
    }

    static const char command_names[] =
        "ask-password\0"
        "set-password-hash\0"
        "help\0"
        "--help\0"
        "-h\0";
    static const CommandId command_ids[] = {
        CMD_ASK_PASSWORD,
        CMD_SET_PASSWORD_HASH,
        CMD_HELP,
        CMD_HELP,
        CMD_HELP,
    };

    ds.command_name =  argv[1];
    int index = 0;
    for (const char *s = command_names; ; ++index) {
        if (strcmp(ds.command_name, s) == 0)
            break;
        s = strchr(s, '\0') + 1;
        if (!*s) {
            usage_error("unknown command %s", ds.command_name);
        }
    }

    CommandId command_id = command_ids[index];
    if (command_id == CMD_HELP) {
        show_usage();
    }

    int command_argc = argc - 1;
    char **command_argv = argv + 1;
    int opt;
    opterr = 0;
    while ((opt = getopt(command_argc, command_argv, "+:hp:t:")) != -1) {
        switch (opt) {
        case 'p':
            ds.hash_path = optarg;
            break;
        case 't':
            ds.gui_title = optarg;
            break;
        case 'h':
            show_usage();
            break;
        case '?':
            usage_error("unknown option -%c", (char) optopt);
            break;
        case ':':
            usage_error("option -%c requires an argument", (char) optopt);
            break;
        }
    }
    if (optind != command_argc) {
        usage_error("unexpected argument %s", command_argv[optind]);
    }

    if (!ds.hash_path) {
        usage_error("-p is required for %s", command_argv[0]);
    }

    int signal_pipe_fds[2];
    int status = pipe(signal_pipe_fds);
    if (status != 0)
        bad_io("pipe");
    ds.quit_read_fd = signal_pipe_fds[0];
    ds.quit_write_fd = signal_pipe_fds[1];

    static const int quit_signals[] = { SIGINT, SIGQUIT, SIGTERM, 0 };
    struct sigaction sa;
    sa.sa_handler = quit_signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    for (const int *s = quit_signals; *s; ++s) {
        int status = sigaction(*s, &sa, NULL);
        if (status != 0)
            bad_io("sigaction signal=%d", *s);
    }

    switch (command_ids[index]) {
        case CMD_ASK_PASSWORD:
            ask_password();
            break;
        case CMD_SET_PASSWORD_HASH:
            set_password_hash();
            break;
        default:
            assert(0);
    }
}
