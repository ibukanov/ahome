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

    char *buffer;
    size_t buffer_capacity;

    int forced_exit;

    int quit_read_fd;
    int quit_write_fd;

} ds = {
    .quit_read_fd = -1,
    .quit_write_fd = -1,
};

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

void ensure_buffer(size_t preserve, size_t extra)
{
    assert(preserve <= ds.buffer_capacity);
    if (extra >= (1 << 30) - preserve)
        bad_io("Too big buffer requested: preserve=%zu extra=%zu", preserve, extra);

    size_t n = ds.buffer_capacity;
    size_t minimal_capacity = preserve + extra;
    if (n >= minimal_capacity)
        return;
    if (n == 0) {
        n = 64;
    }
    do {
        n *= 2;
    } while (n < minimal_capacity);
    if (preserve < ds.buffer_capacity) {
        if (preserve == 0) {
            free(ds.buffer);
            ds.buffer = NULL;
        } else {
            // Try to shrink if possible
            void *tmp = realloc(ds.buffer, preserve);
            if (tmp) {
                ds.buffer = tmp;
            }
        }
    }
    ds.buffer = realloc(ds.buffer, n);
    if (!ds.buffer)
        bad_io("realloc size=%zu", n);
    ds.buffer_capacity = n;
}

__attribute__ ((format (printf, 2, 3)))
size_t sprintf_buffer(size_t offset, const char *format, ...)
{
    assert(offset <= ds.buffer_capacity);
    size_t available = ds.buffer_capacity - offset;
    va_list ap;
    va_start(ap, format);
    int n = vsnprintf(ds.buffer + offset, available, format, ap);
    va_end(ap);
    if (n < 0)
        bad_io("vsnprintf");
    if ((size_t) n >= available) {
        ensure_buffer(offset, (size_t) n + 1);
        va_start(ap, format);
        (void) vsnprintf(ds.buffer + offset, n + 1, format, ap);
        va_end(ap);
    }
    return offset + (size_t) n;
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
    assert(offset <= ds.buffer_capacity);
    assert(length <= ds.buffer_capacity - offset);

    while (length != 0) {
        wait_fd(fd, WAIT_WRITE);
        ssize_t n = write(fd, ds.buffer + offset, length);
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

size_t ask_tty_password(int tty_fd, size_t offset)
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

    ensure_buffer(offset, MAX_PASSWORD_LENGTH + 1);

    size_t n = 0;
    for (;;) {
        wait_fd(tty_fd, WAIT_READ | DELAY_ERROR_EXIT);
        if (ds.forced_exit)
            break;
        ssize_t nread = read(tty_fd, ds.buffer + offset, MAX_PASSWORD_LENGTH + 1);
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

    if (ds.buffer[offset + n - 1] == '\n') {
        --n;

        // Write \n that was not echoed
        poll_write_all(tty_fd, offset + n, 1);
    } else if (n == MAX_PASSWORD_LENGTH + 1) {
        fail("Too long password, its length must not exceed %zu bytes",
            MAX_PASSWORD_LENGTH);
    }

    ds.buffer[offset + n] = '\0';
    return n;
}

void ask_password()
{
    size_t password_length;
    int tty_fd = open_tty();
    if (tty_fd >= 0) {
        size_t n = sprintf_buffer(0, "Enter %s password: ", get_password_title());
        poll_write_all(tty_fd, 0, n);
        password_length = ask_tty_password(tty_fd, 0);

    } else {
        fail("%s without tty is not implemented", ds.command_name);
    }

    char stored_hash[BCRYPT_HASHSIZE];
    int fd = open(ds.hash_path, O_RDONLY);
    if (fd < 0) {
        bad_io("open path=%s", ds.hash_path);
    }

    size_t hash_length;
    for (size_t cursor = 0;;) {
        ssize_t n = read(fd, stored_hash + cursor, BCRYPT_HASHSIZE - cursor);
        if (n < 0) {
            if (errno == EINTR)
                continue;
            bad_io("read path=%s", ds.hash_path);
        }
        if (n == 0) {
            stored_hash[cursor] = '\0';
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

    char computed_hash[BCRYPT_HASHSIZE];

    const char *crypt_status = crypt_rn(
        ds.buffer, stored_hash, computed_hash, BCRYPT_HASHSIZE
    );
    if (!crypt_status) {
        fail("error when computing password hash using data from %s", ds.hash_path);
    }
    if (hash_length != strlen(computed_hash)) {
        info("\n'%s'\n'%s'", stored_hash, computed_hash);
        fail(
            "computed password length mismatch, expected=%zu actual=%zu",
            hash_length, strlen(computed_hash)
        );
    }

    // Use context-independent compare to avoid timing attacks.
    unsigned diff = 0;
    for (size_t i = 0; i != hash_length; ++i) {
        diff |= computed_hash[i] ^ stored_hash[i];
    }
    assert(tty_fd);
    if (diff) {
        size_t n = sprintf_buffer(0, "Password mismatch\n");
        poll_write_all(tty_fd, 0, n);
        cleanup_and_exit(USER_CANCEL_EXIT);
    }
    poll_write_all(1, 0, password_length);

    status = close(tty_fd);
    if (status != 0)
        bad_io("close fd=%d", tty_fd);
}

void set_password_hash()
{
    int tty_fd = open_tty();
    if (tty_fd >= 0) {
        size_t n = sprintf_buffer(
            0, "Enter a new password for %s: ", get_password_title()
        );
        poll_write_all(tty_fd, 0, n);
        size_t password_length = ask_tty_password(tty_fd, 0);
        if (!password_length) {
            fail("password cannot be empty");
        }

        // Skip \0 in the buffer
        size_t offset = password_length + 1;
        n = sprintf_buffer(
            offset, "Repeat to confirm the new value for %s: ", get_password_title()
        );
        poll_write_all(tty_fd, offset, n);
        size_t password_length2 = ask_tty_password(tty_fd, offset);
        if (password_length2 == 0) {
            // Write on tty as this is a GUI message for the user.
            n = sprintf_buffer(0, "Canceled\n");
            poll_write_all(tty_fd, 0, n);
            cleanup_and_exit(USER_CANCEL_EXIT);
        }
        if (password_length2 != password_length
            || memcmp(ds.buffer, ds.buffer + offset, password_length) != 0) {
            n = sprintf_buffer(0, "Password mismatch\n");
            poll_write_all(tty_fd, 0, n);
            cleanup_and_exit(1);
        }

        int status = close(tty_fd);
        if (status != 0)
            bad_io("close fd=%d", tty_fd);

    } else {
        fail("%s without tty is not implemented", ds.command_name);
    }

    enum {
        RANDOM_BYTE_COUNT = 16,
    };
    char random_input[RANDOM_BYTE_COUNT];
    for (size_t cursor = 0; cursor != RANDOM_BYTE_COUNT;) {
        int n = getrandom(random_input + cursor, RANDOM_BYTE_COUNT - cursor, 0);
        if (n < 0) {
            if (errno == EAGAIN)
                continue;
            bad_io("getrandom");
        }
        cursor += n;
    }

    char salt[BCRYPT_HASHSIZE];
    const char *crypt_status = crypt_gensalt_rn(
        "$2y$", BCRYPT_PASSWORD_COST, random_input, sizeof random_input,
        salt, BCRYPT_HASHSIZE);
    if (!crypt_status) {
        bad_io("crypt_gensalt_rn");
    }
    char computed_hash[BCRYPT_HASHSIZE];
    crypt_status = crypt_rn(ds.buffer, salt, computed_hash, BCRYPT_HASHSIZE);
    if (!crypt_status) {
        bad_io("crypt_rn");
    }
    size_t hash_length = strlen(computed_hash);

    int fd = open(ds.hash_path, O_WRONLY | O_CREAT, S_IRUSR | S_IWUSR);
    if (fd < 0) {
        bad_io("open path=%s", ds.hash_path);
    }
    for (size_t cursor = 0; cursor != hash_length;) {
        ssize_t n = write(fd, computed_hash + cursor, hash_length - cursor);
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
