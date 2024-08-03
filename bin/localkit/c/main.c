#define _GNU_SOURCE
#define _GNU_SOURCE

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
#include <sys/types.h>
#include <sys/wait.h>
#include <termios.h>
#include <unistd.h>

#include "vendor/crypt_blowfish/ow-crypt.h"

enum {
    USER_CANCEL_EXIT = 2,

    BCRYPT_HASHSIZE = 64,
    BCRYPT_PASSWORD_COST = 11,

    MAX_PASSWORD_LENGTH = 80,
};

typedef enum {
    CMD_HELP = 1,
    CMD_ASK_PASSWORD,
    CMD_SET_PASSWORD_HASH,
} command_id_t;

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

struct ask_password {
    str_loc_t stored_hash;
};

struct {
    const char *arg0;
    const char *command_name;
    const char *hash_path;
    const char *gui_title;
    const char *output_file;

    command_id_t command_id;

    int forced_exit;

    int signal_read_fd;
    int signal_write_fd;

    int wait_read_fd;
    int wait_write_fd;
    pid_t wait_pid;
    int wait_pid_status;
    bool wait_quit_signal;

    union {
        struct ask_password ask_password;
    };

} ds = {
    .signal_read_fd = -1,
    .signal_write_fd = -1,
    .wait_read_fd = -1,
    .wait_write_fd = -1,
    .wait_pid = -1,
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

void wait_event()
{
    struct pollfd p[3];
    p[0].fd = ds.signal_read_fd;
    p[0].events = POLLIN;
    size_t npolls = 1;
    int poll_read_index = -1;
    int poll_write_index = -1;
    if (ds.wait_read_fd >= 0) {
        poll_read_index = (int) npolls;
        p[npolls].fd = ds.wait_read_fd;
        p[npolls].events = POLLIN;
        ++npolls;
    }
    if (ds.wait_write_fd >= 0) {
        poll_write_index = (int) npolls;
        p[npolls].fd = ds.wait_write_fd;
        p[npolls].events = POLLOUT;
        npolls++;
    }
    assert(npolls <= sizeof(p) / sizeof(p[0]));
    assert(npolls >= 2 || ds.wait_pid >= 0);

    bool got_event = false;
    do {
        int n = poll(p, npolls, -1);
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
        for (size_t i = 0; i != npolls; ++i) {
            if (p[i].revents & (POLLERR | POLLNVAL)) {
                log_bad_io("poll fd=%d revents=%d", p[i].fd, p[i].revents);
                break;
            }
        }
        if (p[0].revents & (POLLIN | POLLHUP)) {
            bool do_waitpid = false;
            for (;;) {
                uint8_t signal_number;
                ssize_t nread = read(ds.signal_read_fd, &signal_number, 1);
                if (nread < 0) {
                    if (errno == EAGAIN || errno == EWOULDBLOCK) {
                        // We emptied the signal pipe
                        break;
                    }
                    log_bad_io("read fd=%d size=1", ds.signal_read_fd);
                    goto after_wait;
                }
                if (nread == 0) {
                    log_bad_io("unexpected EOF from signal pipe  fd=%d", ds.signal_read_fd);
                    goto after_wait;
                }

                if (signal_number == SIGCHLD) {
                    do_waitpid = true;
                } else {
                    assert(
                        signal_number == SIGINT
                        || signal_number == SIGQUIT
                        || signal_number == SIGTERM
                    );
                    if (ds.wait_quit_signal) {
                        ds.wait_quit_signal = false;
                        got_event = true;
                    } else if (!ds.forced_exit) {
                        info("quitting on signal");
                        ds.forced_exit = USER_CANCEL_EXIT;
                        goto after_wait;
                    }
                }
            }
            if (do_waitpid) {
                for (;;) {
                    int status;
                    pid_t pid = waitpid(-1, &status, WNOHANG);
                    if (pid < 0) {
                        if (errno == ECHILD)
                            break;
                        bad_io("waitpid pid=-1");
                    }
                    if (pid == 0)
                        break;
                    if (pid == ds.wait_pid) {
                        ds.wait_pid_status = status;
                        ds.wait_pid = -1;
                        got_event = true;
                    }
                }
            }
        }
        if (poll_read_index >= 0 && p[poll_read_index].revents & (POLLIN | POLLHUP)) {
            ds.wait_read_fd = -1;
            got_event = true;
        }
        if (poll_write_index >= 0 && p[poll_write_index].revents & (POLLOUT | POLLHUP)) {
            ds.wait_write_fd = -1;
            got_event = true;
        }
    } while (!got_event);

after_wait:
    if (ds.forced_exit) {
        if (ds.wait_quit_signal) {
            ds.wait_quit_signal = false;
        } else {
            check_forced_exit();
        }
    }
}

void poll_write_all(int fd, size_t offset, size_t length)
{
    assert(offset <= g_buffer_capacity);
    assert(length <= g_buffer_capacity - offset);

    while (length != 0) {
        assert(ds.wait_write_fd < 0);
        ds.wait_write_fd = fd;
        do {
            wait_event();
        } while (ds.wait_write_fd >= 0);
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

static struct {
    int tty_fd;
    str_loc_t gui_prog;
} user_prompt = {
    .tty_fd = -1,
};

void open_user_prompt()
{
    assert(user_prompt.tty_fd < 0);
    assert(user_prompt.gui_prog.len == 0);
    assert(user_prompt.gui_prog.pos == 0);

    // Follow ssh-add and try to open tty only if stdin is tty.
    do {
        if (!isatty(0))
            break;
        int fd = open("/dev/tty", O_RDWR | O_NONBLOCK | O_CLOEXEC);
        if (fd < 0) {
            if (errno == ENXIO || errno == ENOENT)
                break;
            bad_io("open path=/dev/tty");
        }
        if (isatty(fd)) {
            user_prompt.tty_fd = fd;
            return;
        }
        int status = close(fd);
        if (status != 0)
            bad_io("close fd=%d", fd);
    } while (false);

    const char *env = "SSH_ASKPASS";
    char *ask = getenv(env);
    if (!ask || !*ask) {
        fail("failed to find program to ask for password - %s is unset or empty", env);
    }
    user_prompt.gui_prog = sprintf_buffer("%s", ask);
}

void close_user_prompt()
{
    if (user_prompt.tty_fd >= 0) {
        int status = close(user_prompt.tty_fd);
        if (status != 0)
            bad_io("close fd=%d", user_prompt.tty_fd);
        user_prompt.tty_fd = -1;
    }
    memset(&user_prompt.gui_prog, 0, sizeof user_prompt.gui_prog);
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

str_loc_t prompt_tty_password(int tty_fd, str_loc_t title, bool *canceled)
{
    *canceled = false;

    str_loc_t prompt = sprintf_buffer("%s: ", bufptr(title.pos));
    poll_write_all(tty_fd, prompt.pos, prompt.len);

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

    // One character for detecting too long entered password and 1 for extra \0
    const size_t maxread = MAX_PASSWORD_LENGTH + 1;
    str_loc_t password = { alloc_buffer(maxread + 1), 0 };

    while (!*canceled) {
        assert(ds.wait_read_fd < 0);
        ds.wait_read_fd = tty_fd;
        ds.wait_quit_signal = true;
        do {
            wait_event();
        } while (ds.wait_read_fd >= 0 && ds.wait_quit_signal);
        if (!ds.wait_quit_signal) {
            *canceled = true;
        }
        if (ds.wait_read_fd < 0) {
            ssize_t nread = read(tty_fd, bufptr(password.pos), maxread);
            if (nread < 0) {
                if (errno == EINTR || errno == EAGAIN || errno == EWOULDBLOCK)
                    continue;
                log_bad_io("read");
                break;
            }
            if (nread != 0) {
                password.len = (size_t) nread;
                break;
            }
        }
    }
    tc_status = tcsetattr(tty_fd, TCSAFLUSH, &saved_tio);
    if (tc_status != 0)
        bad_io("tcsetattr");
    check_forced_exit();

    if (*canceled) {
        // First \n closes the prompt line
        str_loc_t msg = sprintf_buffer("\nCanceled\n");
        poll_write_all(tty_fd, msg.pos, msg.len);
        password.len = 0;
    } else if (password.len > 0 && bufchar(password.pos + password.len - 1) == '\n') {
        --password.len;

        // Write \n that was not echoed
        poll_write_all(tty_fd, password.pos + password.len, 1);
    }
    set_bufchar(password.pos + password.len, '\0');
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

str_loc_t prompt_gui_password(str_loc_t gui_prog, str_loc_t message, bool *canceled)
{
    int password_pipe_fds[2];
    int status = pipe2(password_pipe_fds, O_CLOEXEC);
    if (status != 0)
        bad_io("pipe");
    {
        pid_t child = fork();
        if (child < 0)
            bad_io("pipe");
        if (child == 0) {
            int fd = dup2(password_pipe_fds[1], 1);
            if (fd < 0)
                bad_io("dup2 f1=%d fd2=1", password_pipe_fds[1]);
            (void) execlp(
                bufptr(gui_prog.pos), bufptr(gui_prog.pos), bufptr(message.pos), NULL
            );
            fail("failed to execute %s - %s", bufptr(gui_prog.pos), strerror(errno));
        }
        assert(ds.wait_pid < 0);
        ds.wait_pid = child;
    }
    int read_fd = password_pipe_fds[0];
    status = close(password_pipe_fds[1]);
    if (status != 0)
        bad_io("close %d", password_pipe_fds[1]);

    // One character for an optional extra \n returned by the program
    // and one for detecting too long entered password.
    enum {
        max_read = MAX_PASSWORD_LENGTH + 2
    };
    str_loc_t password = { alloc_buffer(max_read), 0 };
    for (;;) {
        assert(ds.wait_read_fd < 0);
        ds.wait_read_fd = read_fd;
        do {
            wait_event(0);
        } while (ds.wait_read_fd >= 0);
        ssize_t nread = read(
            read_fd, bufptr(password.pos + password.len), max_read - password.len
        );
        if (nread < 0) {
            if (errno == EINTR || errno == EAGAIN || EWOULDBLOCK)
                continue;
            log_bad_io("read");
            break;
        }
        if (nread == 0) {
            break;
        }
        password.len += (size_t) nread;
        if (password.len == max_read) {
            break;
        }
    }
    if (password.len > 0 && bufchar(password.pos + password.len - 1) == '\n') {
        password.len--;
    }
    if (password.len == max_read) {
        static_assert(max_read == MAX_PASSWORD_LENGTH + 2, "");
        password.len--;
    }
    status = close(read_fd);
    if (status != 0)
        bad_io("close %d", read_fd);
    while (ds.wait_pid >= 0) {
        wait_event(0);
    }
    if (!WIFEXITED(ds.wait_pid_status)) {
        fail(
            "%s terminated abnormally, status=%d",
            bufptr(gui_prog.pos), ds.wait_pid_status
        );
    }
    *canceled = false;
    if (WEXITSTATUS(ds.wait_pid_status) != 0) {
        *canceled = true;
        password.len = 0;
    }
    set_bufchar(password.pos + password.len, '\0');
    return password;
}

str_loc_t prompt_user_password(str_loc_t title, bool *canceled)
{
    if (user_prompt.tty_fd >= 0) {
        return prompt_tty_password(user_prompt.tty_fd, title, canceled);
    } else {
        return prompt_gui_password(user_prompt.gui_prog, title, canceled);
    }
}

void prompt_user_message(str_loc_t message)
{
    if (user_prompt.tty_fd >= 0) {
        message = sprintf_buffer("%s\n", bufptr(message.pos));
        poll_write_all(user_prompt.tty_fd, message.pos, message.len);
    } else {
        bool canceled;
        (void) prompt_gui_password(user_prompt.gui_prog, message, &canceled);
    }
}

void ask_password_cmd()
{
    struct ask_password *s = &ds.ask_password;

    if (s->stored_hash.len == 0) {
        // Read the hash first not to bother the user with a password
        // question in case of errors.
        s->stored_hash.pos = alloc_buffer(BCRYPT_HASHSIZE);
        int fd = open(ds.hash_path, O_RDONLY);
        if (fd < 0) {
            fail(
                "to open the password hash file %s - %s",
                ds.hash_path, strerror(errno)
            );
        }
        for (;;) {
            ssize_t n = read(
                fd,
                bufptr(s->stored_hash.pos + s->stored_hash.len),
                BCRYPT_HASHSIZE - s->stored_hash.len
            );
            if (n < 0) {
                if (errno == EINTR)
                    continue;
                bad_io("read path=%s", ds.hash_path);
            }
            if (n == 0) {
                set_bufchar(s->stored_hash.pos + s->stored_hash.len, '\0');
                break;
            }
            s->stored_hash.len += (size_t) n;
            if (s->stored_hash.len == BCRYPT_HASHSIZE) {
                fail("size of %s exceeds max supported length %d",
                     ds.hash_path, BCRYPT_HASHSIZE);
            }
        }
        int status = close(fd);
        if (status != 0) {
            bad_io("close fd=%d", fd);
        }
    }

    open_user_prompt();

    str_loc_t password;
    bool canceled = false;
    str_loc_t message = {0, 0};
    str_loc_t title = sprintf_buffer("Enter password for %s", get_password_title());
    for (;;) {
        password = prompt_user_password(title, &canceled);
        if (canceled)
            break;

        if (password.len > MAX_PASSWORD_LENGTH) {
            message = sprintf_buffer(
                "Password length cannot exceed %d", MAX_PASSWORD_LENGTH
            );
            break;
        }

        size_t computed_hash = alloc_buffer(BCRYPT_HASHSIZE);

        const char *crypt_status = crypt_rn(
            bufptr(password.pos), bufptr(s->stored_hash.pos),
            bufptr(computed_hash), BCRYPT_HASHSIZE
        );
        if (!crypt_status) {
            fail("error when computing password hash using data from %s", ds.hash_path);
        }
        if (s->stored_hash.len != strlen(bufptr(computed_hash))) {
            info("\n'%s'\n'%s'", bufptr(s->stored_hash.pos), bufptr(computed_hash));
            fail(
                "computed password length mismatch, expected=%zu actual=%zu",
                s->stored_hash.len, strlen(bufptr(computed_hash))
            );
        }

        // Use context-independent compare to avoid timing attacks.
        unsigned diff = 0;
        for (size_t i = 0; i != s->stored_hash.len; ++i) {
            diff |= bufchar(computed_hash + i) ^ bufchar(s->stored_hash.pos + i);
        }
        if (!diff)
            break;
        title = sprintf_buffer(
            "Password mismatch, enter password for %s again",
            get_password_title()
        );
    }

    if (canceled) {
        cleanup_and_exit(USER_CANCEL_EXIT);
    }
    if (message.len) {
        prompt_user_message(message);
        cleanup_and_exit(1);
    }
    
    int output_fd = 1;
    if (ds.output_file) {
		output_fd = open(ds.output_file, O_WRONLY | O_CLOEXEC);
		if (output_fd < 0) {
			bad_io("to open %s for writing", ds.output_file);	
		}
	}

    poll_write_all(output_fd, password.pos, password.len);

    if (ds.output_file) {
		if (close(output_fd) < 0) {
			bad_io("to close the descriptor for %s", ds.output_file);
		}
	}   

    close_user_prompt();
}

void set_password_hash()
{
    open_user_prompt();

    str_loc_t password;
    bool canceled = false;
    str_loc_t message = {0, 0};
    do {
        str_loc_t title = sprintf_buffer(
            "Enter a new password for %s", get_password_title()
        );
        password = prompt_user_password(title, &canceled);
        if (canceled)
            break;

        if (!password.len) {
            message = sprintf_buffer("Password cannot be empty");
            break;
        }
        if (password.len > MAX_PASSWORD_LENGTH) {
            message = sprintf_buffer(
                "Password length cannot exceed %d", MAX_PASSWORD_LENGTH
            );
            break;
        }
        title = sprintf_buffer(
            "Repeat to confirm the new value for %s", get_password_title()
        );

        str_loc_t password2 = prompt_user_password(title, &canceled);
        if (canceled)
            break;

        if (password2.len != password.len
            || memcmp(bufptr(password2.pos), bufptr(password.pos), password.len) != 0) {
            message = sprintf_buffer("Password mismatch");
            break;
        }
    } while (false);

    if (canceled) {
        cleanup_and_exit(USER_CANCEL_EXIT);
    }
    if (message.len) {
        prompt_user_message(message);
        cleanup_and_exit(1);
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

    close_user_prompt();
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
"  ask-password [-t TITLE] [-o OUTPUT] -p HASH_PATH\n"
"    ask for a password and print it on stdout (or OUTPUT file if given) if\n" "    its hash matches the value in HASH_PATH. The GUI uses TITLE when asking\n"
"    for a password. The default value for TITLE is the name of HASH_PATH.\n"
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

static void quit_signal_handler(int signal)
{
    uint8_t kind = (uint8_t) signal;

    // TODO  handle write errors somehow
    // In particular, deal with full signal pipe
    (void) write(ds.signal_write_fd, &kind, 1);
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
    static const command_id_t command_ids[] = {
        CMD_ASK_PASSWORD,
        CMD_SET_PASSWORD_HASH,
        CMD_HELP,
        CMD_HELP,
        CMD_HELP,
    };

    ds.command_name = argv[1];
    int index = 0;
    for (const char *s = command_names; ; ++index) {
        if (strcmp(ds.command_name, s) == 0)
            break;
        s = strchr(s, '\0') + 1;
        if (!*s) {
            usage_error("unknown command %s", ds.command_name);
        }
    }

    ds.command_id = command_ids[index];
    if (ds.command_id == CMD_HELP) {
        show_usage();
    }

    int command_argc = argc - 1;
    char **command_argv = argv + 1;
    int opt;
    opterr = 0;
    while ((opt = getopt(command_argc, command_argv, "+:ho:p:t:")) != -1) {
        switch (opt) {
        case 'o':
			if (ds.command_id != CMD_ASK_PASSWORD) {
				usage_error(
					"option -%c can only be used with ask-password command", 
					(char) opt);
			}
            ds.output_file = optarg;
            break;
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
    int status = pipe2(signal_pipe_fds, O_CLOEXEC | O_NONBLOCK);
    if (status != 0)
        bad_io("pipe");
    ds.signal_read_fd = signal_pipe_fds[0];
    ds.signal_write_fd = signal_pipe_fds[1];

    static const int signal_set[] = { SIGINT, SIGQUIT, SIGTERM, SIGCLD, 0 };
    struct sigaction sa;
    sa.sa_handler = quit_signal_handler;
    sigemptyset(&sa.sa_mask);
    sa.sa_flags = 0;
    for (const int *s = signal_set; *s; ++s) {
        int status = sigaction(*s, &sa, NULL);
        if (status != 0)
            bad_io("sigaction signal=%d", *s);
    }

    switch (ds.command_id) {
        case CMD_ASK_PASSWORD:
            ask_password_cmd();
            break;
        case CMD_SET_PASSWORD_HASH:
            set_password_hash();
            break;
        default:
            assert(0);
    }
}
