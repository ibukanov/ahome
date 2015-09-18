/**
 **
 **  THIS SOFTWARE IS NOT COPYRIGHTED
 **
 **  This source code is offered for use in the public domain. You may
 **  use, modify or distribute it freely.
 **
 **  This code is distributed in the hope that it will be useful but
 **  WITHOUT ANY WARRANTY. ALL WARRANTIES, EXPRESS OR IMPLIED ARE HEREBY
 **  DISCLAMED. This includes but is not limited to warranties of
 **  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 **
 **  Author: Igor Bukanov, igor@bukanov.net (Игорь Буканов)
 */

#include <stdlib.h>
#include <string.h>
#include <stdio.h>
#include <assert.h>

/* ISO-10646 encoding for russian letters */

#define RUS_A            0x410
#define RUS_B            0x411
#define RUS_V            0x412
#define RUS_G            0x413
#define RUS_D            0x414
#define RUS_E            0x415
#define RUS_JO           0x401
#define RUS_ZH           0x416
#define RUS_Z            0x417
#define RUS_I            0x418
#define RUS_J            0x419
#define RUS_K            0x41A
#define RUS_L            0x41B
#define RUS_M            0x41C
#define RUS_N            0x41D
#define RUS_O            0x41E
#define RUS_P            0x41F
#define RUS_R            0x420
#define RUS_S            0x421
#define RUS_T            0x422
#define RUS_U            0x423
#define RUS_F            0x424
#define RUS_KH           0x425
#define RUS_C            0x426
#define RUS_CH           0x427
#define RUS_SH           0x428
#define RUS_W            0x429   /*SHCH */
#define RUS_HARD_SIGN    0x42A   /* J' */
#define RUS_Y            0x42B
#define RUS_SOFT_SIGN    0x42C
#define RUS_EH           0x42D
#define RUS_JU           0x42E
#define RUS_Q            0x42F   /* JA */

#define RUS_a            0x430
#define RUS_b            0x431
#define RUS_v            0x432
#define RUS_g            0x433
#define RUS_d            0x434
#define RUS_e            0x435
#define RUS_jo           0x451
#define RUS_zh           0x436
#define RUS_z            0x437
#define RUS_i            0x438
#define RUS_j            0x439
#define RUS_k            0x43a
#define RUS_l            0x43b
#define RUS_m            0x43c
#define RUS_n            0x43d
#define RUS_o            0x43e
#define RUS_p            0x43f
#define RUS_r            0x440
#define RUS_s            0x441
#define RUS_t            0x442
#define RUS_u            0x443
#define RUS_f            0x444
#define RUS_kh           0x445
#define RUS_c            0x446
#define RUS_ch           0x447
#define RUS_sh           0x448
#define RUS_w            0x449   /*shch */
#define RUS_hard_sign    0x44a   /* j' */
#define RUS_y            0x44b
#define RUS_soft_sign    0x44c   /* ' */
#define RUS_eh           0x44d
#define RUS_ju           0x44e
#define RUS_q            0x44f   /* ja */

#define is_russian(c) \
    ((RUS_A <= (c) && (c) <= RUS_q) || (c) == RUS_JO || (c) == RUS_jo)

#define WITH_H_SUFFIX  -2
#define WITH_J_PREFIX  -3
static int translate_char(int c);
static int translate_char_and_h_suffix(int c);
static int translate_char_without_h_suffix(int c);
static int translate_j_and_char(int prev_c, int c);

#define UTF8_ENCODING      1
#define ISO8859_5_ENCODING 2
#define KOI8_ENCODING      3
#define WIN1251_ENCODING   4

static int next_char(FILE* input_stream);
static void send_char(int c, FILE* output_stream, int output_encoding);

static int rus_char_to_koi8_byte(int c);
static int rus_char_to_win1251_byte(int c);

#define AS_IS_SWITCH ';'
static void do_translation
    (FILE* input_stream, int with_latin_escapes,
     FILE* output_stream, int output_encoding);

static int translate_char(int c)
{
    assert(c >= 0);
    switch (c) {
        case '\'': return RUS_soft_sign;
        case '`':  return RUS_hard_sign;

        case 'J': case 'j':
            return WITH_J_PREFIX;

        case 'C': case 'c':
        case 'S': case 's':
        case 'E': case 'e':
        case 'K': case 'k':
        case 'Z': case 'z':
            return WITH_H_SUFFIX;

        case 'A': return RUS_A;
        case 'B': return RUS_B;
        case 'V': return RUS_V;
        case 'G': return RUS_G;
        case 'D': return RUS_D;
        case 'I': return RUS_I;
        case 'L': return RUS_L;
        case 'M': return RUS_M;
        case 'N': return RUS_N;
        case 'O': return RUS_O;
        case 'P': return RUS_P;
        case 'R': return RUS_R;
        case 'T': return RUS_T;
        case 'U': return RUS_U;
        case 'F': return RUS_F;
        case 'H': return RUS_KH; /* unprefixed H is the same as KH*/
        case 'X': return RUS_KH; /* X is the same as KH*/
        case 'W': return RUS_W;
        case 'Y': return RUS_Y;
        case 'Q': return RUS_Q;

        case 'a': return RUS_a;
        case 'b': return RUS_b;
        case 'v': return RUS_v;
        case 'g': return RUS_g;
        case 'd': return RUS_d;
        case 'i': return RUS_i;
        case 'l': return RUS_l;
        case 'm': return RUS_m;
        case 'n': return RUS_n;
        case 'o': return RUS_o;
        case 'p': return RUS_p;
        case 'r': return RUS_r;
        case 't': return RUS_t;
        case 'u': return RUS_u;
        case 'f': return RUS_f;
        case 'h': return RUS_kh; /* unprefixed h is the same as kh*/
        case 'x': return RUS_kh; /* X is the same as KH*/
        case 'w': return RUS_w;
        case 'y': return RUS_y;
        case 'q': return RUS_q;
    }
    return c;
}

static int translate_char_and_h_suffix(int c)
{
    assert(translate_char(c) == WITH_H_SUFFIX);
    switch (c) {
        case 'C': return RUS_CH;
        case 'S': return RUS_SH;
        case 'E': return RUS_EH;
        case 'K': return RUS_KH;
        case 'Z': return RUS_ZH;

        case 'c': return RUS_ch;
        case 's': return RUS_sh;
        case 'e': return RUS_eh;
        case 'k': return RUS_kh;
        case 'z': return RUS_zh;
    }
    return c;
}

static int translate_char_without_h_suffix(int c)
{
    assert(translate_char(c) == WITH_H_SUFFIX);
    switch (c) {
        case 'C': return RUS_C;
        case 'S': return RUS_S;
        case 'E': return RUS_E;
        case 'K': return RUS_K;
        case 'Z': return RUS_Z;

        case 'c': return RUS_c;
        case 's': return RUS_s;
        case 'e': return RUS_e;
        case 'k': return RUS_k;
        case 'z': return RUS_z;
    }
    return c;
}

static int translate_j_and_char(int prev_c, int c)
{
    assert(prev_c == 'j' || prev_c == 'J');
    switch (c) {
        case 'A': return RUS_Q;
        case 'O': return RUS_JO;
        case 'U': return RUS_JU;
        case 'H': return RUS_J; /* To permit to write JOG via JHOG*/

        case 'a': return (prev_c == 'J') ? RUS_Q : RUS_q;
        case 'o': return (prev_c == 'J') ? RUS_JO : RUS_jo;
        case 'u': return (prev_c == 'J') ? RUS_JU : RUS_ju;
        case 'h': return (prev_c == 'J') ? RUS_J : RUS_j;

    /* j', j` means capital ' or ` -- not the best option, but it works */
        case '\'': return RUS_SOFT_SIGN; 
        case '`': return RUS_HARD_SIGN; 
    }
    return -1;
}

static void do_translation
    (FILE* input_stream, int with_latin_escapes, 
     FILE* output_stream, int output_encoding)
{

#define S(x) send_char((x), output_stream, output_encoding)

    const int NORMAL = 0;
    const int AS_IS = 1;
    const int AFTER_SWITCH_CHAR = 2;
    const int AFTER_2_SWITCH_CHARS = 3;

    const int AFTER_J_PREFIX = 1;
    const int BEFORE_H_SUFFIX = 2;

    int state = NORMAL, substate = NORMAL;
    int c, prev_c = 0;

    c = next_char(input_stream);
    for (;;) {
        if (c < 0) { break; }
        if (state == NORMAL) {
            if (with_latin_escapes && c == AS_IS_SWITCH) {
                if (substate == BEFORE_H_SUFFIX) {
                    S(translate_char_without_h_suffix(prev_c));
                }
                else if (substate == AFTER_J_PREFIX) {
                    S(prev_c == 'J' ? RUS_J : RUS_j);
                }
                substate = NORMAL;
                state = AFTER_SWITCH_CHAR;
            }
            else {
                int translated = -1;
                if (substate == AFTER_J_PREFIX) {
                    translated = translate_j_and_char(prev_c, c);
                    if (translated < 0) {
                        S(prev_c == 'J' ? RUS_J : RUS_j);
                    }
                }
                else if (substate == BEFORE_H_SUFFIX) {
                    if (c == 'H' || c == 'h') {
                        translated = translate_char_and_h_suffix(prev_c);
                    }
                    else {
                        S(translate_char_without_h_suffix(prev_c));
                    }
                }
                substate = NORMAL;
                if (translated < 0) {
                    translated = translate_char(c);
                    if (translated < 0) {
                        if (translated == WITH_H_SUFFIX) {
                            substate = BEFORE_H_SUFFIX;
                        }
                        else if (translated == WITH_J_PREFIX) {
                            substate = AFTER_J_PREFIX; 
                        }
                    }
                }
                if (translated >= 0) {
                    S(translated);
                }
            }
        }
        else if (state == AS_IS) {
            if (c == AS_IS_SWITCH) {
                substate = AS_IS;
                state = AFTER_SWITCH_CHAR;
            }
            else {
                S(c);
            }
        }
        else if (state == AFTER_SWITCH_CHAR) {
            if (c == AS_IS_SWITCH) {
                state = AFTER_2_SWITCH_CHARS;
            }
            else {
                S(AS_IS_SWITCH); state = substate; continue;
            }
        }
        else if (state == AFTER_2_SWITCH_CHARS) {
            if (c != AS_IS_SWITCH) {
                state = (substate == NORMAL) ? AS_IS : NORMAL;
                substate = NORMAL; continue;
            }
            else {
                S(AS_IS_SWITCH); S(AS_IS_SWITCH); state = substate; 
            }
        }
    
        prev_c = c;
        c = next_char(input_stream);
    }

    if (state == NORMAL) {
        if (substate == BEFORE_H_SUFFIX) {
            S(translate_char_without_h_suffix(prev_c));
        }
        else if (substate == AFTER_J_PREFIX) {
            S(prev_c == 'J' ? RUS_J : RUS_j);
        }
    }
    else if (state == AFTER_SWITCH_CHAR) {
        S(AS_IS_SWITCH);
    }
#undef S
}

static int next_char(FILE* input_stream) 
{
    return getc(input_stream);
}

static void send_char(int c, FILE* output_stream, int output_encoding) 
{
    assert(c >= 0);
    if (is_russian(c)) {
        switch (output_encoding) {
            case UTF8_ENCODING:
            /* In UTF8 Russian letters are 2 bytes */
                putc(0xC0 | (c >> 6), output_stream);
                c = 0x80 | (0x3F & c);
                break;
            case ISO8859_5_ENCODING:
                c -= 0x400 - 0xa0;
                break;
            case KOI8_ENCODING:
                c = rus_char_to_koi8_byte(c);
                break;
            case WIN1251_ENCODING:
                c = rus_char_to_win1251_byte(c);
                break;
        }
    }
    putc(0xFF & c, output_stream);
}

static int rus_char_to_koi8_byte(int c)
{
    assert(is_russian(c));
    switch (c) {
        case RUS_A:              return 0xE1;
        case RUS_B:              return 0xE2;
        case RUS_V:              return 0xF7;
        case RUS_G:              return 0xE7;
        case RUS_D:              return 0xE4;
        case RUS_E:              return 0xE5;
        case RUS_JO:             return 0xB3;
        case RUS_ZH:             return 0xF6;
        case RUS_Z:              return 0xFA;
        case RUS_I:              return 0xE9;
        case RUS_J:              return 0xEA;
        case RUS_K:              return 0xEB;
        case RUS_L:              return 0xEC;
        case RUS_M:              return 0xED;
        case RUS_N:              return 0xEE;
        case RUS_O:              return 0xEF;
        case RUS_P:              return 0xF0;
        case RUS_R:              return 0xF2;
        case RUS_S:              return 0xF3;
        case RUS_T:              return 0xF4;
        case RUS_U:              return 0xF5;
        case RUS_F:              return 0xE6;
        case RUS_KH:             return 0xE8;
        case RUS_C:              return 0xE3;
        case RUS_CH:             return 0xFE;
        case RUS_SH:             return 0xFB;
        case RUS_W:              return 0xFD;
        case RUS_HARD_SIGN:      return 0xFF;
        case RUS_Y:              return 0xF9;
        case RUS_SOFT_SIGN:      return 0xF8;
        case RUS_EH:             return 0xFC;
        case RUS_JU:             return 0xE0;
        case RUS_Q:              return 0xF1;

        case RUS_a:              return 0xC1;
        case RUS_b:              return 0xC2;
        case RUS_v:              return 0xD7;
        case RUS_g:              return 0xC7;
        case RUS_d:              return 0xC4;
        case RUS_e:              return 0xC5;
        case RUS_jo:             return 0xA3;
        case RUS_zh:             return 0xD6;
        case RUS_z:              return 0xDA;
        case RUS_i:              return 0xC9;
        case RUS_j:              return 0xCA;
        case RUS_k:              return 0xCB;
        case RUS_l:              return 0xCC;
        case RUS_m:              return 0xCD;
        case RUS_n:              return 0xCE;
        case RUS_o:              return 0xCF;
        case RUS_p:              return 0xD0;
        case RUS_r:              return 0xD2;
        case RUS_s:              return 0xD3;
        case RUS_t:              return 0xD4;
        case RUS_u:              return 0xD5;
        case RUS_f:              return 0xC6;
        case RUS_kh:             return 0xC8;
        case RUS_c:              return 0xC3;
        case RUS_ch:             return 0xDE;
        case RUS_sh:             return 0xDB;
        case RUS_w:              return 0xDD;
        case RUS_hard_sign:      return 0xDF;
        case RUS_y:              return 0xD9;
        case RUS_soft_sign:      return 0xD8;
        case RUS_eh:             return 0xDC;
        case RUS_ju:             return 0xC0;
        case RUS_q:              return 0xD1;
    }
    return c;
}

static int rus_char_to_win1251_byte(int c)
{
    assert(0); /* MOT IMPLEMENTED */
    assert(is_russian(c));
    switch (c) {
        case RUS_A:              return 0;
        case RUS_B:              return 0;
        case RUS_V:              return 0;
        case RUS_G:              return 0;
        case RUS_D:              return 0;
        case RUS_E:              return 0;
        case RUS_JO:             return 0;
        case RUS_ZH:             return 0;
        case RUS_Z:              return 0;
        case RUS_I:              return 0;
        case RUS_J:              return 0;
        case RUS_K:              return 0;
        case RUS_L:              return 0;
        case RUS_M:              return 0;
        case RUS_N:              return 0;
        case RUS_O:              return 0;
        case RUS_P:              return 0;
        case RUS_R:              return 0;
        case RUS_S:              return 0;
        case RUS_T:              return 0;
        case RUS_U:              return 0;
        case RUS_F:              return 0;
        case RUS_KH:             return 0;
        case RUS_C:              return 0;
        case RUS_CH:             return 0;
        case RUS_SH:             return 0;
        case RUS_W:              return 0;
        case RUS_HARD_SIGN:      return 0;
        case RUS_Y:              return 0;
        case RUS_SOFT_SIGN:      return 0;
        case RUS_EH:             return 0;
        case RUS_JU:             return 0;
        case RUS_Q:              return 0;

        case RUS_a:              return 0;
        case RUS_b:              return 0;
        case RUS_v:              return 0;
        case RUS_g:              return 0;
        case RUS_d:              return 0;
        case RUS_e:              return 0;
        case RUS_jo:             return 0;
        case RUS_zh:             return 0;
        case RUS_z:              return 0;
        case RUS_i:              return 0;
        case RUS_j:              return 0;
        case RUS_k:              return 0;
        case RUS_l:              return 0;
        case RUS_m:              return 0;
        case RUS_n:              return 0;
        case RUS_o:              return 0;
        case RUS_p:              return 0;
        case RUS_r:              return 0;
        case RUS_s:              return 0;
        case RUS_t:              return 0;
        case RUS_u:              return 0;
        case RUS_f:              return 0;
        case RUS_kh:             return 0;
        case RUS_c:              return 0;
        case RUS_ch:             return 0;
        case RUS_sh:             return 0;
        case RUS_w:              return 0;
        case RUS_hard_sign:      return 0;
        case RUS_y:              return 0;
        case RUS_soft_sign:      return 0;
        case RUS_eh:             return 0;
        case RUS_ju:             return 0;
        case RUS_q:              return 0;
    }
    return c;
}

static void usage(const char* prog_name) 
{
    fprintf(stdout, 
"Usage: %s [OPTIONS]... [SOURCE [DESTINATION]]\n"
"Transliterate Russian text in SOURCE typed in Latin into Cyrillic letters\n"
"in DESTINATION. If SOURCE or DESTINATION are not given or -, assume stdin\n" "and stdout correspondingly.\n"
"\n",
prog_name);

    fprintf(stdout,
"Options to select output encoding of generated Cyrillic letters:\n"
"  -i             ISO8859-5\n"
"  -k             KOI8-R\n"
"  -u             UTF-8\n"
"  -w             Windows-1251\n"
"Default is KOI8-R unless string UTF-8 is present in the LC_ALL, LC_CTYPE,\n" "LANG or LESSCHARSET environroment variables, in which case it is UTF-8\n"
"\n");

    fprintf(stdout,
"Other options:\n"
"  -l, --with-latin-escapes  process %c%c escapes, see bellow\n"
"  -r, --no-latin-escapes    turn off processing of %c%c escapes (default)\n"
"  -h, --help     display this help and exit\n"
"  -v, --version  output version information and exit\n"
"\n",
AS_IS_SWITCH, AS_IS_SWITCH, AS_IS_SWITCH, AS_IS_SWITCH);

    fprintf(stdout,
"When processing of %c%c is on, print characters after %c%c as is without\n"
"transliteration until next %c%c. To have %c%c in the output, type %c%c%c.\n"
"\n"
"Reports bugs on suggestions to igor@bukanov.net\n"
"\n",
AS_IS_SWITCH, AS_IS_SWITCH, AS_IS_SWITCH, AS_IS_SWITCH,
AS_IS_SWITCH, AS_IS_SWITCH, AS_IS_SWITCH, AS_IS_SWITCH,
AS_IS_SWITCH, AS_IS_SWITCH, AS_IS_SWITCH);
}

static void version(const char* prog_name) 
{
    fprintf(stdout, "%s 0.9\n"
"Written by Igor Bukanov, igor@bukanov.net (Игорь Буканов)\n"
"\n"
"THIS SOFTWARE IS NOT COPYRIGHTED\n"
"\n",
prog_name
);
}

static int contains_utf_8(const char* str) {
    for (;;) {
    	unsigned c = *str;
	if (c == 0) { break; }
	++str;
	if (c == 'U' || c == 'u') {
	    c = *str;
	    if (c == 'T' || c == 't') {
	    	c = *++str;
	    	if (c == 'F' || c == 'f') {
	    	    if ('-' == *++str) {
	    	    	if ('8' == *++str) {
		    	    return 1;
			}
		    }
	    	}
	    } 
    	}
    }
    return 0;
}

static int default_is_utf8() {
/* Follow convention that if UTF-8 is present in the following environment
   variables then encoding etc. should default to utf-8 */
   
    static const char* const 
        ENV_LIST[] = { "LC_ALL", "LC_CTYPE", "LANG", "LESSCHARSET" };
    size_t i;
    for (i = 0; i != sizeof(ENV_LIST) / sizeof(ENV_LIST[0]); ++i) {
        const char* s = getenv(ENV_LIST[i]);
        if (s != NULL && contains_utf_8(s)) {
	    return 1;
        }
    }
    return 0;
}

int main(int argc, char** argv) 
{
    int status = EXIT_SUCCESS;

    const char* input_file_name = NULL;
    const char* output_file_name = NULL;
    int output_encoding = 0;
    int show_help = 0;
    int show_version = 0;
    int with_latin_escapes = 0;

    const char* unknown_option = NULL;
    int process_options = 1;

    char** argv_end = argv + argc;
    const char* prog_name = *argv;
    {
        const char* real_name = strrchr(prog_name, '/');
        if (real_name != NULL) { prog_name = real_name + 1; } 
        real_name = strrchr(prog_name, '\\');
        if (real_name != NULL) { prog_name = real_name + 1; } 
    }

    for (++argv; argv != argv_end; ++argv) {
        int option_arg = 0;
        char* arg = *argv;        
        if (*arg == '-') {
            int c = *++arg;
            if (c != 0 && process_options) {
            /* -something with arg at something */
                option_arg = 1;
                if (c == '-') {
                /* After -- */
                    ++arg;
                    if (*arg == 0) { 
                    /* stand along -- terminates option processing */
                        process_options = 0; 
                    }
                    else if (strcmp("help", arg) == 0) { 
                        show_help = 1; 
                    }
                    else if (strcmp("no-latin-escapes", arg) == 0) { 
                        with_latin_escapes = 0; 
                    }
                    else if (strcmp("with-latin-escapes", arg) == 0) { 
                        with_latin_escapes = 1; 
                    }
                    else if (strcmp("version", arg) == 0) { 
                        show_version = 1; 
                    }
                    else { unknown_option = *argv; }
                }
                else {
                    do {
                        switch (c) {
                            case 'h': 
                                show_help = 1; break;
                            case 'i': 
                                output_encoding = ISO8859_5_ENCODING; break;
                            case 'k': 
                                output_encoding = KOI8_ENCODING; break;
                            case 'l': 
                                with_latin_escapes = 1; break;
                            case 'r': 
                                with_latin_escapes = 0; break;
                            case 'u': 
                                output_encoding = UTF8_ENCODING; break;
                            case 'v': 
                                show_version = 1; break;
                            case 'w': 
                                output_encoding = WIN1251_ENCODING; break;
                            default: 
                                unknown_option = arg;
                        }
                        c = *++arg;
                    } while (c != 0);
                }
            }
        }
        if (!option_arg) {
            if (input_file_name == NULL) {
                input_file_name = *argv;
            }
            else if (output_file_name == NULL) {
                output_file_name = *argv;
            }
            else {
                break;
            }
        }
    }
    
    if (show_help) { usage(prog_name); }
    else if (show_version) { version(prog_name); }
    else if (unknown_option != NULL) {
        if (*unknown_option == '-') {
            fprintf(stderr, "%s: Error: unknown long option %s\n",
                    prog_name, unknown_option);
        }
        else {
            fprintf(stderr, "%s: Error: unknown one letter option in %s\n",
                    prog_name, unknown_option);
        }
        fprintf(stderr, "Try %s --help for usage\n", prog_name);
        status = EXIT_FAILURE;
    }
    else if (argv != argv_end) {
        fprintf(stderr, "%s: Error: Too many arguments\n", prog_name);
        fprintf(stderr, "Try %s --help for usage\n", prog_name);
        status = EXIT_FAILURE;
    }
    else {
        FILE* input_stream;

    	if (output_encoding == 0) {
    	    output_encoding = default_is_utf8() ? UTF8_ENCODING : KOI8_ENCODING;
	}

        if (input_file_name == NULL || strcmp(input_file_name, "-") == 0) {
            input_stream = stdin;
        }
        else {
            input_stream = fopen(input_file_name, "rb");
        }

        if (input_stream == NULL) {
            fprintf(stderr, "%s: Error: Can not open %s for reading\n", 
                    prog_name, input_file_name);
            status = EXIT_FAILURE;
        }
        else {
            FILE* output_stream;
            if (output_file_name==NULL || strcmp(output_file_name, "-")==0) {
                output_stream = stdout;
            }
            else {
                output_stream = fopen(output_file_name, "wb");
            }

            if (output_stream == NULL) {
                fprintf(stderr, "%s: Error: Can not open %s for writing\n",
                        prog_name, output_file_name);
                status = EXIT_FAILURE;
            }
            else {
                do_translation(input_stream, with_latin_escapes, 
                               output_stream, output_encoding);
                if (output_stream != stdout) fclose(output_stream);
            }
            if (input_stream != stdin) fclose(input_stream);
        }
    }

    return status;
}
