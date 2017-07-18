
// fileno is a POSIX thing
#define _POSIX_SOURCE

#include <stdio.h>
#include <fcntl.h>
#include <stdlib.h>
#include <string.h>
#include <signal.h>
#include <stdint.h>
#include <setjmp.h>

// What we need from <complex.h>
#define complex _Complex
double creal(double complex);
double cimag(double complex);

#include <unistd.h>

// Primitive _Bool not supported?
#if __STDC_VERSION__ < 199901L && __GNUC__ < 3
typedef	int _Bool;
#endif

// ------------------------------------------------------------------------
// Types ------------------------------------------------------------------

typedef unsigned char byte;

typedef byte *Binary;

typedef Binary (*EncodeFun)(Binary, void *);
typedef Binary (*DecodeFun)(void *, Binary);
typedef Binary (*InterfaceFun)(Binary dst, Binary args);

// ------------------------------------------------------------------------
// Names ------------------------------------------------------------------

#define PREFIX(NAME)           __eqc_c_ ## NAME

#define DECODE_FUN(NAME)       PREFIX(decode_ ## NAME)
#define ENCODE_FUN(NAME)       PREFIX(encode_ ## NAME)
#define DECODE_ARRAY_FUN(NAME) PREFIX(decode_array_of_ ## NAME)
#define ENCODE_ARRAY_FUN(NAME) PREFIX(encode_array_of_ ## NAME)
#define CREATE_ARRAY_FUN(NAME) PREFIX(create_array_of_ ## NAME)
#define READ_ARRAY_FUN(NAME)   PREFIX(read_array_of_ ## NAME)
#define WRITE_ARRAY_FUN(NAME)  PREFIX(write_array_of_ ## NAME)

#define GEN_ENCODE_ARRAY_FUN   PREFIX(encode_array)
#define GEN_DECODE_ARRAY_FUN   PREFIX(decode_array)
#define GEN_CREATE_ARRAY_FUN   PREFIX(create_array)
#define GEN_READ_ARRAY_FUN     PREFIX(read_array)
#define GEN_WRITE_ARRAY_FUN    PREFIX(write_array)

#define READ_STRING_FUN        PREFIX(read_string)

#define FREE_FUN               PREFIX(free)
#define PING_FUN               PREFIX(ping)

#define BUF       PREFIX(buf)
#define BUF_START PREFIX(buf_start)
#define BUF_END   PREFIX(buf_end)

#define CLEAR_BUF      PREFIX(clear_buf)
#define SEND_BUF       PREFIX(send_buf)

#define WRITE_CMD   PREFIX(write_cmd)
#define WRITE_EXACT PREFIX(write_exact)
#define READ_CMD    PREFIX(read_cmd)
#define READ_EXACT  PREFIX(read_exact)

#define SET_BINARY_MODE PREFIX(set_binary_mode)

#define SIGNAL_HANDLER PREFIX(signal_handler)
#define INTERPRETER      PREFIX(interpreter)
#define INTERPRETER_LOOP PREFIX(interpreter_loop)

// ------------------------------------------------------------------------
// Low level communication ------------------------------------------------

#ifdef EQC_NOUSE_STDIO
# define OUT 4
# define IN  3
#else
# define OUT 1
# define IN  0
#endif

#define MSG_LEN_BYTES 4

int READ_EXACT(byte *buf, unsigned int len)
{
  int i, got = 0;

  do {
    i = read(IN, buf + got, len - got);
    if (i <= 0) return i;
    got += i;
  } while (got < len);

  return (int)len;
}

int READ_CMD(byte *buf)
{
  unsigned int len;

  if (READ_EXACT(buf, MSG_LEN_BYTES) == MSG_LEN_BYTES)
  {
    int n;
    for(len = 0, n = 0; n < MSG_LEN_BYTES; n++)
      len = (len << 8) | buf[n];
    return READ_EXACT(buf, len);
  }
  return(-1);
}

int WRITE_EXACT(byte *buf, unsigned int len)
{
  int i, wrote = 0;

  do {
    i = write(OUT, buf + wrote, len - wrote);
    if (i <= 0) return (i);
    wrote += i;
  } while (wrote < len);

  return (int)len;
}

int WRITE_CMD(byte *buf, unsigned int len)
{
  byte li[MSG_LEN_BYTES];
  int n;

  for (n = 0; n < MSG_LEN_BYTES; n++)
    li[n] = (len >> (8 * (MSG_LEN_BYTES - n - 1))) & 0xff;

  WRITE_EXACT(li, MSG_LEN_BYTES);

  return WRITE_EXACT(buf, len);
}

// Under Windows, the streams must be set to binary mode.
// Under UNIX, this mode does not exist. As a result, this
// code WILL NOT COMPILE under Unix. Hence the conditional 
// compilation.

#ifdef O_BINARY
int setmode(int,int);
#endif

void SET_BINARY_MODE(void)
{
#ifdef O_BINARY
  setmode(IN, O_BINARY);
  setmode(OUT, O_BINARY);
#endif
}

// ------------------------------------------------------------------------
// Encoding and decoding --------------------------------------------------

// Decoding ---------------------------------------------------------------

#define DECODE_UNSIGNED(NAME, TYPE) \
Binary DECODE_FUN(NAME) (TYPE *dst, Binary src) \
{ \
  int n, len = sizeof(TYPE); \
\
  *dst = 0; \
  for (n = 0; n < len; n++) \
    *dst = 256 * *dst + src[n]; \
  return src + len; \
}

DECODE_UNSIGNED(bool, _Bool)
DECODE_UNSIGNED(unsigned_char, unsigned char)
DECODE_UNSIGNED(unsigned_short, unsigned short int)
DECODE_UNSIGNED(unsigned_int, unsigned int)
DECODE_UNSIGNED(unsigned_long, unsigned long int)
DECODE_UNSIGNED(unsigned_long_long, unsigned long long int)
DECODE_UNSIGNED(uintptr_t, uintptr_t)

#define DECODE_SIGNED(NAME, TYPE) \
Binary DECODE_FUN(NAME) (TYPE *dst, Binary src) \
{ \
  return DECODE_FUN(unsigned_ ## NAME) ((unsigned TYPE *)dst, src); \
}

DECODE_SIGNED(char, char)
DECODE_SIGNED(short, short int)
DECODE_SIGNED(int, int)
DECODE_SIGNED(long, long int)
DECODE_SIGNED(long_long, long long int)

Binary DECODE_FUN(double) (double *dst, Binary src)
{
  *dst = *(double *)src;
  return src + sizeof(double);
}

Binary DECODE_FUN(complex_double) (complex double *dst, Binary src)
{
  double *parts = (double *)dst;  // According to the standard, a complex
                                  // number has the same representation as
                                  // a two element array of the underlying
                                  // type.
  src = DECODE_FUN(double)(parts,     src);
  src = DECODE_FUN(double)(parts + 1, src);
  return src;
}

#define WRAP_DECODE(NAME, TYPE, OLD_NAME, OLD_TYPE) \
Binary DECODE_FUN(NAME) (TYPE *dst, Binary src) \
{ \
  OLD_TYPE tmp; \
  src = DECODE_FUN(OLD_NAME) (&tmp, src); \
  *dst = (TYPE)tmp; \
  return src; \
}

WRAP_DECODE(float, float, double, double)
WRAP_DECODE(long_double, long double, double, double)
WRAP_DECODE(pointer, void *, uintptr_t, uintptr_t)
WRAP_DECODE(complex_float, complex float, complex_double, complex double)
WRAP_DECODE(complex_long_double, complex long double, complex_double, complex double)

// Encoding ---------------------------------------------------------------

#define ENCODE_UNSIGNED(NAME, TYPE) \
Binary ENCODE_FUN(NAME) (Binary dst, TYPE *src) \
{ \
  int n, len = sizeof(TYPE); \
  TYPE tmp = *src; \
  for (n = len - 1; n >= 0; n--) \
  { \
    dst[n] = tmp % 256; \
    tmp /= 256; \
  } \
  return dst + len; \
}

ENCODE_UNSIGNED(bool, _Bool)
ENCODE_UNSIGNED(unsigned_char, unsigned char)
ENCODE_UNSIGNED(unsigned_short, unsigned short int)
ENCODE_UNSIGNED(unsigned_int, unsigned int)
ENCODE_UNSIGNED(unsigned_long, unsigned long int)
ENCODE_UNSIGNED(unsigned_long_long, unsigned long long int)
ENCODE_UNSIGNED(uintptr_t, uintptr_t)

#define ENCODE_SIGNED(NAME, TYPE) \
Binary ENCODE_FUN(NAME) (Binary dst, TYPE *src) \
{ \
  return ENCODE_FUN(unsigned_ ## NAME) (dst, (unsigned TYPE *)src); \
}

ENCODE_SIGNED(char, char)
ENCODE_SIGNED(short, short int)
ENCODE_SIGNED(int, int)
ENCODE_SIGNED(long, long int)
ENCODE_SIGNED(long_long, long long int)

Binary ENCODE_FUN(double) (Binary dst, double *src)
{
  *(double *)dst = *src;
  return dst + sizeof(double);
}

Binary ENCODE_FUN(complex_double) (Binary dst, complex double *src)
{
  double real = creal(*src),
         imag = cimag(*src);
  dst = ENCODE_FUN(double)(dst, &real);
  dst = ENCODE_FUN(double)(dst, &imag);
  return dst;
}

#define WRAP_ENCODE(NAME, TYPE, OLD_NAME, OLD_TYPE) \
Binary ENCODE_FUN(NAME) (Binary dst, TYPE *src) \
{ \
  OLD_TYPE tmp = (OLD_TYPE)*src; \
  return ENCODE_FUN(OLD_NAME) (dst, &tmp); \
}

WRAP_ENCODE(float, float, double, double)
WRAP_ENCODE(long_double, long double, double, double)
WRAP_ENCODE(pointer, void *, uintptr_t, uintptr_t)
WRAP_ENCODE(complex_float, complex float, complex_double, complex double)
WRAP_ENCODE(complex_long_double, complex long double, complex_double, complex double)

// Encoding arrays --------------------------------------------------------

Binary GEN_ENCODE_ARRAY_FUN (Binary dst, void *src, unsigned int len, size_t size, EncodeFun enc)
{
  char *p = (char *)src;
  unsigned int n;

  for (n = 0; n < len; n++, p += size)
    dst = enc(dst, p);
  return dst;
}

#define ENCODE_ARRAY(NAME, TYPE) \
Binary ENCODE_ARRAY_FUN(NAME) (Binary dst, TYPE *src, unsigned int len) \
{ \
  return GEN_ENCODE_ARRAY_FUN (dst, src, len, sizeof(TYPE), (EncodeFun)ENCODE_FUN(NAME)); \
}

ENCODE_ARRAY(pointer, void *)
ENCODE_ARRAY(bool, _Bool)
ENCODE_ARRAY(char, char)
ENCODE_ARRAY(short, short int)
ENCODE_ARRAY(int, int)
ENCODE_ARRAY(long, long int)
ENCODE_ARRAY(long_long, long long int)
ENCODE_ARRAY(unsigned_char, unsigned char)
ENCODE_ARRAY(unsigned_short, unsigned short int)
ENCODE_ARRAY(unsigned_int, unsigned int)
ENCODE_ARRAY(unsigned_long, unsigned long int)
ENCODE_ARRAY(unsigned_long_long, unsigned long long int)
ENCODE_ARRAY(float, float)
ENCODE_ARRAY(double, double)
ENCODE_ARRAY(long_double, long double)
ENCODE_ARRAY(complex_float, complex float)
ENCODE_ARRAY(complex_double, complex double)
ENCODE_ARRAY(complex_long_double, complex long double)

// Decoding arrays --------------------------------------------------------

Binary GEN_DECODE_ARRAY_FUN (void *dst, Binary src, unsigned int len, size_t size, DecodeFun dec)
{
  char *p = (char *)dst;
  unsigned int n;

  for (n = 0; n < len; n++, p += size)
    src = dec(p, src);
  return src;
}

#define DECODE_ARRAY(NAME, TYPE) \
Binary DECODE_ARRAY_FUN(NAME) (TYPE *dst, Binary src, unsigned int len) \
{ \
  return GEN_DECODE_ARRAY_FUN (dst, src, len, sizeof(TYPE), (DecodeFun)DECODE_FUN(NAME)); \
}

DECODE_ARRAY(pointer, void *)
DECODE_ARRAY(bool, _Bool)
DECODE_ARRAY(char, char)
DECODE_ARRAY(short, short int)
DECODE_ARRAY(int, int)
DECODE_ARRAY(long, long int)
DECODE_ARRAY(long_long, long long int)
DECODE_ARRAY(unsigned_char, unsigned char)
DECODE_ARRAY(unsigned_short, unsigned short int)
DECODE_ARRAY(unsigned_int, unsigned int)
DECODE_ARRAY(unsigned_long, unsigned long int)
DECODE_ARRAY(unsigned_long_long, unsigned long long int)
DECODE_ARRAY(float, float)
DECODE_ARRAY(double, double)
DECODE_ARRAY(long_double, long double)
DECODE_ARRAY(complex_float, complex float)
DECODE_ARRAY(complex_double, complex double)
DECODE_ARRAY(complex_long_double, complex long double)

// Strings ----------------------------------------------------------------

Binary ENCODE_FUN(string) (Binary dst, char *src)
{
  return ENCODE_ARRAY_FUN(char) (dst, src, (unsigned int)strlen(src) + 1);
}

// ------------------------------------------------------------------------
// Interface functions ----------------------------------------------------

// create_array -----------------------------------------------------------
// TYPE *create_array(unsigned int len, T array[]);

Binary GEN_CREATE_ARRAY_FUN (Binary dst, Binary args, size_t size, DecodeFun dec)
{
  unsigned int len;
  void *p;
  
  args = DECODE_FUN(unsigned_int) (&len, args);
  p    = malloc(size * len);
  GEN_DECODE_ARRAY_FUN(p, args, len, size, dec);
  return ENCODE_FUN(pointer) (dst, &p);
}

#define CREATE_ARRAY(NAME, TYPE) \
Binary CREATE_ARRAY_FUN(NAME) (Binary dst, Binary args) \
{ \
  return GEN_CREATE_ARRAY_FUN(dst, args, sizeof(TYPE), (DecodeFun)DECODE_FUN(NAME)); \
}

CREATE_ARRAY(pointer, void *)
CREATE_ARRAY(bool, _Bool)
CREATE_ARRAY(char, char)
CREATE_ARRAY(short, short int)
CREATE_ARRAY(int, int)
CREATE_ARRAY(long, long int)
CREATE_ARRAY(long_long, long long int)
CREATE_ARRAY(unsigned_char, unsigned char)
CREATE_ARRAY(unsigned_short, unsigned short int)
CREATE_ARRAY(unsigned_int, unsigned int)
CREATE_ARRAY(unsigned_long, unsigned long int)
CREATE_ARRAY(unsigned_long_long, unsigned long long int)
CREATE_ARRAY(float, float)
CREATE_ARRAY(double, double)
CREATE_ARRAY(long_double, long double)
CREATE_ARRAY(complex_float, complex float)
CREATE_ARRAY(complex_double, complex double)
CREATE_ARRAY(complex_long_double, complex long double)

// read_array -------------------------------------------------------------
// T[] read_array(unsigned int len, T *ptr);

Binary GEN_READ_ARRAY_FUN (Binary dst, Binary args, size_t size, EncodeFun enc)
{
  unsigned int len;
  void *p;

  args = DECODE_FUN(unsigned_int)(&len, args);
  args = DECODE_FUN(pointer)     (&p, args);
  return GEN_ENCODE_ARRAY_FUN(dst, p, len, size, enc);
}

#define READ_ARRAY(NAME, TYPE) \
Binary READ_ARRAY_FUN(NAME) (Binary dst, Binary args) \
{ \
  return GEN_READ_ARRAY_FUN(dst, args, sizeof(TYPE), (EncodeFun)ENCODE_FUN(NAME)); \
}

READ_ARRAY(pointer, void *)
READ_ARRAY(bool, _Bool)
READ_ARRAY(char, char)
READ_ARRAY(short, short int)
READ_ARRAY(int, int)
READ_ARRAY(long, long int)
READ_ARRAY(long_long, long long int)
READ_ARRAY(unsigned_char, unsigned char)
READ_ARRAY(unsigned_short, unsigned short int)
READ_ARRAY(unsigned_int, unsigned int)
READ_ARRAY(unsigned_long, unsigned long int)
READ_ARRAY(unsigned_long_long, unsigned long long int)
READ_ARRAY(float, float)
READ_ARRAY(double, double)
READ_ARRAY(long_double, long double)
READ_ARRAY(complex_float, complex float)
READ_ARRAY(complex_double, complex double)
READ_ARRAY(complex_long_double, complex long double)

// write_array ------------------------------------------------------------
// void write_array(unsigned int len, T *p, T array[]);

Binary GEN_WRITE_ARRAY_FUN (Binary dst, Binary args, size_t size, DecodeFun dec)
{
  unsigned int len;
  void *p;
  
  args = DECODE_FUN(unsigned_int) (&len, args);
  args = DECODE_FUN(pointer)      (&p, args);
  GEN_DECODE_ARRAY_FUN(p, args, len, size, dec);
  return dst;
}

#define WRITE_ARRAY(NAME, TYPE) \
Binary WRITE_ARRAY_FUN(NAME) (Binary dst, Binary args) \
{ \
  return GEN_WRITE_ARRAY_FUN(dst, args, sizeof(TYPE), (DecodeFun)DECODE_FUN(NAME)); \
}

WRITE_ARRAY(pointer, void *)
WRITE_ARRAY(bool, _Bool)
WRITE_ARRAY(char, char)
WRITE_ARRAY(short, short int)
WRITE_ARRAY(int, int)
WRITE_ARRAY(long, long int)
WRITE_ARRAY(long_long, long long int)
WRITE_ARRAY(unsigned_char, unsigned char)
WRITE_ARRAY(unsigned_short, unsigned short int)
WRITE_ARRAY(unsigned_int, unsigned int)
WRITE_ARRAY(unsigned_long, unsigned long int)
WRITE_ARRAY(unsigned_long_long, unsigned long long int)
WRITE_ARRAY(float, float)
WRITE_ARRAY(double, double)
WRITE_ARRAY(long_double, long double)
WRITE_ARRAY(complex_float, complex float)
WRITE_ARRAY(complex_double, complex double)
WRITE_ARRAY(complex_long_double, complex long double)

// read_string ------------------------------------------------------------
// char[] read_string(char *p)
Binary READ_STRING_FUN(Binary dst, Binary args)
{
  char *p;

  args = DECODE_FUN(pointer)((void **)&p, args);
  return ENCODE_FUN(string)(dst, p);
}

// free -------------------------------------------------------------------
// void free(void *p)

Binary FREE_FUN(Binary dst, Binary args)
{
  void *p;

  args = DECODE_FUN(pointer)(&p, args);
  free(p);
  return dst;
}

// ping -------------------------------------------------------------------
// void ping(void)

Binary PING_FUN(Binary dst, Binary args)
{
  return dst;
}

// ------------------------------------------------------------------------
// Interpreter ------------------------------------------------------------

#define OK_VALUE  PREFIX(ok_value)
#define CRASH_VALUE PREFIX(bad_value)
#define EXCEPTION_VALUE PREFIX(exception_value)

char OK_VALUE = 0, CRASH_VALUE = 1, EXCEPTION_VALUE = 2;


/* Returning an erlang term */

byte BUF[1000000]; /* TODO: dynamic buffer */
const Binary BUF_START = BUF;
Binary BUF_END = BUF;

void CLEAR_BUF (void)
{
  BUF_END = BUF_START;
}

void SEND_BUF (void)
{
  WRITE_CMD(BUF_START, BUF_END - BUF_START);
}

/* Signal handling */

void SIGNAL_HANDLER (int sig_id)
{
  // ETERM *res[2];
  char *name;

  CLEAR_BUF();
  BUF_END = ENCODE_FUN(char)(BUF_END, &CRASH_VALUE);

  switch(sig_id)
  {
    case SIGSEGV: name = "segmentation_fault";  break;
    case SIGFPE:  name = "arithmetic_error";    break;
    case SIGILL:  name = "illegal_instruction"; break;
    case SIGABRT: name = "abort";               break;
#ifdef SIGBUS
    case SIGBUS:  name = "bus_error";           break;
#endif
    case SIGINT:  name = "program_interrupt";   break;
#ifdef SIGQUIT
    case SIGQUIT: name = "quit";                break;
#endif
    case SIGTERM: name = "terminate";           break;
    default:      name = "unknown_error";
  }

  BUF_END = ENCODE_FUN(string)(BUF_END, name);
  SEND_BUF();
  
  close(OUT);
  exit(1);
}

/* The interpreter */

#define FUNCTIONS     PREFIX(functions)
#define NUM_FUNCTIONS PREFIX(num_functions)
#define FUN_INDEX     PREFIX(fun_index)
#define READ_PTR      PREFIX(read_ptr)
#define JUMP_ENV      PREFIX(jump_env)

static InterfaceFun *FUNCTIONS;
static unsigned int NUM_FUNCTIONS;
static unsigned int FUN_INDEX;
static jmp_buf      JUMP_ENV;

int INTERPRETER_LOOP () {
  signal(SIGSEGV, SIGNAL_HANDLER);
  signal(SIGFPE, SIGNAL_HANDLER);
  signal(SIGILL, SIGNAL_HANDLER);
  signal(SIGABRT, SIGNAL_HANDLER);
#ifdef SIGBUS
  signal(SIGBUS, SIGNAL_HANDLER);
#endif
  signal(SIGINT, SIGNAL_HANDLER);
#ifdef SIGQUIT
  signal(SIGQUIT, SIGNAL_HANDLER);
#endif
  signal(SIGTERM, SIGNAL_HANDLER);

  SET_BINARY_MODE(); // necessary under Windows

  setjmp(JUMP_ENV);
  CLEAR_BUF();

  while (READ_CMD(BUF_START) > 0)
  {
    BUF_END = DECODE_FUN(unsigned_int)(&FUN_INDEX, BUF_END);

    if (FUN_INDEX < NUM_FUNCTIONS)
    {
      BUF_END = FUNCTIONS[FUN_INDEX](BUF_START + 1, BUF_END);
      ENCODE_FUN(char)(BUF_START, &OK_VALUE);
      SEND_BUF();
    } else
    {
      CLEAR_BUF();
      BUF_END = ENCODE_FUN(char)(BUF_END, &CRASH_VALUE);
      BUF_END = ENCODE_FUN(string)(BUF_END, "bad_function");
      SEND_BUF();
    }

    CLEAR_BUF();
  }
  return 0;
}

int INTERPRETER (InterfaceFun functions[], unsigned int len) {
  FUNCTIONS     = functions;
  NUM_FUNCTIONS = len;
  return INTERPRETER_LOOP();
}

void eqc_throw (char *err) {
  CLEAR_BUF();
  BUF_END = ENCODE_FUN(char)(BUF_END, &EXCEPTION_VALUE);
  BUF_END = ENCODE_FUN(string)(BUF_END, err);
  SEND_BUF();
  longjmp(JUMP_ENV, 0);
}

void eqc_shutdown (char *err) {
  CLEAR_BUF();
  BUF_END = ENCODE_FUN(char)(BUF_END, &CRASH_VALUE);
  BUF_END = ENCODE_FUN(string)(BUF_END, err);
  SEND_BUF();
  close(OUT);
  exit(1);
}


