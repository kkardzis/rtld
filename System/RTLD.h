/* ------------------------------------------------------------------------- */
/* |                                                                         */
/* Module      :  System.RTLD                                                */
/* Copyright   :  Copyright © 2012-2013 Krzysztof Kardzis                    */
/* License     :  ISC License (MIT/BSD-style, see LICENSE file for details)  */
/*                                                                           */
/* Maintainer  :  Krzysztof Kardzis <kkardzis@gmail.com>                     */
/* Stability   :  experimental                                               */
/* Portability :  non-portable                                               */
/*                                                                           */
/* ------------------------------------------------------------------------- */


/* ------------------------------------------------------------------------- */
/*                                                                           */
/* ------------------------------------------------------------------------- */
typedef struct {int vmin; int vmax; char* name;} SYMTABENTRY;

#define TABLEN (sizeof (SYMTAB) / sizeof (*SYMTAB))

#define hsc_FPID(fn)                            \
  { int i; for (i = 0; i < TABLEN; i++) {       \
      if (strcmp(#fn, SYMTAB[i].name) == 0) {   \
        printf("("); hsc_const(i); printf(")"); \
        break;                                  \
      };                                        \
    };                                          \
  }


/* ------------------------------------------------------------------------- */
/* function import macros (for hsc2hs)                                       */
/* ------------------------------------------------------------------------- */

#ifdef STDCALLCONV
#  define hsc_SAFECALL(fn,ft...) CALL(fn, TYPE(ft), stdcall   safe, ARGS(ft));
#  define hsc_FASTCALL(fn,ft...) CALL(fn, TYPE(ft), stdcall unsafe, ARGS(ft));
#else
#  define hsc_SAFECALL(fn,ft...) CALL(fn, TYPE(ft),   ccall   safe, ARGS(ft));
#  define hsc_FASTCALL(fn,ft...) CALL(fn, TYPE(ft),   ccall unsafe, ARGS(ft));
#endif

#define CALL(fn, ft, conv, args)                            \
  printf("\n");                                             \
  printf("{-# NOINLINE " #fn " #-}\n");                     \
  printf(#fn " :: " str(ft) "\n");                          \
  printf(#fn " " str(args) " = peekFP "); hsc_FPID(fn);     \
  printf(" >>= \\fp -> " #fn "FC fp " str(args) "\n");      \
  printf("\n");                                             \
  printf("type FT" #fn " = " str(ft) "\n");                 \
  printf("foreign import " #conv " \"dynamic\"\n");         \
  printf("  " #fn "FC :: FunPtr FT" #fn " -> FT" #fn "\n");

#define CASE(a,b,c,d,e,f,g,h,i,j,k,l,m,n,o,p,X,...) X

#define TYPE(...) CASE(__VA_ARGS__      \
  , T16(__VA_ARGS__) , T15(__VA_ARGS__) \
  , T14(__VA_ARGS__) , T13(__VA_ARGS__) \
  , T12(__VA_ARGS__) , T11(__VA_ARGS__) \
  , T10(__VA_ARGS__) , T09(__VA_ARGS__) \
  , T08(__VA_ARGS__) , T07(__VA_ARGS__) \
  , T06(__VA_ARGS__) , T05(__VA_ARGS__) \
  , T04(__VA_ARGS__) , T03(__VA_ARGS__) \
  , T02(__VA_ARGS__) , T01(__VA_ARGS__) \
  )

#define T16(a, ...) a -> T15(__VA_ARGS__)
#define T15(a, ...) a -> T14(__VA_ARGS__)
#define T14(a, ...) a -> T13(__VA_ARGS__)
#define T13(a, ...) a -> T12(__VA_ARGS__)
#define T12(a, ...) a -> T11(__VA_ARGS__)
#define T11(a, ...) a -> T10(__VA_ARGS__)
#define T10(a, ...) a -> T09(__VA_ARGS__)
#define T09(a, ...) a -> T08(__VA_ARGS__)
#define T08(a, ...) a -> T07(__VA_ARGS__)
#define T07(a, ...) a -> T06(__VA_ARGS__)
#define T06(a, ...) a -> T05(__VA_ARGS__)
#define T05(a, ...) a -> T04(__VA_ARGS__)
#define T04(a, ...) a -> T03(__VA_ARGS__)
#define T03(a, ...) a -> T02(__VA_ARGS__)
#define T02(a, ...) a -> T01(__VA_ARGS__)
#define T01(a     ) a

#define ARGS(...) CASE(__VA_ARGS__ \
  , a b c d e f g h i j k l m n o \
  , a b c d e f g h i j k l m n \
  , a b c d e f g h i j k l m \
  , a b c d e f g h i j k l \
  , a b c d e f g h i j k \
  , a b c d e f g h i j \
  , a b c d e f g h i \
  , a b c d e f g h \
  , a b c d e f g \
  , a b c d e f \
  , a b c d e \
  , a b c d \
  , a b c \
  , a b \
  , a \
  , \
  )

#define str(s) #s


/* ------------------------------------------------------------------------- */
/* callback import macros (for hsc2hs)                                       */
/* ------------------------------------------------------------------------- */

#ifdef STDBACKCONV
#  define BACKCONV "stdcall"
#else
#  define BACKCONV "ccall"
#endif

#define hsc_WRAP(fn, ft)                                       \
  printf("\n");                                                \
  printf("type " #fn " = " #ft "\n");                          \
  printf("\n");                                                \
  printf("foreign import " BACKCONV " \"wrapper\"\n");         \
  printf("  wrap" #fn " :: " #fn " -> IO (FunPtr " #fn ")\n"); \


/* ------------------------------------------------------------------------- */
/* constant import macros (for hsc2hs)                                       */
/* ------------------------------------------------------------------------- */

#define hsc_ENUM(type, ...)                          \
  printf("data " #type "\n");                        \
  { char *x, xs[] = #__VA_ARGS__;                    \
    printf("  = %s\n", strtok(xs,","));              \
    while ((x=strtok(NULL,",")) != NULL) {           \
      printf("  |%s\n", x);                          \
    };                                               \
  };                                                 \
  printf("\n");                                      \
  printf("instance ENUM " #type " where\n");         \
  printf("  enumlist = [ " #__VA_ARGS__ " ]\n");     \
  printf("  toENUM x = case x of\n");                \
  { char *x, xs[] = #__VA_ARGS__;                    \
    int i=1, vs[] = {__VA_ARGS__};                   \
    printf("    %s -> %d\n", strtok(xs,","), vs[0]); \
    while ((x=strtok(NULL,",")) != NULL) {           \
      printf("   %s -> %d\n", x, vs[i++]);           \
    };                                               \
  };                                                 \

#define hsc_GADT(type, ...)                                          \
  printf("data " #type " where\n");                                  \
  { int i=0; struct {int v; char *s;} vs[] = {__VA_ARGS__};          \
    char xs[] = #__VA_ARGS__; char *x = strtok(xs,",");              \
    do { printf("  %s :: %s\n", x+1, vs[i++].s); strtok(NULL,",");   \
    } while ((x=strtok(NULL,",")) != NULL);                          \
  };                                                                 \
  printf("\ninstance ENUM (" #type ") where\n");                     \
  printf("  enumlist = []\n");                                       \
  printf("  toENUM x = case x of\n");                                \
  { int i=0; struct {int v; char *s;} vs[] = {__VA_ARGS__};          \
    char xs[] = #__VA_ARGS__; char *x = strtok(xs,",");              \
    do { printf("    %s -> %d\n", x+1, vs[i++].v); strtok(NULL,","); \
    } while ((x=strtok(NULL,",")) != NULL);                          \
  };                                                                 \

#define T4(x) "(" #x ", " #x ", " #x ", " #x ")"
#define T3(x) "(" #x ", " #x ", " #x ")"
#define T2(x) "(" #x ", " #x ")"
#define T1(x) "(" #x ")"

