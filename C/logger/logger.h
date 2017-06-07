#ifndef __LOGGER__
#define __LOGGER__

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

char *close_color = "\x1b[0m";
char __buf[1024];

typedef enum {
  RED,
  GREEN,
  YELLOW,
  BLUE,
  MAGENTA,
  CYAN,
  WHITE,
} Color;

typedef enum {
  SUCCESS,
  INFO,
  DEBUG,
  WARNING,
  ERROR,
  FATAL,
} LogLevel;

char* string_of_color(Color color) {
  if (color == RED)
    return "\x1b[1;31m";
  else if (color == GREEN)
    return "\x1b[1;32m";
  else if (color == YELLOW)
    return "\x1b[1;33m";
  else if (color == BLUE)
    return "\x1b[1;34m";
  else if (color == MAGENTA)
    return "\x1b[1;35m";
  else if (color == CYAN)
    return "\x1b[1;36m";

  // White is default
  return "\x1b[1;37m";
}

Color color_of_level(LogLevel level) {
  if (level == SUCCESS)
    return GREEN;
  else if (level == INFO)
    return YELLOW;
  else if (level == DEBUG)
    return BLUE;
  else if (level == WARNING)
    return MAGENTA;
  else if (level == ERROR)
    return RED;
  else if (level == FATAL)
    return RED;

  // WHITE is default
  return WHITE;
}

char* string_of_level(LogLevel level) {
  if (level == SUCCESS)
    return "SUCCESS";
  else if (level == INFO)
    return "INFO";
  else if (level == DEBUG)
    return "DEBUG";
  else if (level == WARNING)
    return "WARNING";
  else if (level == ERROR)
    return "ERROR";

  return  "FATAL";
}

// TODO: protect with a write mutex
#define __fprintf(file, fmt, ...) fprintf(file, fmt, ##__VA_ARGS__)

#define __format_buf(buf, fmt, ...) \
  memset(&__buf, 0, sizeof(__buf));		\
  sprintf(buf, fmt, ##__VA_ARGS__);		\

#define fcolorf(file, color, fmt, ...)		\
  __format_buf(__buf, "%s%s%s",			\
    string_of_color(color), fmt, close_color)	\
  __fprintf(file, __buf, ##__VA_ARGS__);

#define colorf(color, fmt, ...) fcolorf(stdout, color, fmt, ##__VA_ARGS__)

#define greenf(fmt, ...)   colorf(GREEN, fmt, ##__VA_ARGS__);
#define yellowf(fmt, ...)  colorf(YELLOW, fmt, ##__VA_ARGS__);
#define redf(fmt, ...)     colorf(RED, fmt, ##__VA_ARGS__);
#define bluef(fmt, ...)    colorf(BLUE, fmt, ##__VA_ARGS__);
#define magentaf(fmt, ...) colorf(MAGENTA, fmt, ##__VA_ARGS__);
#define cyanf(fmt, ...)    colorf(CYAN, fmt, ##__VA_ARGS__);
#define whitef(fmt, ...)   colorf(WHITE, fmt, ##__VA_ARGS__);

#define fgreenf(file, fmt, ...)   fcolorf(file, GREEN, fmt, ##__VA_ARGS__);
#define fyellowf(file, fmt, ...)  fcolorf(file, YELLOW, fmt, ##__VA_ARGS__);
#define fredf(file, fmt, ...)     fcolorf(file, RED, fmt, ##__VA_ARGS__);
#define fbluef(file, fmt, ...)    fcolorf(file, BLUE, fmt, ##__VA_ARGS__);
#define fmagentaf(file, fmt, ...) fcolorf(file, MAGENTA, fmt, ##__VA_ARGS__);
#define fcyanf(file, fmt, ...)    fcolorf(file, CYAN, fmt, ##__VA_ARGS__);
#define fwhitef(file, fmt, ...)   fcolorf(file, WHITE, fmt, ##__VA_ARGS__);

#define flogf(file, level, fmt, ...)					\
  __format_buf(__buf, "%s[%s]%s %s:%d in function %s: %s",		\
    string_of_color(color_of_level(level)), string_of_level(level),	\
    close_color,							\
    __FILE__, __LINE__, __func__, fmt);					\
  __fprintf(file, __buf, ##__VA_ARGS__);

#define logf(fmt, ...) flogf(stdout, fmt, ##__VA_ARGS__)

#define successf(fmt, ...) logf(SUCCESS, fmt, ##__VA_ARGS__);
#define infof(fmt, ...)	   logf(INFO, fmt, ##__VA_ARGS__);
#define debugf(fmt, ...)   logf(DEBUG, fmt, ##__VA_ARGS__);
#define warningf(fmt, ...) logf(WARNING, fmt, ##__VA_ARGS__);
#define errorf(fmt, ...)   logf(ERROR, fmt, ##__VA_ARGS__);
#define fatalf(fmt, ...)   logf(FATAL, fmt, ##__VA_ARGS__);

#define fsuccessf(file, fmt, ...) flogf(file, SUCCESS, fmt, ##__VA_ARGS__);
#define finfof(file, fmt, ...)    flogf(file, INFO, fmt, ##__VA_ARGS__);
#define fdebugf(file, fmt, ...)   flogf(file, DEBUG, fmt, ##__VA_ARGS__);
#define fwarningf(file, fmt, ...) flogf(file, WARNING, fmt, ##__VA_ARGS__);
#define ferrorf(file, fmt, ...)   flogf(file, ERROR, fmt, ##__VA_ARGS__);
#define ffatalf(file, fmt, ...)   flogf(file, FATAL, fmt, ##__VA_ARGS__);

#define fperror_exitf(file, fmt, ...)						\
  if (strncmp(strerror(errno), "Success", 7) == 0) {				\
    fcolorf(file, RED, "Error in %s:%d in function %s: %s",			\
      __FILE__, __LINE__, __func__, fmt, NULL);					\
  } else {									\
    fcolorf(file, RED, "Error %s in %s:%d in function %s: %s",			\
      strerror(errno), __FILE__, __LINE__, __func__, fmt, ##__VA_ARGS__);	\
  }										\
  exit(EXIT_FAILURE);

#define perror_exitf(fmt, ...) fperror_exitf(stderr, ##__VA_ARGS__)

#endif
