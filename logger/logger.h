#ifndef __LOGGER__
#define __LOGGER__

#include <stdio.h>
#include <stdlib.h>
#include <errno.h>
#include <string.h>

#define RED 0
#define GREEN 1
#define YELLOW 2
#define BLUE 3
#define MAGENTA 4
#define CYAN 5
#define WHITE 6

char *close_color = "\x1b[0m";
char __buf[256], __buf2[256], __buf3[256];

typedef enum {
  Red,
  Green,
  Yellow,
  Blue,
  Magenta,
  Cyan,
  White,
} Color;

typedef enum {
  Success,
  Info,
  Debug,
  Warning,
  Error,
  Fatal,
} LogLevel;

Color color_of_level(LogLevel level);
char* string_of_color(Color color);
char* string_of_level(LogLevel level);
void perror_exit(char* msg);

#define colorf(color, fmt, ...) \
  memset(&__buf, 0, sizeof(__buf)); \
  sprintf(__buf, "%s%s%s", \
    string_of_color(color), fmt, close_color); \
  fprintf(stdout, __buf, ##__VA_ARGS__);

#define greenf(fmt, ...) colorf(GREEN, fmt, ##__VA_ARGS__);
#define yellowf(fmt, ...) colorf(YELLOW, fmt, ##__VA_ARGS__);
#define redf(fmt, ...) colorf(RED, fmt, ##__VA_ARGS__);
#define bluef(fmt, ...) colorf(BLUE, fmt, ##__VA_ARGS__);
#define magentaf(fmt, ...) colorf(MAGENTA, fmt, ##__VA_ARGS__);
#define cyanf(fmt, ...) colorf(CYAN, fmt, ##__VA_ARGS__);
#define whitef(fmt, ...) colorf(WHITE, fmt, ##__VA_ARGS__);

#define error_exitf(fmt, ...) \
  memset(&__buf2, 0, sizeof(__buf2)); \
  sprintf(__buf2, "Error in %s:%d in function %s: %s", \
    __FILE__, __LINE__, __func__, fmt); \
  redf(__buf2, ##__VA_ARGS__); \
  exit(EXIT_FAILURE);

#define logf(level, fmt, ...) \
  memset(&__buf3, 0, sizeof(__buf3)); \
  sprintf(__buf3, "[%s] %s:%d in function %s: %s", \
    string_of_level(level), __FILE__, __LINE__, __func__, fmt); \
  colorf(color_of_level(level), __buf3, ##__VA_ARGS__);

/* #define logf2(level, fmt, ...) \ */
/*   __logf(__FILE__, __LINE__, __func__, fmt, ##__VA_ARGS__); */

/* void __logf(char* filename, int line, char* function, char* fmt, ...) {} */

#define log_successf(fmt, ...) logf(Success, fmt, ##__VA_ARGS__);
#define log_infof(fmt, ...) logf(Info, fmt, ##__VA_ARGS__);
#define log_debugf(fmt, ...) logf(Debug, fmt, ##__VA_ARGS__);
#define log_warningf(fmt, ...) logf(Warning, fmt, ##__VA_ARGS__);
#define log_errorf(fmt, ...) logf(Error, fmt, ##__VA_ARGS__);
#define log_fatalf(fmt, ...) logf(Fatal, fmt, ##__VA_ARGS__);

void perror_exit(char* msg) {
  char buf[256];

  memset(&buf, 0, sizeof(buf));

  if (strncmp(strerror(errno), "Success", 7) == 0) {
    sprintf(buf, "%s", msg);
  } else {
    sprintf(buf, "%s => %s", msg, strerror(errno));
  }

  error_exitf(buf, NULL);
}

char* string_of_color(Color color) {
  char *res = "";

  switch (color) {
  case RED:
    res = "\x1b[1;31m";
    break;

  case GREEN:
    res = "\x1b[1;32m";
    break;

  case YELLOW:
    res = "\x1b[1;33m";
    break;

  case BLUE:
    res = "\x1b[1;34m";
    break;

  case MAGENTA:
    res = "\x1b[1;35m";
    break;

  case CYAN:
    res = "\x1b[1;36m";
    break;

  case WHITE:
    res = "\x1b[1;37m";
    break;
  }

  return res;
}

Color color_of_level(LogLevel level) {
  Color color = Green;

  switch (level) {
  case Success:
    color = Green;
    break;

  case Info:
    color = Yellow;
    break;

  case Debug:
    color = Blue;
    break;

  case Warning:
    color = Magenta;
    break;

  case Error:
    color = Red;
    break;

  case Fatal:
    color = Red;
    break;
  }

  return color;
}

char* string_of_level(LogLevel level) {
  char *res = "";

  switch (level) {
  case Success:
    res = "SUCCESS";
    break;

  case Info:
    res = "INFO";
    break;

  case Debug:
    res = "DEBUG";
    break;

  case Warning:
    res = "WARNING";
    break;

  case Error:
    res = "ERROR";
    break;

  case Fatal:
    res = "FATAL";
    break;
  }

  return res;
}

#endif
