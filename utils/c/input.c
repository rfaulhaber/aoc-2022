#include "input.h"
#include <stdio.h>
#include <string.h>

size_t get_line_count(char *path) {
  FILE *fp = fopen(path, "r");

  if (fp == NULL) {
    exit(1);
  }

  char *c;
  size_t lines_size = 0;
  size_t nbytes;

  size_t line_count = 0;

  while ((nbytes = getline(&c, &lines_size, fp)) != -1) {
    line_count++;
  }

  fclose(fp);

  return line_count;
}

char **get_file_contents(char *filename, size_t *length) {
  size_t file_line_count = get_line_count(filename);

  FILE *fp = fopen(filename, "r");

  if (fp == NULL) {
    exit(1);
  }

  char **lines = malloc(file_line_count * sizeof(char *));
  size_t char_count = 0;

  char *line;
  size_t line_length = 0;
  size_t nbytes;

  while ((nbytes = getline(&line, &line_length, fp)) != -1) {
    lines[char_count] = strdup(line);
    if (lines[char_count][line_length - 1] == '\n') {
      lines[char_count][line_length - 1] = '\0';
    }

    char_count++;
  }

  free(line);
  fclose(fp);

  *length = file_line_count;

  return lines;
}

unsigned int count_delim_occurance(char *str, const char delim) {
  size_t str_len = strlen(str);

  int count = 0;

  for (int i = 0; i < str_len; i++) {
    if (str[i] == delim) {
      count++;
    }
  }

  return count;
}

char **split_str(char *str, const char delim, size_t *split_count) {
  size_t str_len = strlen(str);

  unsigned int delim_count = count_delim_occurance(str, delim);

  char **strs = malloc((delim_count + 1) * sizeof(char *));

  int start = 0;
  unsigned int delims_found = 0;

  for (int i = 0; i < str_len; i++) {
    if (str[i] == delim) {
      strs[delims_found] = strndup(str + start, i - start);

      start = i + 1;
      delims_found++;
    }

    if (delims_found == delim_count) {
      strs[delims_found] = strndup(str + start, str_len - start);
      break;
    }
  }

  *split_count = delims_found + 1;

  return strs;
}
