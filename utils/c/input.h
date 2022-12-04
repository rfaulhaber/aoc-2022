#ifndef INPUT_H_
#define INPUT_H_

#include <stdlib.h>

size_t get_line_count(char *path);
char **get_file_contents(char *path, size_t *file_length);
char **split_str(char *str, const char delim, size_t *split_count);

#endif // INPUT_H_
