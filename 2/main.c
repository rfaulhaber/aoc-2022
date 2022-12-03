#include "main.h"

/* TODO generalize */
char **get_lines(char *filename, size_t *length) {
  FILE *fp = fopen(filename, "r");
  char *line = NULL;
  size_t len = 0;
  ssize_t read;

  if (fp == NULL) {
    printf("DEBUG >> could not read file");
    exit(1);
  }

  printf("DEBUG >> read file");

  char c;
  for (c = getc(fp); c != EOF; c = getc(fp)) {
    if (c == '\n') {
      len++;
    }
  }

  char **lines = malloc(len * sizeof(char) * 4);
  size_t char_count = 0;

  printf("lines: %d", len);

  /* fp = fopen(filename, "r"); */

  /* if (fp == NULL) { */
  /*   printf("DEBUG >> could not read file"); */
  /*   exit(1); */
  /* } */

  /* printf("DEBUG >> read file"); */

  /* while ((read = getline(&line, &len, fp)) != -1) { */
  /*   size_t line_len = strlen(line); */
  /*   char *line_cpy = strncpy(malloc(read), line, line_len + 1); */

  /*   if (line_cpy[line_len - 1] == '\n') { */
  /*     line_cpy[line_len - 1] = '\0'; */
  /*   } */

  /*   lines[char_count] = line_cpy; */
  /*   char_count++; */
  /* } */

  /* free(line); */
  /* fclose(fp); */

  *length = char_count;

  return lines;
}

enum left_strat ch_to_left_strat(char ch) {
  switch (ch) {
  case 'A':
    return A;
  case 'B':
    return B;
  case 'C':
    return C;
  }

  return 0;
}

enum right_strat ch_to_right_strat(char ch) {
  switch (ch) {
  case 'X':
    return X;
  case 'Y':
    return Y;
  case 'Z':
    return Z;
  }

  return 0;
}

Strategy *get_strategies(char **file_contents, size_t file_length) {
  Strategy *strategies = malloc(sizeof(Strategy) * file_length);

  for (int i = 0; i < file_length; i++) {
    char left_char = file_contents[i][0];
    char right_char = file_contents[i][2];

    Strategy strat = {ch_to_left_strat(left_char),
                      ch_to_right_strat(right_char)};

    strategies[i] = strat;
  }

  return strategies;
}

enum outcome get_outcome(enum left_strat left, enum right_strat right) {
  /* there's a better way to do this, right? */
  switch (right) {
  case X:
    switch (left) {
    case A:
      return DRAW;
    case B:
      return LOSE;
    case C:
      return WIN;
    }
  case Y:
    switch (left) {
    case A:
      return WIN;
    case B:
      return DRAW;
    case C:
      return LOSE;
    }
    break;
  case Z:
    switch (left) {
    case A:
      return LOSE;
    case B:
      return WIN;
    case C:
      return DRAW;
    }
  }
}

int get_score_for_strat(Strategy strat) {
  enum outcome result = get_outcome(strat.left, strat.right);

  return result + strat.right;
}

int main(int argc, char *argv[]) {
  if (argc > 1) {
    char *path = argv[1];
    size_t filesize;
    printf("got contents");
    char **contents = get_lines(path, &filesize);

    /* for (int i = 0; i < filesize; i++) { */
    /*   printf("line %d, %s", i, contents[i]); */
    /* } */

    /* Strategy *strategies = get_strategies(contents, filesize); */

    /* int part_1_score = 0; */

    /* for (int i = 0; i < filesize; i++) { */
    /*   Strategy strat = strategies[i]; */
    /*   printf("left: %d, right: %d\n", strat.left, strat.right); */
    /*   int score = get_score_for_strat(strat); */
    /*   printf("outcome: %d\n", score); */
    /*   part_1_score += score; */
    /* } */

    /* printf("part 1 score: %d\n", part_1_score); */

    free(contents);
    /* free(strategies); */

    return 0;
  }

  printf("expected file path");
  return 1;
}
