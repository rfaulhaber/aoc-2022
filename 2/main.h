#ifndef MAIN_H_
#define MAIN_H_

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

/*
 * A, X = rock
 * B, Y = paper
 * C, Z = scissors
 *
 * rock beats scissors
 * paper beats rock
 * scissors beats paper
 *
 * so
 * A X = draw
 * A Y = lose
 * A Z = win
 * B X = win
 * B Y = draw
 * B Z =
 */
enum left_strat { A = 1, B, C };
enum right_strat { X = 1, Y, Z };
enum outcome { WIN = 6, DRAW = 3, LOSE = 0 };

typedef struct strategy {
  enum left_strat left;
  enum right_strat right;
} Strategy;

char **get_lines(char *filename, size_t *length);

#endif // MAIN_H_
