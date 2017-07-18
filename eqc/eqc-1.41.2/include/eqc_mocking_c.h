#ifndef C_MOCK_H
#define C_MOCK_H

#include <stddef.h>
#include <stdbool.h>

/* #define NULL (void *)0x0L */

typedef enum{
  NONE, L1, L2
} XAltStatus;

typedef enum{
  SEQ, XALT, EVENT, REPL, PAR, PERM, SUCCESS
} MOCK_TYPE;

typedef enum{
  OK, NO, AMBIGUOUS
} STEP_RES;

typedef struct{
  STEP_RES res;
  void * data;
} StepResult;

typedef struct{
  int funName;
  void * args;
} MOCK_ACTION;

typedef struct{
  MOCK_TYPE type;
  void *    lang;
} MOCK_LANG;

typedef struct{
  MOCK_LANG *l1;
  MOCK_LANG *l2;
  bool     done;
} M_SEQ;

typedef struct{
  MOCK_LANG  *l1;
  MOCK_LANG  *l2;
  XAltStatus status;
} M_XALT;

typedef struct{
  MOCK_ACTION  action;
  void *       result;
  bool         visited;
} M_EVENT;

typedef struct{
  MOCK_LANG *l;
  bool      initial;
} M_REPL;

typedef struct{
  MOCK_LANG *l1;
  MOCK_LANG *l2;
} M_PAR;

typedef struct{
  int length;
  MOCK_LANG **ls;
  bool      *bs;
  int current;
} M_PERM;

void c_mock_init(MOCK_LANG*);
StepResult *c_mock_event(MOCK_ACTION *, void *);

StepResult *small_step(MOCK_ACTION *, MOCK_LANG *, bool);

extern bool eqc_mocking_match(MOCK_ACTION *, MOCK_ACTION *);
extern void eqc_mocking_assign(MOCK_ACTION *, void *, void *);

#endif

