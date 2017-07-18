#include "eqc_mocking_c.h"

extern void *malloc(unsigned long);
extern int printf(const char *, ...);

void prt_lang(MOCK_LANG *l);

MOCK_LANG *theLang;

void c_mock_init(MOCK_LANG *l){
  theLang = l;
}

StepResult *c_mock_event(MOCK_ACTION *a, void *ret){
  /* printf("C_MOCK - EVENT\r\n"); */
  /* printf("BEFORE: "); prt_lang(theLang); printf("\r\n"); */
  StepResult *step_res = small_step(a, theLang, false);
  /* printf("SmallStep result: %p (%d)\r\n", res, res->fail); */
  /* printf("AFTER:  "); prt_lang(theLang); printf("\r\n"); */
  if(step_res->res == OK)
    eqc_mocking_assign(a, step_res->data, ret);
  return step_res;
}

bool acceptEmpty(MOCK_LANG *l, bool reset){
  if(l == NULL) return false;
  switch(l->type){
  case SEQ: ;
    M_SEQ *mseq = (M_SEQ *)l->lang;
    return ((mseq->done && !reset) || acceptEmpty(mseq->l1, reset))
      && acceptEmpty(mseq->l2, reset);
  case XALT: ;
    M_XALT *mxalt = (M_XALT *)l->lang;
    if(mxalt->status == NONE || reset) return acceptEmpty(mxalt->l1, reset)
                                         || acceptEmpty(mxalt->l2, reset);
    if(mxalt->status == L1)   return acceptEmpty(mxalt->l1, reset);
    if(mxalt->status == L2)   return acceptEmpty(mxalt->l2, reset);
  case REPL: ;
    M_REPL *mrepl = (M_REPL *)l->lang;
    return (mrepl->initial || reset) || acceptEmpty(mrepl->l, reset);
  case PAR: ;
    M_PAR *mpar = (M_PAR *)l->lang;
    return acceptEmpty(mpar->l1, reset) && acceptEmpty(mpar->l2, reset);
  case PERM: ;
    M_PERM *mperm = (M_PERM *)l->lang;
    int i;
    for(i = 0; i < mperm->length; i++){
      if((!mperm->bs[i] || reset) && ! acceptEmpty(mperm->ls[i], reset)) return false;
    }
    return true;
  case EVENT: ;
    M_EVENT *mevent = (M_EVENT *)l->lang;
    return (mevent->visited && !reset);
  default:
    return true;
  }

  return false;
}

void prt_lang(MOCK_LANG *l){
  if(l == NULL) return;
  switch(l->type){
  case SEQ: ;
    M_SEQ *mseq = (M_SEQ *)l->lang;
    printf("SEQ(");
    prt_lang(mseq->l1); printf(", "); prt_lang(mseq->l2);
    printf(", %d)", mseq->done);
    break;
  case XALT: ;
    M_XALT *mxalt = (M_XALT *)l->lang;
    printf("XALT(");
    prt_lang(mxalt->l1); printf(", "); prt_lang(mxalt->l2);
    printf(", %d)", mxalt->status);
    break;
  case REPL: ;
    M_REPL *mrepl = (M_REPL *)l->lang;
    printf("REPL(");
    prt_lang(mrepl->l);
    printf(", %d)", mrepl->initial);
    break;
  case PAR: ;
    M_PAR *mpar = (M_PAR *)l->lang;
    printf("PAR(");
    prt_lang(mpar->l1); printf(", "); prt_lang(mpar->l2);
    printf(")");
    break;
  case PERM: ;
    M_PERM *mperm = (M_PERM *)l->lang;
    int i;
    printf("PERM(");
    for(i = 0; i < mperm->length; i++){
      prt_lang(mperm->ls[i]);
    }
    printf(")");
    break;
  case EVENT: ;
    M_EVENT *mevent = (M_EVENT *)l->lang;
    printf("EVENT(X, %d)", mevent->visited);
    break;
  default:
    ;
  }
}


void reset_lang(MOCK_LANG *l){
  if(l == NULL) return;
  switch(l->type){
  case SEQ: ;
    M_SEQ *mseq = (M_SEQ *)l->lang;
    reset_lang(mseq->l1); reset_lang(mseq->l2); mseq->done = false;
    break;
  case XALT: ;
    M_XALT *mxalt = (M_XALT *)l->lang;
    reset_lang(mxalt->l1); reset_lang(mxalt->l2); mxalt->status = NONE;
    break;
  case REPL: ;
    M_REPL *mrepl = (M_REPL *)l->lang;
    reset_lang(mrepl->l); mrepl->initial = true;
    break;
  case PAR: ;
    M_PAR *mpar = (M_PAR *)l->lang;
    reset_lang(mpar->l1); reset_lang(mpar->l2);
    break;
  case PERM: ;
    M_PERM *mperm = (M_PERM *)l->lang;
    int i;
    for(i = 0; i < mperm->length; i++){
      mperm->bs[i] = false;
      reset_lang(mperm->ls[i]);
    }
    mperm->current = -1;
    break;
  case EVENT: ;
    M_EVENT *mevent = (M_EVENT *)l->lang;
    mevent->visited = false;
    break;
  default:
    ;
  }
}

StepResult *mkStepRes(STEP_RES res, void * data){
  StepResult *sres = (StepResult *)malloc(sizeof(StepResult));
  sres->res = res; sres->data = data;
  return sres;
}

bool oneOf(StepResult *res1, StepResult *res2){
  if((res1->res == OK && res2->res == NO)
     || (res2->res == OK && res1->res == NO))
    return true;
  return false;
}

StepResult *mkFail(StepResult *res1, StepResult *res2){
  if(res1->res == OK && res2->res == OK){ /* Ambiguous */
    return mkStepRes(AMBIGUOUS, NULL);
  } else if(res1->res == AMBIGUOUS || res2->res == AMBIGUOUS) {
    return mkStepRes(AMBIGUOUS, NULL);
  }

  return mkStepRes(NO, NULL);
}

StepResult *mkUnexp(){
  return mkStepRes(NO, NULL);
}

StepResult *small_step(MOCK_ACTION *a, MOCK_LANG *l, bool reset){
  StepResult *res1, *res2;
  switch(l->type){
  case SEQ: ;
    M_SEQ *mseq = (M_SEQ *)l->lang;
    /* printf("MSEQ done: %d reset: %d aE: %d \r\n", mseq->done, reset, acceptEmpty(mseq->l1, reset)); */
    /* prt_lang(l); printf("\r\n"); */
    if(!mseq->done || reset){
      if(acceptEmpty(mseq->l1, reset)){
        res1 = small_step(a, mseq->l1, reset);
        res2 = small_step(a, mseq->l2, reset);

        if(!oneOf(res1, res2))
          return mkFail(res1, res2);

        if(res1->res == OK){
          if(!reset) mseq->done = false;
          return res1;
        } else {
          if(!reset) mseq->done = true;
          return res2;
        }
      } else {
        res1 = small_step(a, mseq->l1, reset);

        if(res1->res == OK && !reset){
          mseq->done = false;
        }
        return res1;
      }
    } else { /* L1 is done */
      return small_step(a, mseq->l2, reset);
    }
    break;
  case XALT: ;
    M_XALT *mxalt = (M_XALT *)l->lang;
    /* printf("MXALT status: %d reset: %d \r\n", mxalt->status, reset); */
    if(mxalt->status == NONE || reset){
      res1 = small_step(a, mxalt->l1, reset);
      res2 = small_step(a, mxalt->l2, reset);

      if(!oneOf(res1, res2))
        return mkFail(res1, res2);

      if(res1->res == OK){
        if(!reset) mxalt->status = L1;
        return res1;
      } else {
        if(!reset) mxalt->status = L2;
        return res2;
      }
    } else if(mxalt->status == L1){
      return small_step(a, mxalt->l1, reset);
    } else if(mxalt->status == L2){
      return small_step(a, mxalt->l2, reset);
    }
    break;
  case EVENT: ;
    M_EVENT *mevent = (M_EVENT *)l->lang;
    if((!mevent->visited || reset) && eqc_mocking_match(a, &mevent->action)){
      if(!reset) mevent->visited = true;
      /* Maybe result should be fun(Action -> Result) */
      return mkStepRes(OK, mevent->result);
    } else {
      return mkUnexp();
    }
  case REPL: ;
    M_REPL *mrepl = (M_REPL *)l->lang;
    /* printf("REPL%p: reset: %d aE: %d\r\n", mrepl->l, reset, acceptEmpty(mrepl->l, reset)); */
    res1 = small_step(a, mrepl->l, reset);

    if(res1->res == NO && acceptEmpty(mrepl->l, reset)){
      /* printf("REPL%p: try on reset\r\n", mrepl->l); */
      res1 = small_step(a, mrepl->l, true);
      /* printf("REPL%p: try on reset res: %d\r\n", mrepl->l, res1->fail); */
      if(res1->res == OK && !reset){
        /* printf("REPL resetting\r\n"); */
        reset_lang(mrepl->l);
          return small_step(a, mrepl->l, false);
      }
    }
    if(res1->res == OK && !reset) mrepl->initial = false;
    /* printf("REPL%p: res: %d\r\n", mrepl->l, res1->fail); */
    return res1;
  case PAR: ;
    M_PAR *mpar = (M_PAR *)l->lang;
    res1 = small_step(a, mpar->l1, reset);
    res2 = small_step(a, mpar->l2, reset);
    /* printf("PAR: res1: %d res2: %d\r\n", res1->fail, res2->fail); */
    if(!oneOf(res1, res2))
      return mkFail(res1, res2);
    if(res1->res == OK) return res1;
    return res2;
  case PERM: ;
    printf("ERROR: PERM-operation not supported in eqc_mocking_c\r\n");
    return mkUnexp();
    /* M_PERM *mperm = (M_PERM *)l->lang; */
    /* int i; */
    /* /\* printf("PERM current: %d reset: %d \r\n", mperm->current, reset); *\/ */
    /* if(mperm->current < 0 || reset){ */
    /*   for(i = 0; i < mperm->length; i++){ */
    /*     if(mperm->bs[i] && !reset) continue; /\* We are done with this one already! *\/ */
    /*     res1 = small_step(a, mperm->ls[i], reset); */
    /*     if(!res1->fail){ /\* This one is picked *\/ */
    /*       if(!reset) mperm->current = i; */
    /*       /\* printf("PERM res: %d\r\n", res1->fail); *\/ */
    /*       return res1; */
    /*     } */
    /*   }         */
    /*   /\* printf("PERM failed\r\n"); *\/ */
    /*   return mkFail(); */
    /* } else { /\* mperm->current >= 0 *\/ */
    /*   res1 = small_step(a, mperm->ls[mperm->current], reset); */
    /*   if(res1->fail && acceptEmpty(mperm->ls[mperm->current], reset)){ */
    /*     if(!reset){ */
    /*       mperm->bs[mperm->current] = true; */
    /*       i = mperm->current; */
    /*       mperm->current = -1; */
    /*     } */
    /*     /\* res1 = small_step(a, l, true); *\/ */
    /*     /\* if(!res1->fail) *\/ */
    /*     res1 = small_step(a, l, reset); */
    /*     if(res1->fail){ */
    /*       mperm->bs[mperm->current] = false; */
    /*       mperm->current = i; */
    /*     } */
    /*   } */
    /*   /\* printf("PERM res: %d\r\n", res1->fail); *\/ */
    /*   return res1; */
    /* } */
 case SUCCESS:
    return mkUnexp();
  }

  return mkUnexp();
}


