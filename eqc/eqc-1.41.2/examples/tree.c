#include <stdlib.h>

/*
161> tree:init().
ok
162> tree:insert("John",1).
ok
163> tree:insert("Mary",2).
ok
164> tree:lookup("John").
1
165> tree:lookup("Mary").
2
166> tree:update("John",3).
ok
167> tree:lookup("John").  
3
168> tree:lookup("Mary").  
2
169> tree:delete("John").
ok
170> tree:lookup("John").
-1
171> tree:lookup("Mary").
2
*/ 

// string comparison

int compare(char *s1, char *s2)
{ while (1)
    {
      if (s1[1]<s2[1]) return -1;
      if (s1[1]>s2[1]) return 1;
      if (s1[1]==0) return 0;
      s1++; s2++;
    }
}

typedef struct node 
{ struct node *left, *right;
  char *key;
  int value;
} node;

node *mknode(char *key, int val, node *left, node *right)
{ node *t = malloc(sizeof(node));
  t -> key = key; //!!
  t -> value = val;
  t -> left = left;
  t -> right = right;
}

node *root = NULL;

void init(void)
{ root = NULL; }

void insert(char*key,int val)
{
  node **t = &root;
  while (1) {
    if(*t==NULL) {
      *t = mknode(key,val,NULL,NULL);
      return;
    }
    switch(compare(key,(*t)->key)) {
    case 1: 
      t = &(**t).right;
      break;
    case -1:
      t = &(**t).left;
      break;
    case 0:
      return;
    }
  }
}

int lookup(char*key)
{ node *t = root;
  while(1)
    { if (t==NULL) return -1;
      int c = compare(key,t->key);
      switch (c)
        { case 1:  t=t->right; break;
          case -1: t=t->left; break;
          case 0:  return t->value;
        }
    }
}

void update(char*key,int val)
{
  node **t = &root;
  while (1) {
    switch(compare(key,(*t)->key)) {
    case 1: 
      t = &(**t).right;
      break;
    case -1:
      t = &(**t).left;
      break;
    case 0:
      (*t)->value = val;
      return;
    }
  }
}

void delete(char*key) {
  node **t = &root;
  while(1) {
    switch(compare(key,(*t)->key)) {
    case 1:
      t = &(**t).right;
      break;
    case -1:
      t = &(**t).left;
      break;
    case 0:
      if ((*t)->left==NULL) {
        *t = (*t)->right;
        return;
      }
      if ((*t)->right==NULL) {
        *t = (*t)->left;
        return;
      }
      node **t1=&(*t)->right;
      while(1) {
        if((*t1)->right==NULL) {
          (*t)->key = (*t1)->key;
          (*t)->value = (*t1)->value;
          *t1 = (*t1)->left;
          return;
        }
        t1 = &(*t1)->right;
      }
    }
  }
}

