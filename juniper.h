#ifndef JFE_H
#define JFE_H
#define LIBJ "libj.so"

typedef void V;
typedef intmax_t I;
typedef char C;
typedef V* J;

typedef struct A_RECORD {
  I k,flag,m,t,c,n,r,s[1];
}* A;

typedef struct AREP_RECORD {
  I n,t,c,r,s[1];
}* AREP;

int jedo(C* sentence);
void jefree();
void jefail(C* msg);
int jefirst(int type,C* arg);
J jeload(V* callbacks);
void jepath(char* arg, char* lib);

void addargv(int argc, char* argv[], C* d);
void sigint(int k);

extern J jt;
extern V* hjdll;
extern V* jdllproc;
extern V* jdlljt;

#endif
