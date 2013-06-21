#include <stdio.h>
#include <math.h>
#include <arpa/inet.h>		/* htonl() and ntohl() */
#include "fixedPrecisionC.h"
#include <stdlib.h>

/* eek, C does not have a 'to the power of' operator */
int power10 (int e) {	/* positive exponents only! */
  int i, n=1;
  for (i=0; i<e; i++)
    n = n*10;
  return n;
}

/* convert to a float representation */
float floatOf (sample s) {
    int m, e;
    sample sh = ntohl(s);
    m = mantissa(sh);
    e = exponent(sh);
    if (e <= -32) return 0;
    if (e<0) return (float)((double)m / (double)power10(-e));
    else     return (float)((double)m * (double)power10(e));
}

int signum (int i) {
  return (i>0 ? 1 : (i<0 ? -1 : 0));
}

/* representation conversion */
sample sampleOf (int m, int e) {
  if (m==0) return 0x0;
  if (abs(m) < 1000) return sampleOf(m*10,e-1);
  else if (abs(m) > 9999) {
    int d = m%10;   /* safe? */
    int m0 = m/10;  /* safe? */
    if (d >= 5)       return sampleOf(m0+1,e+1);
    else if (d <= -5) return sampleOf(m0-1,e+1);
    else              return sampleOf(m0,e+1);
  } else return (sample)((e<<16)|((unsigned)m&0x0000ffff));
}

/* CAUTION: note the zero result for exponents < -3 and
   that large exponents will cause overflow. */
int intOf (sample s) {
  int e = exponent(s);
  if (e < -3) return 0;
  if (e < 0) return mantissa(s) / power10(-e);
  return mantissa(s) * power10(e);
}

/* operations on fixed precision values */
sample fixedPrecMult(sample v0, sample v1) {
  return sampleOf(mantissa(v0)*mantissa(v1),exponent(v0)+exponent(v1));
}

sample fixedPrecDivide(sample v0, sample v1) {
  int m0 = mantissa(v0);
  int e0 = exponent(v0);
  int m1 = mantissa(v1);
  int e1 = exponent(v1);
  return sampleOf(quotC(m0*10000,m1),(e0-e1)-4);
}

sample fixedPrecRecip(sample v) {
  int m = mantissa(v);
  int e = exponent(v);
  return sampleOf(quotC(10000000,m),(-e)-7);
}

sample fixedPrecPlus(sample v0, sample v1) {
  int diff = exponent(v1)-exponent(v0);
  if (mantissa(v0)==0) return v1;
  if (mantissa(v1)==0) return v0;
  if (diff > 5)  return v1;
  if (diff < -5) return v0;
  else if (diff >= 0)
     return sampleOf(mantissa(v1)*power10(diff)+mantissa(v0),exponent(v0));
  else
     return sampleOf(mantissa(v0)*power10(-diff)+mantissa(v1),exponent(v1));
}
sample fixedPrecNegate(sample v) {
  return sampleOf(-mantissa(v),exponent(v));
}
sample fixedPrecMinus(sample v0, sample v1) {
  return fixedPrecPlus(v0,fixedPrecNegate(v1));
}
/*
sample fixedPrecSqroot(sample v) {
  int m = mantissa(v);
  int e = exponent(v);
  if ((e%2)==1) { e = e-1; m = m*10; }
  return sampleOf( (int)(sqrt((double)(10000*m)) / 100), e/2 );
}
*/
sample fixedPrecSqroot(sample v) {
  int m = mantissa(v);
  int e = exponent(v);
  if (m==0 && e==0) return v;
  if (m<0) { fprintf(stderr, "sqroot: negative argument"); exit(0); }
  /*
  printf("initial sample value: m = %d, e = %d\n", m, e);
  printf("exponent modulo? two: %d\n", e%2);
  */
  if ((e%2)!=0) { e = e-1; m = m*10; }
  /* printf("evening the exponent: m = %d, e = %d\n", m, e); */
  while (m < 1000000) { m = m*100; e = e-2; }
  /* printf("magnifying  mantissa: m = intSqroot(%d) = %d; e = quot2(%d), result = %d\n",
            m, intSqrootC(m), e, quot2(e)); */
  return sampleOf(intSqrootC(m),quotC(e,2));
}

sample fixedPrecAvg(sample v0, sample v1) {
  return fixedPrecMult(fixedPrecPlus(v0,v1),sampleOf(5000,-4));
}

int quotC(int i, int j) {
  if (j==0) fprintf(stderr, "quot: zero divisor");
  /* using / only with both operands positive */
  if (i>=0) {
    if (j>0) return i/j;
    else return -(i/-j);
  } else {
    if (j>0) return -(-i/j);
    else return (-i/-j);
  }
}

int intSqrootC(int i) {
  int lo = 0;
  int hi = rootBoundC(i);
  int mid;
  /* invariant: lo*lo <= i && i <= hi*hi */
  while (hi > lo+1) {
    mid = (lo+hi) / 2;
    switch (signum(mid*mid-i)) {
    case -1:
      lo = mid; break;
    case  0:
      return mid;
    case  1:
      hi = mid; break;
    }
  }
  if (lo==hi) return hi;
  switch (signum((i-lo*lo)-(hi*hi-i))) {
  case -1:
    return lo;
  case  0:
    return lo;
  case  1:
    return hi;
  }
}

int rootBoundC (int i) {
  if (i < 100) return smallRootBound(i);
  return 10 * rootBoundC((i/100) + 1);
}

int smallRootBound(int i) {
  int squares[10] = {0,1,4,9,16,25,36,49,64,81};
  int r = 0;
  while (r < 10 && i > squares[r]) r++;
  return r;
}

void fixedPrecRead(FILE*f, sample *v, int n) {
  int i;
  fread(v, 4, n, f);
  for (i=0; i<n; i++) {
    *(v+i) = ntohl(*(v+i));
  }
}
void fixedPrecWrite(FILE*f, sample v) {
  v = htonl(v);
  fwrite(&v, 4, 1, f);
}

bool lessThan (sample a, sample b) {
  int ma = mantissa(a), mb = mantissa(b);
  if (ma==0) return 0<mb;
  if (mb==0) return ma<0;
  if (signum(ma)<signum(mb)) return true;
  if (exponent(a)<exponent(b)) return true;
  if (exponent(a)==exponent(b)) return (ma<mb);
  return false;
}
bool greaterThan (sample a, sample b) {
  int ma = mantissa(a), mb = mantissa(b);
  if (ma==0) return 0>mb;
  if (mb==0) return ma>0;
  if (signum(ma)>signum(mb)) return true;
  if (exponent(a)>exponent(b)) return true;
  if (exponent(a)==exponent(b)) return (ma>mb);
  return false;
}

/* function versions of the mantissa and exponent macros, purely for testing */
int fixedMan (sample v) { return mantissa(v); }
int fixedExp (sample v) { return exponent(v); }
