/* fixed precision values are packed into a single Word32 */
typedef unsigned int sample;
typedef int bool;
#define true  1
#define false 0

/* representation conversion for samples */
#define mantissa(x) (((signed)(((sample)x&0x0000ffff)<<16))>>16)
#define exponent(x) (((signed)x)>>16)
sample sampleOf   (int m, int e);
int    intOf      (sample s);	/* samples < 1 are clamped to 0 */
float  floatOf    (sample s);	/* may lose precision for large/small values */

sample fixedPrecMult  (sample v0, sample v1);
sample fixedPrecPlus  (sample v0, sample v1);
sample fixedPrecMinus (sample v0, sample v1);
sample fixedPrecNegate(sample v);
sample fixedPrecSqroot(sample v);
void   fixedPrecRead  (FILE*f, sample *v, int n);
void   fixedPrecWrite (FILE*f, sample  v);

bool   lessThan       (sample a, sample b);
bool   greaterThan    (sample a, sample b);

sample fixedPrecAvg   (sample v0, sample v1);
