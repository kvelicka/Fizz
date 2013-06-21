
#include <stdio.h>

int main()
{
FILE * f = fopen("SCHL.v5d", "rb");
int i, j;
int vv;
unsigned char v[4];

for (i = 0; i < 670; i++)
  {
  printf("%d:\t", i);
  for (j = 0 ; j < 4 ; j++)
    {
    v[j] = (unsigned char)fgetc(f);
    printf( "%d\t", v[j]);
    }
  vv = v[3] + v[2]*256 + v[1]*256*256;
  printf("\t\t%d\n", vv);
  }

}

