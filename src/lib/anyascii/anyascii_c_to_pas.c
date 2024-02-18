/*
 This is the program used to convert the C data tables of anyascii to Pascal.

 This work is marked with CC0 1.0. To view a copy of this license, visit
 http://creativecommons.org/publicdomain/zero/1.0
*/

#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include "anyascii.c"

void pc(int c, int tryprint, int comma, int subst0x8n)
{
	c &= 0xff;
	if (tryprint && c >= ' ' && c < '~' && c != '\'')
		printf("'%c'", c);
	else if (c < 10)
		printf("C(%i)", c);
	else if (c >= 0x82 && c <= 0x89 && c != 0x83 && subst0x8n)
		printf("N%d", c - 0x80);
	else
		printf("C($%x)", c);
	if (comma)
		putchar(',');
}

int main(void)
{
	const char *b;
	unsigned int n, i, j, entries, banklen = 0, l, o;
	unsigned char start[0xffff+0x7f+1] = {0};
	unsigned int *ranges = NULL, nranges = 0, prevstate = 0, thisstate;

	printf("type C = AnsiChar;\n");
	printf("{$MACRO ON}\n");
	printf("{$DEFINE N0:=C(0),C(0),C($80)}\n");
	printf("{$DEFINE N1:=C(0),C($81)}\n");
	for (i = 2; i <= 9; i++)
		if (i != 3)
			printf("{$DEFINE N%d:=C($8%d)}\n", i, i);

	for (n = 0; n < 0x1000000; n++) {
		b = block(n);
		thisstate = 0;
		if (b) {
			entries = (b[0] & 0xff) + 1;
			if (entries != 1 || (b[3] & 0xff) != 0x80)
				thisstate = 1;
		}
		if (thisstate != prevstate) {
			ranges = realloc(ranges, ++nranges * sizeof(*ranges));
			ranges[nranges - 1] = n;
			prevstate = thisstate;
		}
		if (!thisstate)
			continue;
		printf("const b%03x: array [0 .. %u] of C = ( C(%i),\n", n, entries * 3, b[0] & 0xff);
		for (i = 0; i < entries; i++) {
			l = b[i * 3 + 3];
			o = 0x10000;
			if (l & 0x80) {
				l &= 0x7f;
				if (l > 3) {
					o = ((b[i * 3 + 1] & 0xff) << 8) + (b[i * 3 + 2] & 0xff);
					start[o] = 1;
					if (banklen < o + l)
						banklen = o + l;
				}
			} else
				l = 3;
			putchar('\t');
			if (l == 0)
				printf("N0%s", i + 1 < entries ? "," : "");
			else if (l == 1) {
				pc(b[i * 3 + 1], 1, 1, 0);
				printf("N1%s", i + 1 < entries ? "," : "");
			} else
				for (j = 0; j < 3; j++)
					pc(b[i * 3 + 1 + j], l < 4, j < 2 || i + 1 < entries, j == 2);
			putchar('\n');
		}
		printf(");\n\n");
	}
	if (thisstate) {
		ranges = realloc(ranges, ++nranges * sizeof(*ranges));
		ranges[nranges - 1] = n;
	}
	start[banklen] = 1;
	printf("const bank: array [0 .. %u] of C = (\n", banklen - 1);
	j = 0;
	for (i = 0; i < banklen; i++) {
		if (!j) {
			j = 1;
			printf("{%04x}\t", i);
		}
		pc(bank[i], 1, i + 1 < banklen, 0);
		if (start[i + 1]) {
			putchar('\n');
			j = 0;
		}
	}
	printf(");\n\n");
	printf("function block(blocknum: cardinal): PAnsiChar;\nbegin\n\tcase blocknum of\n");
	for (i = 0; i < nranges; i+=2) {
		for (j = ranges[i]; j < ranges[i + 1]; j++)
			printf("\t$%03x: block := @b%03x[0];\n", j, j);
	}
	printf("\telse block := nil;\n\tend;\nend;\n");
	return 0;
}
