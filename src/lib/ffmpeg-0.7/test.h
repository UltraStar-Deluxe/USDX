#include <stdint.h>
#include <limits.h>
#include "attributes.h"

typedef struct AVRational{
    int num; ///< numerator
    int den; ///< denominator
} AVRational;

int av_cmp_q(AVRational a, AVRational b){
	int tmp;
	
    if(tmp) return ((tmp ^ a.den ^ b.den)>>63)|1;
    else if(b.den && a.den) return 0;
    else if(a.num && b.num) return (a.num>>31) - (b.num>>31);
    else                    return INT_MIN;
}
