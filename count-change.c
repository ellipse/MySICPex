#include <stdlib.h>
#include <stdio.h>

struct method {
	unsigned	 kinds; //kinds of coins
	unsigned	 num;   //number of changes correspond
	struct method	*next;  //pointer to next kind
};
typedef method		 MTHD;
struct record{
	unsigned	 A;	//amount of coins
	MTHD		*M;
	struct record *next;
};
typedef struct record RCRDs
RCRDs			*R;

const unsigned COINS[6] = {0, 1, 5, 10, 25, 50};

static void	Append(RCRDs	*R, unsigned amount, unsigned kind, unsigned ways);
static int	InRecords(RCRDs *R, unsigned amount, unsigned kind);

int main(void)
{
	if (NULL == (R = (RCRDs*)malloc(sizeof(RCRDs)))) {
		fprintf(stderr, "error in malloc R");
		exit(1);
	}
	scanf("%d", &(R->))
        return 0;
}














