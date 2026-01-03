#include <stdlib.h>
#include <stdio.h>

#define MAX_SYLOW_NUM 100

static int num_primes;
static int primes[30];
static int powers[30];
static int nsylow[30];
static int sylow[30][MAX_SYLOW_NUM];

static void setup_prime_info(int g);
static int gcd(int a, int b);
static int ipow(int x, int n);
static int isqrt(int n);

int main(int argc, char *argv[])
{
	int g, i, j;
	int p;
	int done;
	int foo, first;
	int min_index;
	int biggest_that_works;
	int elts;
	int advanced = 0;  /* Use "advanced" techniques */

	if (argc == 1)
		return 0;
	if (argc > 2)
		advanced = 1;
	for (g=2; g<=atoi(argv[1]); g++)
	{
		printf("%d\t", g);
		setup_prime_info(g);

		if (num_primes == 1 && g == primes[0])
		{
			printf("prime number\n");
			continue;
		}

		if (num_primes == 1)
		{
			printf("power of %d\n", primes[0]);
			continue;
		}

		min_index = 2;
		j = g;
		while (j > 1)
		{
			min_index++;
			j /= gcd(j, min_index);
		}

		/* Calculate the Sylow numbers */
		done = 0;
		for (i=num_primes-1; i>=0; i--)
		{
			nsylow[i] = 0;
			biggest_that_works = 0;
			foo = g/ipow(primes[i],powers[i]);
			for (j=1; j<=foo; j++)
				if (g % j == 0 && j % primes[i] == 1)
				{
					biggest_that_works = j;
					if (j >= min_index)
					{
						sylow[i][nsylow[i]] = j;
						nsylow[i]++;
						if (nsylow[i] == MAX_SYLOW_NUM)
						{
							printf("\nerror\n");
							return 0;
						}
					}
				}
			if (biggest_that_works == 1)
			{
				printf("normal Sylow %d-subgroup\n", primes[i]);
				done = 1;
				break;
			}
			if (nsylow[i] == 0)
			{
				printf("<= %d Sylow %d-subgroups, and %d does not divide %d!/2\n", biggest_that_works, primes[i], g, biggest_that_works);
				done = 1;
				break;
			}
		}
		if (done) continue;

		done = 0;
		if (advanced)
		    for (i=num_primes-1; i>=0; i--)
			if (powers[i] <= 2 && sylow[i][nsylow[i]-1] * ipow(primes[i],powers[i]) == g)
			{
				if (--nsylow[i] == 0)
				{
					printf("Burnside's Transfer Theorem eliminated the Sylow %d number\n", primes[i]);
					done = 1;
					break;
				}
			}
		if (done) continue;

		if (g % 4 == 2)
		{
			printf("subgroup of index 2 (order is 2 * odd)\n");
			continue;
		}

		/* Try stupid element counting */
		elts = 1;
		for (i=0; i<num_primes; i++)
		{
			if (powers[i] == 1)
				elts += (primes[i] - 1) * sylow[i][0];
			else
				elts += ipow(primes[i], powers[i]);
#if 0
			{
			}
#endif
		}
		if (elts > g)
		{
			printf("Too many elements in Sylow subgroups (\"stupid\" element counting)\n");
			continue;
		}

		if (advanced && num_primes == 2)
		{
			printf("Burnside's \"p^a q^b\" Theorem\n");
			continue;
		}

		if (advanced && g % 2 == 0 && (g % 12 != 0 && g % 8 != 0))
		{
			printf("Frobenius transfer theorem for p=2 (4 but not 3)\n");
		}
		else if (advanced && g % 2 == 0 && (g % 12 != 0 && g % 56 != 0 && g % 16 != 0))
		{
			printf("Frobenius transfer theorem for p=2 (8 but not 3 or 7)\n");
		}
		else if (advanced && g % 2 == 0 && (g % 12 != 0 && g % 56 != 0 && g % 80 != 0 && g % 32 != 0))
		{
			printf("Frobenius transfer theorem for p=2 (16 but not 3, 5, or 7)\n");
		}
		else if (advanced && g % 2 == 0 && (g % 12 != 0 && g % 56 != 0 && g % 80 != 0 && g % (32*31) != 0) && g % 64 != 0)
		{
			printf("Frobenius transfer theorem for p=2 (32 but not 3, 5, 7, or 31)\n");
		}
		else if (advanced && g % 2 == 0 && (g % 12 != 0 && g % 56 != 0 && g % 80 != 0 && g % (32*31) != 0) && g % 128 != 0)
		{
			printf("Frobenius transfer theorem for p=2 (64 but not 3, 5, 7, or 31)\n");
		}

		printf("***** FAILED *****\n");
		printf("You need to deal with this:\n");
		printf("\t%d =", g);
		p = 1;
		foo = g;
		first = 1;
		for (i=0; i<num_primes; i++)
		{
			if (!first) printf(" *"); else first = 0;
			if (powers[i] == 1)
				printf(" %d", primes[i]);
			else
				printf(" %d^%d", primes[i], powers[i]);
		}
		printf("\n");
		printf("\tMinimal subgroup index = %d\n", min_index);
		printf("\tPossible Sylow numbers:\n");
		p = 1;
		for (i=0; i<num_primes; i++)
		{
			p = primes[i];
			printf("\tn_%d = ", p);
			first = 1;
			for (j=0; j<nsylow[i]; j++)
				if (sylow[i][j] >= min_index)
				{
					if (!first)
						printf(", ");
					else
						first = 0;
					printf("%d", sylow[i][j]);
				}
			printf("\n");
		}
	}

	return 0;
}

static void setup_prime_info(int g)
{
	int d = 2;
	int sqrtg = isqrt(g);

	num_primes = 0;
	while (d <= sqrtg)
	{
		if (g % d == 0)
		{
			primes[num_primes] = d;
			powers[num_primes] = 0;
			while (g % d == 0)
			{
				g /= d;
				powers[num_primes]++;
			}
			num_primes++;
			sqrtg = isqrt(g);
		}
		d++;
	}
	if (g != 1)
	{
		if (num_primes > 0 && g == primes[num_primes-1])
		{
			powers[num_primes-1]++;
			printf("IMPOSSIBLE\n");
			exit(1);
		}
		else
		{
			primes[num_primes] = g;
			powers[num_primes] = 1;
			num_primes++;
		}
	}
}

static int gcd(int a, int b)
{
	int t;

	if (a<b)
		return gcd(b, a);

	while (b != 0)
	{
		t = b;
		b = a % b;
		a = t;
	}

	return a;
}

static int ipow(int x, int n)
{
	int ans = 1;
	int i;

	for (i=0; i<n; i++)
		ans *= x;

	return ans;
}

static int isqrt(int n)
{
	int s = n-1;
	int sold = n;

	if (n == 1) return 1;

	while (sold > s)
	{
		sold = s;
		s = (s + n/s)/2;
	}

	return s;
}
