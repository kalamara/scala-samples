#include <stdio.h>
#include <math.h> 
#include <stdbool.h>
#include <stdlib.h>
#include <string.h>
#include <inttypes.h>

 /** 
    * https://en.wikipedia.org/wiki/Lattice_path
    * lattice paths from (0,0) to (n,k) are (n+k)!/n!k!
    * for n = k that is (2n)!/n!n! = 2n*(2n-1)*...*(n+1) / n*(n-1)*(n-2)*...*2*1  
    * that is (2n/n)*(2n-1/n-1)*...*(n+1/1)
    */
double lattice(int n)
{
    int i = 1;
    double accumulator = 1;
    for(;i<=n;i++)
    {
        double numerator = (double)(i + n);
        double denominator = (double)i;
        accumulator *= (numerator / denominator);
    } 
    return accumulator;
}

/**prime integers solution*/
bool isPrime(unsigned int n)
{
    if(n < 2)
        return true;
    double sq = sqrt((double)n);
    int s = floor(sq);
    int i = 2;
    for(; i <= s; i++)
    {
        if(n%i == 0)
            return false;
    }
    return true;
}
/** all primes up to n*/
unsigned int allPrimes(unsigned int n, unsigned int **primes)
{   
    unsigned int i = 2;
    unsigned int c = 0;
    unsigned int buf[n];
    for(; i <= n; i++)
    {
        if(isPrime(i))
        {
 /*           printf("%d is prime\n", i);*/
            buf[c] = i;
            c++;
        }
    }
    *primes = (unsigned int *)malloc(sizeof(unsigned int)*c);
    for(i = 0; i < c; i++)
        (*primes)[i] = buf[i];
    return c;               
}
/**a custom HashMap*/
typedef struct pair
{
    unsigned int value;
    unsigned int times;
} * map;

/**print a map*/
void dump(const map primes, int size)
{
    unsigned int i = 0;
    for(;i<size;i++)
       // if(primes[i].times)
            printf("prime %d %d times\n", primes[i].value, primes[i].times);
}

/**multiply integers in the form of prime numbers products*/
multiply(const map mult1, 
         const map mult2,
         map *res, 
         unsigned int size)
{
    unsigned int i = 0;
    for(;i<size;i++)
    {
            (*res)[i].value = mult1[i].value;
            (*res)[i].times = mult1[i].times + mult2[i].times;    
    }
}

/**divide integers in the form of prime numbers products*/
void divide(const map dividend, 
            const map divisor, 
            map *res,
            unsigned int size)
{
    unsigned int i = 0;
    for(;i<size;i++)
    {
            (*res)[i].value = dividend[i].value;
            (*res)[i].times = dividend[i].times - divisor[i].times;    
    }
}

/**multiply all primes in a map and produce total value*/
double sum(const map product, unsigned int size)
{
    unsigned int i = 0;
    double multiplier = 1;	
    
    for(;i<size;i++)
    {
	unsigned int j = 0;    
	for(;j<product[i].times;j++)
    	    multiplier *= product[i].value;
    }
    return multiplier;
}

unsigned int *Primes = NULL;
unsigned int nPrimes = 0;

/** break to primes*/
toPrimes(int n, map *primeMap)
{
    unsigned int j = 0;
    int m = n;
    for(;j<nPrimes;j++)
    {   
        (*primeMap)[j].value = Primes[j];
        (*primeMap)[j].times = 0;
        while(m % Primes[j] == 0)
        {
                m = m / Primes[j];
                (*primeMap)[j].times++;
        }
    }
    //dump(*primeMap, nPrimes);
}

/**solution*/
map plattice(unsigned int n)
{
    unsigned int bytes = sizeof(struct pair)*nPrimes;

    map numerator = (map)malloc(bytes);
    map denominator = (map)malloc(bytes);
    
    map accnum = (map)malloc(bytes);
    map accden = (map)malloc(bytes);
    map fraction  = (map)malloc(bytes);

    memset(accnum, 0, bytes); 
    memset(accden, 0, bytes);
    memset(fraction, 0, bytes);

    int i = n;
    for(;i>0;i--)
    {
        toPrimes(i + n, &numerator);
        toPrimes(i, &denominator);
       
        multiply( numerator, 
                  accnum,      
                  &accnum,
                  nPrimes);
	
	//printf("numerator (%d + %d):\n",i, n);
       // dump(accnum, nPrimes);
	
	multiply( denominator, 
                  accden,      
                  &accden,
                  nPrimes);
	
	//printf("denominator (%d):\n", i);
       // dump(accden, nPrimes);
    }
	
    divide(accnum, 
           accden, 
           &fraction,
           nPrimes),       
    //printf("fraction:\n");
   // dump(fraction, nPrimes); 
                              
    free(numerator);
    free(denominator);
    free(accnum);
    free(accden);
    return fraction;
}


void main(int argc, char *argv[])
{
    if(argc !=2)
        printf("wrong arguments\n");
    else
    {
        int n = atoi(argv[1]);
        double f;
        int i = 1;
        for(;i<=1000000;i++)
            f = lattice(n);
            
        printf("%d: double solution: %f\n",n,f);
        
        map p = NULL;
        for(i = 1;i<=1000000;i++)
	{
		nPrimes = allPrimes(2*n, &Primes);
        	p = plattice(n);		
		f = sum(p, nPrimes);
		if(p)
		{
            		free(p);
			p = NULL;			
		}
        	if(Primes)
		{
            		free(Primes);
			Primes = NULL;
		}
	}

        printf("%d: primes solution: %f\n",n,f);
    }
}


