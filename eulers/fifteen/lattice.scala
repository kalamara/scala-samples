import scala.math.pow
object lattice extends App {
/** 
* https://en.wikipedia.org/wiki/Lattice_path
* lattice paths from (0,0) to (n,k) are (n+k)!/n!k!
* for n = k that is 
* (2n)!/n!n! = 2n*(2n-1)*...*(n+1) / n*(n-1)*(n-2)*...*2*1  
* that is 
* (2n/n)*(2n-1/n-1)*...*(n+1/1)
*/

/**real numbers solution*/
def lat(n:Int) = {
    var accumulator : Double = 1.0f;
    for(i<-1 to n){
        val numerator = (i + n).toDouble;
        val denominator = i.toDouble;
        accumulator = accumulator * (numerator / denominator);
    }
    accumulator
}

/** real numbers, tail recursion*/
def fLat(n: Int) = {
    def coeff(x: Double) : Double = (x+n) / x

    def loop(acc: Double, i: Int): Double = {
        if (i == 0) 
            acc
        else 
            loop(acc * coeff(i), i-1)
    }
    loop(1, n)
}

/** prime integers solution */

/**find all primes recursively*/
def isPrime(n: Int) : Boolean = {
    Primes.takeWhile(p => p*p <= n)
          .forall(n % _ != 0)
}
val Primes = 2 #:: Stream.from(3).filter(isPrime)

/** how many times can you divide m by p */
def times( p:Int, m:Int ) : Int = {
        if(m % p == 0)
            times(p, m / p) + 1
        else 0
}
    
/**break n to product of primes */
def toPrimes( n:Int ) : Map[Int,Int] = {
    val product = Map(1->0);

    /**break m to primes recursively starting from prime i*/
    /*@tailrec*/
    def toPrimesFrom( i:Int, 
                      m:Int,
                      product: Map[Int,Int]) : Map[Int,Int] = {
        val prime = Primes(i);
        if(m >= prime)
            toPrimesFrom(i+1, m, product + (prime -> times(prime, m)))
        else 
            product
    }      
    toPrimesFrom(0, n, product)        
}

/**multiply all primes in a map and produce total value*/
def sum(m: Map[Int,Int]) : Double = {
    m.map(x=> pow(x._1, x._2)).product 
}

/**map a key to the sum of the list*/
def addValues(key: Int, values: List[(Int,Int)]) = {
  key -> values.map(_._2).sum
}

/**multiply integers in the form of prime numbers products*/
def fmultiply(a: Map[Int, Int],
              b: Map[Int, Int]) : Map[Int,Int] = {
  (a.toList ::: b.toList).groupBy(_._1)
                          .map(t => addValues(t._1, t._2))
}

/*negate the second part of a pair*/
def negateValue(key: Int, 
                value: Int) : (Int,Int) = {
  (key, value*(-1))
}

/**divide integers in the form of prime numbers products*/
def fdivide(a: Map[Int, Int],
            b: Map[Int, Int]) : Map[Int,Int] = {
  (a.toList ::: b.toList
                 .map(x => negateValue(x._1, x._2)))
    .groupBy(_._1)
    .map(x => addValues(x._1, x._2))
}

def pLat(n:Int) = {
    /**return n!/m!, m < n expressed as primes product*/
    /*@tailrec*/
    def coeff(n : Int, 
              m : Int) : Map[Int, Int] = {
        if(n > m
        && m > 0) //println(n + " is " + nP);
           fmultiply(toPrimes(n), coeff(n-1, m))
        else
           Map(1->1)      
        }
    val numerator = coeff(2*n, n);
    val denominator = coeff(n, 1); 
    sum(fdivide(numerator,denominator))
}

val n = args(0).toInt;
var f : Double = 0.0f;
for(i <- 1 to 1000)
    //f = fLat(n);
    //f = lat(n);
f = pLat(n);
println(f);
}

    


