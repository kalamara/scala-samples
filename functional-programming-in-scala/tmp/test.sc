import java.util.concurrent.Executors

import part2.parallelism._

val s = Executors.newFixedThreadPool(4)

val echoer = Actor[String](s){ msg => println (s"Got message: '$msg'")}


echoer ! "yo"
echoer ! "yo"
echoer ! "yo"




