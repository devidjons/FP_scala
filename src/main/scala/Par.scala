class Par[A](value : =>A) {

}
object Par {
    def unit[A](a: => A):Par[A] = ???
    def map2[A,B,C](p1:Par[A], p2:Par[B])(f:(A,B)=>C):Par[C]={
        ???
    }
}
