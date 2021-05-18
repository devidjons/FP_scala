abstract class Par[A] {
    def unit[A](a: => A): Par[A]
    def get[A](a: Par[A]): A
}
