package streams


object Streams1 {
  println("Welcome to the Scala worksheet")       //> Welcome to the Scala worksheet
  
  val vec : Vector[Char] = Vector('a','b','c')    //> vec  : Vector[Char] = Vector(a, b, c)
  
  val vec2 : Vector[Vector[Char]] = Vector(vec,Vector('z'),Vector())
                                                  //> vec2  : Vector[Vector[Char]] = Vector(Vector(a, b, c), Vector(z), Vector())
  
  	vec.indexWhere(x => x == 'b')             //> res0: Int = 1
  	val row = vec2.indexWhere(v  => v.indexOf('b') > -1 )
                                                  //> row  : Int = 0
    val col = vec2(row).indexOf('b')              //> col  : Int = 1
    
}