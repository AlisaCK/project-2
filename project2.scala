object homework4  {
    var localRestaurants : List[Array[String]] = List()
    
    var people : List[List[Array[String]]] = List()

    def filterChains(): List[List[Array[String]]] = { 
        var newList = people.map(person => person.filter{_(1).equals("Chain")})
        return newList
    }

    def mergeMap[A, B](ms: List[Map[A, B]])(f: (B, B) => B): Map[A, B] =
        (Map[A, B]() /: (for (m <- ms; kv <- m) yield kv)) { (a, kv) =>
            a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
        }

    def main(args: Array[String]) {

        //println("Month, Income, Expenses, Profit")
        //first argument load in local restaurants
        val temp: List[String] = io.Source.fromFile(args(0)).getLines.toList
        localRestaurants = temp.map(x => x.split(",").map(_.trim))

        // for(lines <- localRestaurants){
        //     print(lines(0))
        // }

        val tempArgs = args.indices.collect{case i if i > 0 => args(i)} //only the people file
        //create list of lists (unparsed lines)
        val tempPeople = tempArgs.map(arg => io.Source.fromFile(arg).getLines.toList)
        // for(lines <- tempPeople){
        //     println(lines(0))
        // }
        people = tempPeople.map(person => person.map(x =>x.split(",").map(_.trim))).toList
        // for(lines <- people){
        //     println(lines(1)(0))
        // }
        var list = filterChains()
        var numList = list.map(people => people.length)
        //var list = people.map(person => person.filter{_(1).equals("Chain")})

        // for(person <- list){
        //     for(restaurants <- person){
        //         println(restaurants(1))
        //     }
        // }
        
        var bop = list.map(person => person.groupBy(_(2)))
        var cuisineAverage = bop.map( person => person.map{ case (k,v) => (k, List((v.map(_(4).toInt).sum.toDouble/v.length), v.length))})
        //List[Map[String,List[Array[Int]]]]
        //v is list of restaurants of a type of cuisine
        
        // for(person <- cuisineAverage ){
        //     for(restaurants <- person){
        //         println(restaurants)
        //     }
        //     println("wow")
        // }
		println(localRestaurants(1).toArray.toString)
        var adjustedAverage = cuisineAverage.map(person => person.map{case (k,v) => (k,(v.product/numList(cuisineAverage.indexOf(person))))})
        // for(person <- adjustedAverage ){
        //     for(restaurants <- person){
        //         println(restaurants)
        //     }
        //     println("wow")
        // }
        var templist = adjustedAverage(0).map{ case (k,v) => (k,List(v*0))}
        var intersects = adjustedAverage.map(people => templist = templist.keySet.intersect(people.keySet).map(k => k->(people(k)::templist(k))).toMap)
        //println(templist)
        var vars = templist.map{ case (k,v) => (k, v.sum)}.toSeq.sortBy(_._1).toMap
        //println(vars)
        var next = vars.map{case (k,v) => (localRestaurants.find(_(1) == k), v)}
        next = next.filterKeys(_ != None)
        var (restaurant, score) = next.maxBy(_._2)
        if (restaurant != None) {
			var hello = restaurant.toArray
			print("Your party should go to: ")
			println(hello(0)(0))
		}	
        // var tempp  = localRestaurants.find(_(1) == rest) 
        // var restaurant = tempp match {
        //     case None => localRestaurants(1)//Or handle the lack of a value another way: throw an error, etc.
        //     case Some(v) => v.toArray //return the string to set your value
        // }
        // print("Your party should go to: ")
        // println(restaurant(0))
        // for(restaurants <- restaurant){
        //     println(restaurants)
        // }
        //println("intersects")
		else {
			var union = mergeMap(adjustedAverage)((v1, v2) => v1 + v2)
			
			//for(restaurants <- union){
			//    println(restaurants)
			//}
			//println("union")
			
			var filtered = union.map{case (k,v) => (localRestaurants.find(_(1) == k), v)}
			filtered = filtered.filterKeys(_ != None)
			var (res, sc) = filtered.maxBy(_._2)
			if (res != None) {
				var goodbye = res.toArray
				print("Your party should go to: ")
				println(goodbye(0)(0))
			}
			else {
				print("Your party should go to: ")
				println(localRestaurants(1))
			}
		}
    
        //val bufferLocalRes = io.Source.fromFile(arg(0))
        // for (line <- bufferLocalRes.getLines) {
        //     val cols = line.split(",").map(_.trim)
        //     localRestaurants += ((s"${cols(0)}" ->List(s"${cols(1)}", s"${cols(2)}"))
        //     // do whatever you want with the columns here
        //     //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
        // }
        // bufferLocalRes.close
        //rest of arguments load into people list
        // for(i <- 1 to args.length-1){
        //     val bufferPeople = io.Source.fromFile(arg(i))
        //     for (line <- bufferPeople.getLines) {
        //         val cols = line.split(",").map(_.trim)
        //         people
        //         peopleRes += ((s"${cols(0)}" ->List(s"${cols(1)}", s"${cols(2)}"))
        //         // do whatever you want with the columns here
        //         //println(s"${cols(0)}|${cols(1)}|${cols(2)}|${cols(3)}")
        //     }
        //     bufferLocalRes.close
        // }

        
    } 

    
}
