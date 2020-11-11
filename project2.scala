object homework4  {
    //global variable for local restaurants
    var localRestaurants : List[Array[String]] = List()
    //global var with list of peoples data
    var people : List[List[Array[String]]] = List()

    def filterChains(): List[List[Array[String]]] = { 
        var newList = people.map(person => person.filter{_(1).equals("Chain")})
        return newList
    }

    //funtion to union 2 maps and add their scores together
    def mergeMap[A, B](ms: List[Map[A, B]])(f: (B, B) => B): Map[A, B] =
        (Map[A, B]() /: (for (m <- ms; kv <- m) yield kv)) { (a, kv) =>
            a + (if (a.contains(kv._1)) kv._1 -> f(a(kv._1), kv._2) else kv)
        }

    def main(args: Array[String]) {

        //read in local restaurant file
        val temp: List[String] = io.Source.fromFile(args(0)).getLines.toList
        //set global variable data
        localRestaurants = temp.map(x => x.split(",").map(_.trim))

        //filter args for only people files
        val tempArgs = args.indices.collect{case i if i > 0 => args(i)} //only the people file

        //create list of lists (unparsed lines)
        val tempPeople = tempArgs.map(arg => io.Source.fromFile(arg).getLines.toList)
        
        //map data to global variable
        people = tempPeople.map(person => person.map(x =>x.split(",").map(_.trim))).toList

        //filter data for only chain restaurants
        var list = filterChains()
        var numList = list.map(people => people.length)
        
        //group results by cuisine type (creates map)
        var bop = list.map(person => person.groupBy(_(2)))
        //creates list of maps for cuisine average
        //List[Map[String,List[Array[Int]]]]
        var cuisineAverage = bop.map( person => person.map{ case (k,v) => (k, List((v.map(_(4).toInt).sum.toDouble/v.length), v.length))})
        
        //calculate theta for cuisine from averages
        var adjustedAverage = cuisineAverage.map(person => person.map{case (k,v) => (k,(v.product/numList(cuisineAverage.indexOf(person))))})
        
        //intersect peoples's cuisine preferences
        //map will only contain cuisine that is owned by everyone
        var intersects = adjustedAverage(0).map{ case (k,v) => (k,List(v*0))}
        var templist = adjustedAverage.map(people => intersects = intersects.keySet.intersect(people.keySet).map(k => k->(people(k)::intersects(k))).toMap)
        
        //sort map alphabetically
        //vars is alphabetical map
        var vars = intersects.map{ case (k,v) => (k, v.sum)}.toSeq.sortBy(_._1).toMap
        
        //find coorisponding local restaurants and set them to key values
        //if restaurant not found, key is set to None
        var next = vars.map{case (k,v) => (localRestaurants.find(_(1) == k), v)}

        //filter out cuisines that could not be found
        next = next.filterKeys(_ != None)

        //--------------------TRAY------------------------------
        //CHECK IF NEXT IS EMPTY FOR IF STATEMENT HERE

        //retrieve restaurant with highest score
        //if there are no restaurants, maxBy throws error so must check for no restaurants before this 
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
				var name = restaurant.toArray
				print("Your party should go to: ")
				println(name(0)(0))
			}
		}

        //logistics to retrieve restaurant name
        
    
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
