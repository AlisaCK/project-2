object project 2 {
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

        //check if step 4 has available restaurants
		if (!next.isEmpty) {
        //retrieve restaurant with highest score
        //if there are no restaurants, maxBy throws error so must check for no restaurants before this 
			var (restaurant, score) = next.maxBy(_._2)
			var hello = restaurant.toArray
			print("Your party should go to: ")
			println(hello(0)(0))
		}	
        //if not, switch to step 5
		else {
			var union = mergeMap(adjustedAverage)((v1, v2) => v1 + v2)
			
			
			var filtered = union.map{case (k,v) => (localRestaurants.find(_(1) == k), v)}
			filtered = filtered.filterKeys(_ != None)
			
			if (!filtered.isEmpty) {
				var (res, sc) = filtered.maxBy(_._2)
				var goodbye = res.toArray
				print("Your party should go to: ")
				println(goodbye(0)(0))
			}
            //step 6
			else {
				var name = localRestaurants.toArray
				print("Your party should go to: ")
				println(name(1)(0))
			}
		}

        
    } 

    
}
