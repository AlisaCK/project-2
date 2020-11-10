object homework4  {
    var localRestaurants : List[Array[String]] = List()
    
    var people : List[List[Array[String]]] = List()

    def filterChains(): List[List[Array[String]]] = { 
        var newList = people.map(person => person.filter{_(1).equals("Chain")})
        return newList
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
        for(lines <- tempPeople){
            println(lines(0))
        }
        people = tempPeople.map(person => person.map(x =>x.split(",").map(_.trim))).toList
        for(lines <- people){
            println(lines(1)(0))
        }
        var list = filterChains()
        //var list = people.map(person => person.filter{_(1).equals("Chain")})

        for(person <- list){
            for(restaurants <- person){
                println(restaurants(1))
            }
        }
        
        var bop = list.map(person => person.groupBy(_(2)))
        var cuisineAverage = bop.map( person => person.map{ case (k,v) => (k, ((v.map(_(4).toInt).sum/v.length), v.length))})
        //List[Map[String,List[Array[Int]]]]
        //v is list of restaurants of a type of cuisine
        
        for(person <- cuisineAverage ){
            for(restaurants <- person){
                println(restaurants)
            }
            println("wow")
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