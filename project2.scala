object homework4  {
    var localRestaurants : List[Array[String]] = List()
    
    val people : List[List[(String, String,String, String, String)]] = List()

    def main(args: Array[String]) {

        //println("Month, Income, Expenses, Profit")
        //first argument load in local restaurants
        val temp: List[String] = io.Source.fromFile(args(0)).getLines.toList
        localRestaurants = temp.map(x => x.split(",").map(_.trim))
        for(lines <- localRestaurants){
            print(lines(0))
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