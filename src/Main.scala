import scala.annotation.tailrec
import scala.io.Source
import scala.io.StdIn.readLine

object Main {
    var appData: Map[String, List[Int]] = Map()

    def main(args: Array[String]): Unit = {
        getData() match {
            case Some(inData) =>
                appData = inData
            case None =>
                println("Unable to read data. Application closing.")
                return
        }

        mainMenu(false)
    }

    // function for reading in data from the data.txt file
    def getData(): Option[Map[String, List[Int]]] = {
        var appData: Map[String, List[Int]] = Map() // declares the map to store the data and ultimately return
        try { // catches exceptions thrown from reading in the file
            for (line <- Source.fromFile("data.txt").getLines()) { // loops through the file
                val lineSegments = line.split(',').map(_.trim).toList // splits read line by ,, trims it and adds to list
                val splitSegments = lineSegments.splitAt(1) // splits the trimmed data into the key and the
                val values: List[Int] = splitSegments._2.map(_.toInt) // parses the data to list of ints

                appData = appData ++ Map(splitSegments._1.head -> values) // adds key and data to map
            }
            Some(appData) // returns success containing the data
        } catch { // catches exceptions thrown from reading in the file
            case _:Exception =>
                None // returns None indicating an error occurred
        }
    }

    @tailrec
    def mainMenu(stop: Boolean): Unit = {
        println("")
        println("=====================================================")
        println("")

        if (!stop) { // checks if this iteration is to stop
            println("1: Current Prices") // display option 1
            println("2: Highest and Lowest Prices") // display option 2
            println("3: Median Prices") // display option 3
            println("4: Largest Price Rise") // display option 4
            println("5: Compare Average Prices") // display option 5
            println("6: Build Food Basket") // display option 6
            println("0: Quit") // display option 0 to quit

            val input = getMenuResponse(false, 7) // gets user's input
            println("") // puts a space between menu and output

            input match { // compares input with various options
                case 0 => // if 0, user wants to quit
                    mainMenu(true) // calls main menu telling it to quit

                case 1 => // if 1, user wants to see current prices
                    println("Displaying current prices: ")
                    displayCurrentPrices(0) // calls function to show current prices
                    mainMenu(false) // calls main menu telling it to continue

                case 2 =>
                    displayHighestLowestPrices(0) // calls function to show highest and lowest prices of each product
                    mainMenu(false) // calls main menu telling it to continue

                case 3 =>
                    displayMedianPrices() // calls function to showing median prices for each product
                    mainMenu(false) // calls main menu telling it to continue

                case 4 =>
                    print("Product with highest price rise in 6 months: ")
                    displayLargestPriceRise(0, "", 0) // calls function showing product with highest price rise
                    mainMenu(false) // calls main menu telling it to continue

                case 5 =>
                    compareAveragePrices(Map()) // calls function allowing user to compare average prices of two items
                    mainMenu(false) // calls main menu telling it to continue

                case 6 =>
                    buildFoodBasket() // calls function allowing user to create a food basket
                    mainMenu(false) // calls main menu telling it to continue
            }
        } else {

        }
    }

    // function to get a menu response that catches any errors in input
    def getMenuResponse(valid: Boolean, maxChoice: Int): Int = {
        try {
            print("Choose an option: ") // ask user to pick an option
            val input:Int = readLine().toInt // gets user's selection and parse to Int
            if (input < 0 || input > maxChoice) { // checks input is within valid range
                println("Invalid selection!") // if outside range, shows error message
                getMenuResponse(false, maxChoice) // calls the function again to try again
            } else { // if input is valid
                input // returns the input to the calling function
            }
        } catch {
            case _:Exception =>
                println("Invalid selection!") // if the input wasnt an Int, shows error message
                getMenuResponse(false, maxChoice) // calls function again
        }
    }

    // function to display current prices of all products
    @tailrec
    def displayCurrentPrices(index:Int): Unit = {
        if (index < appData.toList.length) { // checks if still in range of data
            val keys: List[String] = appData.keys.toList // produces a list of keys
            val values = appData(keys(index)) // gets the data associated with the keys
            println(keys(index) + " -> " + values.tail(0) + "p") // prints the key and the current price
            displayCurrentPrices(index + 1) // calls function again for next product
        }
    }

    // function to display the highest and lowest prices of each product
    @tailrec
    def displayHighestLowestPrices(index:Int): Unit = {
        if (index < appData.toList.length) { // checks still working in range of the data
            val keys: List[String] = appData.keys.toList // gets a list of keys
            val values = appData(keys(index)) // uses list of keys to get data associated with key(index)
            val hl = getHighestLowest(values, 0, values.head, values.head) // gets a tuple containing the highest and lowest prices for a product
            println(keys(index) + " -> Highest: " + hl._1 + "p, Lowest: " + hl._2 + "p") // prints results to the console
            displayHighestLowestPrices(index + 1) // calls method again for next product
        }
    }

    @tailrec
    def getHighestLowest(data: List[Int], index:Int, highest:Int, lowest:Int): (Int, Int) = {
        if (index < data.length) { // checks still in range of data
            var l:Int = lowest // declares variable to use to store lowest
            var h:Int = highest // declares variable to use to store highest

            if (data(index) < lowest) { // checks if current value is less than current lowest
                l = data(index)
            }

            if (data(index) > highest)  { // checks if current value is greater than current highest
                h = data(index)
            }

            getHighestLowest(data, index + 1, h, l) // calls function again for next value
        } else {
            (highest, lowest) // returns tuple containing highest and lowest values
        }
    }

    def displayMedianPrices(): Unit = {
        println("Median prices not implemented")
    }

    @tailrec
    def displayLargestPriceRise(index:Int, largestKey: String, largestRise: Int): Unit = {
        if (index < appData.toList.length) { // checks if still in range of data
            val keys = appData.keys.toList // gets list of keys
            val values: List[Int] = appData(keys(index)) // gets values of key associated with value at appdata(index)

            val change:Int = values(23) - values(17) // calculates change across past six months

            if (change > largestRise) { // checks if calculated change is greater than the current highest
                displayLargestPriceRise(index + 1, keys(index), change) // calls method again for next product with new parameters
            } else {
                displayLargestPriceRise(index + 1, largestKey, largestRise) // calls method again for next product with same parameters
            }
        } else {
            println(largestKey + " with a rise of " + largestRise + "p") // outputs the final results
        }
    }

    def compareAveragePrices(chosen: Map[String, Int]): Unit = {
        var chosenTemp: Map[String, Int] = Map()
        if (chosen.toList.nonEmpty) {
             chosenTemp = chosen
        }

        if (chosen.toList.length < 2) {

            val keys: List[String] = appData.keys.toList
            showAllKeys(keys, 0)
            val input: Int = getMenuResponse(false, keys.length + 1)
            val sum = appData(keys(input - 1)).foldLeft(0)(_ + _)
            val average = sum / keys.length

            chosenTemp ++ Map(keys(input - 1) -> average)
            compareAveragePrices(chosenTemp)
        } else {
            val keys = chosen.keys.toList
            println(keys.head + " has an average price of " + chosen(keys.head) + "p")
            println(keys.tail + " has an average price of " + chosen(keys(1)) + "p")

            if (chosen(keys.head) > chosen(keys(1))) {
                println("There is a difference of " + (chosen(keys.head) - chosen(keys(1))))
            } else {
                println("There is a difference of " + (chosen(keys(1)) - chosen(keys.head)))
            }
        }
    }

    def showAllKeys(keys:List[String], index:Int): Unit =  {
        if (index < keys.length) {
            println((index + 1) + ": " + keys(index))
            showAllKeys(keys, index + 1)
        }
    }

    def buildFoodBasket(): Unit = {
        println("Food baskets not implemented!")
    }
}