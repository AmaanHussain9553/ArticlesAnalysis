module Project03

open FSharp.Data

//************PROVIDED CODE IN THIS SECTION SHOULD REQUIRE NO CHANGES, THOUGH ALLOWED********************
type Article(a,b,c,d,e,f,g,h,i,j,k,l,m) =
    member this.news_id = a
    member this.url = b
    member this.publisher = c
    member this.publish_date = d
    member this.author = e
    member this.title = f
    member this.image = g
    member this.body_text = h
    member this.news_guard_score = i
    member this.mbfc_level = j
    member this.political_bias = k
    member this.country = l
    member this.reliablity = m

//
// doubleOrNothing
//
// Given a string containing a double numeric value
// or being an empty string
// returns the double equivalent, with the empty string
// treated as the value 0.0
//
let doubleOrNothing s = 
    match s with
    | "" -> 0.0
    | x -> double x

//
// stringToInteger
//
// Given a string containing an integer value
// or being an empty string
// returns the integer equivalent, with the empty string
// treated as the value 0
//
let stringToInteger s = 
    match s with
    | "" -> 0
    | x -> int x

//
// charListToString
//
// Given a list of characters, return a string 
// containing those characters in order
//
let charListToString L =
    let sb = System.Text.StringBuilder()
    L |> List.iter (fun c -> ignore (sb.Append (c:char)))
    sb.ToString()

//
// stringToCharList
//
// Given a string, return a list of characters 
// containing the characters of the string in order
//
let stringToCharList s =
    Seq.toList s

// Functions to help print items in the correct format

//
// printAllItemsInList
//
// Given a list of strings, print out those strings without quotes
//
let rec printAllItemsInList L =
    List.iter (printfn "%s") L

//
// printStringsAndIntegers
//
// Given a list of pairs between strings and ints,
// print out those strings and integers in a nice format
//
let rec printStringsAndIntegers L =
    List.iter (fun (s,i) -> printfn "%s: %d" s i) L

//
// printNamesAndFloats
//
// Given a list of pairs between strings and floats,
// print out those strings and floats in a nice format
//
let rec printNamesAndFloats L =
    List.iter (fun (s,f) -> printfn "%s: %.3f" s f) L

//
// printNamesAndPercentages
//
// Given a list of pairs between strings and floats,
// print out those strings and floats with a percent sign
//
let rec printNamesAndPercentages L =
    List.iter (fun (s,f) -> printfn "%s: %.3f %%" s f) L

//
// printStars
//
// Given an int n, prints out n stars on the same line.
//
let rec printStars n =
    match n with
    | 0 -> ()
    | 1 -> printf "*"
    | _ -> printf "*"
           printStars (n-1)



//***************************YOUR CODE SHOULD BEGIN HERE******************************
// Implement functions for 10 questions 

let get_info index allData = 
    allData
    |>List.find (fun (s:Article) -> s.news_id =index)

// 1. Given a news id, write a function to return
//    its corresponding news title.
let getTitle index allData =
    let temp:Article = get_info index allData
    temp.title

let rec count_words L counter =
    match L with
    |[] -> 0
    |' '::tail -> count_words tail (counter+1)
    |'\n'::tail -> count_words tail (counter+1)
    |y::tail when counter > 0 -> 1 + count_words tail 0
    |y::tail -> count_words tail counter

// 2. Given a news id, write a function to return 
//    the length (number of words) of its corresponding news body text.
//    Words are separated by space or line separator "\n".
let wordCount index allData =
    let temp:Article = get_info index allData
    let stringList = stringToCharList temp.body_text
    let totalWC = count_words stringList 0 + 1
    totalWC

let rec month_name L =
    match L with
    |'1'::tail -> "January"
    |'2'::tail  -> "February"
    |'3'::tail  -> "March"
    |'4'::tail -> "April"
    |'5'::tail -> "May"
    |'6'::tail  -> "June"
    |'7'::tail  -> "July"
    |'8'::tail  -> "August"
    |'9'::tail  -> "September"
    |'1'::tail when tail.Head = '0' -> "October"
    |'1'::tail when tail.Head = '1' -> "November"
    |'2'::tail when tail.Head = '2' -> "December"
    | _ -> ""

// 3. Given a news id , write a function to return
//    its corresponding publish month as a string.
//    For example, return “January” for 2020-01-21, return “June” for 2020-06-23.
let getMonthName index allData =
   let temp:Article = get_info index allData
   let datePublish = temp.publish_date
   let monthList = month_name (stringToCharList datePublish) 
   monthList


let _publishers allData =
    allData
    |>  List.map (fun (s:Article) -> s.publisher)
   
// 4. Write a function to return a list of unique news publishers.
//    Each publisher is represented by its original name in the string format.
//    The order of the publishers in the output list is the same as the order in the CSV data.
let publishers allData =
    let storePublisherList = _publishers allData
    let uniquePublishers = Seq.distinct storePublisherList
    Seq.toList uniquePublishers


let _countries allData =
    allData
    |>  List.map (fun (s:Article) -> s.country)

// 5. Write a function to return a list of unique countries.
//    Each country is represented by its original name in the string format.
//    The order of the countriesin the output list is the same as the order in the CSV data.
let countries allData =
    let storeCountries = _countries allData
    let uniqueCountries = Seq.distinct storeCountries
    Seq.toList uniqueCountries


let _avgNewsguard allData =
    allData
    |>  List.map (fun (s:Article) -> s.news_guard_score)


let rec average L = 
   let numElem = 0.0
   let rec accum (sum,numElem) L1 =
       match L1 with
       |  [] -> (sum,numElem)
       |  head::tail -> accum(sum + head, numElem + 1.0) tail
   let sum, numElem = accum(0.0,0.0) L
   let averageCompute = (float)sum / (float)numElem
   averageCompute 

// 6. Write a function to return the average news_guard_score among all news articles.
let avgNewsguardscoreForArticles allData =
    let storeNewsGuard = _avgNewsguard allData
    average storeNewsGuard


let _ArticlesPerMonth allData =
    allData
    |>  List.map (fun (s:Article) -> (stringToCharList s.publish_date))

let rec _countJanuary L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "January" -> 1 + _countJanuary tail
    |_::tail -> _countJanuary tail

let rec _countFebruary L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "February" -> 1 + _countFebruary  tail
    |_::tail -> _countFebruary tail

let rec _countMarch L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "March" -> 1 + _countMarch tail
    |_::tail -> _countMarch tail

let rec _countApril L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "April" -> 1 + _countApril tail
    |_::tail -> _countApril tail

let rec _countMay L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "May" -> 1 + _countMay tail
    |_::tail ->  _countMay tail

let rec _countJune L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "June" -> 1 + _countJune tail
    |_::tail -> _countJune tail

let rec _countJuly L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "July" -> 1 + _countJuly tail
    |_::tail -> _countJuly tail

let rec _countAugust L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "August" -> 1 + _countAugust tail
    |_::tail -> _countAugust tail

let rec _countSeptember L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "September" -> 1 + _countSeptember tail
    |_::tail -> _countSeptember tail

let rec _countOctober L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "October" -> 1 + _countOctober tail
    |_::tail -> _countOctober tail

let rec _countNovember L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "November" -> 1 + _countNovember tail
    |_::tail -> _countNovember tail

let rec _countDecember L1 =
    match L1 with
    |[] -> 0
    |head::tail when month_name head = "December" -> 1 + _countDecember tail
    |_::tail -> _countDecember tail

let rec _zip L1 L2 =
    match (L1,L2) with
    | ([],[]) -> []
    | ([],_) -> raise(System.ArgumentException("The input lists are not of equal length."))
    | (_,[]) -> raise(System.ArgumentException("The input lists are not of equal length."))
    | (hd1::tail1, hd2::tail2) -> (hd1, hd2) :: _zip tail1 tail2

// 7. Write a function to return a list containing the amount of news for each month,
//    from January to December. Each month and its corresponding news amount is represented by a tuple.
//    You need to print this information as a histogramin the console.
//    The histogram can be represented by stars (*).
//    Each star (*) should represents 1% of overall amount of data.
//    More instructions to print the histogram can be found in the project description.
let numberOfArticlesEachMonth allData = 
    let tupleMonthArticles = [("January");("February");("March");("April");("May");("June");("July");("August");("September");("October");("November");("December")]
    let l1 = _ArticlesPerMonth allData
    let monthCount = [_countJanuary l1; _countFebruary l1; _countMarch l1; _countApril l1; _countMay l1; _countJune l1; _countJuly l1; _countAugust l1; _countSeptember l1; _countOctober l1; _countNovember l1; _countDecember l1]
    _zip tupleMonthArticles monthCount




// 8. Write a function to return a list of unique publishers
//    and the percentage of news which are marked as reliable (1) published by each publisher.
//    Each publisher and its reliable news percentage are represented by a tuple, e.g., (“CNN”, 65.233).
//    The order of the tuples are the same as the order of publishers in the CSV data.
let reliableArticlePercentEachPublisher allData =
    [("CNN", 65.233)]



 

// 9. Write a function to return a list of unique counties and their average news_guard_score.
//    The output is a list of tuples, e.g., [(“USA”, 92.5), (“Russia”, 35.222)].
//    The order of the tuples are the same as the order of countries in the CSV data.
let avgNewsguardscoreEachCountry allData =
    [("country", 0.0)]



// 10. The political_bias column has multiple values, write a function to categorize them and 
//     return a list of tuples with each category and its average news_guard_score.
//     The order of the tuples are the same as the order of political_bias in the CSV data.
//     Similar to Question 7, you need to print the output as a histogram. You can reuse the code from Question 7.
let avgNewsguardscoreEachBias allData =
    [("bias", 0.0)]



let rec length L =
    match L with
    | [] -> 0
    | _ ::tail -> 1 + length tail 

let _store allData =
    allData
    |>  List.map (fun (s:Article) -> s.news_guard_score) |> ignore
    let a = List.length
    a

//
// buildHistogram
// Takes a list of (string,int) pairs
// prints out string nicely spaced
// followed by a number of stars
// spacing to line up the format of the output and display as a graph
//
// You will want to modify this function to produce the correct output
//
let rec buildHistogram L =
    match L with
    | [] -> ()
    | (category,number)::tl -> printf " %15s : " category
                               let numStars = (number) // or some function of number
                               printStars numStars
                               printf "%d" number
                               printfn ""
                               buildHistogram tl

//
// buildHistogram
// Takes a list of (string,float) pairs
// prints out string nicely spaced
// followed by a number of stars
// spacing to line up the format of the output and display as a graph
//
// You will want to modify this function to produce the correct output
//
let rec buildHistogramFloat L =
    match L with
    | [] -> ()
    | (category,number)::tl -> printf " %15s : " category
                               let numStars = (int number) // or some function of number
                               printStars numStars
                               printf "%.3f" number
                               printfn ""
                               buildHistogramFloat tl


//***********************************APPLICATION START****************************************
  // This is where your application begins, you can change any of the code below this point as well as above
  // Application begins by requesting name of the file
  // see notes on piazza for HW3 about putting the data file in the correct location
  // then requests the task to perform
  // The following match has each task twice, once in the specific task case and once
  // in the run all tasks case
  // Make sure any changes you make are reflected in both cases.

[<EntryPoint>]
let main argv =
    // input file name, to read the data
    printf "Enter name of the csv file containing employee data: "

    // Read the name of the file from standard input
    let filename = System.Console.ReadLine()

    // Use a built-in library to process the CsvFile into a collection of fields
    let contents = CsvFile.Load(filename)

    // List comprehension magic to build a list of Article objects
    // See definition of Article class at the top (type Article)
    let data = [for x in contents.Rows
                 do
                    Article(stringToInteger (x.GetColumn 0),
                            x.GetColumn 1,
                            x.GetColumn 2,
                            x.GetColumn 3,
                            x.GetColumn 4,
                            x.GetColumn 5,
                            x.GetColumn 6,
                            x.GetColumn 7,
                            doubleOrNothing (x.GetColumn 8),
                            x.GetColumn 9,
                            x.GetColumn 10,
                            x.GetColumn 11,
                            stringToInteger (x.GetColumn 12))]

    // Debug information, to get you started and check that the data was loaded correctly
    // Comment out these two lines when submitting the project
    printfn "This is the data you have loaded."
    List.iter (fun (x:Article) -> printfn "%A" x.title) data
    
    printf "Which task to perform (0 for all): "
    let taskNumber = stringToInteger (System.Console.ReadLine())

    printfn ""

    match taskNumber with
    | 1 ->  //1. print title
            printf "Enter id of article: "
            let id = stringToInteger (System.Console.ReadLine())

            printfn ""
            let title = getTitle id data
            printfn "1. Title: %s" title
    | 2 ->  //2. word count
            printf "Enter id of article: "
            let id = stringToInteger (System.Console.ReadLine())

            printfn ""
            let word_count = wordCount id data
            printfn "2. Number of Words in The Article: %d" word_count
    | 3 ->  // 3. printmonth
            printf "Enter id of article: "
            let id = stringToInteger (System.Console.ReadLine())

            printfn ""
            let actualmonth = getMonthName id data
            printfn "3. Month of Chosen Article: %s" actualmonth
    | 4 ->  // 4. unique publishers
            let publisherNames = publishers data
            printfn "4. Unique Publishers: " 
            printAllItemsInList publisherNames
    | 5 ->  // 5. unique countries
            let countryNames = countries data
            printfn "5. Unique Countries: " 
            printAllItemsInList countryNames
    | 6 ->  // 6. average news guard score for all the articles
            let overallguard = avgNewsguardscoreForArticles data
            printfn "6. Average News Guard Score for All Articles: %.3f" overallguard
    | 7 ->  // 7. number of articles for each month
            let nArticles = numberOfArticlesEachMonth data
            printfn "7. Number of Articles for Each Month:" 
            buildHistogram nArticles
    | 8 ->  // 8. percentage of articles that are reliable for each publisher
            let reliablepct = reliableArticlePercentEachPublisher data
            printfn "8. Percentage of Articles That Are Reliable for Each Publisher: " 
            printNamesAndPercentages reliablepct
    | 9 ->  // 9. for each country, print the average news guard score
            let countryNames = countries data
            let averageguard = avgNewsguardscoreEachCountry data
            printfn "9. Average News Guard Score for Each Country: " 
            printNamesAndFloats averageguard

    | 10 -> // 10. categorize articles by political bias, then get the average news guard score for each category
            let bias = avgNewsguardscoreEachBias data
            printfn "10. The Average News Guard Score for Each Political Bias Category: " 
            buildHistogramFloat bias

    | 0 | _ ->  // ALL
                printf "Enter id of article: "
                let id = stringToInteger (System.Console.ReadLine())

                 //1. print title
                printfn ""
                let title = getTitle id data
                printfn "1. Title: %s" title

                //2. word count
                printfn ""
                let word_count = wordCount id data
                printfn "2. Number of Words in The Article: %d" word_count

                // 3. printmonth
                printfn ""
                let actualmonth = getMonthName id data
                printfn "3. Month of Chosen Article: %s" actualmonth

                // 4. unique publishers
                printfn ""
                let publisherNames = publishers data
                printfn "4. Unique Publishers: " 
                printAllItemsInList publisherNames

                // 5. unique countries
                printfn ""
                let countryNames = countries data
                printfn "5. Unique Countries: " 
                printAllItemsInList countryNames

                // 6. average news guard score for all the articles
                printfn ""
                let overallguard = avgNewsguardscoreForArticles data
                printfn "6. Average News Guard Score for All Articles: %.3f" overallguard

                // 7. number of articles for each month
                printfn ""
                let nArticles = numberOfArticlesEachMonth data
                printfn "7. Number of Articles for Each Month:" 
                buildHistogram nArticles

                // 8. percentage of articles that are reliable for each publisher
                printfn ""
                let reliablepct = reliableArticlePercentEachPublisher data
                printfn "8. Percentage of Articles That Are Reliable for Each Publisher: " 
                printNamesAndPercentages reliablepct

                // 9. for each country, print the average news guard score
                printfn ""
                let averageguard = avgNewsguardscoreEachCountry data
                printfn "9. Average News Guard Score for Each Country: " 
                printNamesAndFloats averageguard

                // 10. categorize articles by political bias, then get the average news guard score for each category
                printfn ""
                let bias = avgNewsguardscoreEachBias data
                printfn "10. The Average News Guard Score for Each Political Bias Category: " 
                buildHistogramFloat bias

    0
