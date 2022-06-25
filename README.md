#Articles Dataset Analysis in F#


##Requirements:

* Windows Visual Community Edition
* Knowledge of F#
* .NET development framework

##About the Project:

* F# project that reads in csv file that contains all data about articles such as news_id, url, publisher, title, political_bias, reliability, etc and return important metadata about the entire dataset such as number of articles published each month, average news guard score filtered by country, percentage of articles that are reliable filtered by publisher, etc.
This project uses functional programming that utilizes recursion in order to iterate through its data structures. It has to use recursion efficiently in order to ensure that a stack overflow does not occur.

##Helper/Utility Functions:
* Functions that convert strings to integer or double and vice versa
* Formatting functions that prints out combinations of strings and numbers in a user-friendly way (such as percentages)

##Main Functions:
* Given a news_id, return the corresponding news title
* Given a news_id, return the word count of the article (words separated by space or /n)
* Given a news_id, return the month it was published as a string (needed to convert date as mm/dd/yyyy format to a string that was name of the month)
* Return list of unique countries where a new article was published
* Return list of unique news publishers that published an article
* Return average news guard score based on all articles in dataset
* Return list containing amount of news per month and print this information as a histogram
