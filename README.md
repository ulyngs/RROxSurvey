# RROxSurvey

## 1. FormatPGRdata.R
Starts with `rm(list = ls())` and ends with `source("Functions-and-Parameters.R")`.
This script loads the survey data run on PGR students in HT 2020 and format it for analysis.
The data contains some answers from non-PGR students which is not filtered out (but is labelled in pgrdata$StudentStaff) at this stage.

## 2. Functions-and-Parameters.R
Contains the list of items to judges or categories, and functions to prepare the data for plotting 
(create a skeleton of dataset to merge in real data, summarise items, bind the summaries),
function to create circular plots, and functions to regroup the data previously split by Divisions and to create a stacked bar plot.


PROBLEM ---> The function to create circular plots need to be improved to accept argument (question) to change some layout.
But if implemented as it is (uncommenting the if statement lines), the LongReport.Rmd doesn't compile.


## 3. Sample-sizes.R
To work on this file on its own, one need to uncomment the two first lines 
`#rm(list = ls())`
`#source("FormatPGRdata.R")`
This R file is called in LongReport.Rmd which needs these two lines at the top to be commented.

This script compiles sample sizes of answers per Division, per question, and for staff and students.

## 3. Question.R

There is one R file per question, which creates plots and tables.
To work on one file specifically, one need to uncomment the two first lines 
`#rm(list = ls())`
`#source("FormatPGRdata.R")`

Each of these R files is called in LongReport.Rmd which needs these two lines at the top to be commented.

The list of file in order of the questions in the survey is:
- Awareness.R -> Q4 Which of the following research practices are you aware of, and which do you have experience with?
- Effect.R -> Q5 In your opinion, what would be the overall effect of widespread adoption of the following practices in your field of research?
- Barriers.R -> Q6 Do you face any barriers in adopting the following practices and, if so, what are they?
- Downsides.R -> Q7 In your view, are there any downsides to widespread adoption of the following practices in your field of research?
- OtherBarriersDownsides.R -> free text answers from Q6 and Q7 to categorise.
- CurrentRecruitment.R <- Q8 To the best of your knowledge, to what extent are the following criteria used for recruitment in your field of research at Oxford?
- FutureRecruitement.R -> Q9 In your opinion, to what extent should the following criteria be used for recruitment in your field of research at Oxford?
- Training.R -> Q10 For which of the following topics do you think more guidance is necessary? ‘
- Support.R -> Q11 What additional support would you find useful to implement open research practices?
- Inventory.R -> Q12 Please provide information to help us identify initiatives at Oxford that relate to open research (e.g. courses, workshops, summer schools, working groups, study groups, meet-ups).


PROBLEM ---> plots layout were creating by judging the preview within RStudio, or the output of the RMd in HTML. How can I save them in PDF with the right layout? Plots in the Figures folder were saved using the Export as image button and resizing the window.


## 4. Rmd reports

in Draf-Report folder:
- ShortReport.R -> create combines figures for a landscape draft report made in Gdoc: https://docs.google.com/document/d/1dc3E3oU9evBrBYhIxMUqiEsvxx_bkc_y/edit
- Report.html -> version that compiled before changing Function-and-Parameters.R for circular plot to take more arguments depending on the question.
- LongReport.html -> version that compiled after changing Function-and-Parameters.R (commenting out the argument question to the circular plot)

- LongReport.Rmd
current version of the report.
Starts with 
`source("FormatPGRdata.R")`
`source("Sample-sizes.R")`
and then `source("*Question*.R")` in turns.

It calculates descriptive statistics in R code chunks.
It calls names of plots and tables created in the Question.R files.


QUESTIONS ---> Should the Rmd call saved figures instead? How can tables be similarly saved as pdf?


