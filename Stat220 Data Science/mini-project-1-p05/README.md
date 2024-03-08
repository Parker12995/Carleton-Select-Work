
# Mini Project 1: Data Visualization and Wrangling

## Due date

This mini project is due on GitHub by Thursday, Feb 10 at midnight.

## Description

You will work with a partner to analyze Coronavirus data contained in
the [`coronavirus` R
package](https://ramikrispin.github.io/coronavirus/index.html) and to
write a short blog summarizing your findings. You will need to define
your own question(s) of interest based on your interests—the topic is up
to you. Please incorporate both coronavirus infection and vaccination
information in your analysis. The best projects will:

-   discuss an interesting and well-motivated topic
-   involve some non-trivial data wrangling (not just a bunch of
    `mutate()`s)
-   provide a well thought-out, informative analysis
-   convey some sort of insight
-   be well-written
-   consist of 500–700 words

Your goal is to use the data wrangling and visualization techniques from
this class to tell an interesting story about the evolution of
coronavirus disease in a particular geographical entity (or the whole
world, your choice!). Write your analysis in the form of blog post in R
Markdown and use the data visualization and wrangling tools you learned
in this course.

Please include at least two well-crafted, thoughtfully-prepared data
graphic incorporating a map and trend analysis each. Feel free to use
`ggplot2` graphics and other libraries used in the course to help tell
your story. You will upload your R Markdown (.Rmd) file and knitted PDF
to GitHub. Do not forget to give your post an informative title!

Do not print your code in your PDF, rather make sure to hide it from the
knitted document, but know that I will open your .Rmd to grade your code
— so be sure to be organized and make good use of comments!

You should include a section (which isn’t included in your word count)
that details the main visualization and data wrangling steps required in
your analysis. This can be a bulleted list that **briefly** states the
key steps in sentence form, no code!

## Data

The data for this project come from the Johns Hopkins University Center
for Systems Science and Engineering (JHU CCSE) Coronavirus, and are
updated everyday. These data are available in the `coronavirus` R
package as `coronavirus` and `covid19_vaccine` datasets.

Be sure to read the [package
documentation](https://cran.r-project.org/web/packages/coronavirus/index.html),
[data source](https://systems.jhu.edu/research/public-health/ncov/) and
check the [author’s GitHub
page](https://github.com/RamiKrispin/coronavirus) for useful
directions!! **It is your responsibility to know what data you are
working with!**

To get a list of the available data sets, run the following code:

``` r
# install.packages("devtools")
devtools::install_github("RamiKrispin/coronavirus")
library(coronavirus)
?coronavirus
```

## Grading Rubric

**Technical aspects**

| Topic                                                                 | Excellent: 5                                                                                                                                                                                                                               | Satisfactory: 3                                                                                                                                                                | Needs work: 1                                                                                                                                                                      |
|-----------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Coding style                                                          | Group has gone beyond what was expected and required, coding style is followed, code is well commented                                                                                                                                     | Coding style lacks refinement and has some errors, but code is readable and has some comments                                                                                  | Many errors in coding style, little attention paid to making the code human readable                                                                                               |
| Coding strategy                                                       | Complicated problem broken down into sub-problems that are individually much simpler. Code is efficient, correct, and minimal. Code uses appropriate data structure (list, data frame, vector/matrix/array). Code checks for common errors | Code is correct, but could be edited down to leaner code. Some “hacking” instead of using suitable data structure. Some checks for errors.                                     | Code tackles complicated problem in one big chunk. Code is repetitive and could easily be functionalized. No anticipation of errors.                                               |
| Presentation: graphs/tables                                           | Graph(s) carefully tuned for desired purpose. One graph illustrates one point. Table(s) carefully constructed to make it easy to perform important comparisons. Careful styling highlights important features.                             | Graph(s) well chosen, but with a few minor problems: inappropriate aspect ratios, poor labels. Table(s) generally appropriate but possibly some minor formatting deficiencies. | Graph(s) poorly chosen to support questions. Table(s) with too many, or inconsistent, decimal places. Table(s) not appropriate for questions and findings. Major display problems. |
| Achievement, mastery, cleverness, creativity                          | Student has gone beyond what was expected and required, e.g., extraordinary effort, additional tools not addressed by this course, unusually sophisticated application of tools from course.                                               | Tools and techniques from the course are applied very competently and, perhaps,somewhat creatively. Chosen task was acceptable, but fairly conservative in ambition.           | Student does not display the expected level of mastery of the tools and techniques in this course. Chosen task was too limited in scope.                                           |
| Ease of access, compliance with course conventions for submitted work | Access as easy as possible, code runs, .Rmd knits without error or extra messages, everything is on time                                                                                                                                   | Satisfactory                                                                                                                                                                   | Not an earnest effort to reduce friction and comply with conventions and/or code does not run                                                                                      |

**Data wrangling**

| Topic                                                                   | Excellent: 5                                                                                                                                                                               | Satisfactory: 3                                                                                                                                                      | Needs work: 1                                                                                                                          |
|-------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------|
| Joining data:<br>Achievement, mastery, cleverness, creativity           | Group has gone beyond what was expected and required, e.g., extraordinary effort, additional tools not addressed by this course, unusually sophisticated application of tools from course. | Tools and techniques from the course are applied very competently and, perhaps,somewhat creatively. Chosen task was acceptable, but fairly conservative in ambition. | Group does not display the expected level of mastery of the tools and techniques in this course. Chosen task was too limited in scope. |
| Single table wrangling:<br>Achievement, mastery, cleverness, creativity | Group has gone beyond what was expected and required, e.g., extraordinary effort, additional tools not addressed by this course, unusually sophisticated application of tools from course. | Tools and techniques from the course are applied very competently and, perhaps,somewhat creatively. Chosen task was acceptable, but fairly conservative in ambition. | Group does not display the expected level of mastery of the tools and techniques in this course. Chosen task was too limited in scope. |

**Communication**

| Topic                       | Excellent: 5                                                                                                                                                                                                   | Satisfactory: 3                                                                                                                                                                | Needs work: 1                                                                                                                                                                      |
|-----------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Data introduction           | Clearly and concisely describes the data set, and why it is of interest. Clearly establishes the story (i.e. set of questions) that will be told.                                                              | Introduction and story outlined, but could be clearer or more engaging.                                                                                                        | Context and motivation is lacking; story unclear.                                                                                                                                  |
| Results                     | Results are clearly explained in an engaging way. The questions posed are clearly answered. Potential limitations are outlined.                                                                                | Results are explained and the questions posed are answered.                                                                                                                    | Results are not completely explained and/or questions are left unanswered.                                                                                                         |
| Presentation: graphs/tables | Graph(s) carefully tuned for desired purpose. One graph illustrates one point. Table(s) carefully constructed to make it easy to perform important comparisons. Careful styling highlights important features. | Graph(s) well chosen, but with a few minor problems: inappropriate aspect ratios, poor labels. Table(s) generally appropriate but possibly some minor formatting deficiencies. | Graph(s) poorly chosen to support questions. Table(s) with too many, or inconsistent, decimal places. Table(s) not appropriate for questions and findings. Major display problems. |

## Ackowledgements

This prompt was adapted from a prompt written by Adam Loy and Katie
St. Clair.
