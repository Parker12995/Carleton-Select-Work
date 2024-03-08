
# Mini Project 2: Shiny app development

## Due date

This mini project is due on GitHub by Thursday, March 03, 11:59 p.m.

## Description

This is a follow-up to mini project 1. You will create a published Shiny
app that includes at least two well-crafted, thoughtfully-prepared
interactive graphic incorporating a map and trend analysis each using
data available in the `coronavirus` R package as `coronavirus` and
`covid19_vaccine` datasets. You can reuse a graph from the previous
project or create something new. Make sure to address any feedback
provided about your first mini-project.

The [Shiny Gallery](https://shiny.rstudio.com/gallery/) provides many
Shiny examples and code. Feel free to use these ideas.

### App details

-   **Graphs:** Make sure your visuals tell **different** stories. You
    can reuse graphics from mini project 1 (from either partner), but
    make sure to address any feedback provided. Your graphs should be
    appropriately polished. You can also include text in your app to
    provide more context or explanation, if needed.

-   **Input actions:** You must use at least **two different** types of
    input objects. One input per graph is adequate, but more complex
    interactivity will score a higher interactivity achievement score.

    -   But, any added complexity needs to enhance your ability to tell
        a story!

-   **Layout** Use a *tabbed layout* with one graph on each tab. One
    option for a tabbed layout is `tabsetPanel` and `tabPanel`. A more
    complex layout will score higher layout achievement score.

### Deploying your Shiny app

Sign up for a free account on
[shinyapps.io](https://www.shinyapps.io/admin/#/signup) and connect
RStudio to your shinyapps.io account, as outlined in [Garrett
Grolemund’s
video](https://carleton.hosted.panopto.com/Panopto/Pages/Viewer.aspx?id=38a1b2ee-58c0-4faa-9270-add3013acae9)
on using shinyapps.io. Publish your **app.R** file and any other files
needed to run your code to this site (like data files). Test this out
*early* in the process so you know how to (re)deploy your final app
version. (You can overwrite an old version of an app with new code.)

### Screencast

Each team will create a two minute screencast with narration showing a
demo of your Shiny app. You can download Panopto to do this free from
ITS. Information about how to use Panopto to prepare these screencasts
can be found
[here.](https://apps.carleton.edu/campus/its/services/learning/lecture-capture/)
Please make sure that the sound quality of your video is good. If you
use a different video capture platform (e.g., Zoom), then please upload
it to Panopto and share the link in your README file.

Use principles of good storytelling and presentations to get your key
points across. Focus the majority of your screencast on your main
contributions rather than on technical details. What do you feel is the
best part of your project? What insights did you gain? What is the
single most important thing you would like your audience to take away?
Make sure it is upfront and center rather than at the end!

### Deliverables

Push the following files to your mp2 repo using the names given below:

-   **app.R** This file should contain the code for your Shiny app. This
    will be the file that you deploy to the Shiny server.

-   **README.md** Update the README.md file in your repo by pasting your
    app URL (from shinyapps.io) at the top of this file. In addition,
    add a link to your screencast that you are hosting on Panopto.

-   **(optional, but recommended) Cleaned data files:** This includes
    any files needed to deploy your app. For example, you may choose to
    save cleaned-up data files in your repo to avoid repeating these
    cleanup steps in your app. The more code running in your app, the
    longer it will take to render. Any data needed for the app must be
    deployed (uploaded) to the Shiny server.

### Tips on app construction:

-   If you get an error deploying your app on Shinyapps.io, navigate to
    the dashboard on the apps page and check the “log”. Scroll down
    until you see the error(s). Some common errors that I’ve seen are
    related to “hidden” libraries that are loaded by other libraries in
    a normal R session but not when pushed to the Shiny server. E.g. if
    you use `ggplot2`’s maps, you will likely need to include library
    commands for `mapproj` and `maps` to get the app to run on the Shiny
    server

-   The more commands you have running in a render or reactive function,
    the longer the app will take to react when an input changes. Try to
    place commands that aren’t reacting to `input` values outside of a
    render/reactive function. For example, reading in and cleaning data
    commands can be placed outside of reactive functions.

## Grading Rubric

A scale of 0-5 will be used for the following aspects of your work:

**Technical aspects**

| Topic                                                                 | Excellent: 5                                                                                                                                                 | Satisfactory: 3                                                                               | Needs work: 1                                                                                                                        |
|-----------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------|-----------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------|
| Coding style                                                          | Coding style is followed (e.g. avoids run on “sentences” of code), code is well commented                                                                    | Coding style lacks refinement and has some errors, but code is readable and has some comments | Many errors in coding style, little attention paid to making the code human readable                                                 |
| Coding strategy                                                       | Complicated problem broken down into sub-problems that are individually much simpler. Code is efficient, correct, and minimal. Code checks for common errors | Code is correct, but could be edited down to simplier code chunks. Some checks for errors.    | Code tackles complicated problem in one big chunk. Code is repetitive and could easily be functionalized. No anticipation of errors. |
| Ease of access, compliance with course conventions for submitted work | Access as easy as possible, code runs!                                                                                                                       | Satisfactory                                                                                  | Not an earnest effort to reduce friction and comply with conventions and/or code does not run                                        |

**Shiny App**

| Section                                                        | Excellent: 5                                                                                                                                                                                                   | Satisfactory: 3                                                                                                                                                                | Needs work: 1                                                                                                                                                                      |
|----------------------------------------------------------------|----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| Presentation: graphs/tables                                    | Graph(s) carefully tuned for desired purpose. One graph illustrates one point. Table(s) carefully constructed to make it easy to perform important comparisons. Careful styling highlights important features. | Graph(s) well chosen, but with a few minor problems: inappropriate aspect ratios, poor labels. Table(s) generally appropriate but possibly some minor formatting deficiencies. | Graph(s) poorly chosen to support questions. Table(s) with too many, or inconsistent, decimal places. Table(s) not appropriate for questions and findings. Major display problems. |
| **Interactivity** Achievement, mastery, cleverness, creativity | Student has gone beyond what was expected and required, e.g., extraordinary effort, additional tools not addressed by this course, unusually sophisticated application of tools from course.                   | Tools and techniques from the course are applied very competently and, perhaps,somewhat creatively. Chosen task was acceptable, but fairly conservative in ambition.           | Student does not display the expected level of mastery of the tools and techniques in this course. Chosen task was too limited in scope.                                           |
| **Layout** Achievement, mastery, cleverness, creativity        | Student has gone beyond what was expected and required, e.g., extraordinary effort, additional tools not addressed by this course, unusually sophisticated application of tools from course.                   | Tools and techniques from the course are applied very competently and, perhaps,somewhat creatively. Chosen task was acceptable, but fairly conservative in ambition.           | Student does not display the expected level of mastery of the tools and techniques in this course. Chosen task was too limited in scope.                                           |

**Communication**

| Topic        | Excellent: 5                                      | Satisfactory: 3                                                                                                                       | Needs work: 1                                                                                        |
|--------------|---------------------------------------------------|---------------------------------------------------------------------------------------------------------------------------------------|------------------------------------------------------------------------------------------------------|
| Presentation | The screencast is clear, engaging, and effective? | The screencast demonstrates the use of the app and outlines the main contributions/findings, but could be more effective or engaging. | Screencast does not demonstrate the main features of the app and/or is very unclear or disorganized. |

## Ackowledgements

This prompt was adapted from a prompt written by Adam Loy and Katie
St. Clair.