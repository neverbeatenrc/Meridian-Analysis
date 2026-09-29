# Meridian Open University: Student Success

## A business problem-solving group project using R

### The question

Why do some distance-learning students finish their modules while others fail or withdraw? And when should the university offer support?

This project follows students from registration, through their online activity and assignments, to their final result. The aim is to turn the data into a clear story about where students struggle and where the university could act.

### The story in the data

The analysis looks at who enrolled, when students left, how often they used the virtual learning environment (VLE), and whether they submitted assessments.

The main findings are:

- 47.2% of registrations ended in a pass or distinction.
- 30.7% of recorded withdrawals happened on or before the module start date; half happened by day 30.
- 10.3% of students had no recorded VLE clicks, and most of this group withdrew.
- Students taking heavier study loads withdrew more often than those taking lighter loads.

The story points to the first weeks as an important time for support: help students plan their workload, follow up when they miss an early assessment, and review modules with high withdrawal rates. These are patterns in the data, not proof that any one factor caused an outcome.

### How the data becomes a story

The CSV files describe different parts of a student's experience. The R analysis checks that the tables connect, brings them together by student, module, and presentation, then compares those combined records with final results. It moves from the broad picture to more focused questions: who enrolls, where withdrawals happen, how engagement changes, and which students may need support.

### What each file does

- [`Meridian_Analysis.html`](Meridian_Analysis.html): the complete report in a browser, with the story, charts, and tables.
- [`Meridian_Board_Report.pdf`](Meridian_Board_Report.pdf): a fixed-format version for sharing or presenting.
- [`Meridian_Analysis.Rmd`](Meridian_Analysis.Rmd): the report source, combining explanations and R code. Knit it to create the HTML report.
- [`Meridian_Analysis.R`](Meridian_Analysis.R): the analysis code in script form.
- [`Meridian_Code_Explained.txt`](Meridian_Code_Explained.txt): a step-by-step explanation of the R code.
- [`requirements.txt`](requirements.txt): R packages needed to run the analysis, plus `lintr` for optional style checks.
- [`Data/`](Data/): the original CSV files used as the analysis inputs. See [`Data/README.md`](Data/README.md) for the list.
- [`.lintr`](.lintr): settings for keeping the project's R style checks consistent.
- [`.gitignore`](.gitignore): prevents downloaded raw data files from being added to Git by mistake.

The seven data files each add a piece of the story:

- `studentInfo.csv`: student background and final result.
- `studentRegistration.csv`: registration and withdrawal dates.
- `courses.csv`: module presentation lengths.
- `assessments.csv`: assessment types and due dates.
- `studentAssessment.csv`: student submissions and scores.
- `vle.csv`: information about online learning materials.
- `studentVle.csv`: daily student clicks on those materials.

### Run the analysis

1. Install R and RStudio.
2. Install the analysis packages in R:

   ```r
   install.packages(c("tidyverse", "data.table", "scales", "knitr", "rmarkdown"))
   ```

   To run the optional style checks, also install `lintr`:

   ```r
   install.packages("lintr")
   ```

3. Download OULAD from the [official dataset page](https://analyse.kmi.open.ac.uk/open_dataset) and place all seven CSV files in `Data/` with their original filenames.
4. Open `Meridian_Analysis.Rmd` in RStudio and select **Knit** to recreate the HTML report. The R script can be run from the project folder.

The current project copy is missing `Data/courses.csv`, so the analysis cannot be rerun until that file is added. The existing HTML and PDF reports can still be read without the raw data. The VLE click file is large, so raw CSV files are excluded from Git; download them separately rather than uploading them with the project.

### About the project

The data comes from the Open University Learning Analytics Dataset (OULAD), covering presentations from 2013 and 2014. Meridian Open University and its Board scenario are fictional framing for this group project. The recommendations discuss the report's historical 2016 planning context, not current institutional performance.

**Team:** The R Five: Aarsee, Divyanshi, Jatin, Chaarvi, and Anubhav.
