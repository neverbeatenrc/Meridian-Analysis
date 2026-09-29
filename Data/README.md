# Analysis Data

The analysis reads the seven original OULAD CSV files from this folder:

- `assessments.csv`
- `courses.csv`
- `studentAssessment.csv`
- `studentInfo.csv`
- `studentRegistration.csv`
- `studentVle.csv`
- `vle.csv`

Download the Open University Learning Analytics Dataset from the [official dataset page](https://analyse.kmi.open.ac.uk/open_dataset), then place the files here without renaming them. `studentVle.csv` is large; the R analysis uses `data.table::fread()` to load and summarize it.

Raw CSV files are intentionally excluded from version control because of their size. The current project copy does not contain `courses.csv`, which is required to reproduce the report. Do not commit student-level source data unless you have confirmed that redistribution is permitted by the dataset's terms
