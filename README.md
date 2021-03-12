# read-elp

A simple R script to fetch and tidy trial-level lexical decision data from the OSF page for the English Lexicon Project.

The trial-level data is available on the OSF page https://osf.io/eu5ca/, but the data format is a pain to work with, and there a few cases of false starts of sessions, data errors, or inconsistent formatting. There is also a dead link in the wiki to a script for reading the data into R.

This script, `trial-level-ldt.R`, downloads the data from the OSF page, and produces a single dataframe with one trial per row. Demographics and additional information about each subject are also stored in this trial-level dataframe. The script makes a hacky attempt to standardise the date of birth information from the original data (which was entered manually by participants) with the `read_elp_date()` function, and recodes the universities from numeric representations into their full names.

By default the dataframe will be written to `elp.csv` (452 MB).

## Columns

The following columns are created. I tried to keep original column names where possible.

| column             | explanation                                                                           |
|--------------------|---------------------------------------------------------------------------------------|
| Univ               | The number assigned to the university.                                                |
| Univ_Name          | The name of the university.                                                           |
| Date               | Date of data collection.                                                              |
| Time               | Time of data collection.                                                              |
| Orig_Subject       | Original subject IDs (`Subject`). Some are reused across different testing locations. |
| Subject_ID         | Fixed subject IDs (`paste(Univ, Orig_Subject)`) with 1 ID per individual participant. |
| DOB                | Date of birth (standardised format).                                                  |
| Education          | Years of education.                                                                   |
| Trial_Order        | The number of this trial for this participant.                                        |
| Item_Serial_Number | An item ID number.                                                                    |
| Lexicality         | 0 (nonword) or 1 (word).                                                              |
| Lexicality_label   | "nonword" or "word".                                                                  |
| Accuracy           | Accuracy of response. Mostly 0 (incorrect) and 1 (correct).                           |
| LDT_RT             | Response time in milliseconds.                                                        |
| Item               | The text displayed to the participant.                                                |
| Session_nr         | The number of the session (assuming sets of csv values signify separate sessions).    |
| Gender             | Recorded participant gender.                                                          |
| Task               | The task completed (all LDT, but may be useful if joining to naming data).            |
| Date_Demog         | The date associated with the participant's demographics data.                         |
| Time_Demog         | The time associated with the participant's demographics data.                         |
| MEQ                | Score from the Morningness-Eveningness Questionnaire                                  |
| Shipley_numCorrect | Score from the Shipley Institute of Living Scale.                                     |
| Shipley_rawScore   | Score from the Shipley Institute of Living Scale.                                     |
| Shipley_vocabAge   | Score from the Shipley Institute of Living Scale.                                     |
| Shipley_shipTime   | Score from the Shipley Institute of Living Scale.                                     |
| Shipley_readTime   | Score from the Shipley Institute of Living Scale.                                     |
| presHealth         | A Likert Rating (1-7) of the participant's present health(?)                          |
| pastHealth         | A Likert Rating (1-7) of the participant's past health(?)                             |
| vision             | A Likert Rating (1-7) of the participant's vision(?)                                  |
| hearing            | A Likert Rating (1-7) of the participant's hearing(?)                                 |
| firstLang          | The participant's first language.                                                     |
| file               | The name of the file associated with the data in the OSF.                             |
