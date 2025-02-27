# SQF_fairness_project

In this project we examine the fairness of the Stop, Question, and Frisk (SQF) policy in New York City.
Our main question is, whether the decision to arrest a suspect after a stop shows racial discrimination.
To answer this question, we train machine learning models on different periods of the SQF practice
and evaluate the models' fairness by using the mlr3fairness package.

An in depth explanation of the result and a critical reflection on its limitations, given the broader
context of SQF research can be found in the seminar paper.

To run this project, download the repository and run the `setup.R` script
to install the necessary packages, import the cleaned data, and the trained learners.
To train the learners yourself, see the comments in the code in `fairness_experiment_main.R` and
`fairness_experiment_2011.R`.
