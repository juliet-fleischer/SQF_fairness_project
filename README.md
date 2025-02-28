# SQF_fairness_project

In this project we examine the fairness of the Stop, Question, and Frisk (SQF) policy in New York City.
Our main question is, whether the decision to arrest a suspect after a stop shows racial discrimination.
To answer this question, we compare the fairness and performance of
regular and fairness-adjusted ML models trained on different periods of the SQF practice.

An in depth explanation of the result and a critical reflection on its limitations, given the broader
context of SQF research can be found in the seminar thesis.

To set up this poject, download (or clone) the repository and run the `setup.R` script.
This installs the necessary packages, imports the cleaned data, and the trained learners.
After the setup, you will be able to run the fairness experiment by sourcing `program/fairness_experiment_main.R`.
To recreate the descriptive plots found in the paper, source `program/descriptive_analysis.R`.
You can find the seminar thesis in the `thesis` folder and an overview of common
fairness metrics in `handout`.
