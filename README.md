# survey-data-visualization

This is a brief demonstration for visualizing survey data.


## **Introduction**

I had the honor of being a part of [Assessment & Evaluation](https://www.csulb.edu/student-affairs/assessment-evaluation) with the Department of Student Affairs at California State University, Long Beach. As a graduate assistant, I provided analytics on department-issued campus-wide surveys.

Here, I showcase some ways to visualize survey participation and responses.
The dataset shown here is purely synthetic (refer to the **behind-the-scenes** file) and is inspired by the [Campus Pulse Survey](https://www.csulb.edu/student-affairs/assessment-evaluation/student-affairs-surveys).


## **Background**

Using methods for random generation, the dataset has simulated responses for:
- Mental Health
- Physical Health
- Social Health

There is also participants' information on:
- ID number
- College/Major
- Survey start time and date

In this analysis, we consider the following scenario:
- There is a student population of 1000.
- All are invited to complete the survey.
- **Participants**: Students who click on the electronic link.
- **Respondents**: Students who provide responses to questions (not all questions will have the same number of respondents).
- **Completionists**: Students who completed the survey in its entirety.
- The survey is held during the Fall and Spring semesters (based off of the [CSULB 2024-2025 academic year](https://www.csulb.edu/academic-affairs/academic-affairs-calendar)).

## **Goals**

We are interested in the following:
- Visualizing survey participation
- Did participation rates differ between Fall and Spring?
- How did respondents' health wellness change?
