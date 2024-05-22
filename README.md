# wellbeing22
Spring 2022 Well-being Pilot Study

Viewable data exploration &amp; analysis for the Living Link Lab Well-being study in Spring/Summer 2022
The study was conducted in the Link Lab at the University of Virginia where data was collected from smartphones, smart wearables (Fitbit Sense), IEQ sensors and surveys. More data will be viewable in the future, which will also include data analysis conducted for research that will be presented at the BuildSys - CPSIS '23 Workshop.

## About the Data
The participants are anonymized and identified with a 3 character identifier. The file also contains details on the participant's type of phone/OS, closest Awair (IEQ) sensor indicated by two identifiers, primary place of work and their role at the University. Daily, weekly and bi-weekly survey were collected along with indoor environmental quality (IEQ) data at workspaces and smartphone usage. The data is currently not available due to IRB protocols but the following files show data analysis that have been conducted on the pilot data.

## Files

### baseline.ipynb
Analyses the data from the baseline survey that is given pre-study as part of the onboarding process. In it, we look at the demographics as well as the personality results. While clustering was attempted, there are too few participants for adquate clustering.

### Exploratory Data Analysis & Processing
eda1.ipynb looks over the sleep data primarily and processes it into more understandable forms.
eda2.ipynb merges the FitBit data (sleep and steps) with the corresponding daily survey responses.

<!-- 
### Merged_FitbitSurveys.csv
The file contains observed sleep data for each participant. This is merged with their Daily survey responses, however we split the questions based on which day it was about. The daily survey had four questions about their sleep (Q3_1, Q4, Q15 and Q6) the previous night, thus we merged the sleep and those sleep questions on the day that they woke up from the respective sleep cycle. The remaining questions were merged based on the day they went to bed so that we can see if there was anything that day that impacted sleep. Step count was also merged on the day the participants went to bed for the same reason. The question codes in the column names are documented in **daily-questions.csv**. -->

<!-- ### lll_awair
This contains the indoor environmental quality (IEQ) data collected for the participants. The files are organized such that the remote/at-home participants are separated from those that worked in the office. The separation is primarily due to the difference in the sampling frequency, which is approximately 10 seconds for in-office participants and 5 minutes for at-home participants. A smaller sampling frequency can be achieved when the data can be accessed via the sensor's API but because the sensors were installed outside of the University's network, we were unable to access the data via the API. 

### lll_aware
The study used the AWARE Open Source Framework to collect data from smartphones. A great variety of data was collected but due to IRB and to protect the participants, limited information is provided from AWARE. At the moment, we provide screen usage data for each participant. More information about AWARE and documentation can be found [here](https://awareframework.com). -->