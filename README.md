# Time_Series_Classification

This project is a part of BOUN Advances in Time Series Analytics WS 2021/2022 Course. 

In this project, the main task is to develop an approach for the imbalance prediction for Turkish electricity market from 9 January 2021 to 22 January 2021. 
Every day, the predictions consist of the imbalance prediction (positive, negative or neutral) of the last 12 hours of the next day using the data available, i.e. imbalance data until the day before (included). 
The imbalance data can be accessed from Website of Republic of Turkey Energy Market Regulatory Authority (Turkish: T.C. Enerji Piyasası Düzenleme Kurumu (EPDK)).
In addition to the past data, a weather data including the hourly temperature data belonging to seven different locations close to the big cities in Turkey.
The data is represented in different ways and various distance approaches are used for k-nn classification.
After finding the best representation for the imbalance data based on the k-nn classification using different types of distance measurements, tree-based regression approach is used for the next day's imbalance prediction.
