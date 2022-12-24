# Stroke-Prediction

# Introduction
•According to World Health Organization, stroke is the 2nd leading cause of death globally which approximates to 11% of total deaths.
​
•In the US, stroke is the leading cause of long-term adult disability and fifth leading cause of death which approximates to 795,000 stroke event every year with about 75% of them being the first-time strokes. 
​
•Study shows that strokes can be prevented 80% of the time, through proper education, on signs of the stroke. ​
Hence, it’s critical that hospitals actively participate in Stroke prediction to eliminate treatment delays and improve outcomes for new and acute stroke patients.​

# Dataset
The dataset is obtained from Kaggle — Stroke Prediction. The dataset contains details about the patients like age, BMI, glucose level, gender, work type etc. The dataset has about 5110 observations with 249 patients with stroke occurrence. Hence we can infer from this that the data is highly imbalanced and the number of stroke occurrences account for only 4.87% of the total observations.

#SummaryOfTheProject
A stroke is a kind of brain damage that occurs when the normal flow of blood to the brain
is interrupted. This may happen for several reasons. Strokes account for more than 795,000 cases
of death each year in the United States, placing them in the sixth spot on the list of main causes
of mortality. A history of smoking, diabetes, high blood pressure, elevated cholesterol levels, and
diabetes in general, as well as a high public body mass index, are all risk factors. These
conditions are related with aging.There are around 201 values that the BMI characteristic cannot explain. Due to theabsence of a matter, the modeling process may prove to be much more challenging, and the result may need to be more precise and accurate. It is possible to accomplish this objective in one of three ways: Delete the column that does not include any data at all. Choose whether you want to remove a cue from the dataset or try your hand at estimating the value that falls in the middle.
Strokes occur far less often in younger people and those who have never been employed.
People who smoke cigarettes have a significantly increased chance of acquiring various
cardiovascular diseases. People who work for themselves have a higher risk of suffering a stroke
than those who another company hires. Surprisingly few people in the population will suffer
from a stroke at some point in their lives (that is, relatively few). Most of those who passed away
from strokes were senior citizens within a few years of their death. People who have smoked
throughout their lives have a lower risk of experiencing a stroke compared to those who have
never engaged in the habit. Due to the inherent flaws in the data, the constructed model could
only accurately predict those individuals who would not have a stroke. This is because the model
was built to account for the significant imbalance in the data. We oversampled the training set by
utilizing the ROSE tool so that we could address the problem of the uneven distribution of the
data. The effect of the sample on the size of the first dataset will result in the first dataset,
which was previously unbalanced, becoming balanced. When determining which of the five
models offers the highest level of performance, one of the considerations that will go into the
decision is that three of the models achieved an accuracy of more than 85%. There is almost complete consensus among the models that determining the level of accuracy required is the most critical decision to make. A patient at risk of having a stroke was incorrectly diagnosed as not being at risk, and as a result, the patient was given the incorrect impression that they were not at risk of a stroke. As a result, the patient could pass away or become permanently disabled due to the false negative. This demonstrates that the patient was incorrectly classified as not being at risk of having a stroke in the first place.