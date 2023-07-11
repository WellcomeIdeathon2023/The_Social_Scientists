#!/usr/bin/env python
# coding: utf-8

# In[18]:

#Simulating some misinformation and antivax sentiment monthly as a percentage
#The next month antivax sentiment is correlated with the previous month misinformation
#Antivax sentiment mimics a hypothetical monthly survey on vaccine attitudes
#Saves the dataset as a 3 variable file, with Date, misinformation, and antivaccine sentiment
#Dates are between Aug 2020 and Sep 2023

import datetime
import random
import pandas as pd

start_date = datetime.datetime(2020, 9, 1)
months = [start_date + datetime.timedelta(days=30*i) for i in range(37)]
months = [month.strftime('%Y-%m') for month in months]

misinformation = [round(random.uniform(0, 0.5), 2) for _ in range(37)]
diff = [round(random.uniform(0, 0.2), 2) for _ in range(37)]

antivaccine = [0.2] + [0] * 36
for i in range(36):
    antivaccine[i+1] = misinformation[i] + diff[i]

df = pd.DataFrame({'Date': months, 'misinformation': misinformation, 'antivaccine': antivaccine})


# In[19]:


df


# In[ ]:




