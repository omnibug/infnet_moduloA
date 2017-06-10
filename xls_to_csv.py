# -*- coding: utf-8 -*-
"""
Created on Mon Nov 14 16:13:31 2016

@author: Carlos
"""
import pandas as pd
import numpy as np
import unicodedata
import json
import math
import datetime as dt

pd.set_option('display.max_rows', 3000)
pd.set_option('display.max_columns', 500)
pd.set_option('display.width', 1000)

def dprint(texto):
    print ('<---| ', texto, ' |--->')

#Definindo uma funcao de listagem e descricao do dataframe
def desc_df(df,n):
    print '\nData File data and stats'
    print df.head(n)
    print df.describe()
    print df.info()

# Runnning instructions 
print '='*200
print '\nStarted'

#Reading the dataset in a dataframe using Pandas
home='/home/carlos/Documents/Trabalho/'
file=home+'Data_User_Modeling_Dataset_Hamdi Tolga KAHRAMAN'
df_full = pd.read_excel(file+'.xls') 
df_full.to_csv(file+'.csv') 
