import os
os.environ['QT_API'] = 'PyQt6'

import pandas as pd
import numpy as np
import plotnine as pn
from janitor import clean_names
from matplotlib import rcParams
import seaborn as sns
import matplotlib.pyplot as plt
from pyhere import here
from great_tables import GT as gt
import re

# Set some options
pd.set_option('display.max_columns', None)
pd.set_option('mode.copy_on_write', True)
rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

from functions import nhanes_link_doc, link_convert, read_data_list


med = read_data_list('RXQ_RX')

med[0] = (
  med[0]
  .rename(columns = {'seqn': 'id',
                    'rxduse': 'taken_med_past_month',
                    'rxddrug': 'generic_drug_name',
                    'rxdrsc1': 'icd10_code1',
                    'rxdrsc2': 'icd10_code2',
                    'rxdrsc3': 'icd10_code3',
                    'rxdcount': 'num_med_taken'}
          )
  .loc[:, ['id', 'taken_med_past_month', 'generic_drug_name',
           'icd10_code1', 'icd10_code2', 'icd10_code3', 'num_med_taken']]
)


# from bs4 import BeautifulSoup
# import requests

# url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/RXQ_RX_H.htm"
# response = requests.get(url)
# html_content = response.text

# soup = BeautifulSoup(html_content, 'html.parser')
# table_content = soup.find_all('table')

# len(table_content)

# def parse_html_table(table):
#     """Convert a BeautifulSoup table into a pandas DataFrame"""
#     rows = table.find_all('tr')
#     data = []
#     for row in rows:
#         cols = row.find_all(['td', 'th'])
#         cols = [c.get_text(strip = True) for c in cols]
#         data.append(cols)

#     # Assume first row is header
#     df = pd.DataFrame(data[1:], columns=data[0])
#     return df

# # Example: extract table 1 and table 14
# df1 = parse_html_table(table_content[1])
# df14 = parse_html_table(table_content[14])

# df1 = df1.clean_names(case_type = 'snake')
# df14 = df14.clean_names(case_type = 'snake')

med[0]['generic_drug_name'] = med[0]['generic_drug_name'].str.decode('utf-8')
med[0]['icd10_code1'] = med[0]['icd10_code1'].str.decode('utf-8')
med[0]['icd10_code2'] = med[0]['icd10_code2'].str.decode('utf-8')
med[0]['icd10_code3'] = med[0]['icd10_code3'].str.decode('utf-8')

# med[0].loc[med[0]['generic_drug_name'].str.contains(';')]
med[0] = med[0].drop(columns = ['icd10_code1', 'icd10_code2', 'icd10_code3'])
med[0][['drug_name1', 'drug_name2', 'drug_name3', 'drug_name4']] = med[0]['generic_drug_name'].str.split(';', expand = True)

gt(med[0].loc[med[0]['id'] == 73566]).show()

med_long = (
  med[0]
  .melt(id_vars = ['id', 'taken_med_past_month', 'num_med_taken'],
        value_vars = ['drug_name1', 'drug_name2', 'drug_name3', 'drug_name4'],
        var_name = 'drug_num',
        value_name = 'drug_name')
)

gt(med_long.loc[med_long['id'] == 73566]).show()
gt(med_long.loc[med_long['id'] == 73566].dropna()).show()

def reverse_binary(df: pd.DataFrame,
                   column: str) -> pd.Series:
  """
  Include documentation
  """
  series = df[column].copy()
  if series.value_counts().shape[0] > 2:
    ValueError('Clean data first to remove missing (Don\'t know) and Refuse responses')
  
  series = np.where(series == 1, 1, 0)
  
  return pd.Series(series)

med_long['taken_med_past_month'] = reverse_binary(med_long, 'taken_med_past_month')

med_use = med_long.loc[med_long['taken_med_past_month'] == 1]

med_use = med_use.dropna(subset = 'drug_name')

med_use['drug_name'] = med_use['drug_name'].str.title()

med_use = med_use.loc[med_use['drug_name'] != 77777]

med_use['drug_name'] = np.select([med_use['drug_name'] == '99999',
                                          med_use['drug_name'] == '55555'],
                                         ['Don\'t Know', 'Don\'t know'],
                                         default = med_use['drug_name'])

med_use.head()

count_df = (
  med_use
  .groupby('id')['drug_name']
  .value_counts()
  .reset_index()
  .groupby('id')['count']
  .sum()
  .reset_index()
)

med_use = med_use.merge(count_df, 'inner', 'id')
med_use.head()

(
  med_use
  .loc[med_use['num_med_taken'] != med_use['count']]
  .drop_duplicates()
  .shape
)

gt(med_use.head(250)).show()

# med_use.loc[med_use['id'] == 73566.0]

message = []
for drug in med_use['drug_name']:
  if re.search(r"don't\s*know", str(drug).lower()):
        values = {
            'role': 'user',
            'content': ''   # blank message when it's "Don't know"
        }
  
  else:
     values = {'role': 'user',
               'content': f'Does {drug} cause long-term liver damage? Please include a scale from 1 to 10 with 1 being the least likely and 10 being the most likely. Only choose one value, not a range.'}
  
  message.append(values)

message[0]['content']
message[1]['content']
message[0]
message[1]
message[2]
message[3]

message[0]

#put it back into the dataframe for now
med_use['chat_prompt'] = message

len(message)
med_use.head()
# below should result in no data
med_use.loc[med_use['taken_med_past_month'] == 0].head()

import ollama

# running this will take a while
chat_answer = ollama.chat(model = 'llama3.2:1b', messages = message)

# chat_answer
# chat_answer['message']
# chat_answer['message']['content']
print(chat_answer['message']['content'])

import gensim
import gensim.downloader as api
from gensim.models import KeyedVectors, Word2Vec
