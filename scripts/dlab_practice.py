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


# Set some options
pd.set_option('display.max_columns', None)
pd.set_option('mode.copy_on_write', True)
rcParams.update({'savefig.bbox': 'tight'}) # Keeps plotnine legend from being cut off

from functions import nhanes_link_doc, link_convert, read_data_list

# ----------To View Documentation for NHANES Variables----------
print(nhanes_link_doc.__doc__) 
# nhanes_link_doc(2013, 'TCHOL')

# ----------Loading in data----------

# cog = read_data_list([cog_link, cog_link2])
sleep = read_data_list('SLQ')
depress = read_data_list('DPQ')

# smoke = read_data_list('SMQ')
# smokes = read_data_list('SMQRTU')
med = read_data_list('RXQ_RX')
# phy_func = read_data_list('PFQ')
# pa = read_data_list('PAQ')
# med_cond = read_data_list('MCQ')
# insure = read_data_list('HIQ')
# care_access = read_data_list('HUQ')
# drug = read_data_list('DUQ')
# diabetes = read_data_list('DIQ')
# diet = read_data_list('DBQ')
# alc = read_data_list('ALQ')
# heart = read_data_list('CDQ')
# demo = read_data_list('DEMO')
# hdl = read_data_list('HDL')
# ldl = read_data_list('TRIGLY')
# bp = read_data_list('BPQ')
# act_bp = read_data_list('BPX')
# bio = read_data_list('BIOPRO')
# body = read_data_list('BMX')
# total_chol = read_data_list('TCHOL')

# ----------Data Wrangling-----------


# ----------Medications----------
med[0].columns.tolist()
med[1].columns.tolist()

# [nhanes_link_doc(i, 'RXQ_RX') for i in [2013, 2015]]

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

med[1] = (
  med[1]
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

from bs4 import BeautifulSoup
import requests

url = "https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/RXQ_RX_H.htm"
response = requests.get(url)
html_content = response.text

soup = BeautifulSoup(html_content, 'html.parser')
table_content = soup.find_all('table')

len(table_content)

def parse_html_table(table):
    """Convert a BeautifulSoup table into a pandas DataFrame"""
    rows = table.find_all('tr')
    data = []
    for row in rows:
        cols = row.find_all(['td', 'th'])
        cols = [c.get_text(strip = True) for c in cols]
        data.append(cols)

    # Assume first row is header
    df = pd.DataFrame(data[1:], columns=data[0])
    return df

# Example: extract table 1 and table 14
df1 = parse_html_table(table_content[1])
df14 = parse_html_table(table_content[14])

df1 = df1.clean_names(case_type = 'snake')
df14 = df14.clean_names(case_type = 'snake')

med[0]['generic_drug_name'] = med[0]['generic_drug_name'].str.decode('utf-8')
med[0]['icd10_code1'] = med[0]['icd10_code1'].str.decode('utf-8')
med[0]['icd10_code2'] = med[0]['icd10_code2'].str.decode('utf-8')
med[0]['icd10_code3'] = med[0]['icd10_code3'].str.decode('utf-8')

med[0][['drug_name1', 'drug_name2', 'drug_name3', 'drug_name4']] = med[0]['generic_drug_name'].str.split(';', expand = True)

ex = (
  med[0]
  .melt(id_vars = ['id', 'taken_med_past_month', 'num_med_taken'],
        value_vars = ['icd10_code1', 'icd10_code2', 'icd10_code3'],
        var_name = 'icd10_code_num',
        value_name = 'icd_10_cm_code')
)

ex2 = (
  med[0]
  .melt(id_vars = ['id', 'taken_med_past_month', 'num_med_taken'],
        value_vars = ['drug_name1', 'drug_name2', 'drug_name3', 'drug_name4'],
        var_name = 'drug_num',
        value_name = 'drug_name')
)

ex2 = ex2.loc[(~ex2['drug_name'].isin(['', '55555', '77777', '99999']))]

ex.head()
ex2.head()

[i.shape for i in [ex, ex2]] 

from great_tables import GT as gt
gt(
  ex.merge(ex2, 'inner').head(10)
).show()

ex_join = ex.merge(ex2, 'left')

ex_combo = ex_join.merge(df14, 'left', 'icd_10_cm_code')

ex_combo.head()
ex_combo.shape

ex_combo = ex_combo.drop(columns = 'description') 

gt(
  ex_combo
  .loc[ex_combo['id'] == 73558]
  .dropna(subset = 'drug_name')
  .drop_duplicates('drug_name')
  .sort_values(['drug_num'])
).show()

gt(
  ex_combo
  .dropna(subset = 'drug_name')
).show()

# import torch
# from transformers import pipeline
# import accelerate

# pipe = pipeline('text-generation', model = 'TinyLlama/TinyLlama-1.1B-Chat-v1.0', torch_dtype = torch.bfloat16)
# # We use the tokenizer's chat template to format each message - see https://huggingface.co/docs/transformers/main/en/chat_templating
# messages = [
#     {"role": "user",
#      "content": "What are the side effects for using insulin, ICD-10 number E11? Please print out a list of the side effects"},
# ]
# prompt = pipe.tokenizer.apply_chat_template(messages, tokenize = False, add_generation_prompt = True)
# outputs = pipe(prompt, max_new_tokens = 256, do_sample = True, temperature = 0.7, top_k = 50, top_p = 0.95)
# print(outputs[0]["generated_text"])


gt(ex_join.loc[~ex_join['generic_drug_name'].isin(['', '55555', '77777', '99999'])]).show()

ex_join = ex_join.loc[~ex_join['generic_drug_name'].isin(['', '55555', '77777', '99999'])]
ex_join['generic_drug_name'] = ex_join['generic_drug_name'].str.title()

message = []
for drug, code in zip(ex_join['generic_drug_name'], ex_join['icd_10_cm_code']):
  values = {'role': 'user',
             'content': f'Does {drug} (ICD-10 number {code}) cause long-term liver damage? Please include a scale from 1 to 10 with 1 being the least likely and 10 being the most likely'}
  message.append(values)

message[0]['content']

len(message)

#left off here
role
chat_prompt = [{'content': message[i]['content']} for i in range(len(message))]
            
for i in range(len(message)):
  answers = ollama.chat(model = 'llama3.2:1b', messages = message[i]['content'])
  chat_response.append(answers)] for i in range(len(message))]


import ollama

chat_response = []
for i in range(len(message)):
  answers = ollama.chat(model = 'llama3.2:1b', messages = message[i]['content'])
  chat_response.append(answers)
  
print(chat_response['message']['content'])

llama_answer = chat_response['message']['content']

import re

ex_search = re.search(r'Scale:\s*\d+', llama_answer)
ex_search


import gensim
import gensim.downloader as api
from gensim.models import KeyedVectors, Word2Vec


# ----------Sleep Issues----------
# [nhanes_link_doc(year, 'SLQ') for year in [2013, 2015]]

[print(sleep[i].info()) for i in range(len(sleep))]

sleep[0] = (
  sleep[0]
  .rename(
    columns = {'seqn': 'id',
               'sld010_h': 'sleep_hours',
               'slq050': 'dr_told_trouble_sleep'})
  .loc[:, ['id', 'sleep_hours', 'dr_told_trouble_sleep']]
)
sleep[0]['year'] = 2013

sleep[1] = (
  sleep[1]
  .rename(
    columns = {'seqn': 'id',
               'sld012': 'sleep_hours',
               'slq050': 'dr_told_trouble_sleep'}
  )
  .loc[:, ['id', 'sleep_hours', 'dr_told_trouble_sleep']]
)
sleep[1]['year'] = 2015

sleep = pd.concat([sleep[0], sleep[1]])
# sleep.head()

# ----------Depression Items----------
# [nhanes_link_doc(year, 'DPQ') for year in [2013, 2015]]

depress[0] = (
  depress[0]
  .rename(
    columns = {'seqn': 'id',
               'dpq010': 'little_interest_things',
               'dpq020': 'down_depress_hopeless',
               'dpq030': 'sleep_issues',
               'dpq040': 'tired',
               'dpq050': 'diet_issues',
               'dpq060': 'feed_bad_self',
               'dpq070': 'concentrate_issues',
               'dpq080': 'move_speak_issues',
               'dpq090': 'better_off_dead',
               'dpq100': 'diff_prob_cause'}
  )
)
depress[0]['year'] = 2013

depress[1] = (
  depress[1]
  .rename(
    columns = {'seqn': 'id',
               'dpq010': 'little_interest_things',
               'dpq020': 'down_depress_hopeless',
               'dpq030': 'sleep_issues',
               'dpq040': 'tired',
               'dpq050': 'diet_issues',
               'dpq060': 'feed_bad_self',
               'dpq070': 'concentrate_issues',
               'dpq080': 'move_speak_issues',
               'dpq090': 'better_off_dead',
               'dpq100': 'diff_prob_cause'}
  )
)
depress[1]['year'] = 2015

depress = pd.concat([depress[0], depress[1]])

# depress.head()


# ----------Merging of Data----------
data = sleep.merge(depress, 'left', on = ['id', 'year'])

# data['id'] = data['id'].astype('object')

def remove_and_clean(df: pd.DataFrame,
                     column: str,
                     dontknow: float,
                     refuse: float | None = None) -> pd.Series:
  """
  Include documentation
  """
  series = df[column].copy()
  series = series.replace(dontknow, np.nan)
  
  if refuse is not None:
    series = series[series != refuse]
    
  return series

rules = {
  'sleep_hours': {99, 77},
  'dr_told_trouble_sleep': {9, 7},
  'little_interest_things': {9, 7},
  'down_depress_hopeless': {9, 7},
  'sleep_issues': {9, 7},
  'tired': {9, 7},
  'diet_issues': {9, 7},
  'feed_bad_self': {9, 7},
  'concentrate_issues': {9, 7},
  'move_speak_issues': {9, 7},
  'better_off_dead': {9, 7},
  'diff_prob_cause': {9, 7}
}

rules.keys()
rules.items()

cleaned_dropped = {
  col: remove_and_clean(data, col, dontknow, refuse) for col, (dontknow, refuse) in rules.items()
}

clean = pd.DataFrame(cleaned_dropped)

clean['diff_prob_cause'].value_counts()

clean = clean.round(0)


def reverse_binary(df: pd.DataFrame,
                   column: str) -> pd.Series:
  """
  Include documentation
  """
  series = df[column].copy()
  if series.value_counts().shape[0] > 2:
    ValueError('Clean data first to remove missing (Don\\\'t know) and Refuse responses')
  
  series = np.where(series == 1, 1, 0)
  
  return pd.Series(series)

clean['dr_told_trouble_sleep'] = reverse_binary(clean, 'dr_told_trouble_sleep')

cond = [clean['sleep_hours'] > 8,
        clean['sleep_hours'] < 6,
        clean['sleep_hours'].between(6, 8, 'both')]
choice = [0, 0, 1]

clean['sleep_hours_bi'] = np.select(cond, choice)

# ----------train/test split & EDA----------
from sklearn.model_selection import train_test_split

#prep for train/test split
clean = clean.drop(columns = 'sleep_hours')

clean = clean.astype('category')

y = clean['sleep_hours_bi']
x = clean.drop(columns = 'sleep_hours_bi')

x_train, x_test, y_train, y_test = train_test_split(x, y, test_size = .2, random_state = 93025)
x_train_sub, x_val, y_train_sub, y_val = train_test_split(x_train, y_train, test_size = .2, random_state = 93025)

# use for visualizing training dataset
train = x_train_sub.join(y_train_sub).reset_index()
train.shape

# ----------LDA on Text Data----------
from gensim import corpora
from gensim.models import LdaModel
from gensim.parsing.preprocessing import preprocess_string, remove_stopwords, strip_punctuation

# Sample documents
documents = [
    "The quick brown fox jumps over the lazy dog",
    "A dog is a man's best friend",
    "Cats and dogs are popular pets",
    "Artificial intelligence is transforming the world",
    "Machine learning is a subset of AI"
]

# Preprocessing: Tokenize, remove stopwords and punctuation
processed_docs = [preprocess_string(doc) for doc in documents]

# Create a dictionary and corpus
dictionary = corpora.Dictionary(processed_docs)
corpus = [dictionary.doc2bow(doc) for doc in processed_docs]

# Build the LDA model
num_topics = 2  # Example: number of topics
lda_model = LdaModel(corpus=corpus, id2word=dictionary, num_topics=num_topics, passes=10)

# Print the topics
for topic_id, topic_words in lda_model.print_topics(num_words=5):
    print(f"Topic {topic_id}: {topic_words}")


# ----------Pipeline----------
from sklearn.impute import SimpleImputer
from sklearn.pipeline import make_pipeline, Pipeline
from sklearn.compose import ColumnTransformer
from sklearn.preprocessing import StandardScaler, OneHotEncoder

def preprocess_pipeline(df: pd.DataFrame):
  cat_col = df.select_dtypes(['category', 'object']).columns
  num_col = df.select_dtypes(['float', 'int']).columns
  
  num_transformer = Pipeline(steps = [
      ('imputer', SimpleImputer(strategy = 'mean')),
      ('scaler', StandardScaler())
  ])

  cat_transformer = Pipeline(steps = [
      ('imputer', SimpleImputer(strategy = 'constant')),
      ('onehot', OneHotEncoder(handle_unknown = 'ignore'))
  ])

  preprocessor = ColumnTransformer(
      transformers = [
          ('num', num_transformer, num_col),
          ('cat', cat_transformer, cat_col)
      ]
  )
  
  return preprocessor

# -------------------------------------modeling--------------------------------------------
from sklearn.linear_model import SGDClassifier

pipe = Pipeline(steps = [
    ('preprocessor', preprocess_pipeline(x_train)),
    ('classifier', SGDClassifier(
      class_weight = 'balanced',
      random_state = 93025
    ))
])

pipe.fit(x_train_sub, y_train_sub)

# pred = pipe.predict_proba(x_val)
# pred = np.where(pred >= .5, 1, 0)
# pred = pd.Series(pred[:, 1])

pred = pipe.predict(x_val)

roc_auc_score(y_val, pred)
cm = confusion_matrix(y_val, pred)
cm
tn, fp, fn, tp = cm.ravel().tolist()
total = sum([tn, fp, fn, tp])

round(tn/total*100, 3)
round(fp/total*100, 3)
round(fn/total*100, 3)
round(tp/total*100, 3)

sens = round(tp/total*100, 3)/(round(tp/total*100, 3) + round(fn/total*100, 3))
sens

spec = round(tn/total*100, 3)/(round(tn/total*100, 3) + round(fp/total*100, 3))
spec

accuracy_score(y_val, pred)
balanced_accuracy_score(y_val, pred)
precision_score(y_val, pred)
recall_score(y_val, pred)
f1_score(y_val, pred)

from sklearn.model_selection import GridSearchCV, cross_validate, KFold, StratifiedKFold, permutation_test_score
from sklearn.metrics import accuracy_score, balanced_accuracy_score, roc_auc_score, precision_score, recall_score, f1_score, confusion_matrix
from sklearn.inspection import permutation_importance

cv_score = cross_validate(pipe,
                          x_train,
                          y_train,
                          cv = 10,
                          scoring = ['precision', 'recall', 'roc_auc'],
                          return_train_score = True)
cv_score









# grid search
rf_grid = [
  {'rforest_regressor__n_estimators': [100, 500, 1000, 2000],
  'rforest_regressor__min_samples_split': [2],
  'rforest_regressor__min_samples_leaf': [1, 2],
  'rforest_regressor__max_features': ['sqrt', 'log2']},
  {'rforest_regressor__n_estimators': [100, 500, 1000, 2000],
  'rforest_regressor__min_samples_split': [2],
  'rforest_regressor__min_samples_leaf': [1, 2]}
  ]

tune_pipe = (
  Pipeline([
    ('scale_num_features',
      StandardScaler(),
    ),
    ('rforest_regressor',
    rand_forest(
      random_state = 7282025,
      verbose = 2,
      n_jobs = 10)
    )])
)

rf_search = (
  GridSearchCV(
    tune_pipe,
    param_grid = rf_grid,
    cv = 5,
    scoring = 'neg_root_mean_squared_error',
    verbose = 2
  )
)

rf_search.fit(x_train_imp, y_train_imp)

print(rf_search.best_score_)
print(rf_search.best_params_)

from sklearn.inspection import permutation_importance

important = (
  permutation_importance(
    rf_search.best_estimator_, 
    x_train_imp, 
    y_train_imp, 
    n_repeats = 20,
    n_jobs = 10,
    random_state = 7282025)
)

important_df = (
  pd
  .DataFrame({'var_names': x_train_imp.columns,
  'importance_mean': important.importances_mean})
  .sort_values('importance_mean', ascending = False)
)

print(important_df)

rmse_rf, perm_score_rf, pvalue_rf = permutation_test_score(
    pipe,
    x_train_imp,
    y_train_imp,
    scoring = "neg_root_mean_squared_error",
    cv = 5,
    n_permutations = 1000,
    random_state = 7282025,
    njobs = 10,
    verbose = 2
)

print(rmse_rf)
print(perm_score_rf)
print(pvalue_rf)

# -------------------------------------test set--------------------------------------------