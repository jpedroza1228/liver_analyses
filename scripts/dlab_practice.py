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

from functions import nhanes_link_doc, link_convert, read_data_list, reverse_binary, remove_and_clean


# ----------To View Documentation for NHANES Variables----------
print(nhanes_link_doc.__doc__) 
# nhanes_link_doc(2013, 'TCHOL')

# ----------Loading in data----------
years = [2013, 2015, 2017]

# issue with cog because no cog for 2015
# cog = read_data_list()

med_cond = read_data_list('MCQ')
sleep = read_data_list('SLQ')
smoke = read_data_list('SMQ')
insure = read_data_list('HIQ')
demo = read_data_list('DEMO')
hdl = read_data_list('HDL')
ldl = read_data_list('TRIGLY')
total_chol = read_data_list('TCHOL')
bio = read_data_list('BIOPRO')
alc = read_data_list('ALQ')
body = read_data_list('BMX')
diet = read_data_list('DBQ')

# import pyarrow as pa
# import pyarrow.parquet as pq

# diet2013 = pa.Table.from_pandas(diet[0])
# diet2015 = pa.Table.from_pandas(diet[1])
# diet2017 = pa.Table.from_pandas(diet[2])

# pq.write_table(diet2013, here('data/diet2013.parquet'))
# pq.write_table(diet2015, here('data/diet2015.parquet'))
# pq.write_table(diet2017, here('data/diet2017.parquet'))



# not sure if including in model
# diabetes = read_data_list('DIQ')
# depress = read_data_list('DPQ')
# smokes = read_data_list('SMQRTU')
# med = read_data_list('RXQ_RX')
# phy_func = read_data_list('PFQ')
# pa = read_data_list('PAQ')
# care_access = read_data_list('HUQ')
# drug = read_data_list('DUQ')


# heart = read_data_list('CDQ')
# bp = read_data_list('BPQ')
# act_bp = read_data_list('BPX')



# ----------Data Wrangling-----------


# ----------Medical Conditions-----------
[nhanes_link_doc(year, 'MCQ') for year in years]

[med_cond[i].columns for i in range(len(med_cond))] 

med_cond1 = [df
 .rename(
   columns = {
     'seqn': 'id',
     'mcq160_l': 'told_liver_cond',
     'mcq365_a': 'dr_told_lose_wt',
     'mcq365_b': 'dr_told_exercise',
     'mcq365_c': 'dr_told_reduce_salt',
     'mcq365_d': 'dr_told_reduce_fat',
     'mcq370_a': 'you_control_wt',
     'mcq370_b': 'you_increase_exercise',
     'mcq370_c': 'you_reduce_salt',
     'mcq370_d': 'you_reduce_fat'
   }
 )
 for df in [med_cond[0], med_cond[1]]
]

med_cond2 = (
  med_cond[2]
  .rename(
   columns = {
     'seqn': 'id',
     'mcq160_l': 'told_liver_cond',
     'mcq366_a': 'dr_told_lose_wt',
     'mcq366_b': 'dr_told_exercise',
     'mcq366_c': 'dr_told_reduce_salt',
     'mcq366_d': 'dr_told_reduce_fat',
     'mcq371_a': 'you_control_wt',
     'mcq371_b': 'you_increase_exercise',
     'mcq371_c': 'you_reduce_salt',
     'mcq371_d': 'you_reduce_fat'
   }
   )
)

med_cond1 = [df[['id', 'told_liver_cond', 'dr_told_lose_wt', 'dr_told_exercise',
       'dr_told_reduce_salt', 'dr_told_reduce_fat', 'you_control_wt',
       'you_increase_exercise', 'you_reduce_salt', 'you_reduce_fat']]
 for df in med_cond1
]

med_cond2 = med_cond2[['id', 'told_liver_cond', 'dr_told_lose_wt', 'dr_told_exercise',
       'dr_told_reduce_salt', 'dr_told_reduce_fat', 'you_control_wt',
       'you_increase_exercise', 'you_reduce_salt', 'you_reduce_fat']]

med_cond_df = pd.concat([med_cond1[0], med_cond1[1], med_cond2])

# ----------Depression Items----------
# [nhanes_link_doc(year, 'DPQ') for year in years]

[depress[i].columns for i in range(len(depress))] 

depress = [df
 .rename(
   columns = 
   {'seqn': 'id',
   'dpq010': 'little_interest_things',
   'dpq020': 'down_depress_hopeless',
   'dpq030': 'sleep_issues',
   'dpq040': 'tired',
   'dpq050': 'diet_issues',
   'dpq060': 'feed_bad_self',
   'dpq070': 'concentrate_issues',
   'dpq080': 'move_speak_issues',
   'dpq090': 'better_off_dead',
   'dpq100': 'diff_prob_cause'}) for df in depress]

depress_df = pd.concat([depress[0], depress[1], depress[2]])

# ----------Sleep Issues----------
# [nhanes_link_doc(year, 'SLQ') for year in years]

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

# ----------Smoking----------
# [nhanes_link_doc(year, 'SMQ') for year in years]

smoke[0].columns == smoke[0].columns

# ----------Merging of Data----------
data = sleep.merge(depress, 'left', on = ['id', 'year'])

# data['id'] = data['id'].astype('object')

# depress_df_clean = pd.concat(
#     [remove_and_clean(depress_df, i, 9, 7).reset_index(drop=True)
#      for i in depress_df.columns],
#     axis = 1
# )
# depress_df_clean.columns = depress_df.columns

# depress_df_clean = depress_df_clean.round().astype('Int64')

# depress_df_clean['phq_total'] = depress_df_clean.iloc[:, 1:10].sum(axis = 1)


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