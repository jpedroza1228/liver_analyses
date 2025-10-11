import webbrowser
import pandas as pd
import numpy as np
from janitor import clean_names

def nhanes_link_doc(year: float, file: str):
  """
  year: float value
    - 2013, 2015, and & 2017 for three years of data
  file: str value
    - extension to find specific values for data
    - current extensions include:
      - [SLQ, DPQ, SMQ, SMORTU, RXQ_RX, PFQ,
         PAQ, MCQ, HIQ, HUQ, DUQ, DIQ, DBQ,
         ALQ, CDQ, DEMO, HDL, TRIGLY, DBQ,
         BPX, BIOPRO, BMX, TCHOL]
  """
  if year == 2013:
    link = f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/{file}_H.htm'
  elif year == 2015:
    link = f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/{file}_I.htm'
  else:
    link = f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/{file}_J.htm'
  return webbrowser.open(link)

def link_convert(year, file):
  if year == 2013:
    return f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/{file}_H.xpt'
  elif year == 2015:
    return f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/{file}_I.xpt'
  else: 
    return f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2017/DataFiles/{file}_J.xpt'

def read_data_list(file_abbrev):
  df_list = [link_convert(year, file_abbrev) for year in [2013, 2015, 2017]]
  list_of_df = [pd.read_sas(df).clean_names(case_type = 'snake') for df in df_list]
  return list_of_df

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