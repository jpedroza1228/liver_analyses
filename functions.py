import webbrowser
from pandas import read_sas
from janitor import clean_names

def nhanes_link_doc(year: float, file: str):
  """
  year: float value
    - 2013 or 2015 for two years of data we have
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
  else:
    link = f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/{file}_I.htm'
  return webbrowser.open(link)

def link_convert(year, file):
  if year == 2013:
    return f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2013/DataFiles/{file}_H.xpt'
  else:
    return f'https://wwwn.cdc.gov/Nchs/Data/Nhanes/Public/2015/DataFiles/{file}_I.xpt'

def read_data_list(file_abbrev):
  df_list = [link_convert(year, file_abbrev) for year in [2013, 2015]]
  list_of_df = [read_sas(df).clean_names(case_type = 'snake') for df in df_list]
  return list_of_df