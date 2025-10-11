import os
os.environ['QT_API'] = 'PyQt6'

import duckdb as db
from pyhere import here

from functions import nhanes_link_doc, link_convert, read_data_list, reverse_binary, remove_and_clean

years = [2013, 2015, 2017]

con = db.connect(here('database/nhanes_db.duckdb'))

# will be taking participants 20+

# print(nhanes_link_doc.__doc__)
# [nhanes_link_doc(i, 'SMQ') for i in years]
# .to_parquet('../liver_analyses/data/cleaned/diet.parquet')

con.sql(
  """
  SELECT table_name
  FROM information_schema.tables
  WHERE table_name NOT LIKE '%2013%'
  AND table_name NOT LIKE '%2015%'
  AND table_name NOT LIKE '%2017%'; 
  """)

# nhanes_link_doc(2013, 'ALQ')
# [nhanes_link_doc(i, 'ALQ') for i in years]

# 

# ----------Work Here for Joining & Final Working Dataset----------

con.sql(
  """"
  SELECT *
  FROM bio
  LIMIT 10;
  """
)

con.sql(
  """
  SELECT *
  FROM demo
  INNER JOIN bio ON demo.id = bio.id
  INNER JOIN alcohol ON bio.id = alcohol.id
  INNER JOIN bmi_waist ON bmi_waist.id = alcohol.id
  INNER JOIN diet ON diet.id = bmi_waist.id
  INNER JOIN hdl ON hdl.id = diet.id
  INNER JOIN insurance ON insurance.id = hdl.id
  INNER JOIN ldl ON ldl.id = insurance.id
  INNER JOIN med_cond ON med_cond.id = ldl.id
  INNER JOIN sleep ON sleep.id = med_cond.id
  INNER JOIN smoke ON smoke.id = sleep.id
  INNER JOIN total_chol ON total_chol.id = smoke.id
  WHERE demo.age >= 20;
  """
).show()


con.sql(
  """
  SELECT *
  FROM bio
  INNER JOIN alcohol ON bio.id = alcohol.id
  LIMIT 10;
  """
).show()


con.sql(
  """
  SELECT id,
  CASE
  WHEN ever_45_drink_everyday = 1 THEN 1
  WHEN ever_45_drink_everyday = 2 THEN 0
  ELSE ever_45_drink_everyday
  END AS ever_45_drink_everyday
  FROM alcohol
  LIMIT 10;
  """
)

con.sql("""SELECT * FROM demo LIMIT 1;""").show()

con.sql(
  """
  SELECT *
  FROM 
  """
)

con.sql(
  """
  SELECT id,
  CASE
  WHEN sex = 1 THEN 1
  ELSE sex = 0
  END AS female,
  age,
  race_ethnic,
  birth_country,
  CASE
  WHEN citizen = 1 THEN 1
  ELSE citizen = 0
  END AS citizen,
  length_us,
  ed,
  marital,
  CASE 
  WHEN annual_house_income = 1 THEN 1
  WHEN annual_house_income = 2 THEN 1
  WHEN annual_house_income = 3 THEN 1
  WHEN annual_house_income = 4 THEN 1
  WHEN annual_house_income = 5 THEN 2
  WHEN annual_house_income = 6 THEN 2
  WHEN annual_house_income = 7 THEN 3
  WHEN annual_house_income = 8 THEN 3
  WHEN annual_house_income = 9 THEN 4
  WHEN annual_house_income = 10 THEN 4
  WHEN annual_house_income = 13 THEN 1
  WHEN annual_house_income = 14 THEN 5
  WHEN annual_house_income = 15 THEN 6
  WHEN annual_house_income = 12 THEN 7
  ELSE annual_house_income
  END AS annual_house_income,
  num_people_house,
  num_people_fam,
  fam_income_to_pov_ratio
  FROM demo
  LIMIT 10; 
  """
).show()






# ----------SQL Practice----------

# Looking for NULL values in column
con.sql(
  """
  SELECT 
  COUNT(*)
  FROM demo
  WHERE birth_country IS NULL
  LIMIT 10; 
  """
).show()

# Counting Indiviudal Values in Column
con.sql(
  """
  SELECT 
  race_ethnic,
  COUNT(race_ethnic) AS race_count
  FROM demo
  GROUP BY race_ethnic
  ORDER BY race_ethnic DESC
  LIMIT 10; 
  """
).show()

# ---------Putting Cleaned Parquet Files Into DataFrame----------
# df_names = ['alcohol', 'bio', 'bmi_waist', 'demo', 'diet', 'hdl',
#             'insurance', 'ldl', 'med_cond', 'sleep', 'smoke',
#             'total_chol']

# for i in df_names:
#   con.execute(
#       f"""
#       CREATE TABLE {i} AS
#       SELECT * FROM read_parquet('../liver_analyses/data/cleaned/{i}.parquet')
#       """
#   )

# con.execute(
#   f"""
#   CREATE OR REPLACE TABLE alcohol AS
#   SELECT * FROM read_parquet('../liver_analyses/data/cleaned/alcohol.parquet')
#   """
# )

# -----------Already Removed "Refused" and Changed "Don't Know" to Missing----------
# for i in ['alc2013', 'alc2015', 'alc2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['bio2013', 'bio2015', 'bio2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['body2013', 'body2015', 'body2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['demo2013', 'demo2015', 'demo2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['diet2013', 'diet2015', 'diet2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['hdl2013', 'hdl2015', 'hdl2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['insure2013', 'insure2015', 'insure2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)
  
# for i in ['ldl2013', 'ldl2015', 'ldl2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['med_cond2013', 'med_cond2015', 'med_cond2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['sleep2013', 'sleep2015', 'sleep2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)

# for i in ['smoke2013', 'smoke2015', 'smoke2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)
  
# for i in ['total_chol2013', 'total_chol2015', 'total_chol2017']:
#   con.execute(
#     f"""
#     CREATE TABLE {i} AS 
#     SELECT * FROM read_parquet('../liver_analyses/data/{i}.parquet')
#     """)



# # all the data for medical conditions   
# con.sql("""
# WITH combined AS (
#   SELECT seqn AS id,
#          mcq160_l AS told_liver_cond,
#          mcq365_a AS dr_told_lose_wt,
#          mcq365_b AS dr_told_exercise,
#          mcq365_c AS dr_told_reduce_salt,
#          mcq365_d AS dr_told_reduce_fat,
#          mcq370_a AS you_control_wt,
#          mcq370_b AS you_increase_exercise,
#          mcq370_c AS you_reduce_salt,
#          mcq370_d AS you_reduce_fat
#   FROM med_cond2013
#   UNION ALL
#   SELECT seqn, mcq160_l, mcq365_a, mcq365_b, mcq365_c, mcq365_d,
#          mcq370_a, mcq370_b, mcq370_c, mcq370_d
#   FROM med_cond2015
#   UNION ALL
#   SELECT seqn, mcq160_l, mcq366_a, mcq366_b, mcq366_c, mcq366_d,
#          mcq371_a, mcq371_b, mcq371_c, mcq371_d
#   FROM med_cond2017
# )
# SELECT id,
#   CASE
#   WHEN told_liver_cond = 9 THEN NULL
#   ELSE told_liver_cond
#   END AS told_liver_cond,
#   CASE
#   WHEN dr_told_lose_wt = 9 THEN NULL
#   ELSE dr_told_lose_wt
#   END AS dr_told_lose_wt,
#   CASE
#   WHEN dr_told_exercise = 9 THEN NULL
#   ELSE dr_told_exercise
#   END AS dr_told_exercise,
#   CASE
#   WHEN dr_told_reduce_salt = 9 THEN NULL
#   ELSE dr_told_reduce_salt
#   END AS dr_told_reduce_salt,
#   CASE
#   WHEN dr_told_reduce_fat = 9 THEN NULL
#   ELSE dr_told_reduce_fat
#   END AS dr_told_reduce_fat,
#   CASE
#   WHEN you_control_wt = 9 THEN NULL
#   ELSE you_control_wt
#   END AS you_control_wt,
#   CASE
#   WHEN you_increase_exercise = 9 THEN NULL
#   ELSE you_increase_exercise
#   END AS you_increase_exercise,
#   CASE
#   WHEN you_reduce_salt = 9 THEN NULL
#   ELSE you_reduce_salt
#   END AS you_reduce_salt,
#   CASE
#   WHEN you_reduce_fat = 9 THEN NULL
#   ELSE you_reduce_fat
#   END AS you_reduce_fat,
# FROM combined
# WHERE told_liver_cond != 7
#   AND dr_told_lose_wt != 7
#   AND dr_told_exercise != 7
#   AND dr_told_reduce_salt != 7
#   AND dr_told_reduce_fat != 7
#   AND you_control_wt != 7
#   AND you_increase_exercise != 7
#   AND you_reduce_salt != 7
#   AND you_reduce_fat != 7;
# """).to_parquet('../liver_analyses/data/cleaned/med_cond.parquet')

# # Alcohol Use
# [nhanes_link_doc(year, 'ALQ') for year in years]

# con.sql(
#   """
#   WITH alc_combo AS (
#     SELECT seqn AS id,
#     alq151 AS ever_45_drink_everyday
#     FROM alc2013
#     UNION ALL
#     SELECT seqn,
#     alq151
#     FROM alc2015
#     UNION ALL
#     SELECT seqn,
#     alq151
#     FROM alc2017
#   )
#   SELECT id,
#   CASE
#   WHEN ever_45_drink_everyday = 9 THEN NULL
#   ELSE ever_45_drink_everyday
#   END AS ever_45_drink_everyday
#   FROM alc_combo
#   WHERE ever_45_drink_everyday != 7;
#   """).to_parquet('../liver_analyses/data/cleaned/alcohol.parquet')


# # HDL
# [nhanes_link_doc(year, 'HDL') for year in years]

# con.sql(
#   """
#   WITH hdl_combo AS (
#     SELECT seqn AS id,
#     lbdhdd AS hdl_mg_dl
#     FROM hdl2013
#     UNION ALL
#     SELECT seqn,
#     lbdhdd
#     FROM hdl2015
#     UNION ALL
#     SELECT seqn,
#     lbdhdd
#     FROM hdl2017
#     )
#     SELECT *
#     FROM hdl_combo;
#   """
# ).to_parquet('../liver_analyses/data/cleaned/hdl.parquet')

# # LDL
# [nhanes_link_doc(year, 'TRIGLY') for year in years]

# con.sql(
#   """
#   WITH ldl_combo AS (
#     SELECT seqn AS id,
#     lbxtr AS trigly_mg_dl,
#     lbdldl AS ldl_mg_dl
#     FROM ldl2013
#     UNION ALL 
#     SELECT seqn,
#     lbxtr,
#     lbdldl
#     FROM ldl2015
#     UNION ALL
#     SELECT seqn,
#     lbxtr,
#     lbdldl
#     FROM ldl2017
#   )
#   SELECT *
#   FROM ldl_combo;
#   """
# ).to_parquet('../liver_analyses/data/cleaned/ldl.parquet')


# con.sql(
#   """
#   WITH total_chol_combo AS (
#     SELECT seqn AS id,
#     lbxtc AS total_chol_mg_dl
#     FROM total_chol2013
#     UNION ALL
#     SELECT seqn,
#     lbxtc
#     FROM total_chol2015
#     UNION ALL
#     SELECT seqn,
#     lbxtc
#     FROM total_chol2017
#   )
#   SELECT *
#   FROM total_chol_combo;
#   """).to_parquet('../liver_analyses/data/cleaned/total_chol.parquet')


# # Biological Labs
# con.sql(
#   """
#   WITH bio_combo AS (
#     SELECT seqn AS id,
#     lbxsal AS albumin_g_dl,
#     lbxsapsi AS alp_iu_l,
#     lbxsassi AS ast_u_l,
#     lbxsatsi AS alt_u_l,
#     lbxsgtsi AS ggt_u_l,
#     lbxstb AS total_bilirubin_mg_dl,
#     lbxstr as tri_mg_dl
#     FROM bio2013
#     UNION ALL
#     SELECT seqn,
#     lbxsal,
#     lbxsapsi,
#     lbxsassi,
#     lbxsatsi,
#     lbxsgtsi,
#     lbxstb,
#     lbxstr
#     FROM bio2015
#     UNION ALL
#     SELECT seqn,
#     lbxsal,
#     lbxsapsi,
#     lbxsassi,
#     lbxsatsi,
#     lbxsgtsi,
#     lbxstb,
#     lbxstr
#     FROM bio2017
#     )
#   SELECT *
#   FROM bio_combo;
#   """
# ).to_parquet('../liver_analyses/data/cleaned/bio.parquet')


# # Demographics
# con.sql(
#   """
#   WITH demo_combo AS (
#     SELECT seqn AS id,
#     riagendr AS sex,
#     ridageyr AS age,
#     ridreth3 AS race_ethnic,
#     dmdborn4 AS birth_country,
#     dmdcitzn AS citizen,
#     dmdyrsus AS length_us,
#     dmdeduc2 AS ed,
#     dmdmartl AS marital,
#     dmdhhsiz AS num_people_house,
#     dmdfmsiz AS num_people_fam,
#     indhhin2 AS annual_house_income,
#     indfmpir AS fam_income_to_pov_ratio
#     FROM demo2013
#     UNION ALL
#     SELECT seqn,
#     riagendr,
#     ridageyr,
#     ridreth3,
#     dmdborn4,
#     dmdcitzn,
#     dmdyrsus,
#     dmdeduc2,
#     dmdmartl,
#     dmdhhsiz,
#     dmdfmsiz,
#     indhhin2,
#     indfmpir
#     FROM demo2015
#     UNION ALL
#     SELECT seqn,
#     riagendr,
#     ridageyr,
#     ridreth3,
#     dmdborn4,
#     dmdcitzn,
#     dmdyrsus,
#     dmdeduc2,
#     dmdmartl,
#     dmdhhsiz,
#     dmdfmsiz,
#     indhhin2,
#     indfmpir
#     FROM demo2017
#     )
#   SELECT id,
#   sex,
#   age,
#   race_ethnic,
#   CASE
#   WHEN birth_country = 99 THEN NULL
#   ELSE birth_country
#   END AS birth_country,
#   CASE
#   WHEN citizen = 9 THEN NULL
#   ELSE citizen
#   END AS citizen,
#   CASE
#   WHEN length_us = 99 THEN NULL
#   ELSE length_us
#   END AS length_us,
#   CASE
#   WHEN ed = 9 THEN NULL
#   ELSE ed
#   END AS ed,
#   CASE 
#   WHEN marital = 99 THEN NULL
#   ELSE marital
#   END AS marital,
#   CASE
#   WHEN annual_house_income = 99 THEN NULL
#   ELSE annual_house_income
#   END AS annual_house_income, 
#   num_people_house,
#   num_people_fam,
#   fam_income_to_pov_ratio
#   FROM demo_combo
#   WHERE birth_country != 77
#   AND citizen != 7
#   AND length_us != 77
#   AND ed != 7
#   AND marital != 77
#   AND annual_house_income != 77;
#   """
# ).to_parquet('../liver_analyses/data/cleaned/demo.parquet')

# # Diet
# con.sql(
#   """
#   WITH diet_combo AS (
#     SELECT seqn AS id,
#     dbd895 AS num_meal_not_home_prepare,
#     dbd900 AS num_meal_fast_food,
#     dbd905 AS num_ready_eat_food_30day,
#     dbd910 AS num_frozen_meal_30day,
#     cbq596 AS heard_my_plate
#     FROM diet2013
#     UNION ALL
#     SELECT seqn,
#     dbd895,
#     dbd900,
#     dbd905,
#     dbd910,
#     cbq596
#     FROM diet2015
#     UNION ALL
#     SELECT seqn,
#     dbd895,
#     dbd900,
#     dbd905,
#     dbd910,
#     cbq596
#     FROm diet2017
#   )
#   SELECT id,
#   CASE
#   WHEN num_meal_not_home_prepare = 9999 THEN NULL
#   WHEN num_meal_not_home_prepare = 5555 THEN 22
#   ELSE num_meal_not_home_prepare
#   END AS num_meal_not_home_prepare,
#   CASE
#   WHEN num_meal_fast_food = 9999 THEN NULL
#   WHEN num_meal_fast_food = 5555 THEN 22
#   ELSE num_meal_fast_food
#   END AS num_meal_fast_food,
#   CASE
#   WHEN num_ready_eat_food_30day = 9999 THEN NULL
#   WHEN num_ready_eat_food_30day = 6666 THEN 91
#   ELSE num_ready_eat_food_30day
#   END AS num_ready_eat_food_30day,
#   CASE
#   WHEN num_frozen_meal_30day = 9999 THEN NULL
#   WHEN num_frozen_meal_30day = 6666 THEN 91
#   ELSE num_frozen_meal_30day
#   END AS num_frozen_meal_30day,
#   CASE
#   WHEN heard_my_plate = 9 THEN NULL
#   ELSE heard_my_plate
#   END AS heard_my_plate
#   FROM diet_combo
#   WHERE num_meal_not_home_prepare != 7777
#   AND num_meal_fast_food != 7777
#   AND num_ready_eat_food_30day != 7777
#   AND num_frozen_meal_30day != 7777
#   AND heard_my_plate != 7; 
#   """
# ).to_parquet('../liver_analyses/data/cleaned/diet.parquet')

# # Insurance Coverage
# con.sql(
#   """
#   WITH insure_combo AS (
#     SELECT seqn AS id,
#     hiq011 AS insure_coverage
#     FROM insure2013
#     UNION ALL
#     SELECT seqn,
#     hiq011
#     FROM insure2015
#     UNION ALL
#     SELECT seqn,
#     hiq011
#     FROM insure2017
#   )
#   SELECT id,
#   CASE
#   WHEN insure_coverage = 9 THEN NULL
#   ELSE insure_coverage
#   END AS insure_coverage
#   FROM insure_combo
#   WHERE insure_coverage != 7;
#   """
# ).to_parquet('../liver_analyses/data/cleaned/insurance.parquet')


# # BMI/Waist Circumference Measurements
# con.sql(
#   """
#   WITH body_combo AS (
#     SELECT seqn AS id,
#     bmxbmi AS bmi,
#     bmxwaist AS waist_circumference
#     FROM body2013
#     UNION ALL
#     SELECT seqn,
#     bmxbmi,
#     bmxwaist
#     FROM body2015
#     UNION ALL
#     SELECT seqn,
#     bmxbmi,
#     bmxwaist
#     FROM body2017
#   )
#   SELECT *
#   FROM body_combo; 
#   """
# ).to_parquet('../liver_analyses/data/cleaned/bmi_waist.parquet')


# # Sleep Issues
# con.sql(
#   """
#   WITH combo AS (
#     SELECT seqn AS id,
#     sld010_h AS sleep_hours_weekday,
#     slq050 AS told_dr_trouble_sleep
#     FROM sleep2013
#     UNION ALL
#     SELECT seqn,
#     sld012 AS sleep_hours_weekday,
#     slq050
#     FROM sleep2015
#     UNION ALL
#     SELECT seqn,
#     sld012 AS sleep_hours_weekday,
#     slq050
#     FROM sleep2017
#   )
#   SELECT id,
#   CASE
#   WHEN sleep_hours_weekday = 99 THEN NULL
#   ELSE sleep_hours_weekday
#   END AS sleep_hours_weekday,
#   CASE
#   WHEN told_dr_trouble_sleep = 9 THEN NULL
#   ELSE told_dr_trouble_sleep
#   END AS told_dr_trouble_sleep
#   FROM combo
#   WHERE sleep_hours_weekday != 77
#   AND told_dr_trouble_sleep != 7;
#   """
# ).to_parquet('../liver_analyses/data/cleaned/sleep.parquet')

# # Smoking
# con.sql(
#   """
#   WITH smoke_combo AS (
#     SELECT seqn AS id,
#     smq020 AS smoke_100cig_lifetime,
#     smd030 AS age_start_reg_smoke,
#     FROM smoke2013
#     UNION ALL
#     SELECT seqn,
#     smq020,
#     smd030
#     FROM smoke2015
#     UNION ALL
#     SELECT seqn,
#     smq020,
#     smd030
#     FROM smoke2017
#   )
#   SELECT id,
#   CASE
#   WHEN smoke_100cig_lifetime = 9 THEN NULL
#   ELSE smoke_100cig_lifetime
#   END AS smoke_100cig_lifetime,
#   CASE
#   WHEN age_start_reg_smoke = 999 THEN NULL
#   ELSE age_start_reg_smoke
#   END AS age_start_reg_smoke
#   FROM smoke_combo
#   WHERE smoke_100cig_lifetime != 7
#   AND age_start_reg_smoke != 777; 
#   """
# ).to_parquet('../liver_analyses/data/cleaned/smoke.parquet')
