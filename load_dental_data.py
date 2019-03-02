#!/usr/bin/env python2
# -*- coding: utf-8 -*-.
"""
Created on Mon Aug 27 20:33:28 2018

@author: Megan Stamper
"""
#%%
import pandas as pd
from os import listdir

# Import the dentist data
colnames = ['org_id','name','national_grouping','high_level_health_geo','address_line_1',
            'address_line_2','address_line_3','address_line_4','address_line_5','postcode',
            'open_date','close_date','status_code','sub_type_code','parent_org_id','join_parent_date',
            'left_parent_date','telephone','null_1','null_2','null_3','amended_record_flag','null_4','null_5',
            'null_6','null_7','null_8']
dentists_data = pd.read_csv('egdpprac.csv', names = colnames, header = None)
null_columns = [col for col in dentists_data if col.startswith('null')]
dentists_data.drop(dentists_data.loc[:, null_columns], axis = 1, inplace = True)
dentists_data.postcode = dentists_data.postcode.str.replace(' ', '')
#%%
# Get the list of postcode data files
colnames_postcode_data = pd.read_csv('codepo_gb/Doc/Code-Point_Open_Column_Headers.csv', header = None).loc[1]
file_names = listdir('codepo_gb/Data/CSV')

file_name_beginnings = [0] * len(file_names)
for file_name_index, file_name in enumerate(file_names):
    file_name_beginnings[file_name_index] = file_name.split('.')[0]
file_name_beginnings.sort()
#%%
# Iterate through the postcode data files, adding the Eastings and Northings for each postcode
postcode_dataframe = pd.DataFrame()
dentists_data_w_postcode = dentists_data
for index, postcode_file in enumerate(file_name_beginnings):
    postcode_data = pd.read_csv('codepo_gb/Data/CSV/' + postcode_file + '.csv',
                                names = colnames_postcode_data, header = None)
    postcode_data.Postcode = postcode_data.Postcode.str.replace(' ', '')
    dentists_data_w_postcode = dentists_data_w_postcode.merge(postcode_data.loc[:,['Postcode', 'Eastings', 'Northings']], 
                        left_on='postcode', right_on='Postcode', how='left')
    if index >= 1:
        x_columns = [col for col in dentists_data_w_postcode if col.endswith('x')]
        y_columns = [col for col in dentists_data_w_postcode if col.endswith('y')]
        columns_to_remove = x_columns + y_columns
        dentists_data_w_postcode['Postcode'] = dentists_data_w_postcode.Postcode_x.combine_first(dentists_data_w_postcode.Postcode_y)
        dentists_data_w_postcode['Eastings'] = dentists_data_w_postcode.Eastings_x.combine_first(dentists_data_w_postcode.Eastings_y)
        dentists_data_w_postcode['Northings'] = dentists_data_w_postcode.Northings_x.combine_first(dentists_data_w_postcode.Northings_y)
        dentists_data_w_postcode.drop(dentists_data_w_postcode
                                      .loc[:, columns_to_remove], axis = 1, inplace = True)
#%%
# Write out the dentists data with eastings and northings
dentists_data_w_postcode.to_csv('dentist_data.csv')
