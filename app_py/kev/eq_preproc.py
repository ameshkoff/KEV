# ########################################################## #
#                                                            #
# Name: KEV:Constant Evaluator                               #
# Author: GGamov                                             #
# Date: 2019                                                 #
#                                                            #
# ########################################################## #

# import libraries -------------------------------------------

import numpy as np
import re

# basic preprocessing ----------------------------------------
    
def eq_preproc(st_coeff_data, con_data, type_con, lg_k_data, component_name_for_yields):
    
    # checking if there are several series
    
    if 'series' not in con_data.columns:        
        con_data['series'], type_con[np.shape(st_coeff_data)[1]] = '', ''

    # series variables
    
    ser_info = con_data['series'].to_numpy()
    ser_unique = np.unique(ser_info)
    ser_num = np.shape(np.unique(ser_info))[0]

    # matrix of stoich coeff with formal reactions added
    st_coeff_matrix = st_coeff_data.to_numpy()
    formal_matrix = np.eye(np.shape(st_coeff_matrix)[1], dtype = int)
    st_coeff_matrix = np.vstack((formal_matrix, st_coeff_matrix))
    
    # list of products and reagents names for further using in output data
    
    st_coeff_data['prod_names'] = ''
        
    for cl in st_coeff_data.drop('prod_names', axis = 1):
        
        st_coeff_data['prod_names'] = np.where(st_coeff_data[cl] > 0,
                                               st_coeff_data['prod_names'] + '+' + st_coeff_data[cl].apply(str) + cl,
                                               st_coeff_data['prod_names'])
        
        st_coeff_data['prod_names'] = st_coeff_data['prod_names'].replace({r'(\+)1([a-zA-Z])' : r'\1\2'}, regex = True)
        st_coeff_data['prod_names'] = st_coeff_data['prod_names'].replace(to_replace = r'^\+', value = '', regex = True)
        
    # product names lists : full and base components only
    
    prod_names_con = list(con_data.drop('series', axis = 1))
    prod_names = prod_names_con + st_coeff_data['prod_names'].tolist()
    
    # creating the vector of equilibrium constants including the formal reactions
    lg_k = (np.vstack((np.zeros((np.shape(st_coeff_matrix)[1], 1)), lg_k_data.to_numpy().astype(float))))
    
    # checking the consistency of reagent names in different sheets    
    if prod_names_con != list(st_coeff_data.drop('prod_names', axis = 1)):
        print('Check the consistency of reagent names!')
    
    # split concentrations matrix
    con_matrix = [g for _, g in con_data.groupby(['series'])]
        
    for cnm_index, cnm in enumerate(con_matrix):
        con_matrix[cnm_index] = cnm.drop('series', axis = 1).to_numpy().astype(float)
    
    ser_counts = con_data.groupby(['series']).size().tolist();
    
    # creating vector of indices of components with predetermined concentrations
    ign_indices = np.array(type_con.index[type_con == 'eq'])
    
    if component_name_for_yields not in prod_names:
        print('The component name for partition should be among those of basis components')
        
    idx, = np.where(component_name_for_yields == np.array(prod_names_con))
    
    return ser_num, st_coeff_matrix, prod_names, lg_k, prod_names_con, con_matrix, ign_indices, idx, ser_counts, ser_info, type_con 
    # ser_num, ser_counts, ser_info not further used yet! 