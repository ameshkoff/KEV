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
from copy import deepcopy

# basic preprocessing ----------------------------------------
    
def eq_preproc(st_coeff_data, con_data, type_con, lg_k_data, component_name_for_yields):

    # list of products and reagents names for further using in output data
    
    if 'name' not in st_coeff_data.columns:

        st_coeff_data['name'] = ''

        for cl in st_coeff_data.drop('name', axis = 1):

            st_coeff_data['name'] = np.where(st_coeff_data[cl] > 0,
                                                   st_coeff_data['name'] + '+' + st_coeff_data[cl].apply(str) + cl,
                                                   st_coeff_data['name'])

            st_coeff_data['name'] = st_coeff_data['name'].replace({r'(\+)1([a-zA-Z])' : r'\1\2'}, regex = True)
            st_coeff_data['name'] = st_coeff_data['name'].replace(to_replace = r'^\+', value = '', regex = True)
        
    # matrix of stoich coeff with formal reactions added
    
    tmp = st_coeff_data.drop('name', axis=1)
    st_coeff_matrix = tmp.to_numpy()
    formal_matrix = np.eye(np.shape(st_coeff_matrix)[1], dtype = int)
    st_coeff_matrix = np.vstack((formal_matrix, st_coeff_matrix))
    
    # trim series columns if exists
    
    if 'series' in con_data.columns:
        con_data = deepcopy(con_data)
        con_data = con_data.drop('series', 1)
        type_con = type_con[:-1]
    
    # product names lists : full and base components only
    
    prod_names_con = list(con_data.columns)
    prod_names = prod_names_con + st_coeff_data['name'].tolist()
    
    # creating the vector of equilibrium constants including the formal reactions
    lg_k = (np.vstack((np.zeros((np.shape(st_coeff_matrix)[1], 1)), lg_k_data.to_numpy().astype(float))))
    
    # checking the consistency of reagent names in different sheets    
    if prod_names_con != list(st_coeff_data.drop('name', axis = 1)):
        print('Check the consistency of reagent names!')
    
    # concentrations matrix
    
    con_matrix = con_data.to_numpy()    
    
    # creating vector of indices of components with predetermined concentrations
    ign_indices = np.array(type_con.index[type_con == 'eq'])
    
    if component_name_for_yields not in prod_names:
        print('The component name for partition should be among those of basis components')
        
    idx, = np.where(component_name_for_yields == np.array(prod_names_con))
    
    return st_coeff_matrix, prod_names, lg_k, prod_names_con, con_matrix, ign_indices, idx, type_con

