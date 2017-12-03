'''
 Descriptive Data for AphasiaBank
 Creates CSV output files for descriptive statistics of the data, histograms,
 and scatter plot matricies.

 By: Paula
'''

import pandas, sys, os
sys.path.append('/anaconda/lib/python3.5/site-packages') #for seaborn
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np 

def loadCSV(file_name):
    # Loads all datasets for analysis and sets aside the data without cat. columns for future reference. The dataset(s) are split by hardcoded outcomes and all categorical data for the outcomes is removed for descriptive statistics. Descriptive statistics for nominal  values must be manually created (median, mode, etc.)
    cat_cols = ['Handedness', 'Sex', 'Race', 'Knows.Cinderella', 'Unnamed: 0', 'Participant.ID']
    dataset = pandas.read_csv(file_name)

    data_temp =  dataset.drop(cat_cols, axis=1)

    aphasia_X = data_temp[data_temp.outcome == 1]
    aphasia_X = aphasia_X.drop(['outcome'], axis=1)

    noAphasia_X = data_temp[data_temp.outcome == 0]
    noAphasia_X = noAphasia_X.drop(['outcome'], axis=1)

    return aphasia_X, noAphasia_X, data_temp

def descriptiveData(dataset_dict, outpath):
    # Generates a CSV of descriptive statistics for the outcome datasets avail.
    for key in dataset_dict:
        dataset = dataset_dict.get(key)
        desc_data = dataset.describe()
        desc_data.to_csv(outpath+'descriptive'+key+'.csv')

def histograms(dataset_dict, X, outpath):
    # Generates a .png of histograms for each outcome dataset available. Do plt.show instead of L46 to edit the sizing before saving.
    for key in dataset_dict:
        print("Processing histogram for: "+ key)
        dataset = dataset_dict.get(key)
        dataset.hist(alpha=0.4)
        plt.savefig(outpath + key + "_hist.png")

def scatterMatrix(X, outpath):
    print("\nProcessing scatterplot matrix, please wait. This step takes up to 3 min.")
    print("\nLarge datasets will yield a UserWarning Error below.")
    g = sns.PairGrid(X, hue="outcome")
    g.map_diag(plt.hist)
    g.map_offdiag(plt.scatter)
    g.add_legend();
    print("\nProcessing file for scatterplot matrix, please wait.")
    g.savefig(outpath + "scatterplot_matrix.png")

def ageDistribution(aphasia_X,noAphasia_X, outpath):
    fig = plt.figure()
    plt.hist(aphasia_X['Age'], alpha=0.5, label='Aphasia (n=39)')
    plt.hist(noAphasia_X['Age'], alpha=0.5, label='No Aphasia (n=63)')
    plt.legend(loc='upper right')
    plt.plot()
    fig.suptitle('Sampled Population Ages with and without Non-fluent Aphasia', fontsize=14)
    plt.xlabel('Age', fontsize=12)
    plt.ylabel('Number of Individuals', fontsize=12)
    fig.savefig(outpath+'ages.png')

def main():
    file_name = os.path.dirname(os.path.abspath(__file__))+'/Output CSV/combined.csv'
    outpath = os.path.dirname(os.path.abspath(__file__)) + '/Plots/'
    aphasia_X, noAphasia_X, X = loadCSV(file_name)
    dataset_dict = {'Aphasia': aphasia_X, 'No Aphasia': noAphasia_X}
    #descriptiveData(dataset_dict, outpath)
    #histograms(dataset_dict, X, outpath)
    #scatterMatrix(X, outpath)
    ageDistribution(aphasia_X, noAphasia_X,outpath)

if __name__ == "__main__":
    main()