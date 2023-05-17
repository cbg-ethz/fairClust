#!/usr/bin/env python
# coding: utf-8
# %%

import numpy as np
import pandas as pd
from sklearn.cluster import KMeans
from sklearn.metrics import pairwise_distances_argmin_min

# %%

# Assume mydata is a pandas DataFrame
mydata = pd.read_csv('data/processed_data/adult_binerized.csv')

# %%

# Separate the protected attribute from the data
protected_attribute = mydata['sex']
data = mydata.drop(columns='sex')

# Perform k-means clustering
kmeans = KMeans(n_clusters=2, random_state=0).fit(data)

# Get the cluster assignments
assignments = kmeans.labels_

# Create a DataFrame to hold the assignments and the protected attribute
df = pd.DataFrame({'cluster': assignments, 'sex': protected_attribute})

# Calculate the proportion of each sex in each cluster
proportions = df.groupby('cluster')['sex'].value_counts(normalize=True)

# Print the proportions to see if the clusters are fair
print(proportions)

# If the proportions are not fair, you can use various techniques to adjust the clusters.
# Here's a simple approach that reassigns some points from the overrepresented group
# to the underrepresented group until the clusters are fair.

# Define fairness threshold
fairness_threshold = 0.1

for cluster in df['cluster'].unique():
    proportions_in_cluster = proportions.loc[cluster]
    if abs(proportions_in_cluster.diff().iloc[-1]) > fairness_threshold:
        # The cluster is not fair, reassign some points
        group_to_reduce = proportions_in_cluster.idxmax()
        df_to_reassign = df[(df['cluster'] == cluster) & (df['sex'] == group_to_reduce)]
        idx_to_reassign = df_to_reassign.sample(frac=fairness_threshold).index
        df.loc[idx_to_reassign, 'cluster'] = 1 - cluster  # Assuming 2 clusters (0 and 1)

# Check the proportions again to see if the clusters are now fair
proportions = df.groupby('cluster')['sex'].value_counts(normalize=True)
print(proportions)


# %%
