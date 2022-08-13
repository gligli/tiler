import argparse
import numpy as np
from sklearn import cluster, preprocessing
from sklearn.neighbors import NearestCentroid

parser = argparse.ArgumentParser()
parser.add_argument('-i', help = 'filename');
parser.add_argument('-n', help = 'num_clusters', type = int);
parser.add_argument('-t', help = 'threshold', type = float, default = 0.0001);
parser.add_argument('-d', help = 'debug', action = 'store_true');

args = parser.parse_args()

data = np.loadtxt(args.i)
data = np.delete(data, 0, 1)

# data = preprocessing.normalize(data, norm = 'max', axis = 0);

# clus = cluster.AgglomerativeClustering(n_clusters = args.n)
clusK = cluster.KMeans(n_clusters = args.n, verbose = args.d, tol = args.t, random_state = 42, copy_x = False, algorithm = 'full')
clus = cluster.Birch(n_clusters = clusK, copy = False, threshold = args.t, branching_factor = 50)
# clus = cluster.SpectralClustering(n_clusters = args.n, assign_labels = 'discretize', affinity = 'nearest_neighbors', n_neighbors = 15, random_state = 42, eigen_tol = args.t)

clus.fit(data)

clus_ = NearestCentroid()
clus_.fit(data, clus.labels_)

np.savetxt(args.i + '.membership', clus.labels_, fmt = '%d')
np.savetxt(args.i + '.cluster_centres', clus_.centroids_, fmt = '%f')
