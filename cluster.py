import argparse
import numpy as np
from sklearn import cluster, preprocessing, datasets

parser = argparse.ArgumentParser()
parser.add_argument('-l', help = 'filename (libSVM light)');
parser.add_argument('-i', help = 'filename');
parser.add_argument('-n', help = 'num_clusters', type = int);
parser.add_argument('-t', help = 'threshold', type = float, default = 0.0001);
parser.add_argument('-d', help = 'debug', action = 'store_true');
parser.add_argument('-a', help = 'alternate_method', action = 'store_true');

args = parser.parse_args()

if not args.l:
	data = np.loadtxt(args.i)
	data = np.delete(data, 0, 1)
else:
	data = datasets.load_svmlight_file(args.l)[0].toarray();

# clus = cluster.SpectralClustering(n_clusters = args.n, assign_labels = 'discretize', affinity = 'nearest_neighbors', n_neighbors = 15, random_state = 42, eigen_tol = args.t)
clus = cluster.AgglomerativeClustering(n_clusters = args.n)
# clus = cluster.Birch(n_clusters = args.n)

clus.fit(data)

np.savetxt((args.i if args.i else args.l) + '.membership', clus.labels_, fmt = '%d')
# np.savetxt((args.i if args.i else args.l) + '.centroids', clus.cluster_centers_, fmt = '%e')
