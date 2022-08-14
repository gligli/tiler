import argparse
import numpy as np
from sklearn import cluster, preprocessing
from sklearn.neighbors import NearestCentroid
from pyclustering.cluster.cure import cure;
from pyclustering.cluster.birch import birch;
from pyclustering.utils import read_sample

parser = argparse.ArgumentParser()
parser.add_argument('-i', help = 'filename');
parser.add_argument('-n', help = 'num_clusters', type = int);
parser.add_argument('-t', help = 'threshold', type = float, default = 0.0001);
parser.add_argument('-d', help = 'debug', action = 'store_true');

args = parser.parse_args()

if True:
	data = np.loadtxt(args.i)
	data = np.delete(data, 0, 1)

	# data = preprocessing.normalize(data, norm = 'max', axis = 0);

	# clus = cluster.AgglomerativeClustering(n_clusters = args.n)
	clusK = cluster.KMeans(n_clusters = args.n, init = 'k-means++', n_init = 1, verbose = args.d, tol = args.t, copy_x = False, algorithm = 'elkan')
	# clusK = cluster.BisectingKMeans(n_clusters = args.n, init = 'k-means++', n_init = 1, verbose = args.d, tol = args.t, copy_x = False, algorithm = 'elkan')
	clus = cluster.Birch(n_clusters = clusK, copy = False, threshold = args.t, branching_factor = 50)
	# clus = cluster.SpectralClustering(n_clusters = args.n, assign_labels = 'discretize', affinity = 'nearest_neighbors', n_neighbors = 15, random_state = 42, eigen_tol = args.t)

	clus.fit(data)

	np.savetxt(args.i + '.membership', clus.labels_, fmt = '%d')
else:
	data  = read_sample(args.i)

	pyc = cure(data = data, number_cluster = args.n);
	
	pyc.process();
	clusters = pyc.get_clusters();
	print(clusters)
	
	points_clusters = [0] * len(data)
	for i, clus in enumerate(clusters):
		for c in clus:
			points_clusters[c] = i	
	
	np.savetxt(args.i + '.membership', points_clusters, fmt = '%d')
