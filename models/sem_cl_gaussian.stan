data{
	int N; // sample size
	int P; // number of variables
	int C; // number of cross loadings
	int D; // number of factors
	vector[P] X[N]; // data matrix of order [N,P]
}

parameters{
	vector[P] b; // intercepts
	vector<lower=0>[P] lam; // factor loadings
	vector[C] cl; // cross loadings
	vector[D] FS[N]; // factor scores, matrix of order [N,D]
	corr_matrix[D] Rho; // correlation matrix between factors
	vector<lower=0,upper=100>[P] var_p; // variance for each variable, vector
}

transformed parameters{
	vector[D] M;
	vector<lower=0, upper=1000>[D] Sd_d; // sd of factors
	vector[P] mu[N]; // NxP Matrix where each cell is ind N mean score for var P
	matrix[D,D] Ld;

for (m in 1:D) {
	M[m] <- 0;
	Sd_d[m] <- 1;}

	Ld <- diag_matrix(Sd_d) * cholesky_decompose(Rho);

	for(i in 1:N){
		mu[i,1] <- b[1] + lam[1]*FS[i,1] + cl[1]*FS[i,2] + cl[2]*FS[i,3];
		mu[i,2] <- b[2] + lam[2]*FS[i,1] + cl[3]*FS[i,2] + cl[4]*FS[i,3];
		mu[i,3] <- b[3] + lam[3]*FS[i,1] + cl[5]*FS[i,2] + cl[6]*FS[i,3];
		mu[i,4] <- b[4] + lam[4]*FS[i,1] + cl[7]*FS[i,2] + cl[8]*FS[i,3];
		mu[i,5] <- b[5] + lam[5]*FS[i,1] + cl[9]*FS[i,2] + cl[10]*FS[i,3];
		mu[i,6] <- b[6] + lam[6]*FS[i,2] + cl[11]*FS[i,1] + cl[12]*FS[i,3];
		mu[i,7] <- b[7] + lam[7]*FS[i,2] + cl[13]*FS[i,1] + cl[14]*FS[i,3];
		mu[i,8] <- b[8] + lam[8]*FS[i,2] + cl[15]*FS[i,1] + cl[16]*FS[i,3];
		mu[i,9] <- b[9] + lam[9]*FS[i,2] + cl[17]*FS[i,1] + cl[18]*FS[i,3];
		mu[i,10] <- b[10] + lam[10]*FS[i,2] + cl[19]*FS[i,1] + cl[20]*FS[i,3];
		mu[i,11] <- b[11] + lam[11]*FS[i,3] + cl[21]*FS[i,1] + cl[22]*FS[i,2];
		mu[i,12] <- b[12] + lam[12]*FS[i,3] + cl[23]*FS[i,1] + cl[24]*FS[i,2];
		mu[i,13] <- b[13] + lam[13]*FS[i,3] + cl[25]*FS[i,1] + cl[26]*FS[i,2];
		mu[i,14] <- b[14] + lam[14]*FS[i,3] + cl[27]*FS[i,1] + cl[28]*FS[i,2];
		mu[i,15] <- b[15] + lam[15]*FS[i,3] + cl[29]*FS[i,1] + cl[30]*FS[i,2];
		}
	}

model{

	b ~ normal(0, 100);
	lam ~ normal(0,var_p);
		
	cl ~ normal(0, .2);

	for(i in 1:N){   
		X[i] ~ normal(mu[i],var_p);
		FS[i] ~ multi_normal_cholesky(M, Ld);
		}

	}