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
	vector[C] z; // used for matt tricking the horseshoe
	vector<lower=0, upper=pi()/2>[C] lambda_unif;
	real<lower=0, upper=pi()/2> tau_unif;
	vector[D] FS[N]; // factor scores, matrix of order [N,D]
	corr_matrix[D] Rho; // correlation matrix between factors
	vector<lower=0,upper=100>[P] var_p; // sd for each variable, vector
	vector<lower=0,upper=100>[P] var_f; // sd for each main factor loading, vector
}

transformed parameters{
	vector[D] M;
	vector<lower=0, upper=1000>[D] Sd_d; // sd of factors
	vector[P] mu[N]; // NxP Matrix where each cell is ind N mean score for var P
	matrix[D,D] Ld;
	vector[C] cl; // cross loadings
    vector[C] lambda;
	real tau;
	
	for(i in 1:C) lambda[i] <- tan(lambda_unif[i]);  // lambda ~ cauchy(0,1)
	tau <- tan(tau_unif);  // tau ~ cauchy(0,1)
	
	cl <- z .* lambda * tau; // cross loadings horseshoe prior (Matt trick)

for (m in 1:D) {
	M[m] <- 0;
	Sd_d[m] <- 1;}

	Ld <- diag_matrix(Sd_d) * cholesky_decompose(Rho);

	for(i in 1:N){
		mu[i,1] <- b[1] + lam[1]*FS[i,1] + cl[1]*FS[i,2] + cl[2]*FS[i,3] + cl[3]*FS[i,4];
		mu[i,2] <- b[2] + lam[2]*FS[i,1] + cl[4]*FS[i,2] + cl[5]*FS[i,3] + cl[6]*FS[i,4];
		mu[i,3] <- b[3] + lam[3]*FS[i,1] + cl[7]*FS[i,2] + cl[8]*FS[i,3] + cl[9]*FS[i,4];
		mu[i,4] <- b[4] + lam[4]*FS[i,1] + cl[10]*FS[i,2] + cl[11]*FS[i,3] + cl[12]*FS[i,4];
		mu[i,5] <- b[5] + lam[5]*FS[i,2] + cl[13]*FS[i,1] + cl[14]*FS[i,3] + cl[15]*FS[i,4];
		mu[i,6] <- b[6] + lam[6]*FS[i,2] + cl[16]*FS[i,1] + cl[17]*FS[i,3] + cl[18]*FS[i,4];
		mu[i,7] <- b[7] + lam[7]*FS[i,2] + cl[19]*FS[i,1] + cl[20]*FS[i,3] + cl[21]*FS[i,4];
		mu[i,8] <- b[8] + lam[8]*FS[i,2] + cl[22]*FS[i,1] + cl[23]*FS[i,3] + cl[24]*FS[i,4];
		mu[i,9] <- b[9] + lam[9]*FS[i,2] + cl[25]*FS[i,1] + cl[26]*FS[i,3] + cl[27]*FS[i,4];
		mu[i,10] <- b[10] + lam[10]*FS[i,3] + cl[28]*FS[i,1] + cl[29]*FS[i,2] + cl[30]*FS[i,4];
		mu[i,11] <- b[11] + lam[11]*FS[i,3] + cl[31]*FS[i,1] + cl[32]*FS[i,2] + cl[33]*FS[i,4];
		mu[i,12] <- b[12] + lam[12]*FS[i,3] + cl[34]*FS[i,1] + cl[35]*FS[i,2] + cl[36]*FS[i,4];
		mu[i,13] <- b[13] + lam[13]*FS[i,3] + cl[37]*FS[i,1] + cl[38]*FS[i,2] + cl[39]*FS[i,4];
		mu[i,14] <- b[14] + lam[14]*FS[i,4] + cl[40]*FS[i,1] + cl[41]*FS[i,2] + cl[42]*FS[i,3];
		mu[i,15] <- b[15] + lam[15]*FS[i,4] + cl[43]*FS[i,1] + cl[44]*FS[i,2] + cl[45]*FS[i,3];
		mu[i,16] <- b[16] + lam[16]*FS[i,4] + cl[46]*FS[i,1] + cl[47]*FS[i,2] + cl[48]*FS[i,3];
		mu[i,17] <- b[17] + lam[17]*FS[i,4] + cl[49]*FS[i,1] + cl[50]*FS[i,2] + cl[51]*FS[i,3];
		mu[i,18] <- b[18] + lam[18]*FS[i,4] + cl[52]*FS[i,1] + cl[53]*FS[i,2] + cl[54]*FS[i,3];
		mu[i,19] <- b[19] + lam[19]*FS[i,4] + cl[55]*FS[i,1] + cl[56]*FS[i,2] + cl[57]*FS[i,3];
	}
}

model{

	b ~ normal(0, 3);
	lam ~ normal(0, var_f);
	z ~ normal(0, 1);

	for(i in 1:N){   
		X[i] ~ normal(mu[i], var_p);
		FS[i] ~ multi_normal_cholesky(M, Ld);
		}

	}
	
generated quantities {
	vector[P] log_lik[N];  // data matrix of order [N,P]

	for(n in 1:P){
		for (i in 1:N){ 
			log_lik[i, n] <- normal_log(X[i, n], mu[i, n], var_p[n]); 	
		}
	}
}