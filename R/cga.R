# Generates chromosomes for a given probability vector
cga_generate_chromosome<-function(prob_vec){
	vect <- rep(0, length(prob_vec))
	for (i in 1:length(vect)){
		if ( runif(1) < prob_vec[i] ) {
			vect[i] <- 1
		} 
	}
	return(vect)
}

# Performs a Compact Genetic Algorithm (CGA) search for a given evaluation function
# Problems is in type of minimization by default 
cga<-function(chsize, popsize, evalFunc){
	prob_vec <- rep(0.5, chsize)
	while (TRUE){
		chromosome1 <- cga_generate_chromosome(prob_vec)
		chromosome2 <- cga_generate_chromosome(prob_vec)
		cost1 <- evalFunc(chromosome1)
		cost2 <- evalFunc(chromosome2)
		#print(cost1)
		winner <- chromosome1
		loser <- chromosome2
		if( cost2 < cost1 ) {
			winner <- chromosome2
			loser <- chromosome1
		}
	
		for (i in 1:chsize){	
			if (winner[i] != loser[i]){
				if(winner[i]==1){
					prob_vec[i] <- prob_vec[i] + (1/popsize)
				}else{
					prob_vec[i] <- prob_vec[i] - (1/popsize)
				}
			}
		}

		t <- 0
		for (i in 1:chsize){
			if(prob_vec[i] <= 0.001 || prob_vec[i] >= 0.999) {
				t <- t + 1
			}		
		}
		if (t == chsize) {
			break
		}
	}
	return(prob_vec)
}	


f<-function(x){
	return(sum(x))
}

# Usage
# c<-cga(chsize=10, popsize=1000,evalFunc=f)

