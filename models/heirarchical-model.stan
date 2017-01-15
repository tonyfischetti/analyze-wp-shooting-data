data{
    int<lower=1> N;
    int<lower=1> N_racenum;
    int raw_unarmed[N];
    int raw_know_armp[N];
    int racenum[N];
}
parameters{
    vector[N_racenum] races;
    real a;
    real<lower=0> sigma;
}
model{
    vector[N] p;
    sigma ~ exponential( 1 );
    a ~ normal( 0 , 3 );
    races ~ normal( a , sigma );
    for ( i in 1:N ) {
        p[i] = races[racenum[i]];
    }
    raw_unarmed ~ binomial_logit( raw_know_armp , p );
}
generated quantities{
    vector[N] p;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        p[i] = races[racenum[i]];
    }
    dev = dev + (-2)*binomial_logit_lpmf( raw_unarmed | raw_know_armp , p );
}

