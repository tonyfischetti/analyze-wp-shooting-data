data{
    int<lower=1> N;
    int<lower=1> N_racenum;
    int raw_unarmed[N];
    int raw_know_armp[N];
    int racenum[N];
}
parameters{
    vector[N_racenum] race;
}
model{
    vector[N] p;
    race ~ normal( 0 , 5 );
    for ( i in 1:N ) {
        p[i] = race[racenum[i]];
    }
    raw_unarmed ~ binomial_logit( raw_know_armp , p );
}
generated quantities{
    vector[N] p;
    real dev;
    dev = 0;
    for ( i in 1:N ) {
        p[i] = race[racenum[i]];
    }
    dev = dev + (-2)*binomial_logit_lpmf( raw_unarmed | raw_know_armp , p );
}

