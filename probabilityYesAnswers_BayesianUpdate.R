" 
Simulate Bayesian Binomial updating
"



sim_bayes<-function(p=0.5,N=10,y_lim=15)
{
  success<-0
  curve(dbeta(x,1,1),xlim=c(0,1),ylim=c(0,y_lim),xlab='p',ylab='Posterior Density',lty=2)
  legend('topright',legend=c('Prior','Updated Posteriors','Final Posterior'),lty=c(2,1,1),col=c('black','black','red'))
  for(i in 1:N)
  {
    if(runif(1,0,1)<=p) #Substitute for if answer==YES
      success<-success+1
    
    curve(dbeta(x,success+1,(i-success)+1),add=TRUE)
    print(paste(success,"successes and ",i-success," failures"))
  }
  curve(dbeta(x,success+1,(i-success)+1),add=TRUE,col='red',lwd=1.5) #Final posterior
}

sim_bayes(p=0.6,N=90)

# True distribution
p <- 0.3
n <- 10

#add new answer
num_yes <- 1 #
#recompute probability (#Yes/Total Answers)

"
###Sampling of answers
Sample one answer at a time from the question, then update the likelihood.
Sample with replacement now, so I can use binomial, because I am only
interested in Yes's (success) and not-Yes (failure). Not-Yes are No and I don't know
answers.

In the future I will sample without replacement, hence will have to use hypergeometric distribution"

"PRIOR FOR PROBABILITY OF YES,s
When I know nothing, the probability of a YES answer is 1/3, 
because the answer options are 3 in E2.
"
prior_prob_yes <- rbeta(1000,1,2)
mean(prior_prob_yes)
var(prior_prob_yes)
plot(density(prior_prob_yes))

number_yes_points <- seq(0,1,length.out = 20)
prior <- dbeta(number_yes_points,1,2)
prior <- dbeta(1/3,1,1)
prior <- 1000 * prior / sum(prior)  # Have to normalize given discreteness
plot(prior~probability_of_yes, col=1, ylim=c(0, 14), type="l")
plot(prior)

# Run for 20 samples
for (i in 1:20) {
  prior <- 
  p_likelihood <- 
  x <- rbinom(1, n, p) 
  ps <- dbinom(x, n, p_values) * pr
  ps <- 1000 * ps / sum(ps)
  lines(ps~p_values, col=(i+1))
  
  pr = ps
}




###############################333

x <- rbeta(n=500, shape1=2, shape2=2)
est.par <- eBeta(x);
est.par
plot(est.par)