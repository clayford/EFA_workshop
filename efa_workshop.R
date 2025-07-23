# Clay Ford
# Exploratory Factor Analysis workshop
# Spring 2018
# UVa Library StatLab


library(psych)
library(GPArotation)
library(car)

# Performing EFA ----------------------------------------------------------

# Let's look at some data that come with R
# Six tests were given to 112 individuals. The covariance matrix is given in this object.
ability.cov
ability.cov$cov

# For more on this data:
?ability.cov

# We can convert the covariance matrix to a correlation matrix using the
# cov2cor() function:
cov2cor(ability.cov$cov)
ability.cor <- cov2cor(ability.cov$cov)

# We can use the base R function factanal to perform EFA. We have to at least
# provide a covariance matrix and the number of factors we want to explore
factanal(covmat = ability.cor, factors = 2)

# Notice we get the same results if we use the covariance matrix:
factanal(covmat = ability.cov$cov, factors = 2)

# save our EFA into an object called f.out
f.out <- factanal(covmat = ability.cor, factors = 2)

# calculate our fitted covariance matrix
# %*% means matrix algebra
# diag() places values on the diagonal of a matrix
L <- f.out$loadings
Psi <- diag(f.out$uniquenesses)

# Estimated covariance matrix
L %*% t(L) + Psi
round(L %*% t(L) + Psi,3)

# calculate residuals
ability.cor - (L %*% t(L) + Psi)
round(ability.cor - (L %*% t(L) + Psi),3)

# maze/picture residual is relatively large

# The fa() function from the psych package also performs factor analysis;
# It returns a lot more output;
# Notice it also returns a different answer;

# That's because it's using a different estimation method and a different
# rotation, which is what we discuss next.
fa(r = ability.cor, nfactors = 2)

# To look at the residuals, use the residuals() function;
# Notice it rounds the numbers and only shows the lower portion;
# uniquenesses are on the diagonal
fa.out <- fa(r = ability.cor, nfactors = 2)
residuals(fa.out)


############
# YOUR TURN:
############

# Try a factor analysis of ability.cor with 3 factors.
# Calculate the residuals. What do you notice?



# back to the presentation...


# Estimation and Rotation -------------------------------------------------

# Maximum likelihood estimation (mle)

# For factanal just specify number of observations;
# Note the p-value at the bottom of output
factanal(covmat = ability.cor, factors = 2, n.obs = 112)

# Same with fa;
# See line that begins "The total number of observations was..."
fa(r = ability.cor, nfactors = 2, fm = "mle", n.obs = 112)

# fa allows other estimation options: 
# "minres", "ols", "pa", "wls", "gls", "alpha", "minchi"
# see ?fa for more information
fa(r = ability.cor, nfactors = 2, fm = "pa", n.obs = 112)

# Recall: If the factor model is appropriate, then it doesn't really matter
# which estimation method you use; they should all produce consistent results.

# Rotation

# "Rotation" helps with interpretation of factors; It "rotates" the loadings
# matrix in such a way that a factor will (hopefully) consist of high and low
# values (nothing in between)

# factanal performs "varimax" rotation by default;
# We can specify no rotation using "none";
# Notice factanal does not print loadings less than abs(0.1)

# No Rotations
fa.none <- factanal(covmat = ability.cor, factors = 2, n.obs = 112, 
         rotation = "none")
fa.none

# Orthogonal Rotations
fa.varimax <- factanal(covmat = ability.cor, factors = 2, n.obs = 112, 
         rotation = "varimax") 
fa.varimax

# Oblique Rotation
fa.promax <- factanal(covmat = ability.cor, factors = 2, n.obs = 112, 
         rotation = "promax")
fa.promax

# Notice we have a loading greater than 1! Recall loadings are correlations. The
# fa() function will actually give a warning when this happens. Perhaps we
# should not use "promax" rotation?

# Notice also "promax" includes a "Factor Correlations" section; that's because
# an oblique rotation assumes factors are correlated

# Loadings are "rotated" by multiplying them by a rotation matrix

# The rotation matrix is stored in the factanal object
fa.varimax$rotmat
fa.promax$rotmat

# We can multiply the loadings in fa.none by fa.varimax$rotmat to get the
# loadings in the fa.varimax object.
round(fa.none$loadings %*% fa.varimax$rotmat, 3)
fa.varimax$loadings

# We can multiply the loadings in fa.none by fa.promax$rotmat to get the
# loadings in the fa.promax object.
round(fa.none$loadings %*% fa.promax$rotmat, 3)
print(fa.promax$loadings, cutoff = 0) # cutoff = 0 to show all loadings


# Base R includes rotations for varimax and promax
?varimax

# The GPArotation package allows us to implement additional rotations;
# see ?rotations
# quartimax - an orthogonal rotation
factanal(covmat = ability.cor, factors = 2, n.obs = 112, 
         rotation = "quartimax")
# quartimin - an oblique rotation
factanal(covmat = ability.cor, factors = 2, n.obs = 112, 
         rotation = "quartimin")

# Notice all loadings are now less than 1.

# fa performs "oblimin" rotation by default;
# specify different rotations using the rotate argument
fa(r = ability.cor, nfactors = 2, n.obs = 112, rotate = "oblimin")
# Using fa with varimax
fa(r = ability.cor, nfactors = 2, n.obs = 112, rotate = "varimax")

# This matches the factanal() default
fa(r = ability.cor, nfactors = 2, n.obs = 112, rotate = "varimax", fm = "mle")

############
# YOUR TURN:
############

# fa() provides orthogonal rotations called "bentlerT", "equamax", and "varimin"
# and oblique rotations called "bentlerQ", simplimax", and "biquartimin".

# Try some of them out with the default estimation method using 2 factors on the
# ability.cor matrix. Do they change the basic interpretation of the factors?


# back to the presentation...


# Factor scores -----------------------------------------------------------

# Factor scores are estimated factor values. If our EFA model suggests, say, a
# "spatial reasoning" factor, the factor score provides a numeric measure of
# that factor.

# R can automatically calculate factor scores if you have subject-level data.

# National Track Records for Women from 2005 (54 countries)
# Taken from Table 1.9 of Wichern and Johnson (2007)
track <- read.csv("https://github.com/clayford/EFA_workshop/raw/refs/heads/master/track.csv")
head(track)

# Let's make life easier for us;
# move country names column to row names attribute
rownames(track) <- track$country
track$country <- NULL

# first 3 columns in seconds, last 4 columns in minutes;
# different scales, so better to look at correlation instead of covariance.
cor(track)
pairs(track)

# All variables highly correlated but also some outliers

# Fit both 1 and 2-factor models. Which is better?
fa.track1 <- factanal(x = track, factors = 1)
fa.track1
fa.track2 <- factanal(x = track, factors = 2)
fa.track2

# 2 factor with oblique rotations
fa.track2p <- factanal(x = track, factors = 2, rotation = "promax")
fa.track2p
fa.track2q <- factanal(x = track, factors = 2, rotation = "quartimin")
fa.track2q

# Hmmm, correlation > 1 on 3000m...

# Factor interpretations (?)
# Factor 1 - distance
# Factor 2 - speed

# What are the factor scores? 
fa.track2 <- factanal(x = track, factors = 2, scores = "regression")
fa.track2$scores

# Scores summary:
summary(fa.track2$scores)

# What are factor scores for USA?
USA <- which(rownames(track) == "USA")
fa.track2$scores[USA,]

# A relatively small positive number on Factor 1 (distance) suggests USA is
# maybe a little slower at distance running than other countries, or at least
# not one of the higher performers. The very low value on Factor 2 (speed)
# suggests USA is one the fastest nations when it comes to speed events.


# Visualize factor scores
plot(fa.track2$scores)

# Something a little nicer
# using scatterplotMatrix() function from car package
scatterplotMatrix(fa.track2$scores, smooth = FALSE,
                  regLine = FALSE, 
                  id = list(method="mahal", n=4, cex=1, location="lr")) 

# add data-concentration ellipses
scatterplotMatrix(fa.track2$scores, smooth = FALSE,
                  regLine = FALSE, 
                  id = list(method="mahal", n=4, cex=1, location="lr"),
                  ellipse = TRUE)

# Allows us to see countries at the extremes of the factor values. This also
# provides a visual diagnostic on the normality of our factors.


# Factor scores for new data

# Our 2-factor EFA model of the Ability and Intelligence Tests suggested two
# latent factors that we might think of as “verbal comprehension” and “spatial
# reasoning”. Calculating factor scores allows us to express someone's “verbal
# comprehension” and “spatial reasoning” as a numeric measure.


# Let's say we have standardized test scores for an individual:
z <- c(.5, 0.75, 1.1, .79, 1.4, 1.2)

# Fit two factor model
fa.ability <- factanal(covmat = ability.cor, factors = 2, n.obs = 112)
fa.ability


# We can use the 2-factor EFA model to estimate factor scores for new data as
# follows:
L <- fa.ability$loadings
Psi <- diag(fa.ability$uniquenesses)

# Calculate factor scores using regression method
t(L) %*% solve(ability.cor) %*% z

# Calculate factor scores using Bartlett method
solve(t(L) %*% solve(Psi) %*% L) %*% 
  t(L) %*% solve(Psi) %*% z


############
# YOUR TURN:
############

# Read in the following data on chicken bone measures. The data come from
# Example 9.14 in Johnson and Wichern (pages 520 - 526). The data contain six
# columns that measure skull length, skull breadth, femur length, tibia length,
# humerus length, and ulna length.

bones <- read.csv("http://people.virginia.edu/~jcf2d/data/bones.csv")
cor(bones)
pairs(bones)

# Perform a 2-factor EFA using fa() with default estimation and rotation, and
# estimate "regression" factor scores. Plot the factor scores. 

# TIP: Use labels = rownames(bones) to label outliers.



# back to presentation...



# Number of factors -------------------------------------------------------

# This is by far the most important part of an EFA. Subject-matter expertise
# plays a big role in choosing the number of factors. Two other methods include
# parallel analysis scree plots and VSS criterion.

# parallel analysis scree plot

# Notice we just need to use the correlation data and that we don't specify
# number of factors.
fa.parallel(ability.cor, n.obs = 112, fa = "fa")

# Note the messages and warnings! 

# Suggests two factors but says we should try a different factor extraction
# method! "Factor extraction method" means how we select the number of factors.
# In this case may need to rely on subject matter expertise.

# Let's try the track data
fa.parallel(cor(track), n.obs = 54, fa = "fa")

# More messages and warnings!
# What to make of it?

# Look again at the correlation matrix. This is may not be a good candidate for
# factor analysis, at least not for multiple factors. There are no clumps of
# high and low correlations. Maybe just a single factor that represents a
# nation's talent for women's track events.
cor(track)

# VSS criterion

# Note the number of factors with highest VSS for each complexity

VSS(ability.cor, n.obs = 112)

# Looks like we prefer 2 factors; 3 factors overfits the model (0 degrees of freedom)

VSS(track, n.obs = 54)

# Looks like 1 factor is really all we need. 

# Visualization -----------------------------------------------------------


# Visualize EFA with a path diagram

# Have to perform EFA using fa() in order to use diagram() function
fa.out <- fa(ability.cor, nfactors = 2, n.obs = 112)
diagram(fa.out)
fa.out$loadings

# 1-factor EFA for track data
fa.out2 <- fa(cor(track), nfactors = 1, n.obs = 54)
diagram(fa.out2)
fa.out2$loadings

# calculate residuals
residuals(fa.out2)

# Correlation of data with different data types ---------------------------


# data that comes with psych package for demonstration purposes;
# 25 personality self report items taken from the International Personality Item
# Pool
data(bfi) 
# subset data
bfi2 <- bfi[,c(1:5,26,28)]
head(bfi2)
dim(bfi2)

# First 5 vars are polytomous, gender is dichotomous, age is continuous;
# use mixedCor to calculate correlation matrix
r <- mixedCor(bfi2)
r

# r is a list object;
# correlation matrix in rho
r$rho

# Now use in an EFA
fa(r$rho, nfactors = 2, n.obs = 2800)

# Try with factanal()
factanal(r$rho, factors = 2, n.obs = 2800)

# The difference is the estimation method;
# apparently the "minres" method works for this matrix but not "mle"



# Splitting data and performing EFA on each set ---------------------------

# Let's randomly split the chicken bone data into two halves.
n <- nrow(bones)
i <- sample(n, size = round(n/2))

bones1 <- bones[i,]
bones2 <- bones[-i,]

# Now run EFA on each half; are the results consistent?
fa.bones1 <- fa(bones1, nfactors = 2)
fa.bones2 <- fa(bones2, nfactors = 2)

# They appear consistent with the EFA performed on the full data.
fa.bones1$loadings
fa.bones2$loadings



############
# YOUR TURN:
############

# 9-dimensional system for rating the burritos in San Diego
# https://srcole.github.io/100burritos/

# measures rated on a scale from 0 to 5, 0 being terrible, and 5 being optimal

# 1. Tortilla quality
# 2. Temperature
# 3. Meat quality
# 4. Non-meat filling quality
# 5. Meat : filling - The ratio between meat and non-meat 
# 6. Uniformity - Bites full of sour cream and cheese with no meat are disappointing.
# 7. Salsa quality 
# 8. Flavor synergy - magical aspect a great burrito has
# 9. Wrap integrity - you ordered a burrito, not a burrito bowl.


# Read in burritos data
burritos <- read.csv("http://people.virginia.edu/~jcf2d/data/Burritos.csv")
summary(burritos)

# Note the missing values;
# easy way to drop records with missing values
burritos <- na.omit(burritos)
dim(burritos)

# NOTE: fa() has an impute argument that allows you to impute missing values
# using either the median or the mean.

# Explore the data
cor(burritos)
pairs(burritos)
summary(burritos)


# Perfom an EFA of this data. Are these measures manifestations of latent
# factors?





# Appendix - Mean item complexity -----------------------------------------

# The fa() output includes a section called Mean item complexity.

# Hofmann's (1978) complexity index represents the average number of latent
# variables needed to account for the manifest variables. A complexity measure
# is calculated for each variable, then those complexities are averaged.

fa.ability2 <- fa(r = ability.cor, nfactors = 2, n.obs = 112)
fa.ability2

# Mean item complexity =  1.2

# Notice each variable has a complexity value
fa.ability2$complexity

# see ?fa for formula
# complexity for first variable: 100m
sum(fa.ability2$loadings[1,]^2)^2/(sum(fa.ability2$loadings[1,]^4))

# Mean item complexity
mean(fa.ability2$complexity)

# The average number of latent variables needed to account for the manifest
# variables is about 1.2.



# Appendix - Rotation plots -----------------------------------------------


# How I created the rotation plots in the presentation. Not the most efficient
# method but got the job done.

# rotate axis per p.35 of Johnson and Wichern
plot(x = 0, y = 0, xlab = "x1", ylab = "x2", xlim = c(-2,2), ylim = c(-2,2), pty = "s")
abline(h = 0, v = 0)

rotate <- function(x,y,angle){
  x <- x*c(1, 1, -1, -1)
  y <- x*c(1, -1, -1, 1)
  x1 <- x*cos(angle * pi/180) + y*sin(angle * pi/180)
  x2 <- -x*sin(angle * pi/180) + y*cos(angle * pi/180)
  list(x1 = x1, x2 = x2)
}
r.out <- rotate(4, 4, 20)
segments(0, 0, x1 = r.out$x1, y1 = r.out$x2, lty = 2)

# rotate axis using orthogonal rotation matrix values (varimax)

fa.out <- factanal(covmat = ability_cor, factors = 2, n.obs = 112, rotation = "none")
plot(fa.out$loadings, xlim = c(-1, 1), ylim = c(-1,1), pty = "s")
abline(h = 0, v = 0)
text(fa.out$loadings, labels = rownames(fa.out$loadings), pos = 2, cex = 0.8)

fa.out.v <- factanal(covmat = ability_cor, factors = 2, n.obs = 112, rotation = "varimax")
fa.out.v$rotmat[1,1]

rotate <- function(x,y,sin,cos){
  x <- x*c(1, 1, -1, -1)
  y <- x*c(1, -1, -1, 1)
  x1 <- x*cos + y*sin
  x2 <- -x*sin + y*cos
  list(x1 = x1, x2 = x2)
}
r.out <- rotate(4, 4, sin = fa.out.v$rotmat[1,2], cos = fa.out.v$rotmat[1,1])
segments(0, 0, x1 = r.out$x1, y1 = r.out$x2, lty = 2)
points(fa.out.v$loadings, pch = 19, col = "blue")
text(fa.out.v$loadings, labels = rownames(fa.out.v$loadings), pos = 2, cex = 0.8, col = "blue")

# rotate axis using oblique rotation matrix values (varimax)

fa.out <- factanal(covmat = ability_cor, factors = 2, n.obs = 112, rotation = "none")
plot(fa.out$loadings, xlim = c(-1, 1), ylim = c(-1,1), pty = "s")
abline(h = 0, v = 0)
text(fa.out$loadings, labels = rownames(fa.out$loadings), pos = 2, cex = 0.8)

fa.out.p <- factanal(covmat = ability_cor, factors = 2, n.obs = 112, rotation = "promax")
fa.out.p$rotmat
c(fa.out.p$rotmat)

rotate_ob <- function(x,y,sin,cos){
  x1 <- x*cos + y*sin
  x2 <- -x*sin + y*cos
  list(x1 = x1, x2 = x2)
}
f1a <- rotate_ob(4,4,sin = -fa.out.p$rotmat[2,1], cos = fa.out.p$rotmat[1,1])
f1b <- rotate_ob(-4,-4,sin = -fa.out.p$rotmat[2,1], cos = fa.out.p$rotmat[1,1])
f2a <- rotate_ob(-4,4,sin = fa.out.p$rotmat[1,2], cos = fa.out.p$rotmat[2,2])
f2b <- rotate_ob(4,-4,sin = fa.out.p$rotmat[1,2], cos = fa.out.p$rotmat[2,2])
segments(0, 0, x1 = f1a$x1, y1 = f1a$x2, lty = 2)
segments(0, 0, x1 = f1b$x1, y1 = f1b$x2, lty = 2)
segments(0, 0, x1 = f2a$x1, y1 = f2a$x2, lty = 2)
segments(0, 0, x1 = f2b$x1, y1 = f2b$x2, lty = 2)
points(fa.out.p$loadings, pch = 19, col = "blue")
text(fa.out.p$loadings, labels = rownames(fa.out.p$loadings), pos = 2, cex = 0.8, col = "blue")



