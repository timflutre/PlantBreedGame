## Copyright 2015,2016,2017,2018,2019 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantBreedGame.
##
## PlantBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.

##' Simulate parents-offsprings data
##'
##' Simulate parents-offsprings data.
##' @param mu.0 phenotypic mean without selection
##' @param sigma2 variance of the error
##' @param h2 narrow-sense heritability
##' @param sigma.a2 additive genetic variance
##' @param I number of genotypes
##' @param J number of years
##' @param seed seed
##' @return list
##' @author Timothee Flutre
##' @export
simulDat <- function(mu.0=40, sigma2=1, h2=0.75, sigma.a2=NULL,
                     I=500, J=1, seed=NULL){
  stopifnot(xor(is.null(h2), is.null(sigma.a2)))
  if(! is.null(seed))
    set.seed(seed)

  ## set means
  mean.midparents <- mu.0
  mean.offsprings <- mu.0

  ## set variances and covariance
  if(is.null(h2)){
    h2 <- sigma.a2 / (sigma.a2 + sigma2)
  } else{ # is.null(sigma.a2)
    sigma.a2 <- (h2 / (1 - h2)) * sigma2
  }
  var.midparents <- sigma.a2 + sigma2
  var.offsprings <- sigma.a2 + sigma2
  covar.midpar.off <- h2 * var.midparents
  Sigma <- matrix(c(var.midparents, covar.midpar.off,
                    covar.midpar.off, var.offsprings),
                  nrow=2, ncol=2)

  ## draw phenotypes
  N <- I * J
  all.y <- MASS::mvrnorm(n=N, mu=c(mean.midparents, mean.offsprings),
                         Sigma=Sigma)
  y <- all.y[,1] # mid-parents
  y.e <- all.y[,2] # offsprings

  return(list(mu.0=mu.0, sigma2=sigma2, sigma.a2=sigma.a2, h2=h2,
              I=I, J=J, N=N, y=y, y.e=y.e))
}

applySelection <- function(mu.0, y, y.e, y.t=43, sigma.02){
  ## selection differential
  is.sel <- (y >= y.t)
  mu.s <- mean(y[is.sel])
  S <- mu.s - mu.0
  i <- S / sqrt(sigma.02)
  alpha <- sum(is.sel) / length(y)

  ## response to selection
  mu.1 <- mean(y.e[is.sel])
  R <- mu.1 - mu.0

  return(list(y.t=y.t, is.sel=is.sel, mu.s=mu.s, S=S, i=i, alpha=alpha,
              mu.1=mu.1, R=R))
}

plotRegMidparentsOffsprings <- function(mu.0, sigma.02, h2, y, y.e,
                                        y.t=43, is.sel, mu.s, S, i, alpha,
                                        mu.1, R){
  xylim <- range(c(y, y.e))
  xylim <- c(0.95*min(c(y, y.e)), 1.1*max(c(y, y.e)))
  plot(x=y, y=y.e, las=1, type="n",
       xlim=xylim, ylim=xylim,
       xlab="Average phenotypes of each parental pair",
       ylab="Phenotypes of each offspring")
  title(main=bquote(bold(Linear~regression~of~offsprings~on~average~parents)~(h^2==.(h2)*","~mu^(s)==.(format(mu.s, digits=2))*","~alpha==.(format(100*alpha, digits=2))*"%"*","~i==.(format(i, digits=3)))))

  ## without selection
  points(x=y[! is.sel], y=y.e[! is.sel], pch=1)
  points(x=y[is.sel], y=y.e[is.sel], pch=20)
  abline(a=0, b=1, lty=2)
  abline(lm(y.e ~ y), col="red")
  legend("topleft",
         legend=c("identity line",
                  "regression line",
                  expression(phenotypic~mean~without~selection~(mu[0])),
                  expression(phenotypic~threshold~(y[t])),
                  expression(phenotypic~means~after~selection~(mu^(s)*","~mu[1])),
                  "unselected parents",
                  "selected parents"),
         lty=c(2,1,2,1,3,0,0),
         lwd=c(1,1,2,2,2,1,0),
         pch=c(-1,-1,-1,-1,-1,1,20),
         col=c("black","red","black","black","black","black","black"),
         bty="n")
  abline(v=mu.0, lty=2, lwd=2)
  abline(h=mu.0, lty=2, lwd=2)

  ## with selection
  abline(v=y.t, lty=1, lwd=2)
  abline(v=mu.s, lty=3, lwd=2)
  arrows(x0=mu.0,
         y0=mu.0 - 3 * sqrt(sigma.02),
         x1=mu.s,
         y1=mu.0 - 3 * sqrt(sigma.02),
         lwd=2, code=3)
  text(x=mu.0 + (S/2),
       y=mu.0 - 2.7 * sqrt(sigma.02),
       labels=paste0("S = ", format(S, digits=2)), cex=1.5)
  abline(h=mu.1, lty=3, lwd=2)
  arrows(x0=mu.0 - 3 * sqrt(sigma.02),
         y0=mu.0,
         x1=mu.0 - 3 * sqrt(sigma.02),
         y1=mu.1, lwd=2, code=3)
  text(x=mu.0 - 2.3 * sqrt(sigma.02),
       y=mu.0+(R/2),
       labels=paste0("R = ", format(R, digits=2),
                     " (+", format(100*R/mu.0, digits=3), "%)"),
       cex=1.5)
}
