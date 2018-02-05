## Copyright 2015,2016,2017,2018 Institut National de la Recherche Agronomique
## and Montpellier SupAgro.
##
## This file is part of PlantSelBreedGame.
##
## PlantSelBreedGame is free software: you can redistribute it and/or modify
## it under the terms of the GNU Affero General Public License as
## published by the Free Software Foundation, either version 3 of the
## License, or (at your option) any later version.
##
## PlantSelBreedGame is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU Affero General Public
## License along with PlantSelBreedGame.  If not, see
## <http://www.gnu.org/licenses/>.


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
    sigma.a2 <- h2 * sigma2
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

  return(list(mu.0=mu.0, sigma2=sigma2, sigma.a2=sigma.a2, h2=h2, I=I, J=J, N=N, y=y, y.e=y.e))
}

applySelection <- function(mu.0, y, y.e, y.t=43, sigma.02){
  ## selection differential
  is.sel <- (y >= y.t)
  mu.s <- mean(y[is.sel])
  S <- mu.s - mu.0
  i <- S / sqrt(sigma.02)

  ## response to selection
  mu.1 <- mean(y.e[is.sel])
  R <- mu.1 - mu.0

  return(list(y.t=y.t, is.sel=is.sel, mu.s=mu.s, S=S, i=i,
              mu.1=mu.1, R=R))
}

plotRegMidparentsOffsprings <- function(mu.0, h2, y, y.e,
                                        y.t=43, is.sel, mu.s, S, i,
                                        mu.1, R){
  xylim <- range(c(y, y.e))
  xylim <- c(0.95*min(c(y, y.e)), 1.05*max(c(y, y.e)))
  plot(x=y, y=y.e, las=1, type="n",
       xlim=xylim, ylim=xylim,
       xlab="Average phenotypes of each parental pair",
       ylab="Phenotypes of each offspring")
  title(main=bquote(bold(Linear~regression~of~offsprings~on~average~parents)~(h^2==.(h2)~and~i==.(format(i, digits=3)))))

  ## without selection
  points(x=y[! is.sel], y=y.e[! is.sel], pch=1)
  points(x=y[is.sel], y=y.e[is.sel], pch=20)
  abline(a=0, b=1, lty=2)
  abline(lm(y.e ~ y), col="red")
  legend("topleft",
         legend=c("identity line",
                  "regression line",
                  "unselected parents",
                  "selected parents"),
         lty=c(2,1,0,0), pch=c(-1,-1,1,20), col=c("black","red","black"),
         bty="n")
  abline(v=mu.0, lty=2, lwd=2)
  text(x=mu.0-0.4, y=34.4, labels=expression(mu["0"]), cex=1.5)
  abline(h=mu.0, lty=2, lwd=2)
  text(x=34.8, y=mu.0+0.4, labels=expression(mu["0"]), cex=1.5)
  abline(v=y.t, lty=1, lwd=2)

  ## with selection
  text(x=y.t-0.4, y=34.4, labels=expression("y"["t"]), cex=1.5)
  abline(v=mu.s, lty=3, lwd=2)
  text(x=mu.s+0.6, y=34.8, labels=expression(mu^"(s)"), cex=1.5)
  arrows(x0=mu.0, y0=35.2,
         x1=mu.s, y1=35.2, lwd=2, code=3)
  text(x=mu.0+(S/2), y=36, labels=paste0("S = ", format(S, digits=2)), cex=1.5)
  abline(h=mu.1, lty=3, lwd=2)
  text(x=34.8, y=mu.1+0.4, labels=expression(mu["1"]), cex=1.5)
  arrows(x0=36, y0=mu.0,
         x1=36, y1=mu.1, lwd=2, code=3)
  text(x=36.8, y=mu.0+(R/2), labels=paste0("R = ", format(R, digits=2)), cex=1.5)
}
