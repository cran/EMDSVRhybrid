\name{EMDSVRhybrid}
\alias{EMDSVRhybrid}
\title{Hybrid Machine Learning Model}
\usage{EMDSVRhybrid(data,k,funct)}
\description{It fits Empirical Mode Decomposition and Support Vector Regression based hybrid model for nonlinear and nonstationary timeseries data. It will also provide you with accuracy measures along with an option to select the proportion of training and testing data sets. Users can choose among the available choices of kernel for fitting the Support Vector Regression model. In this package we have modelled the dependency of the study variable assuming first order autocorrelation. This package will help the researchers working in the area of hybrid machine learning models.
}

%-
\arguments{
  \item{data}{
a univariate time series data
}
\item{k}{
partition value for spliting the data set into training and testing

}
\item{funct}{
the available choices of kernel functions for fitting Support Vector Regression

}
}
\details{the package implemets EMD-SVR hybrid approach for forecasting nonlinear and nonstationary time series data proposed by Das et al (2020). In this method, EMD was used for decomposing the nonlinear and nonstationary series into finite and small numbers of sub-signals. Then these sub-signals were individually modelled and forecasted using SVR technique. Finally, all forecasted values of sub-signals were aggregated to make final ensemble forecast. This package is used to investigate the accuracy of the EMD SVR based hybrid machine learning model.
}


\value{It returns the accuracy measures of the fitted model.
}

\references{
%%
Das et al.(2020)<http://www.isee.org.in/uploadpaper/56,April%20-%20June,02.pdf>
}
\author{
Pankaj Das,Achal Lama, Girish Jha
}
\seealso{EMD,e1071}
\examples{
set.seed(6)
example_data=rnorm(500,30,5)
EMDSVRhybrid(example_data,0.9,funct="radial")
}
\keyword{Hybrid}
\keyword{Machine learning}
\keyword{EMD}
\keyword{SVR}
