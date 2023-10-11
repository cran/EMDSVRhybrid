\name{EMDSVRhybrid}
\alias{EMDSVRhybrid}
\title{EMD based SVR model}
\usage{EMDSVRhybrid(data,k,funct)}
\description{It fits Empirical Mode Decomposition and Support Vector Regression based hybrid model for nonlinear and nonstationary timeseries data. Along with the choice to choose the percentage of training and testing data sets, it will also give you accuracy measures. For fitting the support vector regression model, users can select from a variety of kernel options. In this package, we have modelled the study variable's dependence by assuming first-order autocorrelation. Researchers who study hybrid machine learning models will benefit from this package.
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
\details{The package implements EMD-SVR hybrid approach for forecasting nonlinear and nonstationary time series data proposed by Choudhary et al. (2019), Das (2019) and Das et al. (2020,2022,2023). IIn this procedure, the nonlinear and nonstationary series were broken down into a limited number of sub-signals using EMD. The SVR approach was then used to model and forecast each of these sub-signals separately. The final ensemble forecast was created by averaging all forecasted sub-signal levels. This package is used to investigate the accuracy of the EMD SVR based hybrid machine learning model.
}


\value{

  \item{Prediction_Accuracy_EMDSVR }{List of performance measures of the fitted EMDSVR model.}

  \item{Final_Prediction_EMDSVR }{Final forecasted value of the VMD based SVR model. It is obtained by combining the forecasted value of all individual IMF and final residue.}
  }

\references{

Das,P., Jha, G. K., Lama, A., Parsad, R. and Mishra, D. (2020). Empirical Mode Decomposition based Support Vector Regression for Agricultural Price Forecasting. Indian Journal of Extension Education, 56(2): 7-12. (http://krishi.icar.gov.in/jspui/handle/123456789/43174).

Das, P., Jha, G. K. and Lama, A. (2023). Empirical Mode Decomposition Based Ensemble Hybrid Machine Learning Models for Agricultural Commodity Price Forecasting. Statistics and Applications. 21(1),99-112.(http://krishi.icar.gov.in/jspui/handle/123456789/77772).

Das, P., Jha, G. K., Lama, A. and Bharti (2022). EMD-SVR Hybrid Machine Learning Model and its Application in Agricultural Price Forecasting. Bhartiya Krishi Anusandhan Patrika. (http://krishi.icar.gov.in/jspui/handle/123456789/71621)

Das, P. (2019). Study On Machine Learning Techniques Based Hybrid Model for Forecasting in Agriculture. Unpublished Ph.D. Thesis.

Choudhury, K., Jha, G. K., Das, P. and Chaturvedi, K. K. (2019). Forecasting Potato Price using Ensemble Artificial Neural Networks. Indian Journal of Extension Education, 55(1):71-77.(http://krishi.icar.gov.in/jspui/handle/123456789/71621).

}

\author{

  Pankaj Das, Achal Lama and Girish Kumar Jha
}
\seealso{EMD,e1071, EMDSVRhybrid}

\examples{
set.seed(6)
example_data=rnorm(500,30,5)
EMDSVRhybrid(example_data,0.9,funct="radial")
}

\keyword{EMDSVRhybrid}
\keyword{Ensembe Machine learning}
\keyword{EMD}
\keyword{SVR}
