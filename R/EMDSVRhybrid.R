
#EMD based SVR model

EMDSVRhybrid <- function(data, k, funct="")
{
  data_org <- as.matrix(data)
  xt <- as.matrix(data_org)
  xt <- as.vector(data_org)
  #EMD
  #code for display no.of imf and residual

  try <- emd(xt,boundary = "wave")

  imf_extr <- try$imf

  emd_residual=try$residue
  no_of_imf=try$nimf
  len_extr_imf=length(imf_extr[,1])
  length_split=len_extr_imf-1
  test_data_l=ceiling(k*length_split)
  test_data_original=data_org[(test_data_l+2):length(data_org),]
  length_test_data=length(test_data_original)
  # dataset creation
  extr_imf=0
  model_svm=0
  predicted_out=matrix(nrow =length_test_data,ncol = no_of_imf)
  MSE_out=0
  RMSE_out=0
  MAPE_out=0
  MAD_out=0
  final_predict_imf=0
  for (i in 1:no_of_imf)
  {
    extr_imf=imf_extr[,i]

    yt=extr_imf[1:(len_extr_imf-1)]
    xt=extr_imf[2:len_extr_imf]
    data=data.frame(yt,xt)
    len_data=length(data[,1])
    split_train=k*len_data
    r_train=ceiling(split_train)
    traindata=data[1:r_train,]
    testdata=data[(r_train+1):len_data,]

    model_svm<-svm(yt ~ ., data=traindata,kernel=funct)
    print(model_svm)
    predicted_out[,i]<- predict(model_svm,testdata)
    final_predict_imf=final_predict_imf+predicted_out[,i]
  }
  emd_residual
  lenght_of_residual=length(emd_residual)

  #differencing
  dif_resid=diff(emd_residual)
  len_dresid=length(dif_resid)
  #spliting of data set
  ytr=dif_resid[1:(len_dresid-1)]
  xtr=extr_imf[2:len_dresid]
  datar=data.frame(ytr,xtr)
  len_datar=length(datar[,1])
  split_trainr=k*len_datar
  r_trainr=round((split_trainr),1)
  traindatar=datar[1:r_trainr,]
  testdatar=datar[(r_trainr+1):len_datar,]



  model_svmr <-svm(ytr ~ ., data=traindatar,kernel=funct)
  summary(model_svmr)

  #out sample
  predicted_outr <- predict(model_svmr,testdatar)
  length_residual_predict=length(testdatar[,1])
  adding_residual_length=lenght_of_residual-length_residual_predict
  final_prediction=final_predict_imf+predicted_outr+emd_residual[-(1:adding_residual_length)]

  # summarize accuracy
  MSE_out <- mean((test_data_original - final_prediction)^2)
  RMSE_out<- sqrt(MSE_out)


  #mean absolute deviation (MAD)
  MAD_out=mean(abs(test_data_original - final_prediction))


  #Mean absolute percent error (MAPE)
  MAPE_out=mean(abs((test_data_original-final_prediction)/test_data_original))
  prediction_accuracy=cbind(RMSE_out,MAD_out,MAPE_out)
  output_f=list(Prediction_Accuracy_EMDSVR =prediction_accuracy, Final_Prediction_EMDSVR= final_prediction)
  return(output_f)
}
