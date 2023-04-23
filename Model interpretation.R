library(DALEX)
# Model interpretation 
explainer_rf<-explain(rfFit1,label = "rf",
                      data = testdata,
                      y = testy)
explainer_lm<-explain(lmfit,label = "lm",
                       data = testdata,
                       y = testy)
explainer_kknn<-explain(kknn3,label = "kknn",
                       data = testdata,
                       y = testy)
explainer_svmRadfit<-explain(svmRadfit,label = "svmRadial",       
                             data = testdata,y = testy)
#####Model performance
per_rf<-model_performance(explainer_rf)
per_lm<-model_performance(explainer_lm)
per_kknn<-model_performance(explainer_kknn)
per_svm<-model_performance(explainer_svmRadfit)


vip_lm  <- model_parts(explainer = explainer_lm)
vip_rf  <- model_parts(explainer = explainer_rf)
vip_kknn <- model_parts(explainer = explainer_kknn)
vip_svm <- model_parts(explainer = explainer_svmRadfit)

plot(vip_rf, vip_kknn, vip_lm, vip_svm)
plot(vip_lm, vip_kknn)
pdp_rf <- model_profile(explainer = explainer_rf)
plot(pdp_rf)

pdp_lm <- model_profile(explainer = explainer_lm)
plot(pdp_lm)

pdp_kknn <- model_profile(explainer = explainer_kknn)
plot(pdp_kknn)

pdp_svm <- model_profile(explainer = explainer_svmRadfit)
plot(pdp_svm)
plot(pdp_rf,pdp_lm,pdp_kknn,pdp_svm)

pdp_rf <- model_profile(explainer = explainer_rf,variables = c("x7","x11","x12","x13"))
plot(pdp_rf)
pdp_lm <- model_profile(explainer = explainer_lm,variables = c("x7","x11","x12","x13"))
plot(pdp_lm)
pdp_kknn <- model_profile(explainer = explainer_kknn,variables = c("x7","x11","x12","x13"))
plot(pdp_kknn)
pdp_svm <- model_profile(explainer = explainer_svmRadfit,variables = c("x7","x11","x12","x13"))
plot(pdp_svm)

shap_kknn <- predict_parts(explainer = explainer_kknn, new_observation=testdata,type = 'shap')
plot(shap_kknn)


