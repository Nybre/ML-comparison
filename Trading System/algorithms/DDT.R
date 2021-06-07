#-------------------------PANEL 1-------------------------------------------
trainPerc = percentTraining()
SYM = secutiy() #the symbol will be reactive
date = as.Date(SelectedDateR()[2]-1)
endDate = date#as.Date("2016-01-01")
d = as.POSIXlt(endDate)
#take last 2 years data
d$year = d$year -yearscount()
startDate = as.Date(d)
STOCK = getSymbols(SYM, env =NULL,src="yahoo",from =startDate,to=endDate)
  
#3-period RSI
RSI = RSI(Op(STOCK),n=3)
#5-period EMA
EMA = EMA(Op(STOCK),n=5)
#price diff between open price and 5-P EMA
EMAcross = Op(STOCK)-EMA
#MACD with standard parameters
MACD = MACD(Op(STOCK),fast = 12, slow = 26, signal = 9)

MACD = MACD[,2]
SMI = SMI(Op(STOCK),n=13,slow=25,fast=2,signal=9)
SMI = SMI[,1]
WPR = WPR(Cl(STOCK),n=14)
WPR = WPR[,1]
ADX = ADX(STOCK,n=14)
ADX = ADX[,1]
CCI = CCI (Cl(STOCK),n=14)
CCI = CCI[,1]
CMO = CMO(Cl(STOCK),n=14)
CMO=CMO[,1]
ROC = ROC(Cl(STOCK),2)
ROC = ROC[,1]
PriceChange = Cl(STOCK)-Op(STOCK)
Class = ifelse(PriceChange>0,'UP','DOWN')
 
if(modelcontrol() == "Decision Trees"){
  #training dataset preview
  DataSet = data.frame(Class, RSI, EMAcross,MACD,SMI,WPR, ADX, CCI, CMO, ROC)
  colnames(DataSet) = c("Class","RSI","EMAcross","MACD","SMI","WPR","ADX","CCI","CMO","ROC")
   
  TrainingSet = DataSet[1:floor(nrow(DataSet)*trainPerc),]
  TestSet = DataSet[(floor(nrow(DataSet)*trainPerc)+1):nrow(DataSet),]
  
  DecisionTree = rpart(Class~RSI+EMAcross+WPR+ADX+CMO+CCI+ROC,
                       data = TrainingSet,
                       na.action = na.omit,
                       cp = .001)
  
  prp(DecisionTree,type = 2,extra =8)
  
  fit = DecisionTree$cptable
   
  mincp = fit[which.min(fit[,'xerror']),'CP']
   
  PrundDecisionTree = prune(DecisionTree,cp =mincp)
  
  t = prp(PrundDecisionTree, type = 2, extra = 8)
  confmat = table(predict(PrundDecisionTree, TestSet,type="class"),
                  TestSet[,1],
                  dnn = list('predicted','actual'))
  acc = (confmat[1,'DOWN'] + confmat[2,'UP'])*100/(confmat[2,'DOWN']+confmat[1,"UP"]+confmat[1,"DOWN"]+confmat[2,"UP"])
  
  xy = paste('Decision Tree: Considering the output for', SYM, sep = ' ')
  yz = paste('Accuracy =', acc, sep = ' ')
  
  predout = data.frame(predict(PrundDecisionTree, TestSet))
  predval = predout['UP']-predout['DOWN']
  predclass = ifelse(predout['UP']>=predout['DOWN'],1,0)
  predds = data.frame(predclass, TestSet$Class)
  colnames(predds) = c("pred","truth")
  
  predds[,2] = ifelse(predds[,2]=='UP',1,0)
  pred = prediction(predds$pred, predds$truth)
  perf = performance(pred, measure = "tpr",x.measure = 'fpr')
  
  auc.perf = performance(pred, measure = 'auc')
  
  rmse.perf = performance(pred, measure = 'rmse')
  
  RMSE = paste('RMSE =', rmse.perf@y.values, sep = ' ')
  AUC = paste('AUC =', auc.perf@y.values, sep = ' ')
}else if(modelcontrol() == "Support Vector Machine"){ 
 
  DataSet = data.frame(Class, RSI, EMAcross,MACD,SMI,WPR, ADX, CCI, CMO, ROC,stringsAsFactors = T) 
  colnames(DataSet) = c("Class","RSI","EMAcross","MACD","SMI","WPR","ADX","CCI","CMO","ROC")
   
  TrainingSet = DataSet[1:floor(nrow(DataSet)*trainPerc),]
  TestSet = DataSet[(floor(nrow(DataSet)*trainPerc)+1):nrow(DataSet),]
  
  SVM = svm(Class~RSI+EMAcross+WPR+ADX+CMO+CCI+ROC,
            data = TrainingSet,
            kernel = "radial",
            type = "C-classification",
            na.action = na.omit,
            cost = 1,
            gamma = 1/5)
  
  confmat = table(predict(SVM, TestSet,type = "Class"),
                  TestSet[,1],
                  dnn = list('predicted','actual'))
  
  acc = (confmat[1,'DOWN'] + confmat[2,'UP'])*100/(confmat[2,'DOWN']+confmat[1,"UP"]+confmat[1,"DOWN"]+confmat[2,"UP"])
  
  xy = paste('SVM: Considering the output for', SYM, sep = ' ')
  yz = paste('Accuracy =', acc, sep = ' ')
  
  predds = data.frame(predict(SVM,TestSet), TestSet$Class)
  colnames(predds) = c("pred","truth") 
  predds[,1] = ifelse(predds[,1]=='UP',1,0)
  predds[,2] = ifelse(predds[,2]=='UP',1,0)
  
  pred = prediction(predds$pred, predds$truth)
  perf = performance(pred, measure = "tpr",x.measure = 'fpr') 
  
  auc.perf = performance(pred, measure = 'auc', col = 'red')
  
  rmse.perf = performance(pred, measure = 'rmse')
  
  RMSE = paste('RMSE =', rmse.perf@y.values, sep = ' ')
  AUC = paste('AUC =', auc.perf@y.values, sep = ' ')
   
}