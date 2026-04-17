ShopSell = function(input,output){
  S1 = as.numeric(input$ShopS1);  if(is.na(S1)){ S1 = 0 }
  S2 = as.numeric(input$ShopS2);  if(is.na(S2)){ S2 = 0 }
  S3 = as.numeric(input$ShopS3);  if(is.na(S3)){ S3 = 0 }
  S4 = as.numeric(input$ShopS4);  if(is.na(S4)){ S4 = 0 }
  S5 = as.numeric(input$ShopS5);  if(is.na(S5)){ S5 = 0 }
  n = 1000;  X = Y = 1:n;  S = S1 * (S2 - S3 * (X-1));  S = (S + abs(S)) / 2
  for(i in 1:length(Y)){ Y[i] = (S4 %/% i) * sum(S[1:i]);  if(S4 %% i > 0){ Y[i] = Y[i] + sum(S[1:(S4 %% i)]) } }
  output$ShopPlot = renderPlot({ plot(X,Y,type="l",xlab = "Sold per World",ylab = "GP Earned") })
  hopval = function(hop){ if(hop<=0){ return(0) };  A = S4 %/% hop;  B = S4 %% hop;  return(hop*sum(S[1:A]) + B*S[A+1]) }
  H = S4;  V = hopval(H);  Vnext = hopval(H-1)
  while(V-Vnext < S5){ H = H - 1;  V = Vnext;  Vnext = hopval(H-1) }
  if(H > 0){
    text = paste0("Optimally Sell ",S4 %/% H," in ",H - S4 %% H," Hops")
    if(S4 %% H > 0){ text = paste0(text," and " ,S4 %/% H + 1," in ",S4 %% H," Hops") }
    text = paste0(text," for ", V, " GP.")
  }
  else{ text = "" }
  output$ShopHop = renderText(text)
}
