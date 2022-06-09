mse <- function(pred, y_test){
    return((mean((pred-y_test)^2)))
    }   

clasif <- function(pred, p){
    r = pred > p
    return(as.numeric(r))
    }

confusion <- function(pred, y){
    vp = 0
    fp = 0
    fn = 0
    vn = 0
    for (i in 1:length(pred)){
        if(pred[i] == 1 ){
            if(pred[i] == y[i]){
                vp <- vp+1}
            else{
                fp <- fp+1}
               }
        else{
             if(pred[i] != y[i]){
                fn <- fn+1}
            else{
                vn <- vn+1
                }
            }}
        return(cbind(c(vp,fp),c(fn,vn)))
        }

f1_score <- function(pred,y) {
    val <- confusion(pred,y)
    prec <- val[1]/(val[1]+val[2])
    rec <- val[1]/(val[1]+val[3])
    f1 <- 2/(prec^-1+rec^-1)
    return(f1)}