x1 <- readline(prompt = "Enter x1: ")
x2 <- readline(prompt = "Enter x2: ")
x3 <- readline(prompt = "Enter x3: ")
y1 <- readline(prompt = "Enter y1: ")
y2 <- readline(prompt = "Enter y2: ")
y3 <- readline(prompt = "Enter y3: ")


x1 <- as.numeric(x1)
x2 <- as.numeric(x2)
x3 <- as.numeric(x3)
y1 <- as.numeric(y1)
y2 <- as.numeric(y2)
y3 <- as.numeric(y3)

sumx = x1+x2+x3
sumy = y1+y2+y3

if (x1>=0.10 & x1<=0.4 & x2>=0.10 & x2<=0.4 & x3>=0.10 & x3<=0.4 & sumx==1){
  if (y1>=0.01 & y1<=0.05 & y2>=0.01 & y2<=0.05 & y3>=0.01 & y3<=0.05 & sumy==0.12){
    print("Total probability defectiveness: ")
    tpd = (x1*y1)+(x2*y2)+(x3*y3)
    tpd
  }
  else{
    print("Error in user input")
  }
}
if(x1<0.10 | x1>0.4 | x2<0.10 | x2>0.4 | x3<0.10 | x3>0.4 | sumx!=1){
  print("Error in user input")
}
