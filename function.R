#Data set D1.1:  m rows by n columns. n represents number of variables and m represents numbers of students
#Data set proportion1: n columns, n rows. 
phoebe <- function(A,B){
for (m in 1:nrow(A))	 	#for each row of D1.1
{  
  for (x in 1:ncol(B))  #for each row of eigen
  { 
    Sum = 0;   #reset sum for new calculation for each row of D7
    #Adding all multiplies together for each rows of D1
    for (c in 1:nrow(B)){	#for each column
      Sum <- Sum + (A[m,c] * B[x,c])
    }
    A[m,ncol(B) + x] <- Sum   #adding a new column to D7
  }
  return(A)
}
}

XX <- phoebe(D1.1, proportion1) 

#Data set D1.1:  m rows by n columns. n represents number of variables and m represents numbers of students
#Data set proportion1: n columns, n rows. 
phoebe <- function(A,B){
  for (m in 1:nrow(A)){	
  for (x in 1:ncol(B)){ #for each row of eigen
       	#for each row of D1.1
      Sum = 0;   #reset sum for new calculation for each row of D7
      #Adding all multiplies together for each rows of D1
      for (c in 1:nrow(B)){	#for each column
        Sum <- Sum + (A[m,c] * B[x,c])
      }
      A[m,ncol(B) + x] <- Sum  #adding a new column to D7
    }
    return(A)
  }
}
XX <- phoebe(D1.1, proportion1)
