exptest <- function(x,n){
	temp=1;
	fact <- function(n) {
		if (n >= 2)
			 return(n*fact(n-1))
		 else return(n)};
	for (i in 1:n) {temp = temp+(x^i/fact(i))}
	return(temp);
}
