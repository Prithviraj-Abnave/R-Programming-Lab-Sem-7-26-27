for ( i in 1:5 ) 
{ 
  print( i^2 ) 
} 


x = c(2,4,6,8,10,12)

excount = function(x){
  count = 0
  for(xval in x){
    if(xval/2>3)
      count = count + 1
  }
  print(count)
}

excount(x)

child = c("child1", "child2", "child3")
sweet = c("sweet1", "sweet2", "sweet3")

for(x in child){
  for(y in sweet){
    print(paste(x,y))
  }
}

drink = c("coffee","lemonade","tea","juice")

for(x in drink){
  if(x=="tea"){
    break
  }
  print(x)
}

for(x in drink){
  if(x=="lemonade"){
    next
  }
  print(x)
}

i = 1
while (i < 10) { 
  print(i^2)
  i = i+2
}

sumfunction = function(){
  sum = 0
  numebr = as.integer(readline(prompt = "Please select any number less than 25:  "))
  while(number <=25){
    sum = sum+number
    number = number+1
  }
  print(paste("The sum of numbers received from the While Loop: ",sum))
}

i = 1
repeat{
  print( i^2 )
  i = i+2
  if ( i > 10 ) 
    break 
}

abc = function(x,y){
  x^2+y^2
}
abc(3,4)

seq(from=20, to=10, by=-2)