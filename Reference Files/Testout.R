# https://stackoverflow.com/questions/15046682/creating-nice-looking-output

# set your class name and its representation is list here.
setClass("testout_test", representation("list"))

# show method (here's how the output would be printed
# you can format to whatever you want... to show and how to show

setMethod("show", "testout_test", function(object) {
  cat("Test Output \n")
  cat("========================\n")
  cat("  ", object$alabel1, "\t\t", object$alabel2, "\n")
  cat("  ", object$h, "\t\t\t", object$g, "\n")
})


setMethod("plot",
          signature("testout_test"),
          function(x){
            plot(x$h, x$g)
})

testout<-function(Y1,Y2){

  x1<-list(h=sum(Y1),
       g=sum(Y2),
       alabel1="sum of Y1",
       alabel2="sum of Y2")
  new("testout_test", x1)
  #  cat(x$alabel1,"   ",x$alabel2,"\n")
  #  cat("  ",x$h,"        ",x$g,"\n")
  #  x
    }


