
options(digits.secs = 3) # Include milliseconds in time display

shinyServer(
  function(input, output,session){
  #server side management
    callModule(ML_Comp, "ML_Comp-module", pool)      
    }
)