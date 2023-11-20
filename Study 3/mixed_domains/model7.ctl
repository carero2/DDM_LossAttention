# Model 7 
method ml
precision 3
set d 0 
set zr 0.5 
depends a condition 
depends v condition 
format RESPONSE TIME condition
load *.txt
log model7.log