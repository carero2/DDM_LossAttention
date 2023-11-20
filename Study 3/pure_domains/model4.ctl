# Model 4 
method ml
precision 3
set d 0 
set zr 0.5 
depends t0 condition 
depends v condition 
format RESPONSE TIME condition
load *.txt
log model4.log