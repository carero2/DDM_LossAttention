# Model 14 
method ml
precision 3
set d 0 
set st0 0 
set szr 0 
set sv 0 
set zr 0.5 
depends v condition 
format RESPONSE TIME condition difficulty
load *.txt
log model14.log