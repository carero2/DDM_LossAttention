# Model 9 
method ml
precision 3
set d 0 
set st0 0 
set szr 0 
set sv 0 
set zr 0.5 
depends a condition 
depends v condition difficulty 
format RESPONSE TIME condition difficulty
load *.txt
log model9.log