% Configuration File for the Feature Thresholds %

minBrightness :: *<50 ---> black_normal.
minBrightness :: *>=50 ---> black_too_bright.

maxBrightness :: *<200 ---> white_too_dark.
maxBrightness :: *>=200 ---> white_normal.

avgBrightness :: *>170 ---> too_bright.
avgBrightness :: *>=80, *=<170 ---> brightness_normal.
avgBrightness :: *<80 ---> too_dark.

stdBrightness :: *>4500 ---> high_contrast.
stdBrightness :: *>=1500, *=<4500 ---> normal_contrast.
stdBrightness :: *<1500 ---> low_contrast.

avgR :: ~>avgG, ~>avgB, *>100 ---> much_red. 
avgG :: ~>avgR, ~>avgB, *>100 ---> much_green.   
avgB :: ~>avgR, ~>avgG, *>100 ---> much_blue.

sharpness :: *=<3 ---> blurry.
sharpness :: *>3 ---> sharp. 

stdR :: *+stdG, *+stdB, *>15000 ---> colorful. 