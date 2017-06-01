load temp_trj.dat;
lon=temp_trj(:,1);
lat=temp_trj(:,2);
polar(pi*lon/180,cos(pi*lat/180),'.');
print -dpng "-S640,480" polar1.jpg;


