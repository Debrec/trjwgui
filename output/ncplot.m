nc=netcdf("trj_output.nc","r");
Long=nc{'Long'}(:,:); %(tiempo,particula)
Lat=nc{'lat'}(:,:); 
P=nc{'Press'}(:,:);
time=nc{'time'}(:);
polar(Long.*pi./180,cos(Lat.*pi./180),'.');
figure;
plot3(Long(:,3:10),Lat(:,3:10),-7*log(P(:,3:10)/1000)); %(Altura en kilometros)
%plot(time,P,'*');

for i=1:383
   u(i,:)=(Long(i+1,:)-Long(i,:))*6200000*pi/(180*15*60);
end
