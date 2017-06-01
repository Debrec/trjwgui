for i=1:21
  archivo = 'trjmd' + string(i) + '.dat';
  dat = read(archivo,-1,6);
  for j=1:477
    londat(i,j) = dat(j,1);
    latdat(i,j) = dat(j,2);
  end
    clf();
    theta = %pi.*londat(i,:)./180;
    rho = (90+latdat(i,:))./90;
    plot(rho.*cos(theta),rho.*sin(theta),'.');
    title('Figura '+string(i));
    fig = gcf();
    xs2png(fig,'Figura'+string(i)+'.png');
end
