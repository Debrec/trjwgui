for i=1:10
  archivo = 'trjmd' + string(i) + '.dat';
  dat = read(archivo,-1,6);
  for j=1:333
    londat(i,j) = dat(j,1);
    latdat(i,j) = dat(j,2);
  end
    plot(londat(i,:),latdat(i,:),'.');
    title('Figura '+string(i));
    fig = gcf();
    xs2png(fig,'Figura'+string(i)+'.png');
    clf();
end
