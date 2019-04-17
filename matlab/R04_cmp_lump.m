clc,clear,close all;
f=@(m)double(log10(hypergeom([1, -m, 1/2-m], [], 4)/hypergeom([-m, 1/2-m], [], 2)));
x=1:20;
y=arrayfun(f,x);
figure('Position',[680   791   438   307]);
plot(x,y,'-k');
xlabel('order of lump solution','FontName','Times New Roman','FontSize',16);
ylabel('${\rm log}_{10}( N_{old}(m) / N_{new}(m) )$','Interpreter','latex');
saveas(1,'lump_cmp.pdf');
!pdfcrop lump_cmp.pdf lump_cmp.pdf
copyfile lump_cmp.pdf ../paper/fig/lump_cmp.pdf
