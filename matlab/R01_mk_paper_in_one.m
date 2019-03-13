clc,clear,close all;
s=read_tex('../paper/','paper.tex','utf8');
% 下面的替换为了 pandoc 的生成更科学
% s=regexprep(s,'fig/(.*?)\.pdf','fig/pdf2png/$1.png');
% s=strrep(s,'\renewcommand{\arraystretch}','%\renewcommand{\arraystretch}');
% s=strrep(s,'\bibliography{refs}','\input{paper_in_one.bbl.tex}');
writetext('../paper/paper_in_one.tex',s,'utf8');
