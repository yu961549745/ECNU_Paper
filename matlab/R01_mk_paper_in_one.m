clc,clear,close all;
s=read_tex('../paper/','paper.tex','utf8');
writetext('../paper/paper_in_one.tex',s,'utf8');
