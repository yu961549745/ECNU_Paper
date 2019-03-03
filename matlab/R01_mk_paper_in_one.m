clc,clear,close all;

% 此处假设每一个 bib 的格式都是
% @xxx{yyy,
%     ...
% }
% 即头尾是顶格的
bib_reg='(?s)@\w+\{(\w+),(\s|.)*?\n\}';

% 用于提取一个 bib 元素的 label
key_reg='(?<=@\w+\{)(\w+)';
get_key=@(s)regexp(s,key_reg,'match');

s=read_tex('../paper/','paper.tex','utf8');
writetext('../paper/paper_in_one.tex',s,'utf8');


