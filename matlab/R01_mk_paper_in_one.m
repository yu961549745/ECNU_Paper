clc,clear,close all;

% �˴�����ÿһ�� bib �ĸ�ʽ����
% @xxx{yyy,
%     ...
% }
% ��ͷβ�Ƕ����
bib_reg='(?s)@\w+\{(\w+),(\s|.)*?\n\}';

% ������ȡһ�� bib Ԫ�ص� label
key_reg='(?<=@\w+\{)(\w+)';
get_key=@(s)regexp(s,key_reg,'match');

s=read_tex('../paper/','paper.tex','utf8');
writetext('../paper/paper_in_one.tex',s,'utf8');


