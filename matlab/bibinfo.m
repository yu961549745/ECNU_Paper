function [keys,bibs] = bibinfo(f)
% �˴�����ÿһ�� bib �ĸ�ʽ����
% @xxx{yyy,
%     ...
% }
% ��ͷβ�Ƕ����
bib_reg='(?s)@\w+\{(\w+),(\s|.)*?\n\}';

% ������ȡһ�� bib Ԫ�ص� label
key_reg='(?<=@\w+\{)(\w+)';
get_key=@(s)regexp(s,key_reg,'match');

% ��ȡ��Ϣ
s=readtext(f,'utf8');
bibs=regexp(s,bib_reg,'match');
keys=cellfun(get_key,bibs);
end