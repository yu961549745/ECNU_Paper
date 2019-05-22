function fs = getFiles(path,reg,exReg)
% ��ȡ�ļ����������ļ�
% �����ܹ�ͨ��������ʽ���й���
if nargin<3
    % Ĭ������ windows �µ� db �ļ�
    % �� mac �µ� . ��ͷ�������ļ�
    % �Լ� . �� .. 
    exReg='(.*\.db)|(^\.{1,2}.*)';
    if nargin<2
        reg='';
    end
end
% ���ļ���ɸѡ
fs=dir(path);
fs=arrayfun(@(f){f.name},fs);
if isempty(fs)
    error('no file found.');
end
if ~isempty(reg)
    ind=cellfun(@(s)~isempty(regexp(s,reg,'once')),fs);
else
    ind=true(size(fs));
end
if ~isempty(exReg)
    exInd=cellfun(@(s)~isempty(regexp(s,exReg,'once')),fs);
else
    exInd=false(size(fs));
end
fs=fs(ind&(~exInd));
% ����·��
fs=cellfun(@(f){fullfile(path,f)},fs);
end
