function fs = getFiles(path,reg,exReg)
% ��ȡ�ļ����������ļ�
% �����ܹ�ͨ��������ʽ���й���
if nargin<3
    % Ĭ������ windows �µ� db �ļ�
    % �� mac �µ� . ��ͷ�������ļ�
    exReg='(.*\.db)|(^\..*)';
    if nargin<2
        reg='';
    end
end
% ���ļ���ɸѡ
sfs=dir(path);
fs=arrayfun(@(f){f.name},sfs);
fds=arrayfun(@(f){f.folder},sfs);
dfs=arrayfun(@(f)f.isdir,sfs);
fs=fs(~dfs);
fds=fds(~dfs);
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
fds=fds(ind&(~exInd));
fs=cellfun(@(p,f){fullfile(p,f)},fds,fs);
end
