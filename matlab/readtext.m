function t = readtext(f,encoding)
% ����ָ����������ļ�
s=cell(1,10);
k=1;
fid=fopen(f,'r','n',encoding);
while true
    t=fgetl(fid);
    if t==-1 
        break;
    end
    s{k}=[t,newline];
    k=k+1;
end
fclose(fid);
t=sprintf('%s',s{:});
end