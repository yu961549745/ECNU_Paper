function writetext(f,s,encoding)
% ����ָ������д�ļ�
fid=fopen(f,'w','n',encoding);
fprintf(fid,'%s',s);
fclose(fid);
end