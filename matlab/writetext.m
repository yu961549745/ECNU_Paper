function writetext(f,s,encoding)
% 按照指定编码写文件
fid=fopen(f,'w','n',encoding);
fprintf(fid,'%s',s);
fclose(fid);
end