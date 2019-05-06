# timestamp: 2019-05-06 19:56:43
# git  hash: e6c9ce7
LatexUtils:=module()
    option package;
    local DiffPrinter,MyLatex;
    export dprint,mylatex,cbidx,setMyLatex,setDiffPrinter,printLatex,printSetList;
MyLatex:=module()
    export  mylatex,
            maxLineLen:=150,
            maxFracLen:=64,
            maxExpLen:=10;
    local   ModulePrint,
            LatexPrint,
            tex:=table(),
            LatexCat,
            ListInsert,
            TexComp,
            newTexComp,
            TexListCat,
            TexCompCat,
            TexCombine,
            RemoveInnerList,
            CompPrint,
            ListStat,
            LatexDiffPrint,
            MinusSimplify,
            needBracket,
            mulCompPrint,
            `mylatex/numeric`,
            `mylatex/complex`,
            `mylatex/symbol`,
            `mylatex/indexed`,
            `mylatex/+`,
            `mylatex/*`,
            `mylatex/.`,
            `mylatex/^`,
            `mylatex/function`,
            `mylatex/diff`,
            `mylatex/relation`,
            `mylatex/set`,
            `mylatex/list`,
            `mylatex/other`;

tex["+"]:=newTexComp("+",1,right):
tex["-"]:=newTexComp("-",1,right):
tex["*"]:=newTexComp("\\,",1,right):
tex["/"]:=newTexComp("/",1,right):
tex["("]:=newTexComp("\\left( ",1,right):
tex[")"]:=newTexComp("\\right) ",1,left):
tex["["]:=newTexComp("\\left[ ",1,right):
tex["]"]:=newTexComp("\\right] ",1,left):
tex["{"]:=newTexComp("\\left\\{ ",1,right):
tex["}"]:=newTexComp("\\right\\} ",1,left):
tex["."]:=newTexComp("\\cdot ",1,right):
tex[","]:=newTexComp(",",1):
tex["="]:=newTexComp("=",1,right):
tex["<"]:=newTexComp("<",1,right):
tex["<="]:=newTexComp("\\leq ",1,right):
tex["<>"]:=newTexComp("\\neq ",1,right):
tex["inf"]:=newTexComp("\\infty ",1):
tex["nan"]:=newTexComp("{\\rm NAN}",3):
tex["pa"]:=newTexComp("\\partial ",1):
tex["d"]:=newTexComp("{\\rm d}",1):

# 输出可以自动换行的latex公式表达式
mylatex:=proc()
    local tex;
    tex:=LatexPrint(_passed);
    tex:=TexCombine(tex);
    return LatexCat([tex]);
end proc:

# 控制公式换行
LatexCat:=proc(ts::list(TexComp))
    uses StringTools;
    local pos:=0,b:=StringBuffer(),t;
    for t in ts do
        if pos + t:-len <= maxLineLen then
            b:-append(t:-str);
            pos:=pos+t:-len;
        else
            b:-append(" \\\\\n");
            b:-append(t:-str);
            pos:=t:-len;
        end if;
    end do;
    return b:-value();
end proc:

# 合并一些项
# + 合并 +- 为 -
# + {,[,( 和右边的元素合并
# + },],) 和左边的元素合并
TexCombine:=proc(t::seq(TexComp))
    local ts,A,i,n;
    ts:=[t];
    n:=numelems(ts);
    A:=Array(1..n);
    for i from 1 to n do
        if i=1 then
            A[i]:=ts[i];
        elif ts[i-1]:-lnk=right then
            A[i]:=TexListCat([A[i-1],ts[i]]);
            A[i-1]:=0;
        elif ts[i]:-lnk=left then
            A[i]:=TexListCat([A[i-1],ts[i]]);
            A[i-1]:=0;
        else
            A[i]:=ts[i];
        end if;
    end do;
    return remove(type,convert(A,list),0)[];
end proc:

MinusSimplify:=proc(t::seq(TexComp))
    local ts,A,i,n;
    ts:=[t];
    n:=numelems(ts);
    A:=Array(1..n);
    for i from 1 to n do
        if i>1 and ts[i]=tex["-"] and ts[i-1]=tex["+"] then
            # +- 号合并为 - 号
            A[i]:=ts[i];
            A[i-1]:=0;
        else
            A[i]:=ts[i];
        end if;
    end do;
    return remove(type,convert(A,list),0)[];
end proc:

# 在 list 的两个元素中插入指定元素
ListInsert:=proc(t::list,v)
    local res; 
    res:=map(x->(x,v),t);
    return res[1..-2];
end proc:

TexComp:=module()
    option  object;
    export  len::nonnegint,
            str::string,
            lnk,
            `=`::static;
    local   ModulePrint::static;
    ModulePrint:=proc(t::TexComp)
        return sprintf("[%s,len=%d]",t:-str,t:-len);
    end proc:
    `=`:=proc(a::TexComp,b::TexComp)
        return evalb(a:-str=b:-str);
    end proc:
end module:

newTexComp:=proc(str::string,len::nonnegint,lnk:=none)
    local t;
    t:=Object(TexComp);
    t:-len:=len;
    t:-str:=str;
    t:-lnk:=lnk;
    return t;
end proc:

# 将多个元素连接成一个元素
TexListCat:=proc(t::list(TexComp))
    return newTexComp(
        cat(seq(x:-str,x in t)),
        `+`(seq(x:-len,x in t))
    )
end proc:

TexCompCat:=proc(e,i)
    local res:=map(x->[LatexPrint(x)],[op(e)]);
    res:=ListInsert(res,[i]);
    res:=RemoveInnerList(res);
    return res[];
end proc:

# 返回 TexComp 的 seq
# 将不能分割的整体表示为一个 TexComp 对象
LatexPrint:=proc()
    local e:=_passed;
    if _npassed > 1 then
        return TexCompCat([_passed],tex[","]);
    elif type(e,extended_numeric) then
        return `mylatex/numeric`(e);
    elif type(e,complex(extended_numeric)) then
        return `mylatex/complex`(e);
    elif type(e,symbol) then
        return `mylatex/symbol`(e);
    elif type(e,indexed) then
        return `mylatex/indexed`(e);
    elif type(e,`+`) then
        return `mylatex/+`(e);
    elif type(e,`*`) then
        return `mylatex/*`(e);
    elif type(e,`.`) then
        return `mylatex/.`(e);
    elif type(e,`^`) then
        return `mylatex/^`(e);
    elif type(e,function) then
        return `mylatex/function`(e);
    elif type(e,relation) then
        return `mylatex/relation`(e);
    elif type(e,set) then
        return `mylatex/set`(e);
    elif type(e,list) then
        return `mylatex/list`(e);
    else
        return `mylatex/other`(e);
    end if;
end proc:

`mylatex/numeric`:=proc(ee)
    local s,l,e,sn;
    sn:=`if`(ee<0,tex["-"],NULL);
    e:=abs(ee);
    if type(e,{integer,float}) then
        # 暂时不考虑处理超长数
        s:=convert(e,string);
        l:=length(s);
        return sn,newTexComp(s,l);
    elif type(e,fraction) then
        s[1]:=convert(op(1,e),string);
        s[2]:=convert(op(2,e),string);
        l[1]:=length(s[1]);
        l[2]:=length(s[2]);
        if max(l[1],l[2])>maxFracLen then
            return sn,newTexComp(s[1],l[1]),tex["/"],newTexComp(s[2],l[2]);
        else
            return sn,newTexComp(sprintf("\\frac{%s}{%s}",s[1],s[2]),max(l[1],l[2]));
        end if;
    elif type(e,infinity) then
        return sn,tex["inf"];
    elif type(e,undefined) then
        return tex["nan"];
    else
        error "unknow extended numeric %1",e;
    end if;
end proc:

`mylatex/complex`:=proc(e)
    local a,b,mop;
    a:=Re(e);
    b:=Im(e);
    if a<>0 then  
        mop:=`if`(b>0,tex["+"],tex["-"]);
        b:=abs(b);
        if b=0 then
            return LatexPrint(a);
        elif b=1 then
            return LatexPrint(a),mop,LatexPrint(i);
        else
            return LatexPrint(a),mop,LatexPrint(b),tex["*"],LatexPrint(i);
        end if;
    else
        mop:=`if`(b>0,NULL,tex["-"]);
        b:=abs(b);
        if b=1 then
            return mop,LatexPrint(i);
        else
            return mop,LatexPrint(b),tex["*"],LatexPrint(i);
        end if;
    end if;
end proc:

`mylatex/symbol`:=proc(ee)
    uses StringTools;
    local s,l,e;
    e:=convert(ee,`global`);
    s:=latex(e,output=string);
    # 解决 "\mbox {{\tt `xxx`}}" 的问题
    if length(s)>=17 and s[1..13]="\\mbox {{\\tt `" then
        s:=s[14..-4];
    end if;
    if s[1]="\\" then # 处理类似 \alpha 的问题
        return newTexComp(s,1);
    elif RegMatch("\\{\\\\it [a-zA-Z0-9]+\}",s) then
        s:=RegSubs("\\{\\\\it ([a-zA-Z0-9]+)\}"="\\1",s);
        return newTexComp(s,length(s));
    else
        return newTexComp(s,length(s));
    end if;
end proc:

`mylatex/indexed`:=proc(e)
    local v,i;
    v:= TexListCat( [LatexPrint(op(0,e))] );
    i:= TexListCat( [LatexPrint(op(e))] );
    return newTexComp(sprintf("{{%s}_{%s}}",v:-str,i:-str),v:-len+i:-len);
end proc:

`mylatex/+`:=proc(e)
    return MinusSimplify( TexCompCat(e,tex["+"]) );
end proc:

`mylatex/.`:=proc(e)
    return RemoveInnerList( ListInsert( mulCompPrint~([op(e)]) , [tex["."]] ) )[];
end proc:

mulCompPrint:=proc(e)
    if needBracket(e) then
        return [tex["("],LatexPrint(e),tex[")"]];
    else
        return [LatexPrint(e)];
    end if;
end proc:

needBracket:=proc(e)
    return not ( 
             type(e,{extended_numeric,symbol,indexed,function,`^`}) 
        or ( type(e,complex(extended_numeric)) and Re(e)*Im(e)=0 )
    );
end proc:

`mylatex/*`:=proc(e)
    local num,den,Lnum,Lden,sn;
    num:=numer(e);
    den:=denom(e);
    if type(op(1,num),extended_numeric) and op(1,num)<0 then
        sn:=tex["-"];
        num:=-num;
    else
        sn:=NULL;
    end if;
    if den=1 then
        num:=ListInsert( [CompPrint(num)] , [tex["*"]] );
        num:=RemoveInnerList(num);
        return sn,num[];
    else
        num:=ListInsert( [CompPrint(num)] , [tex["*"]] );
        num:=RemoveInnerList(num);
        den:=ListInsert( [CompPrint(den)] , [tex["*"]] );
        den:=RemoveInnerList(den);
        Lnum:=`+`(seq(x:-len,x in num));
        Lden:=`+`(seq(x:-len,x in den));
        if max(Lnum,Lden) < maxFracLen then
            num:=TexListCat(num);
            den:=TexListCat(den);
            return sn,newTexComp(
                sprintf("\\frac{%s}{%s}",num:-str,den:-str),
                max(num:-len,den:-len)
            );
        else
            return sn,tex["("],num[],tex[")"],tex["/"],tex["("],den[],tex[")"];
        end if;
    end if;
end proc:

RemoveInnerList:=proc(v::list)
    return map(x->x[],v);
end proc:

# 乘法元素输出
CompPrint:=proc(e)
    if type(e,`*`) then
        return CompPrint~([op(e)])[];
    end if;
    if needBracket(e) then
        return [tex["("],LatexPrint(e),tex[")"]];
    else
        return [LatexPrint(e)];
    end if;
end proc:

`mylatex/^`:=proc(e)
    local l,u,tl,tu,LL;
    l:=op(1,e);
    u:=op(2,e);
    tl:=LatexPrint(l);
    tu:=LatexPrint(u);
    LL:=`+`(seq(x:-len,x in [tl]));
    if LL <= maxLineLen and type(u,fraction) and op(1,u)=1 and op(2,u)<10 then
        tl:=TexListCat([tl]);
        if op(2,u)=2 then
            return newTexComp(sprintf("\\sqrt {%s}",tl:-str),tl:-len+3);
        else
            return newTexComp(sprintf("\\sqrt [%d]{%s}",op(2,u),tl:-str),tl:-len+3);
        end if;
    else
        tu:=TexListCat([tu]);# 不考虑超长指数
        if numelems([tl])>1 then
            return tex["("],tl,tex[")"],newTexComp(sprintf("^{%s}",tu:-str),tu:-len);
        else
            return newTexComp(sprintf("{%s}^{%s}",tl:-str,tu:-str),tl:-len+tu:-len);
        end if;
    end if;
end proc:

`mylatex/function`:=proc(e)
    local fun,arg,sfun,targ;
    fun:=op(0,e);
    arg:=op(e);
    sfun:=TexListCat( [LatexPrint(fun)] );
    sfun:-lnk:=right;
    targ:=LatexPrint(arg);
    if fun=exp and `+`(seq(x:-len,x in [targ]))<=maxExpLen then
        targ:=TexListCat([targ]);
        if arg=1 then
            return newTexComp("{\\rm e}",1);
        else
            return newTexComp(sprintf("{\\rm e}^{%s}",targ:-str),targ:-len+1);
        end if;
    end if;
    if fun in {diff,Diff} then
        return `mylatex/diff`(e);
    end if;
    if fun in {int,Int} then
        return `mylatex/other`(e);
    end if;
    if fun=`.` then
        return tex["("],LatexPrint(op(1,e)),tex[")"],tex["."],tex["("],LatexPrint(op(2,e)),tex[")"];
    end if;
    return sfun,tex["("],targ,tex[")"];
end proc:

`mylatex/diff`:=proc(ee)
    try
        return LatexDiffPrint(ee);
    catch :
        return `mylatex/other`(ee);
    end try;
end proc:

LatexDiffPrint:=proc(ee)
    local sd,oarg,arg,fun,ord,e,N,i,ind,b,num,den,len:=0;
    e:=convert(ee,D);
    if e=0 then
        return `mylatex/other`(ee);
    end if;
    arg:=[op(e)];
    oarg:=arg;
    arg:=LatexPrint~(arg);
    if numelems(arg)=1 then
        sd:=tex["d"];
    else
        sd:=tex["pa"];
    end if;
    fun:=op([0,1],e);
    ord:=op([0,0],e);
    if type(ord,indexed) then
        ord:=[op(ord)];
    elif ord=D then
        ord:=[1];
    else 
        ord:=[1$op(2,ord)];
    end if;
    N:=numelems(ord);
    ord:=ListStat(ord);
    if N=1 then
        num:=sprintf("%s",sd:-str);
    else
        num:=sprintf("%s^{%d}",sd:-str,N);
    end if;
    ind:=ListTools[Reverse]([indices(ord,nolist)]);
    b:=StringTools:-StringBuffer();
    for i in ind do
        if ord[i]=1 then
            den:=sprintf("{%s %s}",sd:-str,arg[i]:-str);
        else
            den:=sprintf("{%s %s}^{%d}",sd:-str,arg[i]:-str,ord[i]);
        end if;
        b:-append(den);
        len:=len+1+arg[i]:-len;
    end do;
    return newTexComp(sprintf("{\\frac{%s}{%s}}",num,b:-value()),len),LatexPrint(fun(oarg[]));
end proc:

ListStat:=proc(v::list(posint))
    local t,e;
    t:=table();
    for e in v do
        if not assigned(t[e]) then
            t[e]:=1;
        else
            t[e]:=t[e]+1;
        end if;
    end do;
    return eval(t);
end proc:

`mylatex/relation`:=proc(e)
    return LatexPrint(lhs(e)),tex[convert(op(0,e),string)],LatexPrint(rhs(e));
end proc:

`mylatex/set`:=proc(e)
    if numelems(e)=0 then   
        return tex["{"],newTexComp("",0),tex["}"];
    end if;
    return tex["{"],TexCompCat(e,tex[","]),tex["}"];
end proc:

`mylatex/list`:=proc(e)
    if numelems(e)=0 then   
        return tex["["],newTexComp("",0),tex["]"];
    end if;
    return tex["["],TexCompCat(e,tex[","]),tex["]"];
end proc:

`mylatex/other`:=proc(e)
    local r:=latex(e,output=string);
    return newTexComp(r,floor(length(r)/5));
end proc:

ModulePrint:=proc()
    return 'MyLatex'(
        'maxLineLen'=maxLineLen,
        'maxFracLen'=maxFracLen,
        'maxExpLen'=maxExpLen
    );
end proc:

end module:
(*
    将表达式中的微分以下标的形式进行显示
    + 如果函数的参数是简单的变量名则省略参数, 否则返回完整形式
    + 如果函数本身是 int , 则改为 Int
    + 如果微分变量总数大于 3 则采用简化形式
    + 不考虑符号阶导数
*)
DiffPrinter:=module()
    export dprint,maxDiffNum:=10;
    local trans_func,func_convert,diff_convert,tassign,ModulePrint;

dprint:=proc()
    if _npassed=0 then
        return null;
    end if;
    if _npassed>1 then
        return thisproc([_passed])[];
    end if;
    return subsindets[flat](_passed,function,trans_func);
end proc:

trans_func:=proc(e)
    if op(0,e) in {diff,Diff} then
        return diff_convert(e);
    else
        return func_convert(e);
    end if;
end proc:

# 对于 function(name) 类型, 如果不是 procedure 则返回函数名
# 否则返回完整形式, 并且对内部参数进行递归转化
# 如果函数本身是 int , 因为被积函数变成名称之后会影响计算结果
func_convert:=proc(e)
    if not type(op(0,e),procedure) and type(e,function(name)) then
        return op(0,e);
    elif op(0,e)=int then
        return Int(dprint([op(e)])[]);
    else
        return op(0,e)(dprint([op(e)])[]);
    end if;
end proc:

diff_convert:=proc(_e)
    local e,tb,v,vs,sub,ps;
    e:=_e;
    tb:=table();
    # 提取 函数 和 微分变量
    while op(0,e) in {diff,Diff} do
        vs:=[op([2..-1],e)];
        e:=op(1,e);
        vs:=map(t->`if`(type(t,list),t[],t),vs);
        if ormap(t->op(0,t)=`$`,vs) then
            error "Do not support symbolic order differential.";
        end if;
        for v in vs do
            tassign(tb,v,1);
        end do;
    end do;
    # 构造简化的微分下标
    # 如果变量总数大于3则采用简化形式
    # 不考虑符号阶导数
    ps:=[indices(tb,pairs,indexorder)];
    if `+`(entries(tb,nolist))>maxDiffNum then
        sub:=cat(map(v->`if`(rhs(v)>1,cat(rhs(v),lhs(v)),lhs(v)),ps)[]);
    else
        sub:=cat(map(v->(lhs(v)$rhs(v)),ps)[]);
    end if;
    # 为了解决 Maple latex 函数在处理类似于 (u+v)[x] 的表达式时的 BUG
    if type(e,{`+`,`*`,`^`}) then
        return ``(dprint(e))[sub];
    else
        return dprint(e)[sub];
    end if;
end proc:

tassign:=proc(t,k,v)
    if assigned(t[k]) then
        t[k]:=t[k]+v;
    else
        t[k]:=v;
    end if;
    return;
end proc:

ModulePrint:=proc()
    return 'DiffPrinter'('maxDiffNum'=maxDiffNum);
end proc:

end module:

cbidx:=proc(e)
    local x;
    return subsindets(e,indexed(nonnegint),
        (x->op(0,x)[parse(cat(op(x)))])
    );
end proc:

dprint:=DiffPrinter:-dprint;
mylatex:=MyLatex:-mylatex;

setMyLatex:=proc(ps::set(equation))
    local v;
    for v in ps do
        MyLatex[lhs(v)]:=rhs(v);
    end do;
    return MyLatex;
end proc:

setDiffPrinter:=proc(ps::set(equation))
    local v;
    for v in ps do
        DiffPrinter[lhs(v)]:=rhs(v);
    end do;
    return DiffPrinter;
end proc:

printLatex:=proc(_e,{
    use_diff::truefalse:=false,
    use_idx::truefalse:=false
})
    local e;
    e:=_e;
    if use_diff then
        e:=dprint(e);
    end if;
    if use_idx then 
        e:=cbidx(e);
    end if;
    printf("%s\n",mylatex(e));
    return NULL;
end proc:

printSetList:=proc(_e::{set,list},{use_idx::truefalse:=false})
    local e,x;
    e:=_e;
    if use_idx then 
        e:=cbidx(e);
    end if;
    map(x->printf("%s,\\\\\n",mylatex(x)),e):
    return NULL;
end proc:

end module:
