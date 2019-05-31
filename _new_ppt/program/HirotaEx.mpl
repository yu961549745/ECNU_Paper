# timestamp: 2019-05-31 15:55:35
# git  hash: 148e982
(*
    N 阶展开方法通用接口
*)
NEM:=module()
    option  package;
    local   Profiler,
            nem_profile_output;
    export  nem,
            nem_possible_relation,
            NEPoly,
            coefflist,
            coeff2poly;
(*
    N 阶展开多项式 对象
*)
$define ZERO NEPoly
NEPoly:=module()
    option  object;
    export  ModuleApply::static,
            ModulePrint::static,
            rebuild,
            `+`::static,
            `*`::static,
            `^`::static,
            diff::static,
            diffx::static,
            shift::static,
            m::static,
            n:=1,
            p:=0,
            q:=0,
            u:=[0],
            x:=:-x;
    local   O::static,
            `*/NEPoly`::static,
            `diff/NEPoly`::static,
            num_pow::static,
            sym_pow::static,
            check::static;

(*
    构造函数
    n   展开阶数
    p,q 多项式次数为 p*m+q
    u   最高n项的系数(从高到低排列)
    x   自变量
    diffx 对自变量求导的函数, 返回结果还是 NEPoly 对象 
*)
ModuleApply:=proc(n::posint,p,q,_u::list,x)
    local f,nv,u;
    if andmap(type,_u,0) then
        return ZERO;
    end if;
    f:=Object(NEPoly);
    f:-n:=n;
    f:-p:=p;
    f:-q:=q;
    nv:=numelems(_u);
    if nv>=n then
        u:=_u[1..n];
    else
        u:=[_u[],seq(0,k=1..(n-nv))];
    end if;
    f:-u:=u;
    f:-x:=x;
    return f;
end proc:

(*
    展示对象
*)
rebuild:=proc()
    local M,k;
    if u=[0] then
        return 0;
    end if;
    M:=p*m+q;
    return :-`+`(seq(u[k]*x^(M-k+1),k=1..n))
            +`if`(type(M-n,negint),0,O(x^(M-n)));
end proc:

ModulePrint:=proc(f::NEPoly)
    return f:-rebuild();
end proc:

# 位移操作 f(x+r)
(*
    特别备注：
        + seq 的变量不会被外部赋值影响
        + sum 的变量受外部赋值的影响
*)
shift:=proc(f::NEPoly,r)
    local M,v,k,j;
    if f=ZERO then return f; end if;
    if r=0 then
        return f;
    end if;
    if not type(f:-x,name) then
        error "shift only support for simple x, but there is %1.",f:-x;
    end if;
    M:=f:-p*NEPoly:-m+f:-q;
    v:=[seq(
        sum(f:-u[k+1]*binomial(M-k,j-k)*r^(j-k),k=0..j),
        j=0..(f:-n-1)
    )];
    return NEPoly(f:-n,f:-p,f:-q,v,f:-x);
end proc:

# 检查加法和乘法的操作条件
check:=proc(f::NEPoly,g::NEPoly,op)
    if f:-n <> g:-n then 
        error "only support %1 between NEPoly with same expansion orders.",op;
    end if;
    if f:-x <> g:-x then
        error "only support %1 between NEPoly with same variables.",op;
    end if;
end proc:

(*
    真正完美的乘法解决方案
    1. 用 foldl 调用二元解决方案
    2. 二元解决方案包含四种不同的情况
*)
`*/NEPoly`:=overload([
proc(f::NEPoly,g::NEPoly)
    option overload;
    local fp,gp,x;
    check(f,g,`*`);
    fp:=coeff2poly(f:-u,x);
    gp:=coeff2poly(g:-u,x);
    return NEPoly(f:-n,f:-p+g:-p,f:-q+g:-q,coefflist(expand(fp*gp),x),f:-x);
end proc,

proc(f::NEPoly,g)
    option overload;
    return NEPoly(f:-n,f:-p,f:-q,map(:-`*`,f:-u,g),f:-x);
end proc,

proc(f,g::NEPoly)
    option overload;
    return NEPoly(g:-n,g:-p,g:-q,map(:-`*`,g:-u,f),g:-x);
end proc,

proc(f,g)
    option overload;
    return :-`*`(f,g);
end proc
]):

`*`:=proc()
    option overload;
    if has([_passed],ZERO) then return ZERO; end if;
    return foldl(`*/NEPoly`,1,_passed);
end proc:

`^`:=proc(f::NEPoly,n)
    option overload;
    if f=ZERO then return f; end if;
    if type(n,posint) then
        return num_pow(f,n);
    elif type(n,negint) then
        error "only support positive power.";
    else
        return NEPoly(f:-n,f:-p*n,f:-q*n,sym_pow(f:-n,f:-u,n),f:-x);
    end if;
end proc:

# 数值幂次,展开为乘法,利用二分法加速
num_pow:=proc(f::NEPoly,n::posint)
    local g;
    if n=1 then
        return f;
    end if;
    g:=thisproc(f,iquo(n,2));
    if n mod 2 = 0 then 
        return `*`(g,g);
    else
        return `*`(g,g,f);
    end if;
end proc:

# 前 n 项系数为 u 的 n 阶展开多项式的 p 次幂的系数
sym_pow:=proc(n::posint,u,p)
    local s,c,w,i,r,x,res,k,nchoosek,comb2sol,list2coeff;
    nchoosek:=proc(n,k)
        return map(Vector[row],combinat:-choose([seq(0..(n-1))],k));
    end proc:
    # 将组合数对应到 x_1+...+x_n=m 的一个解
    comb2sol:=proc(_v,n,m)
        local v,r,k;
        v:=_v+~1;
        r:=Vector[row](n);
        for k from 1 to n do
            if k=1 then
                r[k]:=v[1]-1;
            elif k=n then
                r[k]:=m+n-v[n-1]-1;
            else
                r[k]:=v[k]-v[k-1]-1;
            end if;
        end do;
        return r;
    end proc:
    list2coeff:=proc(c,u,n)
        local cc,cu,len,k;
        len:=numelems(c);
        cc:=factorial(n);
        cu:=1;
        for k from 1 to len do
            cc:=cc/factorial(c[k]);
            cu:=cu*u[k]^c[k];
        end do;
        return (cc*cu);
    end proc:
    s:=Vector[row](n);
    c:=nchoosek(2*n-2,n-1);
    w:=Vector[row]([seq(k,k=0..(n-1))]);
    x:=Vector[row]([0,1$(n-1)]);
    res:=Vector(n);
    for i in c do
        r:=comb2sol(i,n,n-1);
        k:=r.w;
        if k<=n-1 then
            s[1]:=p-r.x;
            s[2..-1]:=r[2..-1];
            res[k+1]:=res[k+1]+list2coeff(s,u,p);
        end if;
    end do;
    return simplify(convert(res,list));
end proc:

# 微分操作
`diff/NEPoly`:=proc(f::NEPoly,t::name)
    return NEPoly(f:-n,f:-p,f:-q,map(:-diff,f:-u,t),f:-x)
        +diffx(f,t)*NEPoly(f:-n,f:-p,f:-q-1,zip(:-`*`,f:-u,[seq(f:-p*f:-m+f:-q-k,k=0..(f:-n-1))]),f:-x);
end proc:

# 自变量微分的通用实现方式, 但是不一定靠谱, 必要时可以自行覆盖
diffx:=proc(f::NEPoly,t::name)
    local X,u,e;
    e:=algsubs(f:-x=X,:-diff(f:-x,t));
    u:=coefflist(e,X);
    return NEPoly(f:-n,0,numelems(u)-1,u,f:-x);
end proc;

diff:=proc(f::NEPoly)
    option overload;
    if NEPoly:-diffx=NULL then
        error "NEPoly:-diffx must be given when calculate differential.";
    end if;
    if f=ZERO then return f; end if;
    return foldl(`diff/NEPoly`,f,_rest);
end proc:

(*
    加法操作
    另一种多参数的解决方案, 此方案只支持同类型元素相加
*)
`+`:=proc(f::NEPoly,g::NEPoly)
    option overload;
    local u,v,w,d,k;
    if _rest<>NULL then 
        return thisproc(thisproc(f,g),_rest);
    end if;
    if f=ZERO then return g; end if;
    if g=ZERO then return f; end if;
    check(f,g,`+`);
    if     f:-p>g:-p then
        return f;
    elif   f:-p<g:-p then
        return g;
    end if;
    if f:-q>g:-q then 
        u,v:=f:-u,g:-u;
    else
        v,u:=f:-u,g:-u;
    end if;
    d:=abs(f:-q-g:-q);
    w:=[seq(u[k+1]+`if`(k<d,0,v[k-d+1]),k=0..(f:-n-1))];
    return NEPoly(f:-n,max(f:-p,g:-p),max(f:-q,g:-q),w,f:-x);
end proc:
end module:
$undef ZERO
# 获取系数，次数按从高到低排列
coefflist:=proc(p,x::name)
    uses PolynomialTools;
    if p=0 then 
        return [0];
    end if;
    return CoefficientList(p,x,termorder=reverse);
end proc:

# 从系数构造多项式
coeff2poly:=proc(c::list,x::name)
    PolynomialTools:-FromCoefficientList(c,x,termorder=reverse);
end proc:
(*
    N 阶展开方法
    输入:
        方程中的每一个加法项转化为 NEPoly 对象
    输出:
        平衡点上界(非负)
*)
nem:=proc(vs::list(NEPoly),{debug::truefalse:=false,quiet::truefalse:=false})
    local x,s,d,ms,md,L,sd,i,j,m1,m2,m3,needBP3,m,m31,m32,lb,ub,r,k,si,di,sk,sj,print,printf,m31r,pf,st,WARNING,lastExpand;
    st:=time[real]();
    if debug then
        print:=:-print;
        printf:=:-printf;
    end if;
    if not quiet then
        WARNING:=:-WARNING;
    end if;
    L:=numelems(vs);
    if L<2 then
        return FAIL;
    end if;
    pf:=Profiler(nem_profile_output);
    pf["numInput"]:=L;
    # 基本参数
    s:=map(x->x:-p,vs);
    d:=map(x->x:-q,vs);
    sd:=zip((a,b)->[a,b],s,d);
    print(map(x->x[1]*vs[1]:-m+x[2],sd));
    sd:=sort(sd);
    ms,md:=sd[-1][];
    m1,m2,m3,m31,m32:=seq(-infinity,x=1..5);
    # 寻找BP1和BP2上界
    needBP3:=false;
    for i from 1 to L do
        for j from i+1 to L do
            if s[i]<>s[j] then # BP1
                m:=(d[j]-d[i])/(s[i]-s[j]);
                if type(m,nonnegint) 
                and s[i]*m+d[i]>=max(zip((sk,dk)->sk*m+dk,s,d)) then
                    m1:=max(m1,m);
                end if;
            elif s[i]=s[j] and d[i]=d[j] then # BP2
                if s[i]=ms and d[i]=md then # mark to find BP3
                    needBP3:=true;
                    next;
                end if;
                si:=s[i];di:=d[i];
                lb:=floor(max(zip(
                        (sk,dk)->`if`(si>sk,(dk-di)/(si-sk),NULL),
                        s,d
                    )))+1;
                ub:=ceil(min(zip(
                        (sk,dk)->`if`(si<sk,(dk-di)/(si-sk),NULL),
                        s,d
                    )))-1;
                if not type(ub,infinity) and lb<=ub and ub>=0 then
                    m2:=max(m2,ub);
                end if; 
            end if;
        end do;
    end do;
    # 寻找BP3上界
    lastExpand:=1;
    if needBP3 then 
        r:=`+`(vs[]);
        print(r);
        for k from 0 to r:-n-1 do
            if r:-u[k+1]<>0 then
                m31:={solve(r:-u[k+1],r:-m)};
                m31r:=remove(type,m31,constant);
                # 这个很尴尬, 如果保留 RootOf 的话可能会无穷地求解下去
                # 算了算了, 救不回来了
                m31r:=remove(has,m31r,RootOf);
                m31:=max(select(type,m31,nonnegint));
                m32:=floor(max(zip(
                        (sj,dj)->`if`(sj<ms,(dj-md+k)/(ms-sj),NULL),
                        s,d
                    )));
                if m31r<>{} then
                    :-__nem_unsupport_set__:={r:-u[k+1]};
                    WARNING("this equation may have m (>%1) order solutions when one of them is a interger: %2",m32,m31r);
                end if;
                m3:=max(m31,m32);
                lastExpand:=k+1;
                break;
            end if;
        end do;
        if type(m3,infinity) then
            WARNING("Expansion order should be increased.");
        end if;
    end if;
    printf("%d 阶展开\n",lastExpand);
    print(m1,m2,m31,m32);
    pf["nExpand"]:=lastExpand;
    pf["m1"],pf["m2"],pf["m3"]:=m1,m2,m3;
    pf["runtime"]:=time[real]()-st;
    :-NEM_Profiler:=pf;
    return max(m1,m2,m3);
end proc:

# 在这些情况下可能有新的平衡点
nem_possible_relation:=proc()
    local rmfv,s,x;
    rmfv:=s->remove(x->evalb(lhs(x)=rhs(x)),s);
    return simplify(map(rmfv,{solve(:-__nem_unsupport_set__)}));
end proc:

nem_profile_output:=proc(p::Profiler)
    local v2d;
    v2d:=(x)->`if`(type(x,infinity),0,1);
    return sprintf(
            "numInput=%4d, runtime=%.3f sec, nExpand=%d, bptype=%d%d%d",
            p["numInput"],p["runtime"],p["nExpand"],
            v2d(p["m1"]),v2d(p["m2"]),v2d(p["m3"])
        );
end proc:
(*
    用于生成记录某些属性的对象, 主要用于基于代码运行情况.
    作用类似于 table 和 record
    twsolve/SolHolder 和 NS1L/EqnHolder 都是基于相同原理实现的
*)
Profiler:=module()
    option  object;
    local   printFunc::procedure:=NULL,
            defalutPrintFunc::static;
    export  ps,
            get_profile,
            `?[]`::static,
            ModuleApply::static,
            ModulePrint::static;

defalutPrintFunc:=proc(p::Profiler)
    return [indices(p:-ps,'pairs')];
end proc:

ModuleApply:=proc(printFunc::procedure:=NULL)
    local p;
    p:=Object(Profiler);
    p:-ps:=table();
    if not printFunc=NULL then
        p:-printFunc:=printFunc;
    else
        p:-printFunc:=defalutPrintFunc;
    end if;
    return p;
end proc:

`?[]`:=proc(p::Profiler)
    local idxfunc;
(*
    通用下标重载函数
    典型用法为

`?[]`:=proc(p)
    local idxfunc;
$include "../common/idxfunc.mpl"
    return idxfunc(p:-ps,_rest);
end proc:

*)
idxfunc:=proc(tb,ind,val)
    local assert;
assert:=proc(cond)
    if not cond then
        error(_rest);
    end if;
end proc:
    if _npassed=3 then
        tb[ind[]]:=val[];
        return val;
    else
        assert(assigned(tb[ind[]]),"%1 not assigned.",ind[]);
        return tb[ind[]];
    end if;
end proc:
    return idxfunc(p:-ps,_rest);
end proc:

ModulePrint:=proc(p::Profiler)
    'Profiler'(p:-printFunc(p));
end proc:

get_profile:=proc()
    return printFunc(thismodule);
end proc:

end module:
end module:

PAnalyze:=module()
    uses    NEM;
    option  package;
    export  panalyze,is_special_tr;
    local   pabp;

(*
    P 分析核心逻辑
*)
panalyze:=proc(_eq,u,f,{
    nExpand::posint:=2,
    quiet::truefalse:=false,
    select_solution::truefalse:=false,
    debug::truefalse:=false, # NEM 的 debug 选项
    sol_order::posint:=NULL # 直接指定阶数进行求解, 仅限测试
})
    uses PDEtools;
    local eq,vars,m,mu,k,tr,WARNING,print,printf,frac_coeff,eqs,inds,vrs,sol,vs,
        select_sol,assert,r_solve;
# 人工选择解的统一实现
select_sol:=proc(msg::string,rw::set)
    local ind;
    printf("%s\n",msg);
    print(rw);
    while true do
        ind:=readstat(sprintf("input the index (1-%d)",numelems(rw)));
        if type(ind,posint) and ind<=numelems(rw) then
            break;
        end if;
    end do;
    printf("selected\n");
    print(rw[ind]);
    return rw[ind];
end proc:
assert:=proc(cond)
    if not cond then
        error(_rest);
    end if;
end proc:
(*
    递归分支求解
    s_prev 之前求得的解
    n_eq   当前要求解的方程
    eqs    方程列表
    vrs    变量列表
*)
r_solve:=proc(s_prev,n_eq,eqs,vrs)
    local nv,eq,r,k,x,printf,print;
    if assigned(:-__panalyze_debug__) and :-__panalyze_debug__ then
        printf:=:-printf;
        print:=:-print;
    end if;
    # 调用初始情况
    if s_prev={} then 
        return thisproc([],n_eq,eqs,vrs); 
    end if;
    # 对于当前多个解分别求解
    if type(s_prev,set) then 
        return `union`(map(thisproc,s_prev,n_eq,eqs,vrs)[]);
    end if;
    nv:=numelems(s_prev);
    # 所有变量求解成功
    if nv=numelems(vrs) then 
        printf("所有变量求解成功\n");
        # print(s_prev);
        return {s_prev};
    end if;
    # 所有方程求解完成, 但是还有变量未求解, 返回无解, 弃用这个分支
    if n_eq>numelems(eqs) then 
        printf("所有方程求解完成, 但是还有变量未求解\n");
        # print(s_prev);
        return {};
    end if;
    # 将之前的解代入
    eq:=simplify(eval(eqs[n_eq],[seq(vrs[k]=s_prev[k],k=1..nv)]));
    # 当前方程恒成立, 代入下一个方程求解
    if eq=0 then 
        printf("当前方程恒成立\n");
        print(s_prev);
        return thisproc(s_prev,n_eq+1,eqs,vrs); 
    end if;
    # 求解当前方程
    # r:={solve(eq,vrs[nv+1])};
    # 使用 PDEtools:-Solve 能够求解微分方程和代数方程
    r:=rhs~({:-PDEtools:-Solve(eq,vrs[nv+1])});
    # 表示这个分支无解, 直接返回
    if r={} then 
        printf("当前分支无解\n");
        # print(s_prev);print(eq);
        return {}; 
    end if;
    # 递归求解下一个变量
    return thisproc(map(x->[s_prev[],x],r),n_eq+1,eqs,vrs);
end proc:
    # 返回分母次数和分子
    frac_coeff:=proc(e)
        local n,d;
        n:=numer(e);
        d:=denom(e);
        if type(d,`*`) then
            d:=remove(type,d,constant);
        end if;
        if type(d,`^`) then
            d:=op(2,d);
        else
            d:=1;
        end if;
        return d=n;
    end proc:

    if not quiet then
        print:=:-print;
        printf:=:-printf;
        WARNING:=:-WARNING;
    end if;
    # 方程预处理, 这里并没有做太多的预处理, 假设已经处理完了
    assert({op(u)}={op(f)},"u and f should have same variables.");
    eq:=_eq;
    vars:=op(u);

    # 方程定阶
    if sol_order=NULL then 
        m:=pabp(eq,u,f,_options);
    else
        m:=sol_order;
    end if;
    assert(type(m,posint),"no balance point found.");
    printf("upper bound of balance point: %a\n",m);

    # 代入求解
    tr:=`+`(seq(mu[k](vars)/f^(m-k+1),k=1..m));
    eq:=collect(eval(eq,u=tr),f);
    eqs:=table(map(frac_coeff,[op(eq)]));
    inds:=sort([indices(eqs,nolist)],`>`);
    eqs:=map(k->eqs[k],inds);
    :-__pa_eqs__:=eqs;
    vrs:=[seq(mu[k](vars),k=1..m)];
    sol:=r_solve({},1,eqs,vrs);# 递归分支求解
    sol:=remove(s->andmap(type,s,0),sol);# 删除全零解
    if sol={} then # 返回 FAIL 表示无解
        return FAIL;
    end if;
    sol:=map(s->eval(tr,zip((a,b)->a=b,vrs,s)),sol); # 事先生成表达式
    if numelems(sol)>1 then
        if select_solution then
            sol:=select_sol("please select the solution of p-analyze.",sol);
        else
            WARNING("p-analyze have more than one solutions. select the first one by default. you can use option `select_solution` to choose by youself.");
            print(sol);
            sol:=sol[1];
        end if;
    else
        sol:=sol[1];
    end if;
    return sol;
end proc:
# 基于N阶展开方法
pabp:=proc(_eq,u,f,{
    debug::truefalse:=false,
    nExpand::posint:=2
})
    uses NEM;
    local eq,vs,mu,m,vars;
    eq:=expand(_eq);
    if not type(eq,`+`) then
        return 0;
    end if;
    vs:=[op(eq)];
    vars:=op(u);
    vs:=eval(vs,u=NEPoly(nExpand,1,0,[seq(mu[k](vars),k=0..(nExpand-1))],1/f));
    vs:=select(type,vs,NEPoly);# 有的方程有常数项, 需要删除
    m:=nem(vs,_options['debug']);
    return m;
end proc:
is_special_tr:=proc(tr,f)
    local fs,printf,print;
    printf:=:-printf;
    print:=:-print;
    fs:=remove(t->type(op(0,t),procedure),indets(tr,function));
    if fs={f} then
        return false;
    else
        printf("special tranfomation detected:\n");
        print(tr);
        printf("please given the expression of \n");
        print(fs minus {f});
        printf("and using option `_utr` to specify the transformation by yourself.\n");
        return true;
    end if;
end proc:
end module:

(*
    拓展的 Hirota 方法
    求 m-lump n-呼吸子 l-孤子 相互作用解
*)
HirotaEx:=module()
    option  package;
    export  hirota_ex,
            auto_plot3d,
            twtransform,
            f_lump_soliton,
            func_coeffs,
            coeffs_solve,
            SolHolder,
            mytime;
    local   assert;

assert:=proc(cond)
    if not cond then
        error(_rest);
    end if;
end proc:
mytime:=proc(e::uneval)
    local t,r;
    t:=time[real]();
    r:=eval(e);
    printf("time used %f sec.\n",time[real]()-t);
    return r;
end proc:
auto_plot3d:=proc(_f,
    _pms::{list(`=`(name,anything)),set(`=`(name,anything))},
    {
        rest_assign::truefalse:=false,
        savefile::string:=NULL,
        no_simplify::truefalse:=false,
        fix_nan::truefalse:=false,
        animate_range:=NULL,
        n_frame::posint:=24,
        plot_func::procedure:=:-plot3d,
        return_param::truefalse:=false,
        no_title::truefalse:=false
    }
)
    uses FileTools;
    local f,pms,rv,rs,vs,ph,vsr,vsc,sol,fvs,e,tt,st,et,k,x,vt,tl,save_plot,point_plot3d,plotfunc;
point_plot3d:=proc(f,r1,r2,{
    grid:=[50,50],
    map_func::procedure:=Grid:-Map 
})
    local rs,rvs,x,y,s1,s2,e,v,data,ph,vs,ff,fv;
    fv:=f;
    rs:=[r1,r2];
    rvs:=lhs~(rs);
    rs:=rhs~(rs);
    ff:=proc(vs::list(list(constant)))
        local a,b;
        return map(v->[v[],evalf(eval(fv,zip((a,b)->a=b,rvs,v)))],vs);
    end proc;
    s1:=(op(2,rs[1])-op(1,rs[1]))/(grid[1]-1);
    s2:=(op(2,rs[2])-op(1,rs[2]))/(grid[2]-1);
    vs:=[seq(
            [seq([x,y],x=rs[1],s1)],
            y=rs[2],s2
        )];
    if map_func=Grid:-Map then
        Grid:-Set('fv','rvs');
    end if;
    data:=map_func(ff,vs);
    ph:=plots[surfdata](data,labels=[rvs[],""],_rest);
    return ph;
end proc:
(*
    导出绘图到指定文件
*)
save_plot:=proc(pic,file)
    local fname;
    # mac 上 exportplot 不能直接创建非本文件夹的文件
    fname:=cat(".tmp_",FileTools:-Filename(file));
    plottools:-exportplot(fname,pic);
    if FileTools:-Exists(file) then
        FileTools:-Remove(file);
    end if;
    FileTools:-Rename(fname,file);
end proc:

    f:=_f;
    fvs:=indets({f,_pms},And(name,Not(constant)));
    pms:=convert(_pms,list);
    rv:=fvs minus indets(lhs~(pms),name) minus indets(animate_range,name);
    if rest_assign then
        pms:=[pms[],seq(x=1,x in rv)];
    else
        assert(rv={},"%1 should be assigned",rv);
    end if;
    rs,vs:=selectremove(x->type(rhs(x),range(constant)),pms);
    vsc,vsr:=selectremove(type,vs,`=`(name,constant));
    vs:=[eval(vsr,vsc)[],vsc[]];
    vs:=select(e->lhs(e) in fvs,vs);
    print([rs[],vs[]]);
    f:=eval(f,vs);
    # 提升绘图速度
    if not no_simplify then
        f:=simplify(f);
    end if;
    # 修复 NaN 绘图值, 但是会降低速度
    if fix_nan then
        plotfunc:=point_plot3d;
    else
        plotfunc:=plot_func;
    end if;
    if animate_range=NULL then
        ph:=plotfunc(f,rs[],_rest);
    else
        ph:=plots[animate](plotfunc,[f,rs[],_rest],animate_range);
    end if;
    if savefile<>NULL then
        if animate_range=NULL then
            save_plot(ph,savefile);
        else
            if Exists(savefile) then
                RemoveDirectory(savefile,'forceremove','recurse');
            end if;
            MakeDirectory(savefile);
            tt:=lhs(animate_range);
            st,et:=op(rhs(animate_range));
            for k from 1 to n_frame do
                vt:=st+(k-1)*(et-st)/(n_frame-1);
                if no_title then
                    tl:=NULL;
                else
                    tl:=title=(tt=evalf[5](vt));
                end if;
                save_plot(
                    plotfunc(eval(
                        [f,rs[],_rest],
                        tt=vt
                    )[],tl),
                    sprintf("%s/%03d.png",savefile,k)
                );
            end do;
        end if;
    end if;
    if return_param then
        return ph,[rs[],vs[]];
    else
        return ph;
    end if;
end proc:
# 行波变换统一实现 
twtransform:=proc(_eq,tr,fs::set(name):={})
    local eq,s,funcs,vars,trvars,varsubs;
    # 保序的自变量替换
    varsubs:=proc(f,vs)
        local svs,k;
        # 行波变量的位置是第一个替换变量的位置
        svs:=seq(vs[k]=NULL,k=2..numelems(vs)),vs[1]=s(vs[]);
        return f=op(0,f)(subs(svs,[op(f)])[]);
    end proc:
    eq:=_eq;
    funcs:=remove(t->type(op(0,t),procedure),indets(eq,function)); # 所有函数 
    if fs<>{} then # 只对指定的函数进行变换
        funcs:=indets(funcs,specfunc(fs));
    end if;
    vars:=map(op,funcs);# 所有函数变量的并集
    trvars:=indets(rhs(tr),name) intersect vars;# 需要替换的变量
    funcs:=select(t->trvars subset {op(t)}, funcs);# 自变量含需要替换的变量的函数
    eq:=eval(eq,map(varsubs,funcs,trvars));# 行波变量换成抽象函数
    eq:=convert(eq,D);# 统一替换为 D 形式, 保证行波的抽象函数只作为自变量出现
    eq:=subs(s(trvars[])=lhs(tr),eq);# 抽象函数换成新变量名
    eq:=convert(eq,diff);# 转化成正常表达式
    eq:=eval(subs(s(trvars[])=rhs(tr),eq));# 抽象函数换成具体表达式并自动求导
    return eq;
end proc:

# m-lump 与 n-孤子 相互作用解 生成公式
f_lump_soliton:=proc(m::nonnegint,n::nonnegint,
    xi,h,theta,psi,b
)
    local AT,AS,r,M,N,T,nT,p1,p2,p3,p4,p5,mu,nu,S,
        all_subset,all_bs,f_b,f_psi;
all_subset:=proc(n::nonnegint)
    local tmp,t;
    if n=0 then
        return {};
    end if;
    tmp:=thisproc(n-1);
    return tmp,map(t->t union {n},[tmp])[];
end proc:

all_bs:=proc(m)
    local n,s,v,k,dfs,res,p_res,L;
    n:=2*m;
    s:=Array(1..n,0); # 存储当前序列
    v:=Array(1..n,0); # 存储当前访问标记
    L:=sum(binomial(2*m,2*k)*factorial(2*k)/factorial(k)/2^k,k=0..m);
    res:=Array(1..L,fill=NULL); # 用于存储集合
    p_res:=1;
    dfs:=proc(p,l) # 当前位置 p, 目标长度 m 
        local k,t;
        if p>l then # 输出结果
            res[p_res]:=convert(s[1..l],list);
            p_res:=p_res+1;
            return;
        end if;
        if p mod 2 = 1 then # 序列 s 当前可行值的起始位置
            t:=`if`(p=1,1,s[p-2]+1); # 奇数位大于上一个奇数位
        else
            t:=s[p-1]+1; # 偶数位大于上一位
        end if;
        for k from t to n do # 从最小可行值开始 DFS 搜索
            if v[k]=1 then next; end if;
            s[p]:=k; # 序列 s 当前位置的值
            v[k]:=1; # 当前值的访问标记
            thisproc(p+1,l); # 确定下一个位置的值
            v[k]:=0; # 回溯时标记为未访问, 使得可以共用一个访问标记数组
        end do;
    end proc:
    for k from 0 to m do # 序列长度从 0 到 m 遍历
        dfs(1,2*k);
    end do;
    return convert(res,list);
end proc:

f_b:=proc(S,b)
    local k;
    return `*`(seq(b[S[2*k-1],S[2*k]],k=1..numelems(S)/2));
end proc:

f_psi:=proc(_S,_T,theta,psi)
    local S,TS,r,a,b;
    S:=convert(_S,list);
    TS:=combinat:-cartprod([seq(_T,k=1..numelems(S))]);
    r:=0;
    while not TS[finished] do 
        r:=r+`*`(zip((a,b)->`if`(b=0,theta[a],psi[a,b]),S,TS[nextvalue]())[]);
    end do;
    return r;
end proc:

    AT:=[all_subset(n)];
    AT:=map(e->map(x->x+2*m,e),AT);
    AS:=all_bs(m);
    M:={seq(1..2*m)};
    N:={seq(2*m+k,k=1..n)};
    r:=0;
    for T in AT do
        p1:=1;
        nT:=numelems(T);
        for mu from 1 to nT do 
            for nu from mu+1 to nT do 
                p1:=p1*h[T[mu],T[nu]];
            end do;
        end do;
        p2:=exp(`+`(seq(xi[k],k in T)));
        p3:=0;
        for S in AS do
            p3:=p3+f_b(S,b)*f_psi(M minus {S[]},{0} union T,theta,psi);
        end do;
        r:=r+p1*p2*p3;
    end do;
    return expand(r);
end proc:
hirota_ex:=proc(
    _eq,
    DV::set(posint),
    PL::list,
    {
        quiet::truefalse:=false,
        select_solution::truefalse:=false,
        _utr::procedure:=NULL,
        allow_trivial::truefalse:=false,
        debug::truefalse:=false,
        vrs_order::list(name):=[]
    }
)
    uses PAnalyze;
    local print,printf,WARNING,eq_process,do_select_sol,
        sh,oeq,u,x,t,n,vrs,SV,AV,omega,xi,f,phi,utr,i,j,feq,rw,xi_i,xi_j,rh,d,PLN;
# 公用方程预处理函数
# 返回: 方程, 未知函数, 空间变量的向量x, 时间变量t
eq_process:=proc(_eq)
    local eq,u,n,x,t,vars,assert;
assert:=proc(cond)
    if not cond then
        error(_rest);
    end if;
end proc:
    # 原方程处理
    assert(indets(_eq,specfunc({int,Int}))={},"equation cannot contain integral. please do transform first.");
    eq:=_eq;
    if type(eq,equation) then 
        eq:=lhs(eq)-rhs(eq);
    end if;
    eq:=numer(eq);
    # 未知函数提取
    u:=indets(eq,function);
    u:=remove(e->type(op(0,e),procedure),u);
    assert(numelems(u)=1,"only one unkown function admited, but receieved %1",u);
    u:=u[];
    # 变量绑定
    n:=nops(u)-1;
    vars:=[op(u)];
    x:=vars[1..n];
    t:=vars[n+1];
    return eq,u,x,t;
end proc:
# 多解选择的通用处理方式
do_select_sol:=proc(_rw::{set,list},namestr::string,select_solution,allow_trivial)
    local rw,select_sol;
# 人工选择解的统一实现
select_sol:=proc(msg::string,rw::set)
    local ind;
    printf("%s\n",msg);
    print(rw);
    while true do
        ind:=readstat(sprintf("input the index (1-%d)",numelems(rw)));
        if type(ind,posint) and ind<=numelems(rw) then
            break;
        end if;
    end do;
    printf("selected\n");
    print(rw[ind]);
    return rw[ind];
end proc:
    rw:=_rw;
    assert(numelems(rw)>0,"%1 has no solution.",namestr);
    if numelems(rw)>1 then
        if select_solution then
            rw:=select_sol(sprintf("please select the solution if %s.",namestr),rw);
        else
            WARNING("%1 have more than one solutions. select the first one by default. you can use option `select_solution` to choose by youself.",namestr);
            print(rw);
            rw:=rw[1];
        end if;
    else
        rw:=rw[];
    end if;
    if rw=0 and (not allow_trivial) then
        error("allow_trivial solution of %1=0.",namestr);
    end if;
    return simplify(rw);
end proc:

    if not quiet then
        print:=:-print;
        printf:=:-printf;
        WARNING:=:-WARNING;
    end if;

    sh:=SolHolder();
    sh["i"]:=i;
    sh["j"]:=j;

    # 方程预处理
    oeq,u,x,t:=eq_process(_eq);
    if vrs_order<>[] then
        x:=vrs_order[1..-2];
        t:=vrs_order[-1];
    end if;
    n:=numelems(x);
    vrs:={x[],t};
    sh["oeq"]:=oeq;
    sh["u"]:=u;
    sh["vrs"]:=vrs;
    printf("Input equation\n");
    print(oeq=0);

    # 检查 DV 范围
    AV:={seq(1..(n+1))};
    assert(DV subset AV,"DV should subset %1",AV);
    sh["DV"]:=DV;

    assert(numelems(PL)=n+2,"PL should have %1 elements.",n+2);
    # DV 指定的 PL 必须是无下标的名字
    # 没有赋值
    # 且没有出现在方程中
    PLN:=PL[convert(map(`+`,DV union {0},1),list)];
    assert(andmap(type,PLN,And(name,Not(indexed))),"%1 should be name without index.",PLN);
    assert(indets(oeq,name) intersect indets(PL,name)={},"names in PL should not appears in the equation.");
    
    SV:=select(assigned,PLN);
    assert(SV=[],"names in %1 should be unassigned.",SV);
    sh["PL"]:=PL;
    sh["PLN"]:=PLN;

    # Painleve 展开
    f:=phi(op(u));
    if _utr=NULL then
        utr:=panalyze(oeq,u,f,_options['quiet'],_options['select_solution'],_options['debug']);
        if is_special_tr(utr,f) then
            return NULL;
        end if;
        assert(tr<>FAIL,"p-analyze no solution found.");
        assert(tr<>0,"p-analyze get zero.");
        printf("P-Analyze get:\n");
    else
        printf("User give:\n");
        utr:=_utr(f);
    end if;
    feq:=numer(expand(eval(oeq,u=utr)));
    sh["f"]:=f;
    sh["utr"]:=utr;
    sh["feq"]:=feq;
    print(u=utr);

    # 行波变量
    xi:=PL[1]*`+`(seq(PL[k+1]*x[k],k=1..n),omega*t+PL[n+2]);
    print('xi'=xi);
    # 代入前对关系进行唯一化, 能够支持同名 PL 参数
    xi:=subs({seq(v=v[i],v=PLN)}[],xi);
    
    # 孤子解关键参数
    try 
        # 色散关系
        rw:=coeffs_solve(eval(feq,f=1+exp(xi)),omega);
        if rw=FAIL then
            rw:={delta[i]*:-_Omega[i]};
        end if;
        rw:=do_select_sol(rw,"omega",select_solution,allow_trivial);
        xi:=subs(omega=rw,xi);
        
        sh["omega"]:=eval(rw);
        sh["xi_i"]:=eval(xi);

        # 相互作用系数
        xi_i:=xi;
        xi_j:=subs(i=j,xi);
        rh:=coeffs_solve(eval(feq,f=1+exp(xi_i)+exp(xi_j)+h*exp(xi_i)*exp(xi_j)),h);
        if rh=FAIL then
            rh:={delta[i]*delta[j]*:-_H[i,j]};
        end if;
        rh:=do_select_sol(rh,"h[i,j]",select_solution,allow_trivial);
        sh["h_ij"]:=rh;
    catch:
        sh["err_msg"]:=StringTools:-FormatMessage(lastexception[2 .. -1]);
        WARNING("no solution found. caused by %1.",sh["err_msg"]);
        return sh;
    end try;
    print('omega'=sh["omega"]);
    print('xi[i]'=sh["xi_i"]);
    print('h[i,j]'=sh["h_ij"]);

    # LUMP 解 和 相互作用解 的关键参数
    try 
        d:=PL[1];
        sh["theta_i"]:=simplify(eval(diff(sh["xi_i"],d[i]),d[i]=0));
        sh["psi_ij"]:=simplify(eval(diff(sh["h_ij"],d[i]),d[i]=0));
        sh["b_ij"]:=simplify(eval(diff(sh["h_ij"],d[i],d[j]),[d[i]=0,d[j]=0]));
    catch:
        sh["err_msg"]:=StringTools:-FormatMessage(lastexception[2 .. -1]);
        WARNING("no lump solution. caused by %1.",sh["err_msg"]);
        return sh;
    end try;
    print('theta[i]'=sh["theta_i"]);
    print('psi[i,j]'=sh["psi_ij"]);
    print('b[i,j]'=sh["b_ij"]);
    
    return sh;
end proc:
SolHolder:=module()
    option  object;
    local   ModuleApply,ModulePrint;
    export  ps,`?[]`,get_fsol,get_sol,plot_sol,verify_sol,shift_param,get_plot_table,get_conj_param,good_plot_param,
            fsol_elems::static,simplify_fsol::static;

# 用于化简指数中的复数项
simplify_fsol:=proc(_f)
    local f,exps,fs,fs_1,fs_2,F,x,k,simplify_exp;
simplify_exp:=proc(f)
    local x,RE,IM;
    x:=rationalize(op(f));
    RE:=simplify(RealDomain:-Re(x));
    IM:=simplify(RealDomain:-Im(x));
    return exp(RE)*(cos(IM)+I*sin(IM));
end proc:
    f:=_f;
    exps:=indets(f,specfunc(exp));
    exps:=map(x->x=simplify_exp(x),exps);
    f:=subs(exps[],f);
    fs:=indets(f,function);
    fs_1:=map(k->fs[k]=F[k],[seq(1..numelems(fs))]);
    fs_2:=map(k->F[k]=fs[k],[seq(1..numelems(fs))]);
    f:=subs(fs_1[],f);
    f:=rationalize(f);
    f:=simplify(expand(f));
    f:=subs(fs_2[],f);
    return simplify(f);
end proc:
# 获取 取共轭的参数结果 
get_conj_param:=proc(key)
    local e,x,vs,subs_pms;
subs_pms:=proc(e)
    local v,k;
    v:=op(0,e);
    k:=op(e);
    if k mod 2 = 1 then
        return v[k,RE]+I*v[k,IM]
    else
        return v[k-1,RE]-I*v[k-1,IM];
    end if;
end proc:
    e:=eval(ps[key],[ps["i"]=1,ps["j"]=2]);
    vs:=indets(e,specindex(convert(ps["PLN"],set)));
    vs:=map(x->x=subs_pms(x),vs);
    e:=eval(e,vs);
    return RealDomain:-simplify(expand(e));
end proc:

# 在给定参数条件下, 使得 b_ij≈val 或 H_ij≈val
good_plot_param:=proc(
    val::realcons, # 需要达到的值
    var::name, # 需要求解的变量
    s::{list(`=`(name,anything)),set(`=`(name,anything))}, # 给定赋值集合, 剩余的默认取 1
    {
        key::{"b_ij","h_ij"}:="h_ij", # 需要求解的值
        round_n::nonnegint:=100 # 近似约化的精度, 因为无理数画图慢, 所以要近似
    }
)
    local vs,rv,eq,ss,res,rel_s,val_s;
    vs:=indets(get_fsol(0,1,0),specindex(convert(ps["PLN"],set)));
    rv:=vs minus {var,map(lhs,s)[]};
    val_s,rel_s:=selectremove(x->type(rhs(x),constant),s);
    rel_s:=eval(rel_s,val_s);
    rv:=[map(x->x=1,rv)[],rel_s[],val_s[]];
    eq:=eval(get_conj_param(key)=val,rv);
    ss:={solve(eq,var)};
    ss:=convert(ss,radical);
    ss:=remove(x->has(x,RootOf) or has(x,I),ss);
    assert(numelems(ss)>0,"no real solution found.");
    ss:=ss[1];
    ss:=round(round_n*ss)/round_n;
    res:=[var=ss,rv[]];
    print(
        b[1,2]=eval(get_conj_param("b_ij"),res),
        h[1,2]=eval(get_conj_param("h_ij"),res)
    );
    return res;
end proc:

ModuleApply:=proc()
    local t,p,e;
    t:=Object(SolHolder);
    p:=["omega","xi_i","h_ij","theta_i","psi_ij","b_ij"]; # 必要属性
    t:-ps:=table(map(e->e=FAIL,p));
    t:-ps["fsol_table"]:=table();
    t:-ps["fsol_time"]:=table();
    t:-ps["plot_table"]:=table();
    return t;
end proc:

get_fsol:=proc(m,n,l)
    local get_fsol_LBS,sol,st,key;
# m-lump n-呼吸子 l-孤子 相互作用解
get_fsol_LBS:=proc(m,n,l,xi_i,h_ij,theta_i,psi_ij,b_ij,_i,_j,PL)
    local f,xi,h,theta,psi,b,subs_idx,ns,chk_lst,subs_pms,ps;
    # 检查各必要参数是否存在
    # 如果不存在则返回 FAIL
    chk_lst:={};
    ns:=2*n+l;
    if ns>=1 then
        chk_lst:=chk_lst union {xi_i};
    end if;
    if ns>=2 then
        chk_lst:=chk_lst union {h_ij};
    end if;
    if m>=1 then
        chk_lst:=chk_lst union {theta_i,b_ij};
    end if;
    if m*ns>0 then
        chk_lst:=chk_lst union {psi_ij};
    end if;
    if ormap(type,chk_lst,identical(FAIL)) then
        error("no solution of %1",[m,n,l]);
    end if;

    # 替换生成公式的各项
    subs_idx:=proc(e)
        local v,i,j;
        v:=op(0,e);
        if   v=xi then
            i:=op(e);
            return subs(_i=i,xi_i);
        elif v=theta then
            i:=op(e);
            return subs(_i=i,theta_i);
        elif v=h then
            i,j:=op(e);
            return subs(_i=i,_j=j,h_ij);
        elif v=psi then
            i,j:=op(e);
            return subs(_i=i,_j=j,psi_ij);
        elif v=b then
            i,j:=op(e);
            return subs(_i=i,_j=j,b_ij);
        else
            return e;
        end if;
    end proc:
    f:=f_lump_soliton(m,2*n+l,xi,h,theta,psi,b);
    ps:=indets(f,indexed);
    ps:=map(e->e=subs_idx(e),ps);
    f:=subs(ps[],f);

    # 替换行波变量的系数
    subs_pms:=proc(e)
        local v,k;
        v:=op(0,e);
        k:=op(e);
        if k<=2*(m+n) then
            if k mod 2 = 1 then
                return v[k,RE]+I*v[k,IM];
            else
                return v[k-1,RE]-I*v[k-1,IM];
            end if;
        else
            return e;
        end if;
    end proc:
    # 只有 PL 中带下标的名字需要替换
    ps:=indets(f,specindex(convert(PL,set)));
    ps:=map(e->e=subs_pms(e),ps);
    f:=eval(f,ps);
    return f;
end proc:
    key:=[m,n,l];
    if not assigned(ps["fsol_table"][key]) then
        st:=time[real]();
        ps["fsol_table"][key]:=get_fsol_LBS(
            m,n,l,
            ps["xi_i"],ps["h_ij"],
            ps["theta_i"],ps["psi_ij"],ps["b_ij"],
            ps["i"],ps["j"],ps["PLN"]
        );
        ps["fsol_time"][key]:=time[real]()-st;
    end if;
    return ps["fsol_table"][key];
end proc:

# 生成公式中的加法项个数
fsol_elems:=proc(a::nonnegint,b::nonnegint,c::nonnegint)
    local m,n;
    m:=a;
    n:=2*b+c;
    return simplify(sum(binomial(n, i)*(i+1)^(2*m)*hypergeom([-m, -m+1/2], [], 2/(i+1)^2),i=0..n));
end proc:

get_sol:=proc(m,n,l)
    return eval(ps["utr"],ps["f"]=get_fsol(m,n,l));
end proc:

plot_sol:=proc(
    _sol,
    _pms::{list(`=`(name,anything)),set(`=`(name,anything))},
    {
        save_key:=NULL
    }
)
    local sol,t1,t2,t3,plt,pms;
    t1:=time[real]();
    if type(_sol,list) then
        sol:=get_sol(_sol[]);
    else
        sol:=_sol;
    end if;
    t2:=time[real]();
    # 可选参数重复, 最后一个生效
    # 所以将用户的参数放在后面可以覆盖程序的默认参数
    # 呼吸子解化简很费时间, 所以不化简比较快
    # 其它的解化简比较快
    plt,pms:=auto_plot3d(
        sol,_pms,
        'no_simplify'=(type(_sol,list) and _sol[2]>0),
        _rest,'return_param'
    );
    t3:=time[real]();
    if type(_sol,list) then
        ps["plot_table"][_sol]:=[t2-t1,t3-t2,pms];
    elif save_key<>NULL then
        ps["plot_table"][save_key]:=[t2-t1,t3-t2,pms];
    end if;
    return plt;
end proc:

get_plot_table:=proc(
    ks::list(list(nonnegint)),
    {
        filter::procedure:=(()->false)
    }
)
    local k,tb;
    tb:=ps["plot_table"];
    printf("type & construct(s) & plot(s) & parameters \\\\\n");
    for k in ks do
        printf("%s & %.3f & %.3f & %s \\\\\n",
            sprintf("%dL-%dB-%dS",k[1],k[2],k[3]),
            ps["fsol_time"][k],
            tb[k][2],
            sprintf("%a",remove(filter,eval(tb[k][3])))[2..-2]
        );
    end do;
end proc:

verify_sol:=proc(
    m,n,l,
    s::list(`=`(name,anything)):=[]
)
    local assign_verify;
(* 
    在对参数进行随机赋值的情况下, 验证:
    将 f=_sol 代入 _eq 之后, 
    各个函数的系数是否非零
    vrs 用于指定自变量
    s   是用户指定的赋值或关系
*)
assign_verify:=proc(
    _eq,f,_sol,
    vrs::set(name),
    s::list(`=`(name,anything)):=[],
    {
        quiet::truefalse:=false, # 无输出
        rnd_assign::truefalse:=false, # 剩余参数随机赋值
        rest_assign::truefalse:=false, # 剩余参数赋值为 1
        rnd_retry::nonnegint:=3, # 随机赋值出错重试次数
        return_val::truefalse:=false # 验证后的剩余系数
    }
)
    uses RandomTools;
    local ns,v,rs,eq,fs,sol,vsc,vsr,res,print;
    if not quiet then
        print:=:-print;
    end if;
    ns:=indets([_eq,_sol,s],name) minus convert(lhs~(s),set) minus vrs;
    # rnd_assign 优先级比 rest_assign 高
    if rnd_assign then
        rs:=[s[],seq(v=Generate(integer(range=1..100))/100,v in ns)];
    elif rest_assign then
        rs:=[s[],seq(v=1,v in ns)];
    else
        rs:=s;
    end if;
    vsc,vsr:=selectremove(type,rs,`=`(name,constant));
    rs:=[eval(vsr,vsc)[],vsc[]];
    print(rs);
    eq:=eval(_eq,rs);
    sol:=eval(_sol,rs);
    eq:=eval(eq,f=sol);
    eq:=func_coeffs(eq);
    eq:=map(coeffs,expand(eq),vrs);
    try
        res:=simplify(eq);
        if return_val then
            return res;
        end if;
        return evalb(res={0});
    catch: # 如果随机赋值出现异常, 则重新尝试
        if rnd_assign and rnd_retry>0 then
            return thisproc(_passed,'rnd_retry'=rnd_retry-1);
        else
            return false;
        end if;
    end try;
end proc:
    return assign_verify(ps["feq"],ps["f"],get_fsol(m,n,l),ps["vrs"],s,_rest);
end proc:

# p 中参数的第一个下标统一加 d
# 便于在绘图时调用以往绘制过的参数
shift_param:=proc(p,d)
    local e;
    return subsindets(
        p,
        specindex(convert(ps["PLN"],set)),
        e->op(0,e)[op(1,e)+d,op(2..-1,e)]
    );
end proc:

# 属性快速访问
`?[]`:=proc(p::SolHolder)
    local idxfunc;
(*
    通用下标重载函数
    典型用法为

`?[]`:=proc(p)
    local idxfunc;
$include "../common/idxfunc.mpl"
    return idxfunc(p:-ps,_rest);
end proc:

*)
idxfunc:=proc(tb,ind,val)
    local assert;
assert:=proc(cond)
    if not cond then
        error(_rest);
    end if;
end proc:
    if _npassed=3 then
        tb[ind[]]:=val[];
        return val;
    else
        assert(assigned(tb[ind[]]),"%1 not assigned.",ind[]);
        return tb[ind[]];
    end if;
end proc:
    return idxfunc(p:-ps,_rest);
end proc:

ModulePrint:=proc()
    return 'SolHolder'(ps["oeq"]=0);
end proc:

end module:
# 返回所有函数的系数
func_coeffs:=proc(_e)
    local e,fs,x;
    e:=_e;
    fs:=indets(e,function);
    fs:=map(x->x=op(0,x)(expand(op(x))),fs); # 展开函数内部, 保持唯一性
    e:=subs(fs[],e);
    fs:=map(rhs,fs); # 保留展开后的结果 
    e:=expand(e,fs[]); # 不对函数进行展开
    return simplify({coeffs(e,fs)});
end proc:
(*
    令单个表达式中所有关于函数的系数为零, 求解单个变量 x
    只会求解第一个系数, 然后通过代入验证保留满足条件的解
*)
coeffs_solve:=proc(_eq,x)
    local eq,fs,r,e;
    if _eq=0 then
        return FAIL;
    end if;
    eq:=func_coeffs(_eq);
    eq:=remove(type,eq,0);
    if eq={} then
        return FAIL;
    end if;
    :-_coeffs_solve_rst_eqs_:=remove(has,eq,x);
    if :-_coeffs_solve_rst_eqs_<>{} then # 所有方程必须含 x 
        return {};
    end if;
    r:={solve(eq[1],x,explicit)}; # 只求解第一个方程
    r:=convert(r,radical); 
    :-_coeffs_solve_origin_sols_:=r;
    r:=remove(e->has(e,I) or has(e,RootOf),r); # 删除复数解, 常数解, 含 RootOf 的隐式解
    r:=select(e->simplify(eval(eq,x=e))={0},r); # 保留满足所有方程的解
    return r;
end proc:

end module:
