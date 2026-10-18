'use strict';
(() => {
  const VERSION='20261018-box4';
  const root=document.getElementById('rm-app');
  const esc=v=>String(v??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
  const number=(v,d=2)=>v==null||!Number.isFinite(Number(v))?'—':Number(v).toFixed(d);
  const pvalue=v=>v==null?'—':v<.001?'<0.001':Number(v).toFixed(3);
  const types={continuous:'连续／近似连续',binary:'二分类',ordinal:'有序分类'};
  const hypothesis={group_equal_time_average:'组间平均水平（时间等权）',time_equal_group_average:'时间变化（组别等权）',group_time_interaction:'组别 × 时间交互',any_group_difference_across_times:'各时点组间差异联合检验'};
  const colors=['#176b58','#b87837','#5e79a4','#98668e','#65934b','#ac6262','#587a84','#8d8459','#6f6998','#a17d6d'];
  const state={step:1,book:null,dataset:null,fileName:'',settings:null,result:null,snapshot:null,error:'',busy:'',tab:'description',outcome:'',chart:'description',category:'',boxOptions:Object.create(null),generation:0};
  let runtime=null,runtimePromise=null,loaded=new Set(),clock=null,started=0;
  const option=(items,value)=>items.map(([v,label])=>`<option value="${esc(v)}" ${String(v)===String(value)?'selected':''}>${esc(label)}</option>`).join('');
  const field=(label,html)=>`<label class="rm-field">${label}${html}</label>`;
  const btn=(label,action,cls='secondary',extra='')=>`<button class="btn ${cls}" data-action="${action}" ${extra}>${label}</button>`;
  const check=(id,label,checked)=>`<label class="rm-check"><input type="checkbox" id="${id}" ${checked?'checked':''}><span>${label}</span></label>`;
  const busy=()=>Boolean(state.busy);
  function setStatus(message){state.busy=message;const e=document.getElementById('rm-status');if(e)e.textContent=message;}
  function invalidate(){state.result=null;state.snapshot=null;state.error='';}
  function observed(o){return [...new Set(state.dataset.data.flatMap(r=>o.measurements.map(m=>r[m.column]).filter(v=>v!=null).map(String)))].sort((a,b)=>a.localeCompare(b,'zh-CN',{numeric:true}));}
  function shell(body){
    return `<div class="shell rm-all"><aside class="sidebar rm-nav"><div class="logo"><div><div class="logo-name">研析</div><div class="logo-sub">CLINSTAT</div></div></div><div class="side-section-label">研究工作台</div><a class="module-link" href="./" target="_blank" rel="noopener">基线特征分析<span class="badge">01</span></a><button class="nav-main">重复测量结果<span class="badge">02</span></button><div class="rm-panel" style="padding:16px;background:#f4f8f4"><strong>同一患者 · 多个时间点</strong><ul class="rm-scope"><li>两组与多组比较</li><li>连续、二分类、有序结局</li><li>GEE 或随机截距混合模型</li></ul></div><div class="side-guide"><strong>文件不上传服务器</strong>数据仅保留在当前页面。关闭或刷新前，请下载结果。</div></aside><main class="main"><header class="topbar"><div class="crumb">研究工作台 › <strong>重复测量结果</strong></div><span class="pill">浏览器内计算</span></header><div class="content"><div class="rm-mobile-nav"><a href="./" target="_blank" rel="noopener">基线特征分析 ↗</a><strong>重复测量结果</strong></div><div class="page-heading"><div><div class="eyebrow">LONGITUDINAL OUTCOMES</div><h1>重复测量结果分析</h1><p>保留每位患者的测量，比较组间差异与随时间的变化。</p>${state.fileName?`<div class="header-file">${esc(state.fileName)} · 仅在当前页面保留</div>`:''}</div>${state.dataset?btn('重新导入','restart'):''}</div><nav class="rm-tabs" aria-label="分析步骤">${['导入数据','确认患者与指标','选择模型','结果与导出'].map((label,i)=>`<button data-step="${i+1}" ${busy()||(!state.dataset&&i>0)||(i===3&&!state.result)?'disabled':''} class="${state.step===i+1?'active':''}">${String(i+1).padStart(2,'0')} ${label}</button>`).join('')}</nav>${state.error?`<div class="rm-error" role="alert">${esc(state.error)}</div>`:''}${body}<div class="rm-app-footer">本版支持一行一名患者、各结局时间点完整的数据；不自动填补或删除患者。关闭或刷新会清除本次页面数据。</div></div></main></div>`;
  }
  function render(){
    const content=busy()?`<section class="rm-panel rm-busy"><span class="spinner"></span><h2 id="rm-status" role="status">${esc(state.busy)}</h2><p class="rm-help">首次运行需加载计算组件，请保持页面打开。数据不上传。</p><p class="rm-help" id="rm-elapsed"></p>${btn('取消本次计算','cancel')}</section>`:
      !state.dataset?importView():state.step===1?dataView():state.step===2?mappingView():state.step===3?modelView():resultView();
    root.innerHTML=shell(content);
  }
  function importView(){return `<section class="rm-panel"><h2>导入重复测量数据</h2><p class="rm-help">宽表格式：一行一位患者，一列一个时间点的测量。支持同一份表内包含多个结局，各结局可使用不同时间点。</p><div class="rm-note">例如：患者编号、组别、疼痛_24h、疼痛_48h、疼痛_72h。二分类可用数字或文字，有序等级需明确从低到高的顺序。</div><div class="actions" style="margin-top:24px">${btn('选择 Excel / CSV','choose','')}${btn('载入四组混合结局案例','sample4')}${btn('载入两组案例','sample2')}</div><input class="rm-file" id="rm-file" type="file" accept=".xlsx,.xls,.csv,.tsv"><p class="rm-help">最大20MB，最多100列、5000名患者。案例为合成数据，包含连续评分、二分类和有序等级，不含真实患者数据。</p></section>`;}
  function dataView(){
    const d=state.dataset,s=state.settings,groups=RMCore.groupsOf(d.data,s.group);
    return `<section class="rm-panel"><h2>数据已在本机读取</h2><div class="rm-grid">${field('工作表',`<select id="rm-sheet">${option(state.book.SheetNames.map(x=>[x,x]),s.sheet)}</select>`)}${field('分组列',`<select id="rm-group">${option(d.names.map(x=>[x,x]),s.group)}</select>`)}</div><div class="rm-stats"><div class="rm-stat"><strong>${d.data.length}</strong><span>患者记录，身份待确认</span></div><div class="rm-stat"><strong>${groups.length}</strong><span>实际组别</span></div><div class="rm-stat"><strong>${d.names.length}</strong><span>数据列</span></div></div><p class="rm-help">分组：${groups.map(g=>`${esc(g)}（${d.data.filter(r=>String(r[s.group])===g).length}条）`).join('、')}</p>${table(d.names,d.data.slice(0,5).map(r=>d.names.map(n=>esc(r[n]??'缺失'))))}<div class="rm-submit"><p class="rm-help">预览前5条记录；后续须确认一行一位患者及各测量的对应关系。</p>${btn('确认患者与指标 →','mapping','')}</div></section>`;
  }
  function mappingView(){
    const d=state.dataset,s=state.settings,available=d.names.filter(x=>![s.group,s.id].includes(x));
    return `<section class="rm-panel"><h2>确认患者与重复测量映射</h2><div class="rm-grid">${field('患者编号列',`<select id="rm-id">${option([['','使用原始Excel行号生成临时编号'],...d.names.filter(x=>x!==s.group).map(x=>[x,x])],s.id)}</select>`)}${field('分组列',`<select id="rm-group">${option(d.names.map(x=>[x,x]),s.group)}</select>`)}</div>${!s.id?check('rm-row-pair','确认一行代表一名患者，同一行所有时间点和指标对应同一人。临时编号保留原始Excel行号。',s.rowPairing):`<p class="rm-help">编号须在全表唯一且无缺失；编号不能只在组内唯一。分析前会检查。</p>`}<p class="rm-help">自动识别只是建议，请核对每项指标、数据类型、时间标签与先后顺序。没有编号时，相同评分记录仍保留，不会自动认定为重复患者。</p>${s.outcomes.map((o,i)=>`<section class="rm-outcome"><div class="rm-outcome-head"><label class="rm-field">结局名称<input class="rm-title-input" data-o="${i}" data-property="label" value="${esc(o.label)}"></label>${btn('移除此指标',`remove-outcome:${i}`,'secondary rm-small')}</div><div class="rm-grid">${field('结局类型',`<select data-o="${i}" data-property="type">${option(Object.entries(types),o.type)}</select>`)}${field('描述表展示',o.type==='continuous'?`<select data-o="${i}" data-property="summary">${option([['median_iqr','中位数（Q1，Q3）'],['mean_sd','均值（SD）']],o.summary)}</select>`:`<span>各类别 n（%）</span>`)}</div>${o.type==='binary'?field('哪一个实际取值代表事件？',`<select data-o="${i}" data-property="event">${option([['','请选择事件值'],...observed(o).map(x=>[x,x])],o.event)}</select>`):o.type==='ordinal'?`${field('等级顺序，从低到高，用 | 分隔',`<input data-o="${i}" data-property="levels" value="${esc(o.levels)}" placeholder="例如：低|中|高，或 1|2|3">`)}<p class="rm-help">实际取值：${observed(o).map(esc).join('、')}。不得根据数字编码自动推断等级。未观测等级在本版不能估计阈值。</p>`:''}${o.measurements.map((m,j)=>`<div class="rm-measure"><label>评分列<select data-o="${i}" data-m="${j}" data-measure="column">${option(available.map(x=>[x,x]),m.column)}</select></label><label>时间标签<input data-o="${i}" data-m="${j}" data-measure="time" value="${esc(m.time)}" placeholder="例如：24h 或 T1"></label><div class="rm-measure-actions">${btn('↑',`up:${i}:${j}`,'secondary rm-small',j===0?'disabled':'')}${btn('↓',`down:${i}:${j}`,'secondary rm-small',j===o.measurements.length-1?'disabled':'')}${btn('移除',`remove-measure:${i}:${j}`,'secondary rm-small',o.measurements.length<=2?'disabled':'')}</div></div>`).join('')}${btn('＋ 添加时间点',`add-measure:${i}`,'secondary rm-small')}${o.type==='continuous'?`<div class="rm-scale">${field('数值要求',`<span>${check(`integer-${i}`,'仅允许整数评分',o.integer)}</span>`)}${field('量表最小值（可选）',`<input type="number" data-o="${i}" data-property="min" value="${esc(o.min)}">`)}${field('量表最大值（可选）',`<input type="number" data-o="${i}" data-property="max" value="${esc(o.max)}">`)}</div>`:''}</section>`).join('')}<div class="actions" style="margin-top:24px">${btn('＋ 添加一个结局','add-outcome')}${btn('重新识别列名建议','suggest')}</div>${check('rm-mapping-confirm','已核对患者身份、各结局的数据类型、事件／等级定义，以及每项结局的时间顺序。',s.mappingConfirmed)}<div class="rm-submit"><p class="rm-help">T1–T5可以保留为分类时间标签，无需猜测具体小时。每项结局支持2至8个时点，最多8项结局。</p>${btn('设置模型 →','models','')}</div></section>`;
  }
  function modelView(){const s=state.settings;return `<section class="rm-panel"><h2>明确分析目标与模型</h2><div class="rm-note">连续结局：估计均值差；二分类：指定事件的 OR；有序分类：向更高声明等级变化的共同 OR。描述表的中位数不改变模型的比较目标。</div>${s.outcomes.map((o,i)=>`<div class="rm-outcome"><h3>${esc(o.label)} <span class="pill">${types[o.type]}</span></h3>${field('分析模型',`<select data-o="${i}" data-property="model">${option([['gee',o.type==='continuous'?'GEE · 高斯均值模型':o.type==='binary'?'GEE · 二项Logistic':'GEE · 有序累积Logistic'],['lmm',o.type==='continuous'?'线性混合模型 · 患者随机截距':o.type==='binary'?'混合效应Logistic · 患者随机截距':'有序混合模型 · 患者随机截距']],o.model)}</select>`)}<p class="rm-help">${o.type==='continuous'?(o.model==='gee'?'GEE使用患者聚类稳健标准误；工作相关结构见下方。':'LMM使用随机截距、同方差条件残差、REML和containment自由度；不是Kenward–Roger校正。'):o.model==='gee'?'GEE使用独立工作结构和患者聚类稳健方差，OR为人群平均层面的效应。':'混合模型使用患者随机截距及Laplace近似；OR和预测概率为患者条件层面的结果，不与GEE混称。'}${o.type==='ordinal'?' 有序模型假设各分界点的效应相同（比例优势），本版不会自动证明该假设成立。':''}</p></div>`).join('')}<div class="rm-outcome">${field('连续结局GEE的工作相关结构',`<select id="rm-corstr">${option([['exchangeable','Exchangeable · 相同工作相关'],['independence','Independence · 独立工作相关']],s.corstr)}</select>`)}<p class="rm-help">所有模型包含组别、分类时间及组别×时间交互。模型选择由研究目的和假设决定，不通过一次Shapiro检验自动切换。GEE与分类混合模型使用渐近推断；患者数较少时需进一步核对可靠性。</p>${check('rm-independent','患者之间相互独立；不存在需要额外建模的中心、家庭或配对分组结构。',s.independent)}${check('rm-target','已确认各结局的比较目标：连续均值差、二分类事件OR、有序更高等级OR；当前用于探索性分析。',s.meanTarget)}</div><div class="rm-warning">多重比较：同时保留各结局内、以及全部结局所有组对和时点的Holm校正。95%置信区间为逐项未校正区间，不是同时置信区间。完整数据不代表分析假设已经满足。</div><div class="rm-submit">${btn('← 返回映射','mapping')}${btn('检查数据并开始分析','run','')}</div></section>`;}
  function table(headers,rows){return `<div class="rm-table-wrap"><table class="rm-table"><thead><tr>${headers.map(h=>`<th>${esc(h)}</th>`).join('')}</tr></thead><tbody>${rows.length?rows.map(r=>`<tr>${r.map(x=>`<td>${x}</td>`).join('')}</tr>`).join(''):`<tr><td colspan="${headers.length}">没有可显示的结果</td></tr>`}</tbody></table></div>`;}
  function readSheet(sheet,sampleGroups=0){
    const ws=state.book.Sheets[sheet];if(!ws||!ws['!ref'])throw Error('工作表为空');
    const range=XLSX.utils.decode_range(ws['!ref']);range.s={r:0,c:0};
    let d=RMCore.fromRows(XLSX.utils.sheet_to_json(ws,{header:1,range,defval:null,raw:true,blankrows:true}));
    const group=['Group','group','GROUP','组别','分组','Arm','arm'].find(x=>d.names.includes(x))||d.names[0];
    if(sampleGroups===2){const groups=RMCore.groupsOf(d.data,group).slice(0,2),indexes=d.data.flatMap((r,i)=>groups.includes(String(r[group]))?[i]:[]);d={...d,data:indexes.map(i=>d.data[i]),sourceRows:indexes.map(i=>d.sourceRows[i])};}
    const id=['ID','id','patient_id','PatientID','患者编号','受试者编号'].find(x=>d.names.includes(x))||'';
    const outcomes=RMCore.suggest(d.names,[group,id]);
    if(sampleGroups)for(const o of outcomes){if(o.label==='事件'){o.type='binary';o.event='有';}if(o.label==='等级'){o.type='ordinal';o.levels='低|中|高';}}
    if(!outcomes.length)outcomes.push(newOutcome(d.names.filter(x=>![group,id].includes(x))));
    state.dataset=d;state.boxOptions=Object.create(null);state.settings={sheet,group,id,outcomes,rowPairing:false,mappingConfirmed:false,independent:false,meanTarget:false,model:'gee',corstr:'exchangeable'};
    state.step=1;invalidate();render();
  }
  function newOutcome(columns){return {label:'新结局',type:'continuous',model:'gee',summary:'median_iqr',integer:false,min:'',max:'',event:'',levels:'',measurements:columns.slice(0,2).map((column,i)=>({column,time:'T'+(i+1)}))};}
  async function load(file,groups=0){
    try{state.error='';setStatus('正在读取数据…');render();let bytes;
      if(file){if(file.size>20*1024*1024)throw Error('文件超过20MB');if(!/\.(xlsx|xls|csv|tsv)$/i.test(file.name))throw Error('请选择Excel、CSV或TSV');bytes=await file.arrayBuffer();state.fileName=file.name;}
      else{const response=await fetch(new URL('repeated-example.csv',document.baseURI));if(!response.ok)throw Error('案例文件读取失败');bytes=await response.arrayBuffer();state.fileName=(groups===2?'两组':'四组')+'重复测量_合成案例.csv';}
      state.book=XLSX.read(bytes,{type:'array',cellDates:false,raw:false,codepage:65001});state.busy='';readSheet(state.book.SheetNames[0],groups);
    }catch(e){state.busy='';state.error=e.message;render();}
  }
  async function ensureRuntime(packages){
    if(!runtimePromise)runtimePromise=(async()=>{
      setStatus('正在加载浏览器计算组件…');
      const {WebR,ChannelType}=await import(new URL('vendor/webr/webr.mjs',document.baseURI).href);
      runtime=new WebR({baseUrl:new URL('vendor/webr/',document.baseURI).href,channelType:ChannelType.PostMessage,interactive:false});
      await runtime.init();setStatus('正在准备结果读写组件…');await runtime.installPackages(['jsonlite']);loaded.add('jsonlite');
      const response=await fetch(new URL('longitudinal_engine.R?v='+VERSION,document.baseURI));if(!response.ok)throw Error('无法读取统计引擎');
      await runtime.evalRVoid(await response.text());return runtime;
    })().catch(e=>{runtimePromise=null;throw e;});
    const r=await runtimePromise;
    for(const pkg of packages){if(loaded.has(pkg))continue;setStatus('首次加载所选模型组件，请稍候…');await r.installPackages([pkg]);
      if(!await r.evalRBoolean(`requireNamespace('${pkg}',quietly=TRUE)`))throw Error('模型组件未能加载，请检查网络并重试：'+pkg);loaded.add(pkg);}
    return r;
  }
  async function run(){
    let payload;try{payload=RMCore.validate(state.dataset,state.settings);}catch(e){state.error=e.message;render();return;}
    const generation=++state.generation;state.error='';state.result=null;started=Date.now();setStatus('正在检查数据与准备模型…');render();
    clock=setInterval(()=>{const e=document.getElementById('rm-elapsed');if(e)e.textContent='已等待 '+Math.floor((Date.now()-started)/1000)+' 秒';},1000);
    let timer;
    try{
      const packages=[...new Set(Object.values(payload.outcomes).map(o=>o.model==='gee'?'geepack':o.type==='continuous'?'nlme':o.type==='binary'?'lme4':'ordinal'))];
      const r=await ensureRuntime(packages);if(generation!==state.generation)return;
      await r.FS.writeFile('/home/web_user/longitudinal_request.json',new TextEncoder().encode(JSON.stringify(payload)));
      setStatus('正在逐项拟合模型与计算比较结果…');
      const code=`local({ req<-jsonlite::fromJSON('/home/web_user/longitudinal_request.json'); d<-as.data.frame(req$data,check.names=FALSE); p<-lt_prepare_wide(d,req$group,req$outcomes,id=req$id,row_pairing_confirmed=isTRUE(req$row_pairing_confirmed),source_rows=req$source_rows,group_levels=req$group_levels); z<-lt_analyze(p,mean_score_target_confirmed=TRUE,corstr=req$corstr,alpha=req$alpha,model=req$model); z$results<-lapply(z$results,function(x){x$fit<-NULL;x$grid_covariance<-NULL;x}); z$metadata$execution<-'Browser webR'; jsonlite::toJSON(z,auto_unbox=TRUE,dataframe='rows',na='null',digits=NA) })`;
      const timeout=new Promise((_,reject)=>{timer=setTimeout(()=>reject(Error('计算超过五分钟，请减少同时分析的指标或检查模型设置后重试')),300000);});
      const result=JSON.parse(await Promise.race([r.evalRString(code),timeout]));if(generation!==state.generation)return;
      for(const name of ['descriptive','distribution','means','pairwise','joint'])if(!Array.isArray(result[name]))result[name]=result[name]?[result[name]]:[];
      state.result=result;state.snapshot=structuredClone(state.settings);state.outcome=state.snapshot.outcomes[0].label;state.step=4;state.tab='description';state.category='';
    }catch(e){if(generation===state.generation){state.error='分析未完成：'+e.message;try{await runtime?.close();}catch{}runtime=null;runtimePromise=null;loaded.clear();}}
    finally{clearTimeout(timer);clearInterval(clock);if(generation===state.generation){state.busy='';render();}}
  }
  async function cancel(){state.generation++;clearInterval(clock);try{await runtime?.close();}catch{}runtime=null;runtimePromise=null;loaded.clear();state.busy='';state.error='本次计算已取消，导入的数据仍在当前页面。';render();}
  const selectedOutcome=()=>state.snapshot.outcomes.find(o=>o.label===state.outcome)||state.snapshot.outcomes[0];
  const selectedResult=()=>state.result.results[selectedOutcome().label];
  function resultView(){
    const r=state.result,o=selectedOutcome(),current=selectedResult();
    const failed=Object.entries(r.results).filter(([,v])=>v.status==='failed');
    const outside=r.means.filter(x=>x.ci_outside_declared_scale).length;
    const tabs={boxplot:'重复测量箱线图',description:'原始数据描述',estimates:'模型估计',comparisons:'组间比较',joint:'联合检验',diagnostics:'数据与模型诊断',methods:'方法与记录'};
    return `<section class="rm-panel"><div class="rm-status-row"><h2 style="margin:0">${r.metadata.status==='partial'?'分析已返回，部分结果需核对':'统计结果已生成'}</h2><span class="pill">探索性分析</span></div><div class="rm-stats"><div class="rm-stat"><strong>${r.metadata.patients}</strong><span>独立患者</span></div><div class="rm-stat"><strong>${r.metadata.group_levels.length}</strong><span>研究组别</span></div><div class="rm-stat"><strong>${Object.keys(r.results).length-failed.length}/${Object.keys(r.results).length}</strong><span>已计算结局／纳入结局</span></div></div><div class="actions">${btn('下载 Word','word','')}${btn('下载 Excel','excel')}${btn('下载分析记录','json')}${btn('修改分析设置','models')}</div>${failed.map(([name,v])=>`<div class="rm-error">${esc(name)}：模型未能可靠估计。${esc(v.error)}。描述数据已保留。</div>`).join('')}${outside?`<div class="rm-warning">${outside}个连续结局均值置信区间超出指定量表范围，已保留并标记；请核对模型近似，不将区间截断为0。</div>`:''}<div class="rm-note">原始数据可用中位数描述；连续模型仍比较平均分。二分类与有序分类报告OR，不报告“平均等级差”。所有95%CI为逐项区间；校正P值不等于同时置信区间。</div><nav class="rm-tabs" aria-label="结果类型">${Object.entries(tabs).map(([key,label])=>`<button data-result-tab="${key}" class="${state.tab===key?'active':''}">${label}</button>`).join('')}</nav><div class="rm-result-controls">${field('选择结局',`<select id="rm-result-outcome">${option(state.snapshot.outcomes.map(x=>[x.label,x.label]),o.label)}</select>`)}<span class="pill">${types[o.type]} · ${o.model==='gee'?'GEE':'随机截距混合模型'}</span></div><div style="margin-top:20px">${state.tab==='boxplot'?boxplotView(o):state.tab==='description'?descriptionView(o):state.tab==='estimates'?estimateView(o):state.tab==='comparisons'?comparisonView(o):state.tab==='joint'?jointView(o):state.tab==='diagnostics'?diagnosticView(o):methodView(o)}</div>${['description','estimates'].includes(state.tab)&&current.status!=='failed'?chartView(o):''}</section>`;
  }
  function descriptionView(o){
    if(o.type==='continuous'){
      const summary=o.summary==='mean_sd'?'均值（SD）':'中位数（Q1，Q3）';
      return `<p class="rm-help">${summary}描述原始分布，不是模型估计值。0分是有效评分；SD、四分位范围与模型95%CI不是同一种误差。</p>`+table(['时间','组别','有效n',summary,'0分 n（%）'],state.result.descriptive.filter(x=>x.outcome===o.label).map(x=>[esc(x.time),esc(x.group),x.n,o.summary==='mean_sd'?`${number(x.mean)}（${number(x.sd)}）`:`${number(x.median)}（${number(x.q1)}，${number(x.q3)}）`,`${x.zeros}（${number(x.zero_fraction*100,1)}%）`]));
    }
    return `<p class="rm-help">按每组、每个时点的实际患者数计算类别百分比，不把同一患者的多次观测当作多名患者。</p>`+table(['时间','组别','类别','n / 总n','比例'],state.result.distribution.filter(x=>x.outcome===o.label).map(x=>[esc(x.time),esc(x.group),esc(x.category),`${x.count} / ${x.n}`,number(x.proportion*100,1)+'%']));
  }
  function estimateView(o){
    const probability=o.type!=='continuous';
    return `<p class="rm-help">${o.type==='continuous'?'模型估计均值及逐项95%CI，不是原始均值±SD。':o.model==='gee'?'GEE预测概率为人群平均层面的估计。':'混合模型概率在患者随机截距设为0时计算，是条件概率；不是积分后的总体平均概率。'}</p>`+table(['时间','组别',...(probability?['类别']:[]),probability?'估计概率':'估计均值','95%CI','提示'],state.result.means.filter(x=>x.outcome===o.label).map(x=>[esc(x.time),esc(x.group),...(probability?[esc(x.category)]:[]),probability?number(x.estimate*100,1)+'%':number(x.estimate),probability?`${number(x.ci_low*100,1)}% ～ ${number(x.ci_high*100,1)}%`:`${number(x.ci_low)} ～ ${number(x.ci_high)}`,x.ci_outside_declared_scale?'区间超量表范围':'—']));
  }
  function comparisonView(o){
    return `<p class="rm-help">方向为左组相对于右组。${o.type==='continuous'?'MD为平均分差。':o.type==='binary'?`OR对应事件“${esc(o.event)}”，不是风险比。`:'OR>1表示左组更倾向于声明顺序中的较高等级，并不自动表示更好。'}“全部结局”校正族包含 ${state.result.metadata.planned_pairwise_hypotheses} 项计划组对时点比较，区间未作多重校正。</p>`+table(['时间','左组－右组',o.type==='continuous'?'MD':'OR','95%CI','原始P','结局内Holm P','全部结局Holm P'],state.result.pairwise.filter(x=>x.outcome===o.label).map(x=>[esc(x.time),`${esc(x.group_left)} − ${esc(x.group_right)}`,number(x.effect_estimate),`${number(x.effect_ci_low)} ～ ${number(x.effect_ci_high)}`,pvalue(x.p_raw),pvalue(x.p_holm_within_outcome),pvalue(x.p_holm_all_outcomes)]));
  }
  function jointView(o){return `<p class="rm-help">交互检验回答变化模式是否不同；各时点组间差异联合检验与它是不同假设。原始P和全部联合检验Holm校正同时保留；不按交互是否显著筛掉预设的组对比较。</p>`+table(['假设','统计量','分子df','分母df','原始P','Holm P','状态'],state.result.joint.filter(x=>x.outcome===o.label).map(x=>[esc(hypothesis[x.hypothesis]||x.hypothesis),`${esc(x.distribution)} ${number(x.statistic,3)}`,x.df,x.df_denominator==null?'渐近':number(x.df_denominator,0),pvalue(x.p_value),pvalue(x.p_holm_all_joint_tests),x.status==='needs_review'?'需核对：'+esc(x.error):'已计算']));}
  function diagnosticView(o){
    const diagnostic=RMCore.diagnostics(state.dataset,state.snapshot),r=selectedResult(),m=r.metadata;
    const residual=m?.residual_diagnostics;
    return `<div class="rm-diagnostic-grid"><div class="rm-note">患者对应关系：${state.snapshot.id?'编号列 '+esc(state.snapshot.id):'按原始Excel行号，一行一人已确认'}<br>纳入${diagnostic.rows}名患者，映射测量缺失${diagnostic.missing}格。<br>相同整行评分模式的额外记录${diagnostic.duplicateProfiles}条，均保留。</div><div class="rm-note">时间为分类变量，顺序：${o.measurements.map(x=>esc(x.time)).join(' → ')}。<br>不会猜测实际时间间隔，也不自动把第一个术后时点当成治疗前基线。<br>未插补、未按评分大小删患者。</div></div>${o.type==='continuous'?table(['时间','组别','最小值','最大值','0分比例','量表上界比例'],state.result.descriptive.filter(x=>x.outcome===o.label).map(x=>[esc(x.time),esc(x.group),number(x.min),number(x.max),number(x.zero_fraction*100,1)+'%',x.upper_bound_fraction==null?'未声明上界':number(x.upper_bound_fraction*100,1)+'%'])):descriptionView(o)}${residual?`<div class="rm-warning">LMM标准化残差Shapiro–Wilk P=${pvalue(residual.shapiro_p)}；这只是筛查，不据此自动更换模型。随机截距SD=${number(residual.random_intercept_sd)}，条件残差SD=${number(residual.residual_sd)}。</div>`:''}${o.type==='ordinal'?'<div class="rm-warning">比例优势假设尚需研究者核对：各等级分界点共享同一组间效应。此处不声称已自动验证，也不将数字等级当成连续距离。</div>':''}${m?.warnings?.length?`<div class="rm-warning">${(Array.isArray(m.warnings)?m.warnings:[m.warnings]).map(esc).join('<br>')}</div>`:''}`;
  }
  function methodView(o){const m=selectedResult().metadata;return `<div class="rm-method"><p>分析为探索性比较，未纳入额外协变量。研究设计、主要结局、量表方向、比较族和模型假设仍应与研究方案一致。</p>${m?`<p><strong>${esc(o.label)}</strong><br>类型：${types[o.type]}<br>模型：${esc(m.model)}；${esc(m.family)}；${esc(m.corstr)}<br>方差：${esc(m.standard_error)}<br>推断：${esc(m.inference||'渐近Wald推断')}<br>${m.interpretation?'解释层面：'+esc(m.interpretation)+'<br>':''}${m.OR_direction?'OR方向：'+esc(m.OR_direction)+'<br>':''}软件包版本：${esc(m.package_version)}</p>`:'<p>此结局模型失败，原因已保留。请核对数据、稀疏类别和模型假设。</p>'}<p>95%CI均为逐项未校正区间。组对比较分别从原始P进行结局内和全结局Holm校正，未重复校正。小样本修正、有序比例优势假设诊断、缺失数据模型及额外聚类层级不在本版自动处理范围内。</p><p>点击“下载分析记录”保存完整配置、诊断、精确P值、软件版本及失败原因。关闭或刷新页面会清空本次内存。</p></div>`;}
  function boxOptions(o){
    const groups=state.result.metadata.group_levels.map(String);
    const opt=state.boxOptions[o.label]||(state.boxOptions[o.label]={showP:false,showMD:false,comparisonMode:'single',showTable:true,showOutliers:true,left:groups[0],right:groups[1],pField:'p_holm_within_outcome',title:o.label,xLabel:'时间点',yLabel:o.label});
    if(!groups.includes(opt.left))opt.left=groups[0];
    if(!groups.includes(opt.right)||opt.right===opt.left)opt.right=groups.find(g=>g!==opt.left);
    return opt;
  }
  function boxplotView(o){
    if(o.type!=='continuous')return '<div class="rm-note">箱线图适用于连续或近似连续结局。当前是分类结局，请使用“原始数据描述”或“模型估计”中的类别比例图；如评分需按近似连续分析，请先核对分析目标后修改指标类型。</div>';
    const opt=boxOptions(o),groups=state.result.metadata.group_levels.map(String);
    const toggle=(prop,label)=>`<label class="rm-check"><input type="checkbox" data-box-prop="${prop}" ${opt[prop]?'checked':''}><span>${label}</span></label>`;
    const select=(prop,items)=>`<select data-box-prop="${prop}">${option(items,opt[prop])}</select>`;
    const graphic=RMBoxPlot.svg({data:state.dataset.data,groupColumn:state.snapshot.group,outcome:o,groups,pairwise:state.result.pairwise,options:opt});
    return `<div class="rm-box-settings"><h3>重复测量箱线图</h3><p class="rm-help">每个时间点并排显示各组原始观测分布。箱体为Q1–Q3、中线为中位数；可独立选择是否标注P值、是否标注模型均值差MD。</p><div class="rm-box-switches">${toggle('showP','显示 P 值')}${toggle('showMD','显示均值差（MD）')}${toggle('showTable','显示下方中位数／四分位数表')}${toggle('showOutliers','显示离群值圆点')}</div><div class="rm-grid">${field('比较范围',select('comparisonMode',[['single','仅显示选定两组'],['all',`显示全部两两比较（${groups.length*(groups.length-1)/2}对／时点）`]]))}${opt.comparisonMode!=='all'?field('比较组（差值左项）',select('left',groups.map(g=>[g,g])))+field('参照组（差值右项）',select('right',groups.filter(g=>g!==opt.left).map(g=>[g,g]))):''}${field('P值标注方式',select('pField',Object.entries(RMBoxPlot.pLabels).map(([v,label])=>[v,label])))}${field('横轴名称（可含单位）',`<input data-box-prop="xLabel" maxlength="100" value="${esc(opt.xLabel??'时间点')}">`)}${field('纵轴名称（可含单位）',`<input data-box-prop="yLabel" maxlength="100" value="${esc(opt.yLabel)}">`)}${field('图标题',`<input data-box-prop="title" maxlength="100" value="${esc(opt.title)}">`)}</div><p class="rm-help">${opt.comparisonMode==='all'?`每个时点显示全部${groups.length*(groups.length-1)/2}对，共${o.measurements.length*groups.length*(groups.length-1)/2}项组对时点比较；方向按图例顺序，前组减后组。标注分层排列，可分别开启P值与MD。`:`差值方向：${esc(opt.left)} − ${esc(opt.right)}。仅标注选定的一对组。`}图中P来自当前${o.model==='lmm'?'LMM':'GEE'}模型的时点内比较，沿用完整比较族的校正；不另外进行t检验或U检验。MD是均值差，不是箱体中位数之差。</p></div><section class="rm-chart-card"><div class="actions">${btn('下载 SVG 图','svg','secondary rm-small')}${btn('下载高清 PNG','png','secondary rm-small')}</div><div class="rm-chart-scroll">${graphic}</div></section>`;
  }
  function chartConfig(){return state.tab==='boxplot'&&selectedOutcome().type==='continuous'?{outcome:selectedOutcome().label,...boxOptions(selectedOutcome())}:null;}
  function chartName(extension){return 'ClinStat_repeated_'+(state.tab==='boxplot'?'boxplot':'trajectory')+'.'+extension;}
  async function png(){
    const svg=document.getElementById('rm-chart');if(!svg)return;
    const url=URL.createObjectURL(new Blob([svg.outerHTML],{type:'image/svg+xml;charset=utf-8'}));
    try{const image=new Image();await new Promise((resolve,reject)=>{image.onload=resolve;image.onerror=()=>reject(Error('图片生成失败，请下载SVG图'));image.src=url;});
      const view=svg.viewBox.baseVal,canvas=document.createElement('canvas');const scale=Math.min(3,8192/view.width,8192/view.height,Math.sqrt(16000000/(view.width*view.height)));canvas.width=Math.floor(view.width*scale);canvas.height=Math.floor(view.height*scale);
      const context=canvas.getContext('2d');if(!context)throw Error('浏览器无法生成PNG，请使用SVG');context.fillStyle='white';context.fillRect(0,0,canvas.width,canvas.height);context.drawImage(image,0,0,canvas.width,canvas.height);
      const blob=await new Promise(resolve=>canvas.toBlob(resolve,'image/png'));if(!blob)throw Error('PNG生成失败，请使用SVG');saveBlob(blob,chartName('png'));
    }finally{URL.revokeObjectURL(url);}
  }
  function plotData(o,kind){
    if(kind==='estimates')return state.result.means.filter(x=>x.outcome===o.label&&(o.type!=='ordinal'||String(x.category)===state.category)).map(x=>({group:String(x.group),time:String(x.time),y:x.estimate,lo:x.ci_low,hi:x.ci_high}));
    if(o.type==='continuous')return state.result.descriptive.filter(x=>x.outcome===o.label).map(x=>({group:String(x.group),time:String(x.time),y:o.summary==='mean_sd'?x.mean:x.median,lo:o.summary==='mean_sd'?x.mean-x.sd:x.q1,hi:o.summary==='mean_sd'?x.mean+x.sd:x.q3}));
    const category=o.type==='binary'?String(o.event):state.category;
    return state.result.distribution.filter(x=>x.outcome===o.label&&String(x.category)===category).map(x=>({group:String(x.group),time:String(x.time),y:x.proportion,lo:x.proportion,hi:x.proportion}));
  }
  function chartView(o){
    if(o.type==='ordinal'){const levels=String(o.levels).split('|').map(x=>x.trim());if(!levels.includes(state.category))state.category=levels[0];}
    const kind=state.tab==='description'?'description':'estimates';
    const label=kind==='estimates'?(o.type==='continuous'?'模型估计均值及95%CI':'模型估计类别概率及95%CI'):(o.type==='continuous'?(o.summary==='mean_sd'?'原始均值及SD':'原始中位数及四分位范围'):'原始类别比例');
    return `<section class="rm-chart-card"><div class="rm-result-controls"><h3>${esc(o.label)} · ${label}</h3>${o.type==='ordinal'?field('图中类别',`<select id="rm-chart-category">${option(String(o.levels).split('|').map(x=>[x.trim(),x.trim()]),state.category)}</select>`):''}${btn('下载 SVG 图','svg','secondary rm-small')}</div><div class="rm-chart-scroll">${chartSVG(o,kind,label)}</div><div class="rm-chart-legend">${state.result.metadata.group_levels.map((g,i)=>`<span><i style="background:${colors[i%colors.length]}"></i>${esc(g)}</span>`).join('')}</div><p class="rm-help">横轴按已确认的分类时间顺序等间距排列，不代表实际时间间隔。${kind==='estimates'?'误差线为逐项未校正95%CI。':o.type==='continuous'?'误差线描述患者分布，不是模型置信区间。':'描述比例未绘制置信区间。'}${o.type!=='continuous'&&o.model==='lmm'&&kind==='estimates'?' 概率为随机截距设为0时的条件预测。':''}</p></section>`;
  }
  function chartSVG(o,kind,label){
    const data=plotData(o,kind);if(!data.length)return '';
    const ts=o.measurements.map(x=>x.time),gs=state.result.metadata.group_levels;
    const width=900,height=360,left=75,right=25,top=35,bottom=65;
    let min=o.type==='continuous'?Math.min(...data.map(x=>x.lo)):0,max=o.type==='continuous'?Math.max(...data.map(x=>x.hi)):1;
    if(max===min){min-=1;max+=1;}const pad=(max-min)*.12;if(o.type==='continuous'){min-=pad;max+=pad;}
    const x=t=>left+ts.indexOf(t)*(width-left-right)/Math.max(1,ts.length-1),y=v=>height-bottom-(v-min)/(max-min)*(height-top-bottom);
    let svg=`<svg id="rm-chart" class="rm-chart" xmlns="http://www.w3.org/2000/svg" viewBox="0 0 ${width} ${height}" role="img" aria-label="${esc(o.label+' '+label)}"><rect width="100%" height="100%" fill="white"/><title>${esc(o.label+' '+label)}</title>`;
    for(let i=0;i<=5;i++){const value=min+(max-min)*i/5,yy=y(value);svg+=`<line x1="${left}" x2="${width-right}" y1="${yy}" y2="${yy}" stroke="#e4ebe5"/><text x="${left-12}" y="${yy+4}" text-anchor="end" font-size="12" fill="#60776a">${o.type==='continuous'?number(value,1):number(value*100,0)+'%'}</text>`;}
    ts.forEach(t=>{svg+=`<text x="${x(t)}" y="${height-30}" text-anchor="middle" font-size="13" fill="#405e4e">${esc(t)}</text>`;});
    gs.forEach((g,i)=>{const points=data.filter(a=>a.group===g).sort((a,b)=>ts.indexOf(a.time)-ts.indexOf(b.time)),color=colors[i%colors.length],offset=(i-(gs.length-1)/2)*4;
      svg+=`<polyline points="${points.map(a=>`${x(a.time)+offset},${y(a.y)}`).join(' ')}" fill="none" stroke="${color}" stroke-width="2.5"/>`;
      for(const a of points){const xx=x(a.time)+offset;svg+=`<g stroke="${color}" fill="${color}"><line x1="${xx}" x2="${xx}" y1="${y(a.lo)}" y2="${y(a.hi)}" stroke-width="1.5"/><line x1="${xx-5}" x2="${xx+5}" y1="${y(a.lo)}" y2="${y(a.lo)}"/><line x1="${xx-5}" x2="${xx+5}" y1="${y(a.hi)}" y2="${y(a.hi)}"/><circle cx="${xx}" cy="${y(a.y)}" r="4"><title>${esc(g+' '+a.time+' '+number(a.y))}</title></circle></g>`;}
    });
    return svg+'</svg>';
  }
  function saveBlob(blob,name){const url=URL.createObjectURL(blob),a=document.createElement('a');a.href=url;a.download=name;a.click();setTimeout(()=>URL.revokeObjectURL(url),2000);}
  function exportTables(){
    const r=state.result,config=state.snapshot;
    const desc=r.descriptive.filter(x=>config.outcomes.find(o=>o.label===x.outcome)?.type==='continuous').map(x=>{const o=config.outcomes.find(o=>o.label===x.outcome);return {结局:x.outcome,时间:x.time,组别:x.group,有效n:x.n,展示:o.summary==='mean_sd'?`${number(x.mean)} (${number(x.sd)})`:`${number(x.median)} (${number(x.q1)}, ${number(x.q3)})`,口径:o.summary==='mean_sd'?'均值(SD)':'中位数(Q1,Q3)',零分比例:x.zero_fraction};});
    const means=r.means.map(x=>({结局:x.outcome,时间:x.time,组别:x.group,类别:x.category||'',量:x.quantity||'score_mean',估计:x.estimate,CI下限:x.ci_low,CI上限:x.ci_high,区间类型:x.ci_type,超量表范围:x.ci_outside_declared_scale}));
    const comparisons=r.pairwise.map(x=>({结局:x.outcome,时间:x.time,左组:x.group_left,右组:x.group_right,效应:x.effect_type,估计:x.effect_estimate,CI下限:x.effect_ci_low,CI上限:x.effect_ci_high,原始P:x.p_raw,结局内HolmP:x.p_holm_within_outcome,全部结局HolmP:x.p_holm_all_outcomes,结局内比较族:x.within_outcome_family_size,全部比较族:x.all_outcome_family_size,CI类型:x.ci_type}));
    const methods=Object.entries(r.results).flatMap(([name,v])=>Object.entries(v.metadata||{status:v.status,error:v.error}).map(([field,value])=>({结局:name,字段:field,内容:typeof value==='object'?JSON.stringify(value):String(value??'')})));
    return {描述表:desc,类别分布:r.distribution,模型估计:means,组间比较:comparisons,联合检验:r.joint,数值审查:r.descriptive,方法日志:methods,配置:config.outcomes.map(o=>({结局:o.label,类型:o.type,模型:o.model,描述口径:o.summary,事件值:o.event,等级顺序:o.levels,时间映射:JSON.stringify(o.measurements)})),运行记录:Object.entries(r.metadata).map(([field,value])=>({字段:field,内容:typeof value==='object'?JSON.stringify(value):String(value??'')}))};
  }
  function excel(){const book=XLSX.utils.book_new();for(const [name,rows] of Object.entries(exportTables())){const sheet=XLSX.utils.json_to_sheet(rows);sheet['!cols']=Array.from({length:Object.keys(rows[0]||{}).length},()=>({wch:22}));XLSX.utils.book_append_sheet(book,sheet,name);}saveBlob(new Blob([XLSX.write(book,{type:'array',bookType:'xlsx'})],{type:'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'}),'ClinStat_repeated_results.xlsx');}
  async function word(){
    const {Document,Packer,Paragraph,Table,TableRow,TableCell,TextRun,WidthType,PageOrientation,TableLayoutType}=docx;
    const tables=exportTables(),children=[new Paragraph({text:'重复测量结果 · 探索性分析',heading:'Heading1'}),new Paragraph({text:`患者${state.result.metadata.patients}名；${state.result.metadata.group_levels.length}组。描述分布与模型估计分别报告。所有CI均为逐项未校正95%区间。`}),new Paragraph({text:'连续结局报告均值差；二分类报告指定事件OR；有序分类报告更高声明等级OR。混合分类模型的概率为随机截距设为0的条件预测。模型假设、稀疏类别及小样本可靠性仍需核对。'})];
    for(const name of ['描述表','类别分布','模型估计','组间比较','联合检验']){
      const source=tables[name];if(!source.length)continue;
      let keys=Object.keys(source[0]);if(name==='组间比较')keys=['结局','时间','左组','右组','效应','估计','CI下限','CI上限','全部结局HolmP'];
      if(name==='模型估计')keys=['结局','时间','组别','类别','量','估计','CI下限','CI上限'];
      if(name==='联合检验')keys=['outcome','hypothesis','distribution','statistic','df','p_value','p_holm_all_joint_tests','status'];
      const width=14000,widths=keys.map(()=>Math.floor(width/keys.length));
      const rows=[keys,...source.map(row=>keys.map(k=>{const v=row[k];return typeof v==='number'?(k.toLowerCase().includes('p')&&!['proportion','upper_bound_fraction'].includes(k)?pvalue(v):number(v,3)):String(v??'—');}))];
      children.push(new Paragraph({text:name,heading:'Heading2'}),new Table({width:{size:width,type:WidthType.DXA},columnWidths:widths,layout:TableLayoutType.FIXED,rows:rows.map((row,i)=>new TableRow({tableHeader:i===0,children:row.map((text,j)=>new TableCell({width:{size:widths[j],type:WidthType.DXA},children:[new Paragraph({children:[new TextRun({text:String(text),bold:i===0,size:17,font:'Arial'})]})]}))}))}));
    }
    children.push(new Paragraph({text:'完整配置、精确数值P、软件版本、模型警告及失败原因请同时保存Excel和JSON分析记录。'}));
    const doc=new Document({sections:[{properties:{page:{size:{orientation:PageOrientation.LANDSCAPE},margin:{top:600,bottom:600,left:500,right:500}}},children}]});
    saveBlob(await Packer.toBlob(doc),'ClinStat_repeated_results.docx');
  }
  function go(step){
    if(busy())return;
    if(step>1&&!state.dataset)return;
    if(step===3&&!state.settings.mappingConfirmed){state.error='请先核对指标映射并勾选确认。';state.step=2;render();return;}
    if(step===4&&!state.result)return;
    state.step=step;state.error='';render();window.scrollTo({top:0,behavior:'smooth'});
  }
  root.addEventListener('click',async e=>{
    const step=e.target.closest('[data-step]');if(step){go(Number(step.dataset.step));return;}
    const tab=e.target.closest('[data-result-tab]');if(tab){state.tab=tab.dataset.resultTab;state.category='';render();return;}
    const target=e.target.closest('[data-action]');if(!target)return;
    const [action,a,b]=target.dataset.action.split(':');
    try{
      if(action==='choose'){document.getElementById('rm-file').click();return;}
      if(action==='sample4'||action==='sample2'){await load(null,action==='sample2'?2:4);return;}
      if(action==='cancel'){await cancel();return;}
      if(busy())return;
      if(action==='mapping'){go(2);return;}if(action==='models'){go(3);return;}
      if(action==='run'){await run();return;}
      if(action==='restart'){state.dataset=null;state.book=null;state.settings=null;state.fileName='';state.step=1;invalidate();render();return;}
      if(action==='excel'){excel();return;}if(action==='word'){await word();return;}
      if(action==='json'){saveBlob(new Blob([JSON.stringify({result:state.result,config:state.snapshot,chart:chartConfig()},null,2)],{type:'application/json'}),'ClinStat_repeated_analysis.json');return;}
      if(action==='svg'){const svg=document.getElementById('rm-chart');if(svg)saveBlob(new Blob([svg.outerHTML],{type:'image/svg+xml;charset=utf-8'}),chartName('svg'));return;}
      if(action==='png'){await png();return;}
      const s=state.settings;if(!s)return;
      if(action==='suggest'){s.outcomes=RMCore.suggest(state.dataset.names,[s.group,s.id]);}
      if(action==='add-outcome'){
        if(s.outcomes.length>=8)throw Error('每次最多8项结局');
        const used=s.outcomes.flatMap(o=>o.measurements.map(m=>m.column));
        const columns=state.dataset.names.filter(x=>![s.group,s.id,...used].includes(x));
        if(columns.length<2)throw Error('至少需要两列未分配的测量数据');
        const o=newOutcome(columns);o.label='新结局 '+(s.outcomes.length+1);s.outcomes.push(o);
      }
      if(action==='remove-outcome')s.outcomes.splice(Number(a),1);
      if(action==='add-measure'){
        const o=s.outcomes[Number(a)];if(o.measurements.length>=8)throw Error('每项结局最多8个时点');
        const used=s.outcomes.flatMap(x=>x.measurements.map(m=>m.column)),column=state.dataset.names.find(x=>![s.group,s.id,...used].includes(x));
        if(!column)throw Error('没有未分配的数据列');o.measurements.push({column,time:'T'+(o.measurements.length+1)});
      }
      if(action==='remove-measure')s.outcomes[Number(a)].measurements.splice(Number(b),1);
      if(action==='up'||action==='down'){
        const ms=s.outcomes[Number(a)].measurements,i=Number(b),j=i+(action==='up'?-1:1);
        if(j>=0&&j<ms.length)[ms[i],ms[j]]=[ms[j],ms[i]];
      }
      s.mappingConfirmed=false;invalidate();render();
    }catch(error){state.error=error.message;render();}
  });
  root.addEventListener('change',async e=>{
    const el=e.target;
    try{
      if(el.id==='rm-file'){if(el.files[0])await load(el.files[0]);return;}
      if(el.id==='rm-result-outcome'){state.outcome=el.value;state.category='';render();return;}
      if(el.id==='rm-chart-category'){state.category=el.value;render();return;}
      if(el.dataset.boxProp&&state.result){const opt=boxOptions(selectedOutcome());opt[el.dataset.boxProp]=el.type==='checkbox'?el.checked:el.value;render();return;}
      const s=state.settings;if(!s||busy())return;
      if(el.id==='rm-sheet'){readSheet(el.value);return;}
      if(el.id==='rm-group'){s.group=el.value;s.mappingConfirmed=false;invalidate();render();return;}
      if(el.id==='rm-id'){s.id=el.value;s.mappingConfirmed=false;s.rowPairing=false;invalidate();render();return;}
      if(el.id==='rm-row-pair'){s.rowPairing=el.checked;invalidate();return;}
      if(el.id==='rm-mapping-confirm'){s.mappingConfirmed=el.checked;return;}
      if(el.id==='rm-independent'){s.independent=el.checked;invalidate();return;}
      if(el.id==='rm-target'){s.meanTarget=el.checked;invalidate();return;}
      if(el.id==='rm-corstr'){s.corstr=el.value;invalidate();return;}
      if(el.id.startsWith('integer-')){s.outcomes[Number(el.id.split('-')[1])].integer=el.checked;s.mappingConfirmed=false;invalidate();return;}
      if(el.dataset.property){
        const o=s.outcomes[Number(el.dataset.o)];o[el.dataset.property]=el.value;
        if(el.dataset.property!=='model')s.mappingConfirmed=false;
        invalidate();render();return;
      }
      if(el.dataset.measure){const o=s.outcomes[Number(el.dataset.o)];o.measurements[Number(el.dataset.m)][el.dataset.measure]=el.value;s.mappingConfirmed=false;invalidate();render();}
    }catch(error){state.error=error.message;render();}
  });
  render();
})();
