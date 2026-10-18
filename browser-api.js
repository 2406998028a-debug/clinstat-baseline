/* Browser-only transport. No files, values or analysis results are sent to a server. */
const BrowserAPI=(()=>{
  const projects=new Map(),jobs=new Map();let runtimePromise=null,active=false;
  const id=()=>crypto.randomUUID().replaceAll('-','');
  const clone=x=>structuredClone(x);
  const fail=(message,status=400)=>{const e=new Error(message);e.status=status;throw e;};
  const unique=x=>[...new Set(x.filter(v=>v!==null&&v!==''))].map(String);
  const levels=x=>{const a=unique(x);return a.every(v=>Number.isFinite(Number(v)))?a.sort((a,b)=>Number(a)-Number(b)):a.sort();};
  function parseWorkbook(bytes,name){
    if(!/\.(xlsx|xls|csv|tsv)$/i.test(name))fail('请选择 Excel、CSV 或 TSV 文件');
    const book=XLSX.read(bytes,{type:'array',cellDates:false,raw:false,codepage:65001});
    return book;
  }
  function inspect(p,options={}){
    const sheet=options.sheet||p.book.SheetNames[0];if(!p.book.Sheets[sheet])fail('工作表不存在');
    const ws=p.book.Sheets[sheet];if(!ws['!ref'])fail('数据表为空');
    const range=XLSX.utils.decode_range(ws['!ref']);range.s={r:0,c:0};
    const aoa=XLSX.utils.sheet_to_json(ws,{header:1,range,defval:null,raw:true,blankrows:false});
    if(aoa.length<2)fail('至少需要变量名称和一行数据');
    const rawNames=aoa[0].map(v=>v==null?'':String(v)),dataRows=aoa.slice(1);
    const keep=[],dropped=[];
    rawNames.forEach((name,i)=>{if(!name.trim()&&dataRows.every(r=>r[i]==null||String(r[i]).trim()===''))dropped.push(i+1);else keep.push(i);});
    const names=keep.map(i=>rawNames[i]);
    if(names.some(x=>!x.trim())||new Set(names).size!==names.length)fail('列名需要完整且不重复；空白排版列可自动忽略');
    if(names.length>100)fail('打开即用版每次最多支持 100 列，请拆分数据后重试');
    const data=dataRows.map(row=>Object.fromEntries(keep.map((i,j)=>[names[j],row[i]==null||typeof row[i]==='string'&&!row[i].trim()?null:row[i]])));
    if(data.length>10000)fail('打开即用版每次最多支持 10000 条记录，请拆分后重试');
    const group=options.group||['group','Group','GROUP','分组','组别','arm','Arm','treatment'].find(v=>names.includes(v))||names[0];
    if(!names.includes(group))fail('分组变量不存在');
    const columns=names.map(name=>{const x=data.map(r=>r[name]),u=levels(x);return {name,n:x.filter(v=>v!==null).length,missing:x.filter(v=>v===null).length,unique:u.length,numeric:x.filter(v=>v!==null).every(v=>typeof v==='number'),levels:u.slice(0,20)};});
    const groups=levels(data.map(r=>r[group])).map(label=>({label,n:data.filter(r=>r[group]!==null&&String(r[group])===label).length}));
    if(groups.length<2)fail('分组列至少需要两个有观测值的组别，请重新选择分组列');
    if(groups.length>20)fail('分组数超过 20，请核对是否误选了编号列');
    const config=columns.filter(c=>c.name!==group).map(c=>({variable:c.name,type:!c.numeric||c.unique<=10?'categorical':'continuous',confirmed:false,label:c.name,method:'auto',summary:'auto',digits:1,levels:''}));
    if(p.synthetic)for(const c of config){if(c.variable==='ASA'){c.type='ordinal';c.levels='1|2';}if(c.variable==='value'||c.variable==='连续指标'){c.type='continuous';}}
    const duplicate_rows=data.length-new Set(data.map(r=>JSON.stringify(r))).size;
    const issues=[];
    if(dropped.length)issues.push({kind:'empty_layout_columns',message:`已忽略 ${dropped.length} 个无列名且整列为空的排版列，原文件未修改`});
    if(duplicate_rows)issues.push({kind:'duplicate',message:`${duplicate_rows} 条完全重复记录，已保留，请核对`});
    const missingGroup=data.filter(r=>r[group]===null).length;
    if(missingGroup)issues.push({kind:'group_missing',message:`${missingGroup} 条记录缺少分组，分析时排除并记录`});
    for(const c of config){const missing=groups.filter(g=>data.filter(r=>r[group]!==null&&String(r[group])===g.label).every(r=>r[c.variable]===null));if(missing.length)issues.push({kind:'coverage',variable:c.variable,message:`${c.variable}：第 ${missing.map(x=>x.label).join('、')} 组无观测值，不生成不完整的全组比较 P 值`});}
    p.data=data;p.inspection={sheet,sheets:p.book.SheetNames,group,rows:data.length,column_count:names.length,columns,groups,config,missing_cells:columns.reduce((s,c)=>s+c.missing,0),duplicate_rows,issues,import_notes:{raw_column_count:rawNames.length,dropped_unnamed_empty_columns:dropped},preview:data.slice(0,6)};
    p.settings=null;p.last_job=null;return clone(p.inspection);
  }
  async function runtime(){
    if(!runtimePromise)runtimePromise=(async()=>{
      const {WebR,ChannelType}=await import(new URL('vendor/webr/webr.mjs',document.baseURI).href);
      const r=new WebR({baseUrl:new URL('vendor/webr/',document.baseURI).href,channelType:ChannelType.PostMessage,interactive:false});
      await r.init();await r.installPackages(['jsonlite']);
      const code=await (await fetch(new URL('baseline_engine.R',document.baseURI))).text();
      await r.evalRVoid(code);return r;
    })().catch(e=>{runtimePromise=null;throw new Error('统计引擎未能加载，请检查网络后重试。'+e.message);});
    return runtimePromise;
  }
  async function analyze(p,settings){
    if(active)fail('已有计算进行中，请稍候');active=true;
    let timer;
    try{
      const r=await runtime();
      const payload={data:p.data,config:settings.config,group:settings.group,mode:settings.mode,normality_policy:settings.normality_policy,independent:settings.independent,posthoc:settings.posthoc,p_adjust:settings.p_adjust,import_notes:p.inspection.import_notes};
      await r.FS.writeFile('/home/web_user/request.json',new TextEncoder().encode(JSON.stringify(payload)));
      const compute=r.evalRString(`local({req <- jsonlite::fromJSON('/home/web_user/request.json'); d <- as.data.frame(req$data,check.names=FALSE); attr(d,'import_notes') <- req$import_notes; z <- baseline_analyze(d,req$group,req$config,mode=req$mode,normality_policy=req$normality_policy,independent=isTRUE(req$independent),posthoc=isTRUE(req$posthoc),p_adjust=req$p_adjust); z$metadata$execution <- 'Browser webR 0.6.0'; jsonlite::toJSON(unclass(z),auto_unbox=TRUE,na='null',dataframe='rows',digits=NA)})`);
      const timeout=new Promise((_,reject)=>{timer=setTimeout(async()=>{try{await r.close();}catch{}runtimePromise=null;reject(new Error('计算超过三分钟，请减少变量或调整检验方法后重试'));},180000);});
      const result=JSON.parse(await Promise.race([compute,timeout]));
      for(const name of ['table','methods','normality','continuous_summary','posthoc','config','groups'])if(!Array.isArray(result[name]))result[name]=result[name]? [result[name]]:[];
      p.settings=clone(settings);return result;
    }finally{clearTimeout(timer);active=false;}
  }
  function start(p,kind,settings={}){
    const jid=id(),job={id:jid,project_id:p.id,kind,status:'queued',message:'',settings:clone(settings)};jobs.set(jid,job);
    Promise.resolve().then(async()=>{job.status='running';try{job.result=kind==='inspect'?inspect(p,settings):await analyze(p,settings);job.status='complete';if(kind==='analyze')p.last_job=jid;}catch(e){job.status='failed';job.message=e.message||'计算未完成，请检查变量设置';}});
    return {job_id:jid,project_id:p.id};
  }
  async function request(path,body){
    if(path==='/api/health')return {ok:true,local:true,r_available:typeof WebAssembly!=='undefined',sample_available:true};
    if(path==='/api/me')return {name:'当前页面',email:''};
    if(path==='/api/logout'){if(active)fail('请等待计算完成后再重新开始');if(runtimePromise){try{await (await runtimePromise).close();}catch{}runtimePromise=null;}projects.clear();jobs.clear();return {ok:true};}
    if(path==='/api/projects')return {projects:[...projects.values()].map(({book,data,...p})=>clone(p))};
    if(path==='/api/projects/upload'||path==='/api/projects/sample'){
      let bytes,name,synthetic=path.endsWith('/sample');
      if(synthetic){bytes=new Uint8Array(await (await fetch(new URL('example.csv',document.baseURI))).arrayBuffer());name='两组功能验证_模拟数据.csv';}
      else {bytes=Uint8Array.from(atob(body.content),c=>c.charCodeAt(0));name=body.filename;}
      const p={id:id(),title:name.replace(/\.[^.]+$/,''),source_name:name,created:Date.now()/1000,is_sample:synthetic,synthetic,book:parseWorkbook(bytes,name),inspection:null,settings:null,last_job:null};projects.set(p.id,p);return start(p,'inspect');
    }
    const job=path.match(/^\/api\/jobs\/([a-f0-9]+)$/);if(job){if(!jobs.has(job[1]))fail('本次分析记录已清除',404);return clone(jobs.get(job[1]));}
    const m=path.match(/^\/api\/projects\/([a-f0-9]+)(?:\/(inspect|analyze|delete))?$/);if(m){
      const p=projects.get(m[1]);if(!p)fail('项目不存在',404);
      if(!m[2]){const {book,data,...info}=p;return clone(info);}
      if(m[2]==='delete'){for(const [key,j] of jobs)if(j.project_id===p.id){if(j.status==='running')fail('请等待任务完成');jobs.delete(key);}projects.delete(p.id);return {ok:true};}
      if(m[2]==='analyze'&&(!body.confirmed||!body.config?.length))fail('请先确认变量');
      return start(p,m[2],body);
    }
    fail('不支持的操作',404);
  }
  function exportRows(r){return r.table.map(row=>{const out={变量:row.label,'统计量 / 分类':({'Mean (SD)':'均值（SD）','Median (Q1, Q3)':'中位数（Q1，Q3）','Nonmissing n':'有效 n','Missing n':'缺失 n'}[row.level]||row.level),[`总体 (N=${r.metadata.analyzed_records})`]:row.G0};for(const g of r.groups)out[`第 ${g.group} 组 (N=${g.n})`]=row[g.column];out['P 值']=row.p_value;out['状态']=row.status;return out;});}
  function excel(r){const book=XLSX.utils.book_new();const add=(name,rows)=>XLSX.utils.book_append_sheet(book,XLSX.utils.json_to_sheet(rows),name);add('Table1',exportRows(r));add('Methods_log',r.methods);add('Config',r.config);add('Diagnostics',r.normality);add('Continuous_numeric',r.continuous_summary);if(r.posthoc.length)add('Posthoc',r.posthoc);add('Run',Object.entries(r.metadata).map(([key,value])=>({key,value:typeof value==='object'?JSON.stringify(value):value})));return XLSX.write(book,{bookType:'xlsx',type:'array'});}
  async function word(r){
    const {Document,Packer,Paragraph,Table,TableRow,TableCell,TextRun,PageOrientation,WidthType,TableLayoutType}=docx;
    const rows=exportRows(r),columns=Object.keys(rows[0]),groupCount=r.groups.length;
    const landscape=groupCount>2;const width=landscape?15000:10000;const widths=[2200,1800,...Array(columns.length-4).fill(Math.floor((width-5400)/(columns.length-4))),700,700];
    const tableRows=[columns,...rows.map(row=>columns.map(k=>String(row[k]??'')))].map((row,i)=>new TableRow({tableHeader:i===0,children:row.map((text,j)=>new TableCell({width:{size:widths[j],type:WidthType.DXA},children:[new Paragraph({children:[new TextRun({text,bold:i===0,size:16,font:'Arial'})]})]}))}));
    const document=new Document({sections:[{properties:{page:{size:{orientation:landscape?PageOrientation.LANDSCAPE:PageOrientation.PORTRAIT},margin:{top:600,bottom:600,left:500,right:500}}},children:[new Paragraph({text:'基线特征表',heading:'Heading1'}),new Paragraph({text:`${r.metadata.analyzed_records} 条记录 · ${groupCount} 组；空白作为缺失，不插补。`}),new Table({rows:tableRows,width:{size:width,type:WidthType.DXA},columnWidths:widths,layout:TableLayoutType.FIXED}),new Paragraph({text:'分类百分比分母为各列非缺失记录数；自动展示根据组内分布筛查，具体检验及诊断请同时保存 Excel 分析记录。P 值为逐变量探索性结果。'}),new Paragraph({text:'运行状态：'+r.metadata.status})]}]});
    return Packer.toBlob(document);
  }
  async function download(path){const m=path.match(/^\/api\/jobs\/([a-f0-9]+)\/download\/(xlsx|docx|json)$/);if(!m||!jobs.has(m[1]))fail('结果已清除，请重新计算');const r=jobs.get(m[1]).result;if(!r)fail('结果尚未生成');let blob;if(m[2]==='xlsx')blob=new Blob([excel(r)],{type:'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet'});else if(m[2]==='docx')blob=await word(r);else blob=new Blob([JSON.stringify(r,null,2)],{type:'application/json'});const url=URL.createObjectURL(blob),a=document.createElement('a');a.href=url;a.download='Table1.'+m[2];a.click();setTimeout(()=>URL.revokeObjectURL(url),1000);}
  return {request,download,inspect,parseWorkbook,analyze,excel,word};
})();
