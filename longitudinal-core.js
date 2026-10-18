/* Pure import/mapping helpers. No networking or persistence. */
(function(root){
  'use strict';
  const blank=v=>v==null || typeof v==='string'&&!v.trim();
  const groupsOf=(data,column)=>[...new Set(data.map(r=>r[column]).filter(v=>!blank(v)).map(String))].sort((a,b)=>a.localeCompare(b,'zh-CN',{numeric:true}));
  function fromRows(rows){
    if(!rows.length)throw Error('工作表为空');
    const width=Math.max(...rows.map(r=>r.length));
    const headers=Array.from({length:width},(_,i)=>blank(rows[0][i])?'':String(rows[0][i]));
    const keep=headers.flatMap((name,i)=>!name.trim()&&rows.slice(1).every(r=>blank(r[i]))?[]:[i]);
    const names=keep.map(i=>headers[i]);
    if(names.some(x=>!x.trim())||new Set(names).size!==names.length)throw Error('第一行列名须完整且不重复');
    if(names.length>100)throw Error('每次最多支持100列');
    const data=[],sourceRows=[];
    rows.slice(1).forEach((row,i)=>{if(keep.every(j=>blank(row[j])))return;
      sourceRows.push(i+2);data.push(Object.fromEntries(keep.map((j,k)=>[names[k],blank(row[j])?null:row[j]])));
    });
    if(!data.length)throw Error('没有数据记录');
    if(data.length>5000)throw Error('重复测量试用版每次最多支持5000名患者');
    return {names,data,sourceRows,droppedColumns:headers.length-names.length};
  }
  function suggest(names,excluded=[]){
    const found=new Map();
    names.filter(n=>!excluded.includes(n)).forEach(column=>{
      const m=column.match(/^(.+?)[（(]([^）)]+)[）)]$/)||column.match(/^(.+?)[_\s]+((?:post)?\d+(?:\.\d+)?\s*(?:h|d|天|小时)|T\d+)$/i);
      if(!m)return;
      const label=m[1].trim(),time=m[2].replace(/^post/i,'').trim();
      if(!found.has(label))found.set(label,[]);found.get(label).push({column,time});
    });
    return [...found].filter(([,v])=>v.length>=2).slice(0,8).map(([label,measurements])=>({label,measurements,
      type:'continuous',model:'gee',event:'',levels:'',summary:'median_iqr',integer:false,min:'',max:''}));
  }
  function validate(dataset,settings){
    if(!dataset||!dataset.data.length)throw Error('请先导入数据');
    const {data,names}=dataset;
    if(!names.includes(settings.group))throw Error('请选择分组列');
    if(data.some(r=>blank(r[settings.group])))throw Error('分组列有缺失，请先核对；本模块不静默排除患者');
    const groups=groupsOf(data,settings.group);
    if(groups.length<2||groups.length>10)throw Error('分组列须包含2至10组；请勿选择患者编号列');
    if(settings.id){
      if(!names.includes(settings.id)||settings.id===settings.group)throw Error('请正确选择患者编号列');
      const ids=data.map(r=>blank(r[settings.id])?'':String(r[settings.id]));
      if(ids.some(x=>!x)||new Set(ids).size!==ids.length)throw Error('患者编号须在全表唯一且无缺失；每位患者只占一行');
    }else if(!settings.rowPairing)throw Error('无编号列时，请确认一行一位患者且各时间点评分一一对应');
    if(!settings.outcomes.length||settings.outcomes.length>8)throw Error('请配置1至8项结局');
    const labels=settings.outcomes.map(o=>o.label.trim());
    if(labels.some(x=>!x)||new Set(labels).size!==labels.length)throw Error('结局名称须完整且不重复');
    const used=new Set(),mapping=Object.create(null);
    for(const outcome of settings.outcomes){
      const ms=outcome.measurements;
      const type=outcome.type||'continuous';
      if(!['continuous','binary','ordinal'].includes(type))throw Error('请选择正确的结局类型');
      if(ms.length<2||ms.length>8)throw Error(`${outcome.label}：每项结局支持2至8个时间点`);
      if(ms.some(m=>!m.time.trim())||new Set(ms.map(m=>m.time.trim())).size!==ms.length)throw Error(`${outcome.label}：时间标签不能为空或重复`);
      let bounds=null;
      if(outcome.min!==''||outcome.max!==''){
        if(outcome.min===''||outcome.max==='')throw Error(`${outcome.label}：请同时填写量表最小值和最大值，或都留空`);
        bounds=[Number(outcome.min),Number(outcome.max)];
        if(!bounds.every(Number.isFinite)||bounds[0]>=bounds[1])throw Error(`${outcome.label}：量表范围无效`);
      }
      for(const m of ms){
        if(!names.includes(m.column)||[settings.group,settings.id].includes(m.column))throw Error(`${outcome.label}：请选择正确的评分列`);
        if(used.has(m.column))throw Error(`${m.column} 被重复分配，请核对`);used.add(m.column);
        for(let i=0;i<data.length;i++){
          const v=data[i][m.column],where=`${m.column}，Excel第${dataset.sourceRows[i]}行`;
          if(blank(v))throw Error(`${where}存在缺失；本版要求完整数据，不会补零或删除患者`);
          if(type==='continuous'&&(typeof v!=='number'||!Number.isFinite(v)))throw Error(`${where}不是有效数值，请核对`);
          if(type==='continuous'&&bounds&&(v<bounds[0]||v>bounds[1]))throw Error(`${where}超出指定量表范围`);
          if(type==='continuous'&&outcome.integer&&!Number.isInteger(v))throw Error(`${where}不是整数评分`);
        }
      }
      const observed=[...new Set(data.flatMap(r=>ms.map(m=>String(r[m.column]))))];
      let category={};
      if(type==='binary'){
        if(observed.length!==2||!observed.includes(String(outcome.event)))throw Error(`${outcome.label}：须正好两类，并明确选择事件值`);
        category={event:String(outcome.event)};
      }
      if(type==='ordinal'){
        const levels=String(outcome.levels||'').split('|').map(x=>x.trim());
        if(levels.length<2||levels.some(x=>!x)||new Set(levels).size!==levels.length||
           levels.length!==observed.length||!observed.every(x=>levels.includes(x)))throw Error(`${outcome.label}：请用竖线填写从低到高的全部实际等级，例如 低|中|高`);
        if(levels.length>12||data.length*ms.length*(levels.length-1)*(groups.length*ms.length+levels.length)>3000000)
          throw Error(`${outcome.label}：有序模型规模较大，请减少患者数、等级或时间点后分别分析`);
        category={levels};
      }
      mapping[outcome.label.trim()]={columns:ms.map(m=>m.column),times:ms.map(m=>m.time.trim()),
        type,model:outcome.model||settings.model||'gee',integer:outcome.integer,...category,...(type==='continuous'&&bounds?{bounds}:{})};
    }
    if(!settings.mappingConfirmed)throw Error('请确认患者对应关系、指标映射和时间顺序');
    if(!settings.independent)throw Error('请确认患者彼此独立，无需额外中心、家庭等聚类层级');
    if(!settings.meanTarget)throw Error('请确认连续结局比较平均评分、二分类比较事件优势、有序结局比较更高等级优势');
    if(!['gee','lmm'].includes(settings.model))throw Error('请选择模型');
    return {data,group:settings.group,id:settings.id||null,row_pairing_confirmed:!!settings.rowPairing,
      source_rows:dataset.sourceRows,group_levels:groups,outcomes:mapping,model:settings.model,
      corstr:settings.corstr,mean_score_target_confirmed:true,alpha:.05};
  }
  function diagnostics(dataset,settings){
    const data=dataset.data,columns=settings.outcomes.flatMap(o=>o.measurements.map(m=>m.column));
    return {rows:data.length,groups:groupsOf(data,settings.group).map(g=>({group:g,n:data.filter(r=>String(r[settings.group])===g).length})),
      missing:data.reduce((n,r)=>n+columns.filter(c=>blank(r[c])).length,0),
      duplicateProfiles:data.length-new Set(data.map(r=>JSON.stringify([r[settings.group],...columns.map(c=>r[c])]))).size};
  }
  const api={fromRows,suggest,validate,diagnostics,groupsOf};
  if(typeof module!=='undefined'&&module.exports)module.exports=api;else root.RMCore=api;
})(typeof globalThis!=='undefined'?globalThis:this);
