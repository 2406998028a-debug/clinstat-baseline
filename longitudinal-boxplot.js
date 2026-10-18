/* Browser-local descriptive boxplots. No fitting, networking or persistence. */
(function(root){
  'use strict';
  const palette=['#3f819e','#66558f','#278569','#b97d32','#a65372','#627f3e','#427986','#995e42','#6973a2','#947632'];
  const esc=v=>String(v??'').replace(/[&<>"']/g,c=>({'&':'&amp;','<':'&lt;','>':'&gt;','"':'&quot;',"'":'&#39;'}[c]));
  const finite=v=>typeof v==='number'&&Number.isFinite(v);
  const num=v=>!finite(v)?'未估计':Math.abs(v)>0&&Math.abs(v)<.01?v.toExponential(1):v.toFixed(2);
  const pvalue=v=>!finite(v)?'未估计':v<.001?'<0.001':'='+v.toFixed(3);
  const pLabels={p_holm_within_outcome:'结局内Holm校正',p_holm_all_outcomes:'全部结局Holm校正',p_raw:'原始P，未校正'};
  function quantile(sorted,p){const h=(sorted.length-1)*p,i=Math.floor(h);return sorted[i]+(h-i)*(sorted[Math.min(i+1,sorted.length-1)]-sorted[i]);}
  function summarize(values){
    if(!values.length||values.some(v=>!finite(v)))throw Error('箱线图需要完整、有限的数值观测');
    const a=[...values].sort((x,y)=>x-y),q1=quantile(a,.25),median=quantile(a,.5),q3=quantile(a,.75),iqr=q3-q1;
    const inside=a.filter(v=>v>=q1-1.5*iqr&&v<=q3+1.5*iqr);
    return {n:a.length,q1,median,q3,min:a[0],max:a[a.length-1],whiskerLow:inside[0],whiskerHigh:inside[inside.length-1],outliers:a.filter(v=>v<q1-1.5*iqr||v>q3+1.5*iqr)};
  }
  function cells(data,groupColumn,outcome,groups){
    if(outcome.type!=='continuous')throw Error('箱线图仅用于连续或近似连续结局');
    return outcome.measurements.flatMap(m=>groups.map(group=>({group:String(group),time:m.time,column:m.column,...summarize(data.filter(r=>String(r[groupColumn])===String(group)).map(r=>r[m.column]))})));
  }
  function comparison(rows,outcome,time,left,right,pField){
    if(!Object.hasOwn(pLabels,pField))throw Error('P值标注方式无效');
    const row=rows.find(r=>r.outcome===outcome&&r.time===time&&r.effect_type==='mean_difference'&&
      ((String(r.group_left)===left&&String(r.group_right)===right)||(String(r.group_left)===right&&String(r.group_right)===left)));
    if(!row)return {estimate:null,p:null};
    const sign=String(row.group_left)===left?1:-1;
    return {estimate:finite(row.effect_estimate)?sign*row.effect_estimate:null,p:finite(row[pField])?row[pField]:null};
  }
  function comparisonPairs(groups,options={}){
    const gs=groups.map(String),mode=options.comparisonMode||'single';
    if(mode==='all')return gs.flatMap((left,i)=>gs.slice(i+1).map(right=>[left,right]));
    if(mode!=='single')throw Error('比较范围无效');
    const left=String(options.left??gs[0]),right=String(options.right??gs[1]);
    if(left===right||!gs.includes(left)||!gs.includes(right))throw Error('请选择不同的两组进行标注');
    return [[left,right]];
  }
  function annotationLabels(c,left,right,showP,showMD,includePair){
    const labels=[];let prefix=includePair?`${left} − ${right}: `:'';
    if(showMD){labels.push(prefix+'MD='+num(c.estimate));prefix='';}
    if(showP)labels.push(prefix+(finite(c.p)?'P'+pvalue(c.p):'P：未估计'));
    return labels;
  }
  // Character-width wrapping keeps long Chinese and English labels inside SVG exports.
  function lines(text,maxWidth,fontSize){
    const result=[];let line='',width=0;
    for(const ch of String(text)){const w=(ch.charCodeAt(0)>255?1:.58)*fontSize;
      if(line&&width+w>maxWidth){result.push(line);line='';width=0;}line+=ch;width+=w;
    }
    result.push(line);return result;
  }
  function svg({data,groupColumn,outcome,groups,pairwise=[],options={}}){
    const stats=cells(data,groupColumn,outcome,groups),times=outcome.measurements.map(m=>m.time);
    const showP=Boolean(options.showP),showMD=Boolean(options.showMD),annotate=showP||showMD;
    const showTable=options.showTable!==false,showOutliers=options.showOutliers!==false;
    const left=String(options.left??groups[0]),right=String(options.right??groups[1]),pField=options.pField||'p_holm_within_outcome';
    const comparisonMode=options.comparisonMode||'single';
    const pairs=annotate?comparisonPairs(groups,options):[];
    const layered=annotate&&pairs.length>1;
    const width=Math.max(980,times.length*Math.max(150,groups.length*32)+200);
    const margin=170,plotRight=width-30,plotWidth=plotRight-margin,step=plotWidth/times.length;
    const title=options.title||outcome.label,axis=options.yLabel||outcome.label;
    const titleLines=lines(title,width-100,22),legendCols=Math.max(1,Math.floor((width-100)/225));
    const titleBottom=35+titleLines.length*27,legendTop=titleBottom+12,legendRows=Math.ceil(groups.length/legendCols);
    const annotationTop=legendTop+legendRows*40+12;
    const annotationRows=pairs.map(([l,r])=>Math.max(...times.map(t=>{
      const c=comparison(pairwise,outcome.label,t,l,r,pField);
      return annotationLabels(c,l,r,showP,showMD,true).flatMap(label=>lines(label,step-16,13)).length;
    }))*17+16);
    const annotationHeight=layered?annotationRows.reduce((a,b)=>a+b,0)+14:0;
    const plotTop=legendTop+legendRows*40+20+annotationHeight,plotBottom=plotTop+390;
    const maxTimeLines=Math.max(...times.map(t=>lines(t,step-8,14).length));
    const tableTop=plotBottom+45+maxTimeLines*17+18,tableRowHeight=34;
    const tableBottom=tableTop+(showTable?(groups.length+1)*tableRowHeight:0);
    const family=pLabels[pField];if(!family)throw Error('P值标注方式无效');
    const model=outcome.model==='lmm'?'LMM':'GEE';
    const caption=[`箱：Q1–Q3；中线：中位数；须：1.5×IQR内最远观测。${showOutliers?'圆点：离群值（保留在分析中）。':'离群值未画出，仍保留在分析中。'}`,
      '四分位数采用线性插值（R quantile type 7）；横轴为分类时间，等间距不代表实际时间间隔。',
      ...(annotate?[`标注来源：${model}各时点组间比较；${comparisonMode==='all'?`每时点全部${pairs.length}对，方向为图例顺序中前组减后组。`:`方向：${left} − ${right}。`}${showMD?'MD为模型均值差，不是中位数差。':''}`]:[]),
      ...(showP?[`图中P：${family}。沿用已计划的完整比较族，不因图中显示范围而重新校正。`]:[])];
    const captionLines=caption.flatMap(x=>lines(x,width-100,13)),height=tableBottom+35+captionLines.length*20+20;
    let min=Math.min(...stats.map(c=>c.min)),max=Math.max(...stats.map(c=>c.max)),span=max-min;
    if(span===0)span=Math.max(1,Math.abs(min)*.2);
    const yMin=min-.08*span,yMax=max+(annotate&&!layered?.29:.08)*span;
    const y=v=>plotBottom-(v-yMin)/(yMax-yMin)*(plotBottom-plotTop);
    const center=t=>margin+(times.indexOf(t)+.5)*step,clusterWidth=Math.min(step*.76,groups.length*32);
    const boxWidth=Math.min(27,clusterWidth/groups.length*.72);
    const x=(t,g)=>center(t)+(groups.map(String).indexOf(String(g))-(groups.length-1)/2)*clusterWidth/groups.length;
    const text=(value,xx,yy,size=14,anchor='start',extra='')=>`<text x="${xx}" y="${yy}" text-anchor="${anchor}" font-size="${size}" fill="#23322e" stroke="none" ${extra}>${esc(value)}</text>`;
    const line=(x1,y1,x2,y2,color='#273a33',sw=1.2)=>`<line x1="${x1}" y1="${y1}" x2="${x2}" y2="${y2}" stroke="${color}" stroke-width="${sw}"/>`;
    let result=`<svg id="rm-chart" class="rm-chart rm-boxplot" xmlns="http://www.w3.org/2000/svg" width="${width}" height="${height}" viewBox="0 0 ${width} ${height}" role="img" aria-label="${esc(title+' 重复测量分组箱线图')}" style="font-family:Arial,'PingFang SC','Microsoft YaHei',sans-serif;fill:none;stroke:none"><title>${esc(title+' 重复测量分组箱线图')}</title><desc>${esc(caption.join(' '))}</desc><rect width="${width}" height="${height}" fill="white"/>`;
    result+=`<metadata>${esc(JSON.stringify({chart:'grouped_boxplot',quantiles:'R type 7',whiskers:'1.5 IQR',outcome:outcome.label,model,showP,showMD,comparisonMode,pairs,left,right,pField,showTable,showOutliers}))}</metadata>`;
    titleLines.forEach((t,i)=>{result+=text(t,width/2,35+i*27,22,'middle','font-weight="600"');});
    groups.forEach((g,i)=>{const xx=50+(i%legendCols)*(width-100)/legendCols,yy=legendTop+Math.floor(i/legendCols)*40;
      result+=`<rect x="${xx}" y="${yy-11}" width="17" height="12" fill="white" stroke="${palette[i%palette.length]}" stroke-width="2"/>`;
      lines(String(g),(width-100)/legendCols-40,14).slice(0,2).forEach((t,j)=>{result+=text(t,xx+26,yy+j*15,14);});
    });
    for(let i=0;i<=5;i++){const v=yMin+(yMax-yMin)*i/5,yy=y(v);result+=line(margin,yy,plotRight,yy,'#e8ecea',1)+text(num(v),margin-12,yy+5,13,'end');}
    result+=line(margin,plotTop,margin,plotBottom)+line(margin,plotBottom,plotRight,plotBottom);
    const axisLines=lines(axis,plotBottom-plotTop-20,15);
    axisLines.forEach((t,i)=>{result+=text(t,0,i*17,15,'middle',`transform="translate(${25+i*17} ${(plotTop+plotBottom)/2}) rotate(-90)"`);});
    stats.forEach(c=>{const xx=x(c.time,c.group),color=palette[groups.map(String).indexOf(c.group)%palette.length],lo=y(c.whiskerLow),hi=y(c.whiskerHigh);
      result+=`<g data-box-time="${esc(c.time)}" data-box-group="${esc(c.group)}"><title>${esc(`${c.group}, ${c.time}: n=${c.n}, median=${num(c.median)}, Q1=${num(c.q1)}, Q3=${num(c.q3)}`)}</title>`;
      result+=line(xx,hi,xx,lo,color,1.5)+line(xx-boxWidth*.3,hi,xx+boxWidth*.3,hi,color,1.5)+line(xx-boxWidth*.3,lo,xx+boxWidth*.3,lo,color,1.5);
      result+=`<rect x="${xx-boxWidth/2}" y="${y(c.q3)}" width="${boxWidth}" height="${Math.max(.2,y(c.q1)-y(c.q3))}" stroke="${color}" stroke-width="2" fill="white"/>`;
      result+=line(xx-boxWidth/2,y(c.median),xx+boxWidth/2,y(c.median),color,2.3);
      if(showOutliers)c.outliers.forEach((v,i)=>{const jitter=(((i*7)%11)/10-.5)*boxWidth*.5;result+=`<circle cx="${xx+jitter}" cy="${y(v)}" r="2.6" fill="${color}" fill-opacity=".7"/>`;});
      result+='</g>';
    });
    times.forEach(time=>{
      lines(time,step-8,14).forEach((t,i)=>{result+=text(t,center(time),plotBottom+25+i*17,14,'middle');});
      if(!annotate)return;
      let rowTop=annotationTop;
      pairs.forEach(([l,r],i)=>{
        const c=comparison(pairwise,outcome.label,time,l,r,pField),localMax=Math.max(...stats.filter(s=>s.time===time).map(s=>s.max));
        const yy=layered?rowTop+annotationRows[i]-8:y(localMax)-15;
        const x1=Math.min(x(time,l),x(time,r)),x2=Math.max(x(time,l),x(time,r));
        result+=`<g data-comparison-time="${esc(time)}" data-comparison-left="${esc(l)}" data-comparison-right="${esc(r)}">`+line(x1,yy+5,x1,yy)+line(x1,yy,x2,yy)+line(x2,yy,x2,yy+5);
        const labels=annotationLabels(c,l,r,showP,showMD,layered).flatMap(label=>lines(label,step-16,13));
        labels.forEach((label,j)=>{result+=text(label,center(time),layered?rowTop+13+j*17:yy-10-(labels.length-1-j)*19,13,'middle');});
        result+='</g>';rowTop+=annotationRows[i];
      });
    });
    result+=text('时间点',margin+plotWidth/2,tableTop-17,14,'middle');
    if(showTable){
      const tableX=30,labelWidth=margin-tableX,cellWidth=step;
      const tableCell=(value,col,row,fill,anchor='middle')=>{const xx=tableX+(col===0?0:labelWidth+(col-1)*cellWidth),w=col===0?labelWidth:cellWidth,yy=tableTop+row*tableRowHeight;
        result+=`<rect x="${xx}" y="${yy}" width="${w}" height="${tableRowHeight}" fill="${fill}" stroke="white"/>`;
        const wrapped=lines(value,w-12,12).slice(0,2);wrapped.forEach((t,i)=>{result+=text(t,anchor==='start'?xx+8:xx+w/2,yy+tableRowHeight/2+(wrapped.length===1?4:-4)+i*14,12,anchor);});};
      tableCell('组别 / n',0,0,'#dce8ef','start');times.forEach((t,i)=>tableCell(t,i+1,0,'#dce8ef'));
      groups.forEach((g,j)=>{const n=stats.find(c=>c.group===String(g)).n;tableCell(`${g} (n=${n})`,0,j+1,j%2?'#f3f0f8':'#edf3f7','start');times.forEach((t,i)=>{const c=stats.find(c=>c.time===t&&c.group===String(g));tableCell(`${num(c.median)} (${num(c.q1)}, ${num(c.q3)})`,i+1,j+1,j%2?'#f3f0f8':'#edf3f7');});});
    }
    captionLines.forEach((t,i)=>{result+=text(t,50,tableBottom+35+i*20,13);});
    return result+'</svg>';
  }
  const api={summarize,cells,comparison,comparisonPairs,svg,pLabels};
  if(typeof module!=='undefined'&&module.exports)module.exports=api;else root.RMBoxPlot=api;
})(typeof globalThis!=='undefined'?globalThis:this);
