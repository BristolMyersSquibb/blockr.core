(()=>{var od=Object.create;var X0=Object.defineProperty;var yd=Object.getOwnPropertyDescriptor;var md=Object.getOwnPropertyNames;var dd=Object.getPrototypeOf,hd=Object.prototype.hasOwnProperty;var ft=(l,t)=>()=>{try{return t||l((t={exports:{}}).exports,t),t.exports}catch(u){throw t=0,u}};var gd=(l,t,u,a)=>{if(t&&typeof t=="object"||typeof t=="function")for(let n of md(t))!hd.call(l,n)&&n!==u&&X0(l,n,{get:()=>t[n],enumerable:!(a=yd(t,n))||a.enumerable});return l};var xn=(l,t,u)=>(u=l!=null?od(dd(l)):{},gd(t||!l||!l.__esModule?X0(u,"default",{value:l,enumerable:!0}):u,l));var $0=ft(N=>{"use strict";/**
 * @license React
 * react.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */var Bf=Symbol.for("react.transitional.element"),Sd=Symbol.for("react.portal"),Td=Symbol.for("react.fragment"),Ed=Symbol.for("react.strict_mode"),sd=Symbol.for("react.profiler"),zd=Symbol.for("react.consumer"),bd=Symbol.for("react.context"),Od=Symbol.for("react.forward_ref"),_d=Symbol.for("react.suspense"),Ad=Symbol.for("react.memo"),x0=Symbol.for("react.lazy"),Nd=Symbol.for("react.activity"),Dd=Symbol.for("react.view_transition"),Q0=Symbol.iterator;function Md(l){return l===null||typeof l!="object"?null:(l=Q0&&l[Q0]||l["@@iterator"],typeof l=="function"?l:null)}var L0={isMounted:function(){return!1},enqueueForceUpdate:function(){},enqueueReplaceState:function(){},enqueueSetState:function(){}},p0=Object.assign,K0={};function xu(l,t,u){this.props=l,this.context=t,this.refs=K0,this.updater=u||L0}xu.prototype.isReactComponent={};xu.prototype.setState=function(l,t){if(typeof l!="object"&&typeof l!="function"&&l!=null)throw Error("takes an object of state variables to update or a function which returns an object of state variables.");this.updater.enqueueSetState(this,l,t,"setState")};xu.prototype.forceUpdate=function(l){this.updater.enqueueForceUpdate(this,l,"forceUpdate")};function J0(){}J0.prototype=xu.prototype;function qf(l,t,u){this.props=l,this.context=t,this.refs=K0,this.updater=u||L0}var Gf=qf.prototype=new J0;Gf.constructor=qf;p0(Gf,xu.prototype);Gf.isPureReactComponent=!0;var j0=Array.isArray;function Yf(){}var p={H:null,A:null,T:null,S:null},w0=Object.prototype.hasOwnProperty;function Xf(l,t,u){var a=u.ref;return{$$typeof:Bf,type:l,key:t,ref:a!==void 0?a:null,props:u}}function Ud(l,t){return Xf(l.type,t,l.props)}function Qf(l){return typeof l=="object"&&l!==null&&l.$$typeof===Bf}function rd(l){var t={"=":"=0",":":"=2"};return"$"+l.replace(/[=:]/g,function(u){return t[u]})}var V0=/\/+/g;function Rf(l,t){return typeof l=="object"&&l!==null&&l.key!=null?rd(""+l.key):t.toString(36)}function Cd(l){switch(l.status){case"fulfilled":return l.value;case"rejected":throw l.reason;default:switch(typeof l.status=="string"?l.then(Yf,Yf):(l.status="pending",l.then(function(t){l.status==="pending"&&(l.status="fulfilled",l.value=t)},function(t){l.status==="pending"&&(l.status="rejected",l.reason=t)})),l.status){case"fulfilled":return l.value;case"rejected":throw l.reason}}throw l}function Zu(l,t,u,a,n){var e=typeof l;(e==="undefined"||e==="boolean")&&(l=null);var f=!1;if(l===null)f=!0;else switch(e){case"bigint":case"string":case"number":f=!0;break;case"object":switch(l.$$typeof){case Bf:case Sd:f=!0;break;case x0:return f=l._init,Zu(f(l._payload),t,u,a,n)}}if(f)return n=n(l),f=a===""?"."+Rf(l,0):a,j0(n)?(u="",f!=null&&(u=f.replace(V0,"$&/")+"/"),Zu(n,t,u,"",function(o){return o})):n!=null&&(Qf(n)&&(n=Ud(n,u+(n.key==null||l&&l.key===n.key?"":(""+n.key).replace(V0,"$&/")+"/")+f)),t.push(n)),1;f=0;var c=a===""?".":a+":";if(j0(l))for(var i=0;i<l.length;i++)a=l[i],e=c+Rf(a,i),f+=Zu(a,t,u,e,n);else if(i=Md(l),typeof i=="function")for(l=i.call(l),i=0;!(a=l.next()).done;)a=a.value,e=c+Rf(a,i++),f+=Zu(a,t,u,e,n);else if(e==="object"){if(typeof l.then=="function")return Zu(Cd(l),t,u,a,n);throw t=String(l),Error("Objects are not valid as a React child (found: "+(t==="[object Object]"?"object with keys {"+Object.keys(l).join(", ")+"}":t)+"). If you meant to render a collection of children, use an array instead.")}return f}function Ln(l,t,u){if(l==null)return l;var a=[],n=0;return Zu(l,a,"","",function(e){return t.call(u,e,n++)}),a}function Hd(l){if(l._status===-1){var t=l._result,u=t();u.then(function(a){(l._status===0||l._status===-1)&&(l._status=1,l._result=a,u.status===void 0&&(u.status="fulfilled",u.value=a))},function(a){(l._status===0||l._status===-1)&&(l._status=2,l._result=a,u.status===void 0&&(u.status="rejected",u.reason=a))}),l._status===-1&&(l._status=0,l._result=u)}if(l._status===1)return l._result.default;throw l._result}var Z0=typeof reportError=="function"?reportError:function(l){if(typeof window=="object"&&typeof window.ErrorEvent=="function"){var t=new window.ErrorEvent("error",{bubbles:!0,cancelable:!0,message:typeof l=="object"&&l!==null&&typeof l.message=="string"?String(l.message):String(l),error:l});if(!window.dispatchEvent(t))return}else if(typeof process=="object"&&typeof process.emit=="function"){process.emit("uncaughtException",l);return}console.error(l)};function W0(l){var t=p.T,u={};u.types=t!==null?t.types:null,p.T=u;try{var a=l(),n=p.S;n!==null&&n(u,a),typeof a=="object"&&a!==null&&typeof a.then=="function"&&a.then(Yf,Z0)}catch(e){Z0(e)}finally{t!==null&&u.types!==null&&(t.types=u.types),p.T=t}}function F0(l){var t=p.T;if(t!==null){var u=t.types;u===null?t.types=[l]:u.indexOf(l)===-1&&u.push(l)}else W0(F0.bind(null,l))}var Rd={map:Ln,forEach:function(l,t,u){Ln(l,function(){t.apply(this,arguments)},u)},count:function(l){var t=0;return Ln(l,function(){t++}),t},toArray:function(l){return Ln(l,function(t){return t})||[]},only:function(l){if(!Qf(l))throw Error("React.Children.only expected to receive a single React element child.");return l}};N.Activity=Nd;N.Children=Rd;N.Component=xu;N.Fragment=Td;N.Profiler=sd;N.PureComponent=qf;N.StrictMode=Ed;N.Suspense=_d;N.ViewTransition=Dd;N.__CLIENT_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE=p;N.__COMPILER_RUNTIME={__proto__:null,c:function(l){return p.H.useMemoCache(l)}};N.addTransitionType=F0;N.cache=function(l){return function(){return l.apply(null,arguments)}};N.cacheSignal=function(){return null};N.cloneElement=function(l,t,u){if(l==null)throw Error("The argument must be a React element, but you passed "+l+".");var a=p0({},l.props),n=l.key;if(t!=null)for(e in t.key!==void 0&&(n=""+t.key),t)!w0.call(t,e)||e==="key"||e==="__self"||e==="__source"||e==="ref"&&t.ref===void 0||(a[e]=t[e]);var e=arguments.length-2;if(e===1)a.children=u;else if(1<e){for(var f=Array(e),c=0;c<e;c++)f[c]=arguments[c+2];a.children=f}return Xf(l.type,n,a)};N.createContext=function(l){return l={$$typeof:bd,_currentValue:l,_currentValue2:l,_threadCount:0,Provider:null,Consumer:null},l.Provider=l,l.Consumer={$$typeof:zd,_context:l},l};N.createElement=function(l,t,u){var a,n={},e=null;if(t!=null)for(a in t.key!==void 0&&(e=""+t.key),t)w0.call(t,a)&&a!=="key"&&a!=="__self"&&a!=="__source"&&(n[a]=t[a]);var f=arguments.length-2;if(f===1)n.children=u;else if(1<f){for(var c=Array(f),i=0;i<f;i++)c[i]=arguments[i+2];n.children=c}if(l&&l.defaultProps)for(a in f=l.defaultProps,f)n[a]===void 0&&(n[a]=f[a]);return Xf(l,e,n)};N.createRef=function(){return{current:null}};N.forwardRef=function(l){return{$$typeof:Od,render:l}};N.isValidElement=Qf;N.lazy=function(l){return{$$typeof:x0,_payload:{_status:-1,_result:l},_init:Hd}};N.memo=function(l,t){return{$$typeof:Ad,type:l,compare:t===void 0?null:t}};N.startTransition=W0;N.unstable_useCacheRefresh=function(){return p.H.useCacheRefresh()};N.use=function(l){return p.H.use(l)};N.useActionState=function(l,t,u){return p.H.useActionState(l,t,u)};N.useCallback=function(l,t){return p.H.useCallback(l,t)};N.useContext=function(l){return p.H.useContext(l)};N.useDebugValue=function(){};N.useDeferredValue=function(l,t){return p.H.useDeferredValue(l,t)};N.useEffect=function(l,t){return p.H.useEffect(l,t)};N.useEffectEvent=function(l){return p.H.useEffectEvent(l)};N.useId=function(){return p.H.useId()};N.useImperativeHandle=function(l,t,u){return p.H.useImperativeHandle(l,t,u)};N.useInsertionEffect=function(l,t){return p.H.useInsertionEffect(l,t)};N.useLayoutEffect=function(l,t){return p.H.useLayoutEffect(l,t)};N.useMemo=function(l,t){return p.H.useMemo(l,t)};N.useOptimistic=function(l,t){return p.H.useOptimistic(l,t)};N.useReducer=function(l,t,u){return p.H.useReducer(l,t,u)};N.useRef=function(l){return p.H.useRef(l)};N.useState=function(l){return p.H.useState(l)};N.useSyncExternalStore=function(l,t,u){return p.H.useSyncExternalStore(l,t,u)};N.useTransition=function(){return p.H.useTransition()};N.version="19.3.1"});var pn=ft((FS,I0)=>{"use strict";I0.exports=$0()});var l1=ft(El=>{"use strict";/**
 * @license React
 * react-dom.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */var Yd=pn();function P0(l){var t="https://react.dev/errors/"+l;if(1<arguments.length){t+="?args[]="+encodeURIComponent(arguments[1]);for(var u=2;u<arguments.length;u++)t+="&args[]="+encodeURIComponent(arguments[u])}return"Minified React error #"+l+"; visit "+t+" for the full message or use the non-minified dev environment for full errors and additional helpful warnings."}function jt(){}var bl={d:{f:jt,r:function(){throw Error(P0(522))},D:jt,C:jt,L:jt,m:jt,X:jt,S:jt,M:jt},p:0,findDOMNode:null},Bd=Symbol.for("react.portal"),qd=Symbol.for("react.recoverable"),k0=Symbol.for("react.optimistic_key");function Gd(l,t,u){var a=3<arguments.length&&arguments[3]!==void 0?arguments[3]:null;return{$$typeof:Bd,key:a==null?null:a===k0?k0:""+a,children:l,containerInfo:t,implementation:u}}var ja=Yd.__CLIENT_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE;function Kn(l,t){if(l==="font")return"";if(typeof t=="string")return t==="use-credentials"?t:""}El.__DOM_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE=bl;El.browser=function(l){return{$$typeof:qd,_reason:l}};El.createPortal=function(l,t){var u=2<arguments.length&&arguments[2]!==void 0?arguments[2]:null;if(!t||t.nodeType!==1&&t.nodeType!==9&&t.nodeType!==11)throw Error(P0(299));return Gd(l,t,null,u)};El.flushSync=function(l){var t=ja.T,u=bl.p;try{if(ja.T=null,bl.p=2,l)return l()}finally{ja.T=t,bl.p=u,bl.d.f()}};El.preconnect=function(l,t){typeof l=="string"&&(t?(t=t.crossOrigin,t=typeof t=="string"?t==="use-credentials"?t:"":void 0):t=null,bl.d.C(l,t))};El.prefetchDNS=function(l){typeof l=="string"&&bl.d.D(l)};El.preinit=function(l,t){if(typeof l=="string"&&t&&typeof t.as=="string"){var u=t.as,a=Kn(u,t.crossOrigin),n=typeof t.integrity=="string"?t.integrity:void 0,e=typeof t.fetchPriority=="string"?t.fetchPriority:void 0;u==="style"?bl.d.S(l,typeof t.precedence=="string"?t.precedence:void 0,{crossOrigin:a,integrity:n,fetchPriority:e}):u==="script"&&bl.d.X(l,{crossOrigin:a,integrity:n,fetchPriority:e,nonce:typeof t.nonce=="string"?t.nonce:void 0})}};El.preinitModule=function(l,t){if(typeof l=="string")if(typeof t=="object"&&t!==null){if(t.as==null||t.as==="script"){var u=Kn(t.as,t.crossOrigin);bl.d.M(l,{crossOrigin:u,integrity:typeof t.integrity=="string"?t.integrity:void 0,nonce:typeof t.nonce=="string"?t.nonce:void 0,fetchPriority:typeof t.fetchPriority=="string"?t.fetchPriority:void 0})}}else t==null&&bl.d.M(l)};El.preload=function(l,t){if(typeof l=="string"&&typeof t=="object"&&t!==null&&typeof t.as=="string"){var u=t.as,a=Kn(u,t.crossOrigin);bl.d.L(l,u,{crossOrigin:a,integrity:typeof t.integrity=="string"?t.integrity:void 0,nonce:typeof t.nonce=="string"?t.nonce:void 0,type:typeof t.type=="string"?t.type:void 0,fetchPriority:typeof t.fetchPriority=="string"?t.fetchPriority:void 0,referrerPolicy:typeof t.referrerPolicy=="string"?t.referrerPolicy:void 0,imageSrcSet:typeof t.imageSrcSet=="string"?t.imageSrcSet:void 0,imageSizes:typeof t.imageSizes=="string"?t.imageSizes:void 0,media:typeof t.media=="string"?t.media:void 0})}};El.preloadModule=function(l,t){if(typeof l=="string")if(t){var u=Kn(t.as,t.crossOrigin);bl.d.m(l,{as:typeof t.as=="string"&&t.as!=="script"?t.as:void 0,crossOrigin:u,integrity:typeof t.integrity=="string"?t.integrity:void 0,nonce:typeof t.nonce=="string"?t.nonce:void 0,fetchPriority:typeof t.fetchPriority=="string"?t.fetchPriority:void 0})}else bl.d.m(l)};El.requestFormReset=function(l){bl.d.r(l)};El.unstable_batchedUpdates=function(l,t){return l(t)};El.useFormState=function(l,t,u){return ja.H.useFormState(l,t,u)};El.useFormStatus=function(){return ja.H.useHostTransitionStatus()};El.version="19.3.1"});var jf=ft((IS,u1)=>{"use strict";function t1(){if(!(typeof __REACT_DEVTOOLS_GLOBAL_HOOK__>"u"||typeof __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE!="function"))try{__REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE(t1)}catch(l){console.error(l)}}t1(),u1.exports=l1()});var m1=ft(W=>{"use strict";/**
 * @license React
 * scheduler.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */function Lf(l,t){var u=l.length;l.push(t);l:for(;0<u;){var a=u-1>>>1,n=l[a];if(0<Jn(n,t))l[a]=t,l[u]=n,u=a;else break l}}function ct(l){return l.length===0?null:l[0]}function Wn(l){if(l.length===0)return null;var t=l[0],u=l.pop();if(u!==t){l[0]=u;l:for(var a=0,n=l.length,e=n>>>1;a<e;){var f=2*(a+1)-1,c=l[f],i=f+1,o=l[i];if(0>Jn(c,u))i<n&&0>Jn(o,c)?(l[a]=o,l[i]=u,a=i):(l[a]=c,l[f]=u,a=f);else if(i<n&&0>Jn(o,u))l[a]=o,l[i]=u,a=i;else break l}}return t}function Jn(l,t){var u=l.sortIndex-t.sortIndex;return u!==0?u:l.id-t.id}W.unstable_now=void 0;typeof performance=="object"&&typeof performance.now=="function"?(a1=performance,W.unstable_now=function(){return a1.now()}):(Vf=Date,n1=Vf.now(),W.unstable_now=function(){return Vf.now()-n1});var a1,Vf,n1,Ot=[],Vt=[],Xd=1,pl=null,sl=3,pf=!1,Va=!1,Za=!1,Kf=!1,c1=typeof setTimeout=="function"?setTimeout:null,i1=typeof clearTimeout=="function"?clearTimeout:null,e1=typeof setImmediate<"u"?setImmediate:null;function wn(l){for(var t=ct(Vt);t!==null;){if(t.callback===null)Wn(Vt);else if(t.startTime<=l)Wn(Vt),t.sortIndex=t.expirationTime,Lf(Ot,t);else break;t=ct(Vt)}}function Jf(l){if(Za=!1,wn(l),!Va)if(ct(Ot)!==null)Va=!0,pu||(pu=!0,Lu());else{var t=ct(Vt);t!==null&&wf(Jf,t.startTime-l)}}var pu=!1,xa=-1,v1=5,o1=-1;function y1(){return Kf?!0:!(W.unstable_now()-o1<v1)}function Zf(){if(Kf=!1,pu){var l=W.unstable_now();o1=l;var t=!0;try{l:{Va=!1,Za&&(Za=!1,i1(xa),xa=-1),pf=!0;var u=sl;try{t:{for(wn(l),pl=ct(Ot);pl!==null&&!(pl.expirationTime>l&&y1());){var a=pl.callback;if(typeof a=="function"){pl.callback=null,sl=pl.priorityLevel;var n=a(pl.expirationTime<=l);if(l=W.unstable_now(),typeof n=="function"){pl.callback=n,wn(l),t=!0;break t}pl===ct(Ot)&&Wn(Ot),wn(l)}else Wn(Ot);pl=ct(Ot)}if(pl!==null)t=!0;else{var e=ct(Vt);e!==null&&wf(Jf,e.startTime-l),t=!1}}break l}finally{pl=null,sl=u,pf=!1}t=void 0}}finally{t?Lu():pu=!1}}}var Lu;typeof e1=="function"?Lu=function(){e1(Zf)}:typeof MessageChannel<"u"?(xf=new MessageChannel,f1=xf.port2,xf.port1.onmessage=Zf,Lu=function(){f1.postMessage(null)}):Lu=function(){c1(Zf,0)};var xf,f1;function wf(l,t){xa=c1(function(){l(W.unstable_now())},t)}W.unstable_IdlePriority=5;W.unstable_ImmediatePriority=1;W.unstable_LowPriority=4;W.unstable_NormalPriority=3;W.unstable_Profiling=null;W.unstable_UserBlockingPriority=2;W.unstable_cancelCallback=function(l){l.callback=null};W.unstable_forceFrameRate=function(l){0>l||125<l?console.error("forceFrameRate takes a positive int between 0 and 125, forcing frame rates higher than 125 fps is not supported"):v1=0<l?Math.floor(1e3/l):5};W.unstable_getCurrentPriorityLevel=function(){return sl};W.unstable_next=function(l){switch(sl){case 1:case 2:case 3:var t=3;break;default:t=sl}var u=sl;sl=t;try{return l()}finally{sl=u}};W.unstable_requestPaint=function(){Kf=!0};W.unstable_runWithPriority=function(l,t){switch(l){case 1:case 2:case 3:case 4:case 5:break;default:l=3}var u=sl;sl=l;try{return t()}finally{sl=u}};W.unstable_scheduleCallback=function(l,t,u){var a=W.unstable_now();switch(typeof u=="object"&&u!==null?(u=u.delay,u=typeof u=="number"&&0<u?a+u:a):u=a,l){case 1:var n=-1;break;case 2:n=250;break;case 5:n=1073741823;break;case 4:n=1e4;break;default:n=5e3}return n=u+n,l={id:Xd++,callback:t,priorityLevel:l,startTime:u,expirationTime:n,sortIndex:-1},u>a?(l.sortIndex=u,Lf(Vt,l),ct(Ot)===null&&l===ct(Vt)&&(Za?(i1(xa),xa=-1):Za=!0,wf(Jf,u-a))):(l.sortIndex=n,Lf(Ot,l),Va||pf||(Va=!0,pu||(pu=!0,Lu()))),l};W.unstable_shouldYield=y1;W.unstable_wrapCallback=function(l){var t=sl;return function(){var u=sl;sl=t;try{return l.apply(this,arguments)}finally{sl=u}}}});var h1=ft((PS,d1)=>{"use strict";d1.exports=m1()});var td=ft(Cf=>{"use strict";/**
 * @license React
 * react-dom-client.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */var fl=h1(),Pv=pn(),Qd=jf();function E(l){var t="https://react.dev/errors/"+l;if(1<arguments.length){t+="?args[]="+encodeURIComponent(arguments[1]);for(var u=2;u<arguments.length;u++)t+="&args[]="+encodeURIComponent(arguments[u])}return"Minified React error #"+l+"; visit "+t+" for the full message or use the non-minified dev environment for full errors and additional helpful warnings."}function lo(l){return!(!l||l.nodeType!==1&&l.nodeType!==9&&l.nodeType!==11)}function Cn(l){for(var t=l,u=t;u&&!u.alternate;)t=u,(t.flags&4098)!==0&&(l=t.return),u=t.return;for(;t.return;)t=t.return;return t.tag===3?l:null}function to(l){if(l.tag===13){var t=l.memoizedState;if(t===null&&(l=l.alternate,l!==null&&(t=l.memoizedState)),t!==null)return t.dehydrated}return null}function uo(l){if(l.tag===31){var t=l.memoizedState;if(t===null&&(l=l.alternate,l!==null&&(t=l.memoizedState)),t!==null)return t.dehydrated}return null}function g1(l){if(Cn(l)!==l)throw Error(E(188))}function jd(l){var t=l.alternate;if(!t){if(t=Cn(l),t===null)throw Error(E(188));return t!==l?null:l}for(var u=l,a=t;;){var n=u.return;if(n===null)break;var e=n.alternate;if(e===null){if(a=n.return,a!==null){u=a;continue}break}if(n.child===e.child){for(e=n.child;e;){if(e===u)return g1(n),l;if(e===a)return g1(n),t;e=e.sibling}throw Error(E(188))}if(u.return!==a.return)u=n,a=e;else{for(var f=!1,c=n.child;c;){if(c===u){f=!0,u=n,a=e;break}if(c===a){f=!0,a=n,u=e;break}c=c.sibling}if(!f){for(c=e.child;c;){if(c===u){f=!0,u=e,a=n;break}if(c===a){f=!0,a=e,u=n;break}c=c.sibling}if(!f)throw Error(E(189))}}if(u.alternate!==a)throw Error(E(190))}if(u.tag!==3)throw Error(E(188));return u.stateNode.current===u?l:t}function ao(l){var t=l.tag;if(t===5||t===26||t===27||t===6)return l;for(l=l.child;l!==null;){if(t=ao(l),t!==null)return t;l=l.sibling}return null}function Hl(l,t,u,a,n,e){for(;l!==null;){if((l.tag===5||l.tag===27||l.tag===6)&&u(l,a,n,e)||(l.tag!==22||l.memoizedState===null)&&(t||l.tag!==5&&l.tag!==27)&&Hl(l.child,t,u,a,n,e))return!0;l=l.sibling}return!1}function Gu(l){for(l=l.return;l!==null;){if(l.tag===3||l.tag===5||l.tag===27)return l;l=l.return}return null}function S1(l){var t=!1;for(l=l.return;l!==null&&(l.tag===4&&(t=!0),!(l.tag===3||l.tag===5||l.tag===27));)l=l.return;return t}function no(l){var t=[null,null],u=Gu(l);return u===null||eo(t,l,u.child,{foundSelf:!1}),t}function eo(l,t,u,a){for(;u!==null;){if(u===t)a.foundSelf=!0;else if(u.tag===5||u.tag===27||u.tag===6){if(a.foundSelf)return l[1]=u,!0;l[0]=u}else if((u.tag!==22||u.memoizedState===null)&&eo(l,t,u.child,a))return!0;u=u.sibling}return!1}function el(l){switch(l.tag){case 5:case 27:case 6:return l.stateNode;case 3:return l.stateNode.containerInfo;default:throw Error(E(559))}}var Iu=null,Nc=null;function Vd(l,t,u){return l===u?!0:l===t?(Iu=l,!0):!1}function Zd(l,t,u){return l===u?(Nc=l,!1):l===t?(Nc!==null&&(Iu=l),!0):!1}function T1(l){if(l===null)return null;do l=l===null?null:l.return;while(l&&l.tag!==5&&l.tag!==27&&l.tag!==3);return l||null}function Dc(l,t,u){for(var a=0,n=l;n;n=u(n))a++;n=0;for(var e=t;e;e=u(e))n++;for(;0<a-n;)l=u(l),a--;for(;0<n-a;)t=u(t),n--;for(;a--;){if(l===t||t!==null&&l===t.alternate)return l;l=u(l),t=u(t)}return null}var L=Object.assign,xd=Symbol.for("react.element"),Fn=Symbol.for("react.transitional.element"),Fa=Symbol.for("react.portal"),ku=Symbol.for("react.fragment"),fo=Symbol.for("react.strict_mode"),Mc=Symbol.for("react.profiler"),co=Symbol.for("react.consumer"),dt=Symbol.for("react.context"),Gi=Symbol.for("react.forward_ref"),Uc=Symbol.for("react.suspense"),rc=Symbol.for("react.suspense_list"),Xi=Symbol.for("react.memo"),pt=Symbol.for("react.lazy"),Cc=Symbol.for("react.activity"),Ld=Symbol.for("react.legacy_hidden"),pd=Symbol.for("react.memo_cache_sentinel"),Hc=Symbol.for("react.view_transition"),Kd=Symbol.for("react.recoverable"),E1=Symbol.iterator;function La(l){return l===null||typeof l!="object"?null:(l=E1&&l[E1]||l["@@iterator"],typeof l=="function"?l:null)}var Jd=Symbol.for("react.client.reference");function Rc(l){if(l==null)return null;if(typeof l=="function")return l.$$typeof===Jd?null:l.displayName||l.name||null;if(typeof l=="string")return l;switch(l){case ku:return"Fragment";case Mc:return"Profiler";case fo:return"StrictMode";case Uc:return"Suspense";case rc:return"SuspenseList";case Cc:return"Activity";case Hc:return"ViewTransition"}if(typeof l=="object")switch(l.$$typeof){case Fa:return"Portal";case dt:return l.displayName||"Context";case co:return(l._context.displayName||"Context")+".Consumer";case Gi:var t=l.render;return l=l.displayName,l||(l=t.displayName||t.name||"",l=l!==""?"ForwardRef("+l+")":"ForwardRef"),l;case Xi:return t=l.displayName||null,t!==null?t:Rc(l.type)||"Memo";case pt:t=l._payload,l=l._init;try{return Rc(l(t))}catch{}}return null}var $a=Array.isArray,A=Pv.__CLIENT_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE,G=Qd.__DOM_INTERNALS_DO_NOT_USE_OR_WARN_USERS_THEY_CANNOT_UPGRADE,_u={pending:!1,data:null,method:null,action:null},Yc=[],Pu=-1;function zt(l){return{current:l}}function hl(l){0>Pu||(l.current=Yc[Pu],Yc[Pu]=null,Pu--)}function w(l,t){Pu++,Yc[Pu]=l.current,l.current=t}var Tt=zt(null),hn=zt(null),Pt=zt(null),qe=zt(null);function Ge(l,t){switch(w(Pt,t),w(hn,l),w(Tt,null),t.nodeType){case 9:case 11:l=(l=t.documentElement)&&(l=l.namespaceURI)?Rv(l):0;break;default:if(l=t.tagName,t=t.namespaceURI)t=Rv(t),l=Cm(t,l);else switch(l){case"svg":l=1;break;case"math":l=2;break;default:l=0}}hl(Tt),w(Tt,l)}function sa(){hl(Tt),hl(hn),hl(Pt)}function Bc(l){var t=l.memoizedState;t!==null&&(ra._currentValue=t.memoizedState,w(qe,l)),t=Tt.current;var u=Cm(t,l.type);t!==u&&(w(hn,l),w(Tt,u))}function Xe(l){hn.current===l&&(hl(Tt),hl(hn)),qe.current===l&&(hl(qe),ra._currentValue=_u)}var Wf,s1;function xt(l){if(Wf===void 0)try{throw Error()}catch(u){var t=u.stack.trim().match(/\n( *(at )?)/);Wf=t&&t[1]||"",s1=-1<u.stack.indexOf(`
    at`)?" (<anonymous>)":-1<u.stack.indexOf("@")?"@unknown:0:0":""}return`
`+Wf+l+s1}var Ff=!1;function $f(l,t){if(!l||Ff)return"";Ff=!0;var u=Error.prepareStackTrace;Error.prepareStackTrace=void 0;try{var a={DetermineComponentFrameRoot:function(){try{if(t){var T=function(){throw Error()};if(Object.defineProperty(T.prototype,"props",{set:function(){throw Error()}}),typeof Reflect=="object"&&Reflect.construct){try{Reflect.construct(T,[])}catch(s){var y=s}Reflect.construct(l,[],T)}else{try{T.call()}catch(s){y=s}T=!1;try{var h=Object.getOwnPropertyDescriptor(l.prototype,"props");Object.defineProperty(l.prototype,"props",{configurable:!0,set:function(){throw Error()}}),T=!0,new l}finally{T&&(h!==void 0?Object.defineProperty(l.prototype,"props",h):delete l.prototype.props)}}}else{try{throw Error()}catch(s){y=s}(T=l())&&typeof T.catch=="function"&&T.catch(function(){})}}catch(s){if(s&&y&&typeof s.stack=="string")return[s.stack,y.stack]}return[null,null]}};a.DetermineComponentFrameRoot.displayName="DetermineComponentFrameRoot";var n=Object.getOwnPropertyDescriptor(a.DetermineComponentFrameRoot,"name");n&&n.configurable&&Object.defineProperty(a.DetermineComponentFrameRoot,"name",{value:"DetermineComponentFrameRoot"});var e=a.DetermineComponentFrameRoot(),f=e[0],c=e[1];if(f&&c){var i=f.split(`
`),o=c.split(`
`);for(n=a=0;a<i.length&&!i[a].includes("DetermineComponentFrameRoot");)a++;for(;n<o.length&&!o[n].includes("DetermineComponentFrameRoot");)n++;if(a===i.length||n===o.length)for(a=i.length-1,n=o.length-1;1<=a&&0<=n&&i[a]!==o[n];)n--;for(;1<=a&&0<=n;a--,n--)if(i[a]!==o[n]){if(a!==1||n!==1)do if(a--,n--,0>n||i[a]!==o[n]){var g=`
`+i[a].replace(" at new "," at ");return l.displayName&&g.includes("<anonymous>")&&(g=g.replace("<anonymous>",l.displayName)),g}while(1<=a&&0<=n);break}}}finally{Ff=!1,Error.prepareStackTrace=u}return(u=l?l.displayName||l.name:"")?xt(u):""}function wd(l,t){switch(l.tag){case 26:case 27:case 5:return xt(l.type);case 16:return xt("Lazy");case 13:return l.child!==t&&t!==null?xt("Suspense Fallback"):xt("Suspense");case 19:return xt("SuspenseList");case 0:case 15:return $f(l.type,!1);case 11:return $f(l.type.render,!1);case 1:return $f(l.type,!0);case 31:return xt("Activity");case 30:return xt("ViewTransition");default:return""}}function z1(l){try{var t="",u=null;do t+=wd(l,u),u=l,l=l.return;while(l);return t}catch(a){return`
Error generating stack: `+a.message+`
`+a.stack}}var qc=Object.prototype.hasOwnProperty,Qi=fl.unstable_scheduleCallback,If=fl.unstable_cancelCallback,Wd=fl.unstable_shouldYield,Fd=fl.unstable_requestPaint,Ql=fl.unstable_now,$d=fl.unstable_getCurrentPriorityLevel,io=fl.unstable_ImmediatePriority,vo=fl.unstable_UserBlockingPriority,Qe=fl.unstable_NormalPriority,Id=fl.unstable_LowPriority,oo=fl.unstable_IdlePriority,kd=fl.log,Pd=fl.unstable_setDisableYieldValue,Hn=null,jl=null;function wt(l){if(typeof kd=="function"&&Pd(l),jl&&typeof jl.setStrictMode=="function")try{jl.setStrictMode(Hn,l)}catch{}}var Vl=Math.clz32?Math.clz32:uh,lh=Math.log,th=Math.LN2;function uh(l){return l>>>=0,l===0?32:31-(lh(l)/th|0)|0}var $n=256,In=262144,kn=4194304;function Eu(l){var t=l&42;if(t!==0)return t;switch(l&-l){case 1:return 1;case 2:return 2;case 4:return 4;case 8:return 8;case 16:return 16;case 32:return 32;case 64:return 64;case 128:return 128;case 256:case 512:case 1024:case 2048:case 4096:case 8192:case 16384:case 32768:case 65536:case 131072:return l&-l;case 262144:case 524288:case 1048576:case 2097152:return l&3932160;case 4194304:case 8388608:case 16777216:case 33554432:return l&62914560;case 67108864:return 67108864;case 134217728:return 134217728;case 268435456:return 268435456;case 536870912:return 536870912;case 1073741824:return 0;default:return l}}function yf(l,t,u){var a=l.pendingLanes;if(a===0)return 0;var n=0,e=l.suspendedLanes,f=l.pingedLanes;l=l.warmLanes;var c=a&134217727;return c!==0?(a=c&~e,a!==0?n=Eu(a):(f&=c,f!==0?n=Eu(f):u||(u=c&~l,u!==0&&(n=Eu(u))))):(c=a&~e,c!==0?n=Eu(c):f!==0?n=Eu(f):u||(u=a&~l,u!==0&&(n=Eu(u)))),n===0?0:t!==0&&t!==n&&(t&e)===0&&(e=n&-n,u=t&-t,e>=u||e===32&&(u&4194048)!==0)?t:n}function Rn(l,t){return(l.pendingLanes&~(l.suspendedLanes&~l.pingedLanes)&t)===0}function yo(l,t){(t&8)!==0&&(t|=t&32);var u=l.entangledLanes;if(u!==0)for(l=l.entanglements,u&=t;0<u;){var a=31-Vl(u),n=1<<a;t|=l[a],u&=~n}return t}function ah(l,t){switch(l){case 1:case 2:case 4:case 8:case 64:return t+250;case 16:case 32:case 128:case 256:case 512:case 1024:case 2048:case 4096:case 8192:case 16384:case 32768:case 65536:case 131072:case 262144:case 524288:case 1048576:case 2097152:return t+5e3;case 4194304:case 8388608:case 16777216:case 33554432:return-1;case 67108864:case 134217728:case 268435456:case 536870912:case 1073741824:return-1;default:return-1}}function mo(){var l=kn;return kn<<=1,(kn&62914560)===0&&(kn=4194304),l}function kf(l){for(var t=[],u=0;31>u;u++)t.push(l);return t}function Yn(l,t){l.pendingLanes|=t,t!==268435456&&(l.suspendedLanes=0,l.pingedLanes=0,l.warmLanes=0)}function nh(l,t,u,a,n,e){var f=l.pendingLanes;l.pendingLanes=u,l.suspendedLanes=0,l.pingedLanes=0,l.warmLanes=0,l.expiredLanes&=u,l.entangledLanes&=u,l.errorRecoveryDisabledLanes&=u,l.shellSuspendCounter=0;var c=l.entanglements,i=l.expirationTimes,o=l.hiddenUpdates;for(u=f&~u;0<u;){var g=31-Vl(u),T=1<<g;c[g]=0,i[g]=-1;var y=o[g];if(y!==null)for(o[g]=null,g=0;g<y.length;g++){var h=y[g];h!==null&&(h.lane&=-536870913)}u&=~T}a!==0&&ho(l,a,0),e!==0&&n===0&&l.tag!==0&&(l.suspendedLanes|=e&~(f&~t))}function ho(l,t,u){l.pendingLanes|=t,l.suspendedLanes&=~t;var a=31-Vl(t);l.entangledLanes|=t,l.entanglements[a]=l.entanglements[a]|1073741824|u&261930}function go(l,t){var u=l.entangledLanes|=t;for(l=l.entanglements;u;){var a=31-Vl(u),n=1<<a;n&t|l[a]&t&&(l[a]|=t),u&=~n}}function So(l,t){var u=t&-t;return u=(u&42)!==0?1:ji(u),(u&(l.suspendedLanes|t))!==0?0:u}function ji(l){switch(l){case 2:l=1;break;case 8:l=4;break;case 32:l=16;break;case 256:case 512:case 1024:case 2048:case 4096:case 8192:case 16384:case 32768:case 65536:case 131072:case 262144:case 524288:case 1048576:case 2097152:case 4194304:case 8388608:case 16777216:case 33554432:l=128;break;case 268435456:l=134217728;break;default:l=0}return l}function Vi(l){return l&=-l,2<l?8<l?(l&134217727)!==0?32:268435456:8:2}function To(){var l=G.p;return l!==0?l:(l=window.event,l===void 0?32:km(l.type))}function b1(l,t){var u=G.p;try{return G.p=l,t()}finally{G.p=u}}var qt=Math.random().toString(36).slice(2),ml="__reactFiber$"+qt,Rl="__reactProps$"+qt,Ra="__reactContainer$"+qt,O1="__reactEvents$"+qt,eh="__reactListeners$"+qt,fh="__reactHandles$"+qt,_1="__reactResources$"+qt,Bn="__reactMarker$"+qt,je="__reactLoad$"+qt;function mf(l){delete l[ml],delete l[Rl],delete l[eh],delete l[fh]}function bu(l){var t;if(t=l[ml])return t;for(var u=l.parentNode;u;){if(t=u[Ra]||u[ml]){if(u=t.alternate,t.child!==null||u!==null&&u.child!==null)for(l=Vv(l);l!==null;){if(u=l[ml])return u;l=Vv(l)}return t}l=u,u=l.parentNode}return null}function Ya(l){if(l=l[ml]||l[Ra]){var t=l.tag;if(t===5||t===6||t===13||t===31||t===26||t===27||t===3)return l}return null}function Ia(l){var t=l.tag;if(t===5||t===26||t===27||t===6)return l.stateNode;throw Error(E(33))}function va(l){var t=l[_1];return t||(t=l[_1]={hoistableStyles:new Map,hoistableScripts:new Map}),t}function vl(l){l[Bn]=!0}function Eo(l){l[je]=void 0}var so=new Set,zo={};function Xu(l,t){za(l,t),za(l+"Capture",t)}function za(l,t){for(zo[l]=t,l=0;l<t.length;l++)so.add(t[l])}var ch=RegExp("^[:A-Z_a-z\\u00C0-\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD][:A-Z_a-z\\u00C0-\\u00D6\\u00D8-\\u00F6\\u00F8-\\u02FF\\u0370-\\u037D\\u037F-\\u1FFF\\u200C-\\u200D\\u2070-\\u218F\\u2C00-\\u2FEF\\u3001-\\uD7FF\\uF900-\\uFDCF\\uFDF0-\\uFFFD\\-.0-9\\u00B7\\u0300-\\u036F\\u203F-\\u2040]*$"),A1={},N1={};function ih(l){return qc.call(N1,l)?!0:qc.call(A1,l)?!1:ch.test(l)?N1[l]=!0:(A1[l]=!0,!1)}var B=!1;function D1(){var l=B;return B=!1,l}function Se(l,t,u){if(ih(t))if(u===null)l.removeAttribute(t);else{switch(typeof u){case"undefined":case"function":case"symbol":l.removeAttribute(t);return;case"boolean":var a=t.toLowerCase().slice(0,5);if(a!=="data-"&&a!=="aria-"){l.removeAttribute(t);return}}l.setAttribute(t,u)}}function Pn(l,t,u){if(u===null)l.removeAttribute(t);else{switch(typeof u){case"undefined":case"function":case"symbol":case"boolean":l.removeAttribute(t);return}l.setAttribute(t,u)}}function _t(l,t,u,a){if(a===null)l.removeAttribute(u);else{switch(typeof a){case"undefined":case"function":case"symbol":case"boolean":l.removeAttribute(u);return}l.setAttributeNS(t,u,a)}}function Bl(l){switch(typeof l){case"bigint":case"boolean":case"number":case"string":case"undefined":return l;case"object":return l;default:return""}}function bo(l){var t=l.type;return(l=l.nodeName)&&l.toLowerCase()==="input"&&(t==="checkbox"||t==="radio")}function vh(l,t,u){var a=Object.getOwnPropertyDescriptor(l.constructor.prototype,t);if(!l.hasOwnProperty(t)&&typeof a<"u"&&typeof a.get=="function"&&typeof a.set=="function"){var n=a.get,e=a.set;return Object.defineProperty(l,t,{configurable:!0,get:function(){return n.call(this)},set:function(f){u=""+f,e.call(this,f)}}),Object.defineProperty(l,t,{enumerable:a.enumerable}),{getValue:function(){return u},setValue:function(f){u=""+f},stopTracking:function(){l._valueTracker=null,delete l[t]}}}}function Gc(l){if(!l._valueTracker){var t=bo(l)?"checked":"value";l._valueTracker=vh(l,t,""+l[t])}}function Oo(l){if(!l)return!1;var t=l._valueTracker;if(!t)return!0;var u=t.getValue(),a="";return l&&(a=bo(l)?l.checked?"true":"false":l.value),l=a,l!==u?(t.setValue(l),!0):!1}var oh=/[\n"\\]/g;function Fl(l){return l.replace(oh,function(t){return"\\"+t.charCodeAt(0).toString(16)+" "})}function Xc(l,t,u,a,n,e,f,c){l.name="",f!=null&&typeof f!="function"&&typeof f!="symbol"&&typeof f!="boolean"?l.type=f:l.removeAttribute("type"),t!=null?f==="number"?(t===0&&l.value===""||l.value!=t)&&(l.value=""+Bl(t)):l.value!==""+Bl(t)&&(l.value=""+Bl(t)):f!=="submit"&&f!=="reset"||l.removeAttribute("value"),t!=null?f==="number"&&l.value==t?Pf(l,Bl(l.value)):Pf(l,Bl(t)):u!=null?Pf(l,Bl(u)):a!=null&&l.removeAttribute("value"),n==null&&e!=null&&(l.defaultChecked=!!e),n!=null&&(l.checked=n&&typeof n!="function"&&typeof n!="symbol"),c!=null&&typeof c!="function"&&typeof c!="symbol"&&typeof c!="boolean"?l.name=""+Bl(c):l.removeAttribute("name")}function _o(l,t,u,a,n,e,f,c){if(e!=null&&typeof e!="function"&&typeof e!="symbol"&&typeof e!="boolean"&&(l.type=e),t!=null||u!=null){if(!(e!=="submit"&&e!=="reset"||t!=null)){Gc(l);return}u=u!=null?""+Bl(u):"",t=t!=null?""+Bl(t):u,c||t===l.value||(l.value=t),l.defaultValue=t}a=a??n,a=typeof a!="function"&&typeof a!="symbol"&&!!a,l.checked=c?l.checked:!!a,l.defaultChecked=!!a,f!=null&&typeof f!="function"&&typeof f!="symbol"&&typeof f!="boolean"&&(l.name=f),Gc(l)}function Pf(l,t){l.defaultValue!==""+t&&(l.defaultValue=""+t)}function oa(l,t,u,a){if(l=l.options,t){t={};for(var n=0;n<u.length;n++)t["$"+u[n]]=!0;for(u=0;u<l.length;u++)n=t.hasOwnProperty("$"+l[u].value),l[u].selected!==n&&(l[u].selected=n),n&&a&&(l[u].defaultSelected=!0)}else{for(u=""+Bl(u),t=null,n=0;n<l.length;n++){if(l[n].value===u){l[n].selected=!0,a&&(l[n].defaultSelected=!0);return}t!==null||l[n].disabled||(t=l[n])}t!==null&&(t.selected=!0)}}function Ao(l,t,u){if(t!=null&&(t=""+Bl(t),t!==l.value&&(l.value=t),u==null)){l.defaultValue!==t&&(l.defaultValue=t);return}l.defaultValue=u!=null?""+Bl(u):""}function No(l,t,u,a){if(t==null){if(a!=null){if(u!=null)throw Error(E(92));if($a(a)){if(1<a.length)throw Error(E(93));a=a[0]}u=a}u==null&&(u=""),t=u}u=Bl(t),l.defaultValue=u,a=l.textContent,a===u&&a!==""&&a!==null&&(l.value=a),Gc(l)}function ba(l,t){if(t){var u=l.firstChild;if(u&&u===l.lastChild&&u.nodeType===3){u.nodeValue=t;return}}l.textContent=t}var yh=new Set("animationIterationCount aspectRatio borderImageOutset borderImageSlice borderImageWidth boxFlex boxFlexGroup boxOrdinalGroup columnCount columns flex flexGrow flexPositive flexShrink flexNegative flexOrder gridArea gridRow gridRowEnd gridRowSpan gridRowStart gridColumn gridColumnEnd gridColumnSpan gridColumnStart fontWeight lineClamp lineHeight opacity order orphans scale tabSize widows zIndex zoom fillOpacity floodOpacity stopOpacity strokeDasharray strokeDashoffset strokeMiterlimit strokeOpacity strokeWidth MozAnimationIterationCount MozBoxFlex MozBoxFlexGroup MozLineClamp msAnimationIterationCount msFlex msZoom msFlexGrow msFlexNegative msFlexOrder msFlexPositive msFlexShrink msGridColumn msGridColumnSpan msGridRow msGridRowSpan WebkitAnimationIterationCount WebkitBoxFlex WebKitBoxFlexGroup WebkitBoxOrdinalGroup WebkitColumnCount WebkitColumns WebkitFlex WebkitFlexGrow WebkitFlexPositive WebkitFlexShrink WebkitLineClamp".split(" "));function M1(l,t,u){var a=t.indexOf("--")===0;u==null||typeof u=="boolean"||u===""?a?l.setProperty(t,""):t==="float"?l.cssFloat="":l[t]="":a?l.setProperty(t,u):typeof u!="number"||u===0||yh.has(t)?t==="float"?l.cssFloat=u:l[t]=(""+u).trim():l[t]=u+"px"}function Do(l,t,u){if(t!=null&&typeof t!="object")throw Error(E(62));if(l=l.style,u!=null){for(var a in u)!u.hasOwnProperty(a)||t!=null&&t.hasOwnProperty(a)||(a.indexOf("--")===0?l.setProperty(a,""):a==="float"?l.cssFloat="":l[a]="",B=!0);for(var n in t)a=t[n],t.hasOwnProperty(n)&&u[n]!==a&&(M1(l,n,a),B=!0)}else for(var e in t)t.hasOwnProperty(e)&&M1(l,e,t[e])}function Zi(l){if(l.indexOf("-")===-1)return!1;switch(l){case"annotation-xml":case"color-profile":case"font-face":case"font-face-src":case"font-face-uri":case"font-face-format":case"font-face-name":case"missing-glyph":return!1;default:return!0}}var mh=new Map([["acceptCharset","accept-charset"],["htmlFor","for"],["httpEquiv","http-equiv"],["crossOrigin","crossorigin"],["accentHeight","accent-height"],["alignmentBaseline","alignment-baseline"],["arabicForm","arabic-form"],["baselineShift","baseline-shift"],["capHeight","cap-height"],["clipPath","clip-path"],["clipRule","clip-rule"],["colorInterpolation","color-interpolation"],["colorInterpolationFilters","color-interpolation-filters"],["colorProfile","color-profile"],["colorRendering","color-rendering"],["dominantBaseline","dominant-baseline"],["enableBackground","enable-background"],["fillOpacity","fill-opacity"],["fillRule","fill-rule"],["floodColor","flood-color"],["floodOpacity","flood-opacity"],["fontFamily","font-family"],["fontSize","font-size"],["fontSizeAdjust","font-size-adjust"],["fontStretch","font-stretch"],["fontStyle","font-style"],["fontVariant","font-variant"],["fontWeight","font-weight"],["glyphName","glyph-name"],["glyphOrientationHorizontal","glyph-orientation-horizontal"],["glyphOrientationVertical","glyph-orientation-vertical"],["horizAdvX","horiz-adv-x"],["horizOriginX","horiz-origin-x"],["imageRendering","image-rendering"],["letterSpacing","letter-spacing"],["lightingColor","lighting-color"],["markerEnd","marker-end"],["markerMid","marker-mid"],["markerStart","marker-start"],["maskType","mask-type"],["overlinePosition","overline-position"],["overlineThickness","overline-thickness"],["paintOrder","paint-order"],["panose-1","panose-1"],["pointerEvents","pointer-events"],["renderingIntent","rendering-intent"],["shapeRendering","shape-rendering"],["stopColor","stop-color"],["stopOpacity","stop-opacity"],["strikethroughPosition","strikethrough-position"],["strikethroughThickness","strikethrough-thickness"],["strokeDasharray","stroke-dasharray"],["strokeDashoffset","stroke-dashoffset"],["strokeLinecap","stroke-linecap"],["strokeLinejoin","stroke-linejoin"],["strokeMiterlimit","stroke-miterlimit"],["strokeOpacity","stroke-opacity"],["strokeWidth","stroke-width"],["textAnchor","text-anchor"],["textDecoration","text-decoration"],["textRendering","text-rendering"],["transformOrigin","transform-origin"],["underlinePosition","underline-position"],["underlineThickness","underline-thickness"],["unicodeBidi","unicode-bidi"],["unicodeRange","unicode-range"],["unitsPerEm","units-per-em"],["vAlphabetic","v-alphabetic"],["vHanging","v-hanging"],["vIdeographic","v-ideographic"],["vMathematical","v-mathematical"],["vectorEffect","vector-effect"],["vertAdvY","vert-adv-y"],["vertOriginX","vert-origin-x"],["vertOriginY","vert-origin-y"],["wordSpacing","word-spacing"],["writingMode","writing-mode"],["xmlnsXlink","xmlns:xlink"],["xHeight","x-height"]]),dh=/^[\u0000-\u001F ]*j[\r\n\t]*a[\r\n\t]*v[\r\n\t]*a[\r\n\t]*s[\r\n\t]*c[\r\n\t]*r[\r\n\t]*i[\r\n\t]*p[\r\n\t]*t[\r\n\t]*:/i;function Te(l){return dh.test(""+l)?"javascript:throw new Error('React has blocked a javascript: URL as a security precaution.')":l}function ht(){}var Qc=null;function xi(l){return l=l.target||l.srcElement||window,l.correspondingUseElement&&(l=l.correspondingUseElement),l.nodeType===3?l.parentNode:l}var la=null,ya=null;function U1(l){var t=Ya(l);if(t&&(l=t.stateNode)){var u=l[Rl]||null;l:switch(l=t.stateNode,t.type){case"input":if(Xc(l,u.value,u.defaultValue,u.defaultValue,u.checked,u.defaultChecked,u.type,u.name),t=u.name,u.type==="radio"&&t!=null){for(u=l;u.parentNode;)u=u.parentNode;for(u=u.querySelectorAll('input[name="'+Fl(""+t)+'"][type="radio"]'),t=0;t<u.length;t++){var a=u[t];if(a!==l&&a.form===l.form){var n=a[Rl]||null;if(!n)throw Error(E(90));Xc(a,n.value,n.defaultValue,n.defaultValue,n.checked,n.defaultChecked,n.type,n.name)}}for(t=0;t<u.length;t++)a=u[t],a.form===l.form&&Oo(a)}break l;case"textarea":Ao(l,u.value,u.defaultValue);break l;case"select":t=u.value,t!=null&&oa(l,!!u.multiple,t,!1)}}}var lc=!1;function Mo(l,t,u){if(lc)return l(t,u);lc=!0;try{var a=l(t);return a}finally{if(lc=!1,(la!==null||ya!==null)&&(Df(),la&&(t=la,l=ya,ya=la=null,U1(t),l)))for(t=0;t<l.length;t++)U1(l[t])}}function gn(l,t){var u=l.stateNode;if(u===null)return null;var a=u[Rl]||null;if(a===null)return null;u=a[t];l:switch(t){case"onClick":case"onClickCapture":case"onDoubleClick":case"onDoubleClickCapture":case"onMouseDown":case"onMouseDownCapture":case"onMouseMove":case"onMouseMoveCapture":case"onMouseUp":case"onMouseUpCapture":case"onMouseEnter":(a=!a.disabled)||(l=l.type,a=!(l==="button"||l==="input"||l==="select"||l==="textarea")),l=!a;break l;default:l=!1}if(l)return null;if(u&&typeof u!="function")throw Error(E(231,t,typeof u));return u}var rt=!(typeof window>"u"||typeof window.document>"u"||typeof window.document.createElement>"u"),jc=!1;if(rt)try{Ku={},Object.defineProperty(Ku,"passive",{get:function(){jc=!0}}),window.addEventListener("test",Ku,Ku),window.removeEventListener("test",Ku,Ku)}catch{jc=!1}var Ku,Wt=null,Li=null,Ee=null;function Uo(){if(Ee)return Ee;var l,t=Li,u=t.length,a,n="value"in Wt?Wt.value:Wt.textContent,e=n.length;for(l=0;l<u&&t[l]===n[l];l++);var f=u-l;for(a=1;a<=f&&t[u-a]===n[e-a];a++);return Ee=n.slice(l,1<a?1-a:void 0)}function se(l){var t=l.keyCode;return"charCode"in l?(l=l.charCode,l===0&&t===13&&(l=13)):l=t,l===10&&(l=13),32<=l||l===13?l:0}function le(){return!0}function r1(){return!1}function Nl(l){function t(u,a,n,e,f){this._reactName=u,this._targetInst=n,this.type=a,this.nativeEvent=e,this.target=f,this.currentTarget=null;for(var c in l)l.hasOwnProperty(c)&&(u=l[c],this[c]=u?u(e):e[c]);return this.isDefaultPrevented=(e.defaultPrevented!=null?e.defaultPrevented:e.returnValue===!1)?le:r1,this.isPropagationStopped=r1,this}return L(t.prototype,{preventDefault:function(){this.defaultPrevented=!0;var u=this.nativeEvent;u&&(u.preventDefault?u.preventDefault():typeof u.returnValue!="unknown"&&(u.returnValue=!1),this.isDefaultPrevented=le)},stopPropagation:function(){var u=this.nativeEvent;u&&(u.stopPropagation?u.stopPropagation():typeof u.cancelBubble!="unknown"&&(u.cancelBubble=!0),this.isPropagationStopped=le)},persist:function(){},isPersistent:le}),t}var hu={eventPhase:0,bubbles:0,cancelable:0,timeStamp:function(l){return l.timeStamp||Date.now()},defaultPrevented:0,isTrusted:0},df=Nl(hu),qn=L({},hu,{view:0,detail:0}),hh=Nl(qn),tc,uc,pa,hf=L({},qn,{screenX:0,screenY:0,clientX:0,clientY:0,pageX:0,pageY:0,ctrlKey:0,shiftKey:0,altKey:0,metaKey:0,getModifierState:pi,button:0,buttons:0,relatedTarget:function(l){return l.relatedTarget===void 0?l.fromElement===l.srcElement?l.toElement:l.fromElement:l.relatedTarget},movementX:function(l){return"movementX"in l?l.movementX:(l!==pa&&(pa&&l.type==="mousemove"?(tc=l.screenX-pa.screenX,uc=l.screenY-pa.screenY):uc=tc=0,pa=l),tc)},movementY:function(l){return"movementY"in l?l.movementY:uc}}),C1=Nl(hf),gh=L({},hf,{dataTransfer:0}),Sh=Nl(gh),Th=L({},qn,{relatedTarget:0}),ac=Nl(Th),Eh=L({},hu,{animationName:0,elapsedTime:0,pseudoElement:0}),sh=Nl(Eh),zh=L({},hu,{clipboardData:function(l){return"clipboardData"in l?l.clipboardData:window.clipboardData}}),bh=Nl(zh),Oh=L({},hu,{data:0}),H1=Nl(Oh),_h={Esc:"Escape",Spacebar:" ",Left:"ArrowLeft",Up:"ArrowUp",Right:"ArrowRight",Down:"ArrowDown",Del:"Delete",Win:"OS",Menu:"ContextMenu",Apps:"ContextMenu",Scroll:"ScrollLock",MozPrintableKey:"Unidentified"},Ah={8:"Backspace",9:"Tab",12:"Clear",13:"Enter",16:"Shift",17:"Control",18:"Alt",19:"Pause",20:"CapsLock",27:"Escape",32:" ",33:"PageUp",34:"PageDown",35:"End",36:"Home",37:"ArrowLeft",38:"ArrowUp",39:"ArrowRight",40:"ArrowDown",45:"Insert",46:"Delete",112:"F1",113:"F2",114:"F3",115:"F4",116:"F5",117:"F6",118:"F7",119:"F8",120:"F9",121:"F10",122:"F11",123:"F12",144:"NumLock",145:"ScrollLock",224:"Meta"},Nh={Alt:"altKey",Control:"ctrlKey",Meta:"metaKey",Shift:"shiftKey"};function Dh(l){var t=this.nativeEvent;return t.getModifierState?t.getModifierState(l):(l=Nh[l])?!!t[l]:!1}function pi(){return Dh}var Mh=L({},qn,{key:function(l){if(l.key){var t=_h[l.key]||l.key;if(t!=="Unidentified")return t}return l.type==="keypress"?(l=se(l),l===13?"Enter":String.fromCharCode(l)):l.type==="keydown"||l.type==="keyup"?Ah[l.keyCode]||"Unidentified":""},code:0,location:0,ctrlKey:0,shiftKey:0,altKey:0,metaKey:0,repeat:0,locale:0,getModifierState:pi,charCode:function(l){return l.type==="keypress"?se(l):0},keyCode:function(l){return l.type==="keydown"||l.type==="keyup"?l.keyCode:0},which:function(l){return l.type==="keypress"?se(l):l.type==="keydown"||l.type==="keyup"?l.keyCode:0}}),Uh=Nl(Mh),rh=L({},hf,{pointerId:0,width:0,height:0,pressure:0,tangentialPressure:0,tiltX:0,tiltY:0,twist:0,pointerType:0,isPrimary:0}),R1=Nl(rh),Ch=L({},hu,{submitter:0}),Hh=Nl(Ch),Rh=L({},qn,{touches:0,targetTouches:0,changedTouches:0,altKey:0,metaKey:0,ctrlKey:0,shiftKey:0,getModifierState:pi}),Yh=Nl(Rh),Bh=L({},hu,{propertyName:0,elapsedTime:0,pseudoElement:0}),qh=Nl(Bh),Gh=L({},hf,{deltaX:function(l){return"deltaX"in l?l.deltaX:"wheelDeltaX"in l?-l.wheelDeltaX:0},deltaY:function(l){return"deltaY"in l?l.deltaY:"wheelDeltaY"in l?-l.wheelDeltaY:"wheelDelta"in l?-l.wheelDelta:0},deltaZ:0,deltaMode:0}),Xh=Nl(Gh),Qh=L({},hu,{newState:0,oldState:0,source:0}),jh=Nl(Qh),Vh=[9,13,27,32],Ki=rt&&"CompositionEvent"in window,ln=null;rt&&"documentMode"in document&&(ln=document.documentMode);var Zh=rt&&"TextEvent"in window&&!ln,ro=rt&&(!Ki||ln&&8<ln&&11>=ln),Y1=" ",B1=!1;function Co(l,t){switch(l){case"keyup":return Vh.indexOf(t.keyCode)!==-1;case"keydown":return t.keyCode!==229;case"keypress":case"mousedown":case"focusout":return!0;default:return!1}}function Ho(l){return l=l.detail,typeof l=="object"&&"data"in l?l.data:null}var ta=!1;function xh(l,t){switch(l){case"compositionend":return Ho(t);case"keypress":return t.which!==32?null:(B1=!0,Y1);case"textInput":return l=t.data,l===Y1&&B1?null:l;default:return null}}function Lh(l,t){if(ta)return l==="compositionend"||!Ki&&Co(l,t)?(l=Uo(),Ee=Li=Wt=null,ta=!1,l):null;switch(l){case"paste":return null;case"keypress":if(!(t.ctrlKey||t.altKey||t.metaKey)||t.ctrlKey&&t.altKey){if(t.char&&1<t.char.length)return t.char;if(t.which)return String.fromCharCode(t.which)}return null;case"compositionend":return ro&&t.locale!=="ko"?null:t.data;default:return null}}var ph={color:!0,date:!0,datetime:!0,"datetime-local":!0,email:!0,month:!0,number:!0,password:!0,range:!0,search:!0,tel:!0,text:!0,time:!0,url:!0,week:!0};function q1(l){var t=l&&l.nodeName&&l.nodeName.toLowerCase();return t==="input"?!!ph[l.type]:t==="textarea"}function Ro(l,t,u,a){la?ya?ya.push(a):ya=[a]:la=a,t=cf(t,"onChange"),0<t.length&&(u=new df("onChange","change",null,u,a),l.push({event:u,listeners:t}))}var tn=null,Sn=null;function Kh(l){Mm(l,0)}function gf(l){var t=Ia(l);if(Oo(t))return l}function G1(l,t){if(l==="change")return t}var Yo=!1;rt&&(rt?(ue="oninput"in document,ue||(nc=document.createElement("div"),nc.setAttribute("oninput","return;"),ue=typeof nc.oninput=="function"),te=ue):te=!1,Yo=te&&(!document.documentMode||9<document.documentMode));var te,ue,nc;function X1(){tn&&(tn.detachEvent("onpropertychange",Bo),Sn=tn=null)}function Bo(l){if(l.propertyName==="value"&&gf(Sn)){var t=[];Ro(t,Sn,l,xi(l)),Mo(Kh,t)}}function Jh(l,t,u){l==="focusin"?(X1(),tn=t,Sn=u,tn.attachEvent("onpropertychange",Bo)):l==="focusout"&&X1()}function wh(l){if(l==="selectionchange"||l==="keyup"||l==="keydown")return gf(Sn)}function Wh(l,t){if(l==="click")return gf(t)}function Fh(l,t){if(l==="input"||l==="change")return gf(t)}function $h(l,t){return l===t&&(l!==0||1/l===1/t)||l!==l&&t!==t}var xl=typeof Object.is=="function"?Object.is:$h;function Tn(l,t){if(xl(l,t))return!0;if(typeof l!="object"||l===null||typeof t!="object"||t===null)return!1;var u=Object.keys(l),a=Object.keys(t);if(u.length!==a.length)return!1;for(a=0;a<u.length;a++){var n=u[a];if(!qc.call(t,n)||!xl(l[n],t[n]))return!1}return!0}function Vc(l){if(l=l||(typeof document<"u"?document:void 0),typeof l>"u")return null;try{return l.activeElement||l.body}catch{return l.body}}function Q1(l){for(;l&&l.firstChild;)l=l.firstChild;return l}function j1(l,t){var u=Q1(l);l=0;for(var a;u;){if(u.nodeType===3){if(a=l+u.textContent.length,l<=t&&a>=t)return{node:u,offset:t-l};l=a}l:{for(;u;){if(u.nextSibling){u=u.nextSibling;break l}u=u.parentNode}u=void 0}u=Q1(u)}}function qo(l,t){return l&&t?l===t?!0:l&&l.nodeType===3?!1:t&&t.nodeType===3?qo(l,t.parentNode):"contains"in l?l.contains(t):l.compareDocumentPosition?!!(l.compareDocumentPosition(t)&16):!1:!1}function Go(l){l=l!=null&&l.ownerDocument!=null&&l.ownerDocument.defaultView!=null?l.ownerDocument.defaultView:window;for(var t=Vc(l.document);t instanceof l.HTMLIFrameElement;){try{var u=typeof t.contentWindow.location.href=="string"}catch{u=!1}if(u)l=t.contentWindow;else break;t=Vc(l.document)}return t}function Ji(l){var t=l&&l.nodeName&&l.nodeName.toLowerCase();return t&&(t==="input"&&(l.type==="text"||l.type==="search"||l.type==="tel"||l.type==="url"||l.type==="password")||t==="textarea"||l.contentEditable==="true")}var Ih=rt&&"documentMode"in document&&11>=document.documentMode,ua=null,Zc=null,un=null,xc=!1;function V1(l,t,u){var a=u.window===u?u.document:u.nodeType===9?u:u.ownerDocument;xc||ua==null||ua!==Vc(a)||(a=ua,"selectionStart"in a&&Ji(a)?a={start:a.selectionStart,end:a.selectionEnd}:(a=(a.ownerDocument&&a.ownerDocument.defaultView||window).getSelection(),a={anchorNode:a.anchorNode,anchorOffset:a.anchorOffset,focusNode:a.focusNode,focusOffset:a.focusOffset}),un&&Tn(un,a)||(un=a,a=cf(Zc,"onSelect"),0<a.length&&(t=new df("onSelect","select",null,t,u),l.push({event:t,listeners:a}),t.target=ua)))}function Su(l,t){var u={};return u[l.toLowerCase()]=t.toLowerCase(),u["Webkit"+l]="webkit"+t,u["Moz"+l]="moz"+t,u}var aa={animationend:Su("Animation","AnimationEnd"),animationiteration:Su("Animation","AnimationIteration"),animationstart:Su("Animation","AnimationStart"),transitionrun:Su("Transition","TransitionRun"),transitionstart:Su("Transition","TransitionStart"),transitioncancel:Su("Transition","TransitionCancel"),transitionend:Su("Transition","TransitionEnd")},ec={},Xo={};rt&&(Xo=document.createElement("div").style,"AnimationEvent"in window||(delete aa.animationend.animation,delete aa.animationiteration.animation,delete aa.animationstart.animation),"TransitionEvent"in window||delete aa.transitionend.transition);function Qu(l){if(ec[l])return ec[l];if(!aa[l])return l;var t=aa[l],u;for(u in t)if(t.hasOwnProperty(u)&&u in Xo)return ec[l]=t[u];return l}var Qo=Qu("animationend"),jo=Qu("animationiteration"),Vo=Qu("animationstart"),kh=Qu("transitionrun"),Ph=Qu("transitionstart"),lg=Qu("transitioncancel"),Zo=Qu("transitionend"),xo=new Map,Lc="abort auxClick beforeToggle cancel canPlay canPlayThrough click close contextMenu copy cut drag dragEnd dragEnter dragExit dragLeave dragOver dragStart drop durationChange emptied encrypted ended error fullscreenChange fullscreenError gotPointerCapture input invalid keyDown keyPress keyUp load loadedData loadedMetadata loadStart lostPointerCapture mouseDown mouseMove mouseOut mouseOver mouseUp paste pause play playing pointerCancel pointerDown pointerMove pointerOut pointerOver pointerUp progress rateChange reset resize seeked seeking stalled submit suspend timeUpdate touchCancel touchEnd touchStart volumeChange scroll toggle touchMove waiting wheel".split(" ");Lc.push("scrollEnd");function et(l,t){xo.set(l,t),Xu(t,[l])}var tg=0;function Ct(l,t){if(l.name!=null&&l.name!=="auto")return l.name;if(t.autoName!==null)return t.autoName;l=nt.identifierPrefix;var u=tg++;return l="_"+l+"t_"+u.toString(32)+"_",t.autoName=l}function Z1(l){if(l==null||typeof l=="string")return l;var t=null,u=Ea;if(u!==null)for(var a=0;a<u.length;a++){var n=l[u[a]];if(n!=null){if(n==="none")return"none";t=t==null?n:t+(" "+n)}}return t??l.default}function Gt(l,t){return l=Z1(l),t=Z1(t),t==null?l==="auto"?null:l:t==="auto"?null:t}var Ve=typeof reportError=="function"?reportError:function(l){if(typeof window=="object"&&typeof window.ErrorEvent=="function"){var t=new window.ErrorEvent("error",{bubbles:!0,cancelable:!0,message:typeof l=="object"&&l!==null&&typeof l.message=="string"?String(l.message):String(l),error:l});if(!window.dispatchEvent(t))return}else if(typeof process=="object"&&typeof process.emit=="function"){process.emit("uncaughtException",l);return}console.error(l)},Jl=[],na=0,wi=0;function Sf(){for(var l=na,t=wi=na=0;t<l;){var u=Jl[t];Jl[t++]=null;var a=Jl[t];Jl[t++]=null;var n=Jl[t];Jl[t++]=null;var e=Jl[t];if(Jl[t++]=null,a!==null&&n!==null){var f=a.pending;f===null?n.next=n:(n.next=f.next,f.next=n),a.pending=n}e!==0&&Lo(u,n,e)}}function Tf(l,t,u,a){Jl[na++]=l,Jl[na++]=t,Jl[na++]=u,Jl[na++]=a,wi|=a,l.lanes|=a,l=l.alternate,l!==null&&(l.lanes|=a)}function Wi(l,t,u,a){return Tf(l,t,u,a),Ze(l)}function ju(l,t){return Tf(l,null,null,t),Ze(l)}function Lo(l,t,u){l.lanes|=u;var a=l.alternate;a!==null&&(a.lanes|=u);for(var n=!1,e=l.return;e!==null;)e.childLanes|=u,a=e.alternate,a!==null&&(a.childLanes|=u),e.tag===22&&(l=e.stateNode,l===null||l._visibility&1||(n=!0)),l=e,e=e.return;return l.tag===3?(e=l.stateNode,n&&t!==null&&(n=31-Vl(u),l=e.hiddenUpdates,a=l[n],a===null?l[n]=[t]:a.push(t),t.lane=u|536870912),e):null}function Ze(l){if(50<dn)throw dn=0,re=null,Error(E(185));for(var t=l.return;t!==null;)l=t,t=l.return;return l.tag===3?l.stateNode:null}var ea={};function ug(l,t,u,a){this.tag=l,this.key=u,this.sibling=this.child=this.return=this.stateNode=this.type=this.elementType=null,this.index=0,this.refCleanup=this.ref=null,this.pendingProps=t,this.dependencies=this.memoizedState=this.updateQueue=this.memoizedProps=null,this.mode=a,this.subtreeFlags=this.flags=0,this.deletions=null,this.childLanes=this.lanes=0,this.alternate=null}function rl(l,t,u,a){return new ug(l,t,u,a)}function Fi(l){return l=l.prototype,!(!l||!l.isReactComponent)}function Mt(l,t){var u=l.alternate;return u===null?(u=rl(l.tag,t,l.key,l.mode),u.elementType=l.elementType,u.type=l.type,u.stateNode=l.stateNode,u.alternate=l,l.alternate=u):(u.pendingProps=t,u.type=l.type,u.flags=0,u.subtreeFlags=0,u.deletions=null),u.flags=l.flags&1206910976,u.childLanes=l.childLanes,u.lanes=l.lanes,u.child=l.child,u.memoizedProps=l.memoizedProps,u.memoizedState=l.memoizedState,u.updateQueue=l.updateQueue,t=l.dependencies,u.dependencies=t===null?null:{lanes:t.lanes,firstContext:t.firstContext},u.sibling=l.sibling,u.index=l.index,u.ref=l.ref,u.refCleanup=l.refCleanup,u}function po(l,t){l.flags&=1206910978;var u=l.alternate;return u===null?(l.childLanes=0,l.lanes=t,l.child=null,l.subtreeFlags=0,l.memoizedProps=null,l.memoizedState=null,l.updateQueue=null,l.dependencies=null,l.stateNode=null):(l.childLanes=u.childLanes,l.lanes=u.lanes,l.child=u.child,l.subtreeFlags=0,l.deletions=null,l.memoizedProps=u.memoizedProps,l.memoizedState=u.memoizedState,l.updateQueue=u.updateQueue,l.type=u.type,t=u.dependencies,l.dependencies=t===null?null:{lanes:t.lanes,firstContext:t.firstContext}),l}function ze(l,t,u,a,n,e){var f=0;if(a=l,typeof a=="function")Fi(a)&&(f=1);else if(typeof a=="string")f=US(l,u,Tt.current)?26:l==="html"||l==="head"||l==="body"?27:5;else l:switch(a){case Cc:return l=rl(31,u,t,n),l.elementType=Cc,l.lanes=e,l;case ku:return Au(u.children,n,e,t);case fo:f=8,n|=24;break;case Mc:return l=rl(12,u,t,n|2),l.elementType=Mc,l.lanes=e,l;case Uc:return l=rl(13,u,t,n),l.elementType=Uc,l.lanes=e,l;case rc:return l=rl(19,u,t,n),l.elementType=rc,l.lanes=e,l;case Ld:case Hc:return l=n|32,l=rl(30,u,t,l),l.elementType=Hc,l.lanes=e,l.stateNode={autoName:null,paired:null,clones:null,ref:null},l;default:if(typeof a=="object"&&a!==null)switch(a.$$typeof){case dt:f=10;break l;case co:f=9;break l;case Gi:f=11;break l;case Xi:f=14;break l;case pt:f=16,a=null;break l}f=29,u=Error(E(130,l===null?"null":typeof l,"")),a=null}return t=rl(f,u,t,n),t.elementType=l,t.type=a,t.lanes=e,t}function Au(l,t,u,a){return l=rl(7,l,a,t),l.lanes=u,l}function fc(l,t,u){return l=rl(6,l,null,t),l.lanes=u,l}function Ko(l){var t=rl(18,null,null,0);return t.stateNode=l,t}function cc(l,t,u){return t=rl(4,l.children!==null?l.children:[],l.key,t),t.lanes=u,t.stateNode={containerInfo:l.containerInfo,pendingChildren:null,implementation:l.implementation},t}var x1=new WeakMap;function $l(l,t){if(typeof l=="object"&&l!==null){var u=x1.get(l);return u!==void 0?u:(t={value:l,source:t,stack:z1(t)},x1.set(l,t),t)}return{value:l,source:t,stack:z1(t)}}var fa=[],ca=0,xe=null,En=0,wl=[],Wl=0,vu=null,gt=1,St="";function Nt(l,t){fa[ca++]=En,fa[ca++]=xe,xe=l,En=t}function Jo(l,t,u){wl[Wl++]=gt,wl[Wl++]=St,wl[Wl++]=vu,vu=l;var a=gt;l=St;var n=32-Vl(a)-1;a&=~(1<<n),u+=1;var e=32-Vl(t)+n;if(30<e){var f=n-n%5;e=(a&(1<<f)-1).toString(32),a>>=f,n-=f,gt=1<<32-Vl(t)+n|u<<n|a,St=e+l}else gt=1<<e|u<<n|a,St=l}function Ef(l){l.return!==null&&(Nt(l,1),Jo(l,1,0))}function $i(l){for(;l===xe;)xe=fa[--ca],fa[ca]=null,En=fa[--ca],fa[ca]=null;for(;l===vu;)vu=wl[--Wl],wl[Wl]=null,St=wl[--Wl],wl[Wl]=null,gt=wl[--Wl],wl[Wl]=null}function wo(l,t){wl[Wl++]=gt,wl[Wl++]=St,wl[Wl++]=vu,gt=t.id,St=t.overflow,vu=l}var ol=null,J=null,U=!1,lu=null,Il=!1,pc=Error(E(519));function ou(l){var t=Error(E(418,1<arguments.length&&arguments[1]!==void 0&&arguments[1]?"text":"HTML",""));throw sn($l(t,l)),pc}function L1(l){var t=l.stateNode,u=l.type,a=l.memoizedProps;switch(t[ml]=l,t[Rl]=a,u){case"dialog":r("cancel",t),r("close",t);break;case"iframe":case"object":case"embed":r("load",t);break;case"video":case"audio":for(u=0;u<_n.length;u++)r(_n[u],t);break;case"source":r("error",t);break;case"img":case"image":case"link":r("error",t),r("load",t);break;case"details":r("toggle",t);break;case"input":r("invalid",t),_o(t,a.value,a.defaultValue,a.checked,a.defaultChecked,a.type,a.name,!0);break;case"select":r("invalid",t);break;case"textarea":r("invalid",t),No(t,a.value,a.defaultValue,a.children)}u=a.children,typeof u!="string"&&typeof u!="number"&&typeof u!="bigint"||t.textContent===""+u||a.suppressHydrationWarning===!0||rm(t.textContent,u)?(a.popover!=null&&(r("beforetoggle",t),r("toggle",t)),a.onScroll!=null&&r("scroll",t),a.onScrollEnd!=null&&r("scrollend",t),a.onClick!=null&&(t.onclick=ht),t=!0):t=!1,t||ou(l,!0)}function Le(l){for(ol=l.return;ol;)switch(ol.tag){case 5:case 31:case 13:Il=!1;return;case 27:case 3:Il=!0;return;default:ol=ol.return}}function Ju(l){if(l!==ol)return!1;if(!U)return Le(l),U=!0,!1;var t=l.tag,u;if((u=t!==3&&t!==27)&&((u=t===5)&&(u=l.type,u=!(u!=="form"&&u!=="button")||Ui(l.type,l.memoizedProps)),u=!u),u&&J&&ou(l),Le(l),t===13){if(l=l.memoizedState,l=l!==null?l.dehydrated:null,!l)throw Error(E(317));J=jv(l)}else if(t===31){if(l=l.memoizedState,l=l!==null?l.dehydrated:null,!l)throw Error(E(317));J=jv(l)}else t===27?(t=J,gu(l.type)?(l=Ri,Ri=null,J=l):J=t):J=ol?kl(l.stateNode.nextSibling):null;return!0}function Uu(){J=ol=null,U=!1}function ic(){var l=lu;return l!==null&&(Ml===null?Ml=l:Ml.push.apply(Ml,l),lu=null),l}function sn(l){lu===null?lu=[l]:lu.push(l)}var Kc=zt(null),Vu=null,Dt=null;function Ft(l,t,u){w(Kc,t._currentValue),t._currentValue=u}function Ut(l){l._currentValue=Kc.current,hl(Kc)}function be(l,t,u){for(;l!==null;){var a=l.alternate;if((l.childLanes&t)!==t?(l.childLanes|=t,a!==null&&(a.childLanes|=t)):a!==null&&(a.childLanes&t)!==t&&(a.childLanes|=t),l===u)break;l=l.return}}function Jc(l,t,u,a){var n=l.child;for(n!==null&&(n.return=l);n!==null;){var e=n.dependencies;if(e!==null){var f=n.child;e=e.firstContext;l:for(;e!==null;){var c=e;e=n;for(var i=0;i<t.length;i++)if(c.context===t[i]){e.lanes|=u,c=e.alternate,c!==null&&(c.lanes|=u),be(e.return,u,l),a||(f=null);break l}e=c.next}}else if(n.tag===18){if(f=n.return,f===null)throw Error(E(341));f.lanes|=u,e=f.alternate,e!==null&&(e.lanes|=u),be(f,u,l),f=null}else n.tag===13&&n.memoizedState!==null&&n.memoizedState.dehydrated===null?(n.lanes|=u,f=n.alternate,f!==null&&(f.lanes|=u),be(n.return,u,l),f=n.child,f=f!==null?f.sibling:null):f=n.child;if(f!==null)f.return=n;else for(f=n;f!==null;){if(f===l){f=null;break}if(n=f.sibling,n!==null){n.return=f.return,f=n;break}f=f.return}n=f}}function ru(l,t,u,a){l=null;for(var n=t,e=!1;n!==null;){if(!e){if((n.flags&524288)!==0)e=!0;else if((n.flags&262144)!==0)break}if(n.tag===10){var f=n.alternate;if(f===null)throw Error(E(387));if(f=f.memoizedProps,f!==null){var c=n.type;xl(n.pendingProps.value,f.value)||(l!==null?l.push(c):l=[c])}}else if(n===qe.current){if(f=n.alternate,f===null)throw Error(E(387));f.memoizedState.memoizedState!==n.memoizedState.memoizedState&&(l!==null?l.push(ra):l=[ra])}n=n.return}return l!==null&&Jc(t,l,u,a),t.flags|=262144,l!==null}function pe(l){for(l=l.firstContext;l!==null;){if(!xl(l.context._currentValue,l.memoizedValue))return!0;l=l.next}return!1}function Cu(l){Vu=l,Dt=null,l=l.dependencies,l!==null&&(l.firstContext=null)}function dl(l){return Wo(Vu,l)}function ae(l,t){return Vu===null&&Cu(l),Wo(l,t)}function Wo(l,t){var u=t._currentValue;if(t={context:t,memoizedValue:u,next:null},Dt===null){if(l===null)throw Error(E(308));Dt=t,l.dependencies={lanes:0,firstContext:t},l.flags|=524288}else Dt=Dt.next=t;return u}var ag=typeof AbortController<"u"?AbortController:function(){var l=[],t=this.signal={aborted:!1,addEventListener:function(u,a){l.push(a)}};this.abort=function(){t.aborted=!0,l.forEach(function(u){return u()})}},ng=fl.unstable_scheduleCallback,eg=fl.unstable_NormalPriority,ul={$$typeof:dt,Consumer:null,Provider:null,_currentValue:null,_currentValue2:null,_threadCount:0};function Ii(){return{controller:new ag,data:new Map,refCount:0}}function Gn(l){l.refCount--,l.refCount===0&&ng(eg,function(){l.controller.abort()})}function p1(l,t){if((l.pendingLanes&4194048)!==0){var u=l.transitionTypes;for(u===null&&(u=l.transitionTypes=[]),l=0;l<t.length;l++){var a=t[l];u.indexOf(a)===-1&&u.push(a)}}}var ka=null;function fg(l){var t=l.transitionTypes;return l.transitionTypes=null,t}var an=null,wc=0,Hu=0,ma=null;function cg(l,t){if(an===null){var u=an=[];wc=0,Hu=N0(),ma={status:"pending",value:void 0,then:function(a){u.push(a)}}}return wc++,t.then(K1,K1),t}function K1(){if(--wc===0&&(ka=null,an!==null)){ma!==null&&(ma.status="fulfilled");var l=an;an=null,Hu=0,ma=null;for(var t=0;t<l.length;t++)(0,l[t])()}}function ig(l,t){var u=[],a={status:"pending",value:null,reason:null,then:function(n){u.push(n)}};return l.then(function(){a.status="fulfilled",a.value=t;for(var n=0;n<u.length;n++)(0,u[n])(t)},function(n){for(a.status="rejected",a.reason=n,n=0;n<u.length;n++)(0,u[n])(void 0)}),a}var J1=A.S;A.S=function(l,t){if(mm=Ql(),typeof t=="object"&&t!==null&&typeof t.then=="function"&&cg(l,t),ka!==null)for(var u=Da;u!==null;)p1(u,ka),u=u.next;if(u=l.types,u!==null){for(var a=Da;a!==null;)p1(a,u),a=a.next;if(Hu!==0){a=ka,a===null&&(a=ka=[]);for(var n=0;n<u.length;n++){var e=u[n];a.indexOf(e)===-1&&a.push(e)}}}J1!==null&&J1(l,t)};var Nu=zt(null);function ki(){var l=Nu.current;return l!==null?l:x.pooledCache}function Oe(l,t){t===null?w(Nu,Nu.current):w(Nu,t.pool)}function Fo(){var l=ki();return l===null?null:{parent:ul._currentValue,pool:l}}var Ba=Error(E(460)),Pi=Error(E(474)),sf=Error(E(542)),Ke={then:function(){}};function w1(l){return l=l.status,l==="fulfilled"||l==="rejected"}function $o(l,t,u){switch(u=l[u],u===void 0?l.push(t):u!==t&&(t.then(ht,ht),t=u),t.status){case"fulfilled":return t.value;case"rejected":throw l=t.reason,F1(l),l===void 0&&!("reason"in t)?Error(E(600)):l;default:if(typeof t.status=="string")t.then(ht,ht);else{if(l=x,l!==null&&100<l.shellSuspendCounter)throw Error(E(482));l=t,l.status="pending",l.then(function(a){if(t.status==="pending"){var n=t;n.status="fulfilled",n.value=a}},function(a){if(t.status==="pending"){var n=t;n.status="rejected",n.reason=a}})}switch(t.status){case"fulfilled":return t.value;case"rejected":throw l=t.reason,F1(l),l}throw Du=t,Ba}}function su(l){try{var t=l._init;return t(l._payload)}catch(u){throw u!==null&&typeof u=="object"&&typeof u.then=="function"?(Du=u,Ba):u}}var Du=null;function W1(){if(Du===null)throw Error(E(459));var l=Du;return Du=null,l}function F1(l){if(l===Ba||l===sf)throw Error(E(483))}var da=null,zn=0;function ne(l){var t=zn;return zn+=1,da===null&&(da=[]),$o(da,l,t)}function Zt(l,t){t=t.props.ref,l.ref=t!==void 0?t:null}function ee(l,t){throw t.$$typeof===xd?Error(E(525)):(l=Object.prototype.toString.call(t),Error(E(31,l==="[object Object]"?"object with keys {"+Object.keys(t).join(", ")+"}":l)))}function Io(l){function t(m,v){if(l){var d=m.deletions;d===null?(m.deletions=[v],m.flags|=16):d.push(v)}}function u(m,v){if(!l)return null;for(;v!==null;)t(m,v),v=v.sibling;return null}function a(m){for(var v=new Map;m!==null;)m.key===null?v.set(m.index,m):v.set(m.key,m),m=m.sibling;return v}function n(m,v){return m=Mt(m,v),m.index=0,m.sibling=null,m}function e(m,v,d){return m.index=d,l?(d=m.alternate,d!==null?(d=d.index,d<v?(m.flags|=2,v):d):(m.flags|=134217730,v)):(m.flags|=1048576,v)}function f(m){return l&&m.alternate===null&&(m.flags|=134217730),m}function c(m,v,d,S){return v===null||v.tag!==6?(v=fc(d,m.mode,S),v.return=m,v):(v=n(v,d),v.return=m,v)}function i(m,v,d,S){var z=d.type;return z===ku?(m=g(m,v,d.props.children,S,d.key),Zt(m,d),m):v!==null&&(v.elementType===z||typeof z=="object"&&z!==null&&z.$$typeof===pt&&su(z)===v.type)?(v=n(v,d.props),Zt(v,d),v.return=m,v):(v=ze(d.type,d.key,d.props,null,m.mode,S),Zt(v,d),v.return=m,v)}function o(m,v,d,S){return v===null||v.tag!==4||v.stateNode.containerInfo!==d.containerInfo||v.stateNode.implementation!==d.implementation?(v=cc(d,m.mode,S),v.return=m,v):(v=n(v,d.children||[]),v.return=m,v)}function g(m,v,d,S,z){return v===null||v.tag!==7?(v=Au(d,m.mode,S,z),v.return=m,v):(v=n(v,d),v.return=m,v)}function T(m,v,d){if(typeof v=="string"&&v!==""||typeof v=="number"||typeof v=="bigint")return v=fc(""+v,m.mode,d),v.return=m,v;if(typeof v=="object"&&v!==null){switch(v.$$typeof){case Fn:return d=ze(v.type,v.key,v.props,null,m.mode,d),Zt(d,v),d.return=m,d;case Fa:return v=cc(v,m.mode,d),v.return=m,v;case pt:return v=su(v),T(m,v,d)}if($a(v)||La(v))return v=Au(v,m.mode,d,null),v.return=m,v;if(typeof v.then=="function")return T(m,ne(v),d);if(v.$$typeof===dt)return T(m,ae(m,v),d);ee(m,v)}return null}function y(m,v,d,S){var z=v!==null?v.key:null;if(typeof d=="string"&&d!==""||typeof d=="number"||typeof d=="bigint")return z!==null?null:c(m,v,""+d,S);if(typeof d=="object"&&d!==null){switch(d.$$typeof){case Fn:return d.key===z?i(m,v,d,S):null;case Fa:return d.key===z?o(m,v,d,S):null;case pt:return d=su(d),y(m,v,d,S)}if($a(d)||La(d))return z!==null?null:g(m,v,d,S,null);if(typeof d.then=="function")return y(m,v,ne(d),S);if(d.$$typeof===dt)return y(m,v,ae(m,d),S);ee(m,d)}return null}function h(m,v,d,S,z){if(typeof S=="string"&&S!==""||typeof S=="number"||typeof S=="bigint")return m=m.get(d)||null,c(v,m,""+S,z);if(typeof S=="object"&&S!==null){switch(S.$$typeof){case Fn:return m=m.get(S.key===null?d:S.key)||null,i(v,m,S,z);case Fa:return m=m.get(S.key===null?d:S.key)||null,o(v,m,S,z);case pt:return S=su(S),h(m,v,d,S,z)}if($a(S)||La(S))return m=m.get(d)||null,g(v,m,S,z,null);if(typeof S.then=="function")return h(m,v,d,ne(S),z);if(S.$$typeof===dt)return h(m,v,d,ae(v,S),z);ee(v,S)}return null}function s(m,v,d,S){for(var z=null,H=null,O=v,_=v=0,ll=null;O!==null&&_<d.length;_++){O.index>_?(ll=O,O=null):ll=O.sibling;var Y=y(m,O,d[_],S);if(Y===null){O===null&&(O=ll);break}l&&O&&Y.alternate===null&&t(m,O),v=e(Y,v,_),H===null?z=Y:H.sibling=Y,H=Y,O=ll}if(_===d.length)return u(m,O),U&&Nt(m,_),z;if(O===null){for(;_<d.length;_++)O=T(m,d[_],S),O!==null&&(v=e(O,v,_),H===null?z=O:H.sibling=O,H=O);return U&&Nt(m,_),z}for(O=a(O);_<d.length;_++)ll=h(O,m,_,d[_],S),ll!==null&&(l&&(Y=ll.alternate,Y!==null&&O.delete(Y.key===null?_:Y.key)),v=e(ll,v,_),H===null?z=ll:H.sibling=ll,H=ll);return l&&O.forEach(function(Qt){return t(m,Qt)}),U&&Nt(m,_),z}function b(m,v,d,S){if(d==null)throw Error(E(151));for(var z=null,H=null,O=v,_=v=0,ll=null,Y=d.next();O!==null&&!Y.done;_++,Y=d.next()){O.index>_?(ll=O,O=null):ll=O.sibling;var Qt=y(m,O,Y.value,S);if(Qt===null){O===null&&(O=ll);break}l&&O&&Qt.alternate===null&&t(m,O),v=e(Qt,v,_),H===null?z=Qt:H.sibling=Qt,H=Qt,O=ll}if(Y.done)return u(m,O),U&&Nt(m,_),z;if(O===null){for(;!Y.done;_++,Y=d.next())Y=T(m,Y.value,S),Y!==null&&(v=e(Y,v,_),H===null?z=Y:H.sibling=Y,H=Y);return U&&Nt(m,_),z}for(O=a(O);!Y.done;_++,Y=d.next())Y=h(O,m,_,Y.value,S),Y!==null&&(l&&(ll=Y.alternate,ll!==null&&O.delete(ll.key===null?_:ll.key)),v=e(Y,v,_),H===null?z=Y:H.sibling=Y,H=Y);return l&&O.forEach(function(vd){return t(m,vd)}),U&&Nt(m,_),z}function D(m,v,d,S){if(typeof d=="object"&&d!==null&&d.type===ku&&d.key===null&&d.props.ref===void 0&&(d=d.props.children),typeof d=="object"&&d!==null){switch(d.$$typeof){case Fn:l:{for(var z=d.key;v!==null;){if(v.key===z){if(z=d.type,z===ku){if(v.tag===7){u(m,v.sibling),S=n(v,d.props.children),Zt(S,d),S.return=m,m=S;break l}}else if(v.elementType===z||typeof z=="object"&&z!==null&&z.$$typeof===pt&&su(z)===v.type){u(m,v.sibling),S=n(v,d.props),Zt(S,d),S.return=m,m=S;break l}u(m,v);break}else t(m,v);v=v.sibling}d.type===ku?(S=Au(d.props.children,m.mode,S,d.key),Zt(S,d),S.return=m,m=S):(S=ze(d.type,d.key,d.props,null,m.mode,S),Zt(S,d),S.return=m,m=S)}return f(m);case Fa:l:{for(z=d.key;v!==null;){if(v.key===z)if(v.tag===4&&v.stateNode.containerInfo===d.containerInfo&&v.stateNode.implementation===d.implementation){u(m,v.sibling),S=n(v,d.children||[]),S.return=m,m=S;break l}else{u(m,v);break}else t(m,v);v=v.sibling}S=cc(d,m.mode,S),S.return=m,m=S}return f(m);case pt:return d=su(d),D(m,v,d,S)}if($a(d))return s(m,v,d,S);if(La(d)){if(z=La(d),typeof z!="function")throw Error(E(150));return d=z.call(d),b(m,v,d,S)}if(typeof d.then=="function")return D(m,v,ne(d),S);if(d.$$typeof===dt)return D(m,v,ae(m,d),S);ee(m,d)}return typeof d=="string"&&d!==""||typeof d=="number"||typeof d=="bigint"?(d=""+d,v!==null&&v.tag===6?(u(m,v.sibling),S=n(v,d),S.return=m,m=S):(u(m,v),S=fc(d,m.mode,S),S.return=m,m=S),f(m)):u(m,v)}return function(m,v,d,S){try{zn=0;var z=D(m,v,d,S);return da=null,z}catch(O){if(O===Ba||O===sf)throw O;var H=rl(29,O,null,m.mode);return H.lanes=S,H.return=m,H}}}var Ru=Io(!0),ko=Io(!1),Kt=!1;function l0(l){l.updateQueue={baseState:l.memoizedState,firstBaseUpdate:null,lastBaseUpdate:null,shared:{pending:null,lanes:0,hiddenCallbacks:null},callbacks:null}}function Wc(l,t){l=l.updateQueue,t.updateQueue===l&&(t.updateQueue={baseState:l.baseState,firstBaseUpdate:l.firstBaseUpdate,lastBaseUpdate:l.lastBaseUpdate,shared:l.shared,callbacks:null})}function tu(l){return{lane:l,tag:0,payload:null,callback:null,next:null}}function uu(l,t,u){var a=l.updateQueue;if(a===null)return null;if(a=a.shared,(q&2)!==0){var n=a.pending;return n===null?t.next=t:(t.next=n.next,n.next=t),a.pending=t,t=Ze(l),Lo(l,null,u),t}return Tf(l,a,t,u),Ze(l)}function nn(l,t,u){if(t=t.updateQueue,t!==null&&(t=t.shared,(u&4194048)!==0)){var a=t.lanes;a&=l.pendingLanes,u|=a,t.lanes=u,go(l,u)}}function vc(l,t){var u=l.updateQueue,a=l.alternate;if(a!==null&&(a=a.updateQueue,u===a)){var n=null,e=null;if(u=u.firstBaseUpdate,u!==null){do{var f={lane:u.lane,tag:u.tag,payload:u.payload,callback:null,next:null};e===null?n=e=f:e=e.next=f,u=u.next}while(u!==null);e===null?n=e=t:e=e.next=t}else n=e=t;u={baseState:a.baseState,firstBaseUpdate:n,lastBaseUpdate:e,shared:a.shared,callbacks:a.callbacks},l.updateQueue=u;return}l=u.lastBaseUpdate,l===null?u.firstBaseUpdate=t:l.next=t,u.lastBaseUpdate=t}var Fc=!1;function en(){if(Fc){var l=ma;if(l!==null)throw l}}function fn(l,t,u,a){Fc=!1;var n=l.updateQueue;Kt=!1;var e=n.firstBaseUpdate,f=n.lastBaseUpdate,c=n.shared.pending;if(c!==null){n.shared.pending=null;var i=c,o=i.next;i.next=null,f===null?e=o:f.next=o,f=i;var g=l.alternate;g!==null&&(g=g.updateQueue,c=g.lastBaseUpdate,c!==f&&(c===null?g.firstBaseUpdate=o:c.next=o,g.lastBaseUpdate=i))}if(e!==null){var T=n.baseState;f=0,g=o=i=null,c=e;do{var y=c.lane&-536870913,h=y!==c.lane;if(h?(R&y)===y:(a&y)===y){y!==0&&y===Hu&&(Fc=!0),g!==null&&(g=g.next={lane:0,tag:c.tag,payload:c.payload,callback:null,next:null});l:{var s=l,b=c;y=t;var D=u;switch(b.tag){case 1:if(s=b.payload,typeof s=="function"){T=s.call(D,T,y);break l}T=s;break l;case 3:s.flags=s.flags&-65537|128;case 0:if(s=b.payload,y=typeof s=="function"?s.call(D,T,y):s,y==null)break l;T=L({},T,y);break l;case 2:Kt=!0}}y=c.callback,y!==null&&(l.flags|=64,h&&(l.flags|=8192),h=n.callbacks,h===null?n.callbacks=[y]:h.push(y))}else h={lane:y,tag:c.tag,payload:c.payload,callback:c.callback,next:null},g===null?(o=g=h,i=T):g=g.next=h,f|=y;if(c=c.next,c===null){if(c=n.shared.pending,c===null)break;h=c,c=h.next,h.next=null,n.lastBaseUpdate=h,n.shared.pending=null}}while(!0);g===null&&(i=T),n.baseState=i,n.firstBaseUpdate=o,n.lastBaseUpdate=g,e===null&&(n.shared.lanes=0),du|=f,l.lanes=f,l.memoizedState=T}}function Po(l,t){if(typeof l!="function")throw Error(E(191,l));l.call(t)}function ly(l,t){var u=l.callbacks;if(u!==null)for(l.callbacks=null,l=0;l<u.length;l++)Po(u[l],t)}var yu=zt(null),Je=zt(0);function $1(l,t){l=Bt,w(Je,l),w(yu,t),Bt=l|t.baseLanes}function $c(){w(Je,Bt),w(yu,yu.current)}function t0(){Bt=Je.current,hl(yu),hl(Je)}var Tl=zt(null),zl=null;function au(l){var t=l.alternate;w(gl,gl.current&1),w(Tl,l),zl===null&&(t===null||yu.current!==null||t.memoizedState!==null)&&(zl=l)}function Ic(l){w(gl,gl.current),w(Tl,l),zl===null&&(zl=l)}function ty(l){l.tag===22?(w(gl,gl.current),w(Tl,l),zl===null&&(zl=l)):nu()}function nu(){w(gl,gl.current),w(Tl,Tl.current)}function ql(l){hl(Tl),zl===l&&(zl=null),hl(gl)}var gl=zt(0);function bn(l,t){w(Tl,Tl.current),w(gl,t)}function u0(l){hl(gl),hl(Tl),zl===l&&(zl=null)}function we(l){for(var t=l;t!==null;){if(t.tag===13){var u=t.memoizedState;if(u!==null&&(u=u.dehydrated,u===null||Hi(u)||r0(u)))return t}else if(t.tag===19&&t.memoizedProps.revealOrder!=="independent"){if((t.flags&128)!==0)return t}else if(t.child!==null){t.child.return=t,t=t.child;continue}if(t===l)break;for(;t.sibling===null;){if(t.return===null||t.return===l)return null;t=t.return}t.sibling.return=t.return,t=t.sibling}return null}var Ht=0,M=null,Z=null,tl=null,We=!1,ha=!1,Yu=!1,Fe=0,On=0,ga=null,vg=0;function I(){throw Error(E(321))}function a0(l,t){if(t===null)return!1;for(var u=0;u<t.length&&u<l.length;u++)if(!xl(l[u],t[u]))return!1;return!0}function n0(l,t,u,a,n,e){return Ht=e,M=t,t.memoizedState=null,t.updateQueue=null,t.lanes=0,A.H=l===null||l.memoizedState===null?Ry:Yy,Yu=!1,e=u(a,n),Yu=!1,ha&&(e=ay(t,u,a,n)),uy(l),e}function uy(l){A.H=$e;var t=Z!==null&&Z.next!==null;if(Ht=0,tl=Z=M=null,We=!1,On=0,ga=null,t)throw Error(E(300));l===null||al||(l=l.dependencies,l!==null&&pe(l)&&(al=!0))}function ay(l,t,u,a){M=l;var n=0;do{if(ha&&(ga=null),On=0,ha=!1,25<=n)throw Error(E(301));if(n+=1,tl=Z=null,l.updateQueue!=null){var e=l.updateQueue;e.lastEffect=null,e.events=null,e.stores=null,e.memoCache!=null&&(e.memoCache.index=0)}A.H=Tg,e=t(u,a)}while(ha);return e}function og(){var l=A.H,t=l.useState()[0];return t=typeof t.then=="function"?Xn(t):t,l=l.useState()[0],(Z!==null?Z.memoizedState:null)!==l&&(M.flags|=1024),t}function e0(){var l=Fe!==0;return Fe=0,l}function f0(l,t,u){t.updateQueue=l.updateQueue,t.flags&=-2053,l.lanes&=~u}function c0(l){if(We){for(l=l.memoizedState;l!==null;){var t=l.queue;t!==null&&(t.pending=null),l=l.next}We=!1}Ht=0,tl=Z=M=null,ha=!1,On=Fe=0,ga=null}function Al(){var l={memoizedState:null,baseState:null,baseQueue:null,queue:null,next:null};return tl===null?M.memoizedState=tl=l:tl=tl.next=l,tl}function P(){if(Z===null){var l=M.alternate;l=l!==null?l.memoizedState:null}else l=Z.next;var t=tl===null?M.memoizedState:tl.next;if(t!==null)tl=t,Z=l;else{if(l===null)throw M.alternate===null?Error(E(467)):Error(E(310));Z=l,l={memoizedState:Z.memoizedState,baseState:Z.baseState,baseQueue:Z.baseQueue,queue:Z.queue,next:null},tl===null?M.memoizedState=tl=l:tl=tl.next=l}return tl}function zf(){return{lastEffect:null,events:null,stores:null,memoCache:null}}function Xn(l){var t=On;return On+=1,ga===null&&(ga=[]),l=$o(ga,l,t),t=M,(tl===null?t.memoizedState:tl.next)===null&&(t=t.alternate,A.H=t===null||t.memoizedState===null?Ry:Yy),l}function bf(l){if(l!==null&&typeof l=="object"){if(typeof l.then=="function")return Xn(l);if(l.$$typeof===Kd)return;if(l.$$typeof===dt)return dl(l)}throw Error(E(438,String(l)))}function i0(l){var t=null,u=M.updateQueue;if(u!==null&&(t=u.memoCache),t==null){var a=M.alternate;a!==null&&(a=a.updateQueue,a!==null&&(a=a.memoCache,a!=null&&(t={data:a.data.map(function(n){return n.slice()}),index:0})))}if(t==null&&(t={data:[],index:0}),u===null&&(u=zf(),M.updateQueue=u),u.memoCache=t,u=t.data[t.index],u===void 0)for(u=t.data[t.index]=Array(l),a=0;a<l;a++)u[a]=pd;return t.index++,u}function Rt(l,t){return typeof t=="function"?t(l):t}function _e(l){var t=P();return v0(t,Z,l)}function v0(l,t,u){var a=l.queue;if(a===null)throw Error(E(311));a.lastRenderedReducer=u;var n=l.baseQueue,e=a.pending;if(e!==null){if(n!==null){var f=n.next;n.next=e.next,e.next=f}t.baseQueue=n=e,a.pending=null}if(e=l.baseState,n===null)l.memoizedState=e;else{t=n.next;var c=f=null,i=null,o=t,g=!1;do{var T=o.lane&-536870913;if(T!==o.lane?(R&T)===T:(Ht&T)===T){var y=o.revertLane;if(y===0)i!==null&&(i=i.next={lane:0,revertLane:0,gesture:null,action:o.action,hasEagerState:o.hasEagerState,eagerState:o.eagerState,next:null}),T===Hu&&(g=!0);else if((Ht&y)===y){o=o.next,y===Hu&&(g=!0);continue}else T={lane:0,revertLane:o.revertLane,gesture:null,action:o.action,hasEagerState:o.hasEagerState,eagerState:o.eagerState,next:null},i===null?(c=i=T,f=e):i=i.next=T,M.lanes|=y,du|=y;T=o.action,Yu&&u(e,T),e=o.hasEagerState?o.eagerState:u(e,T)}else y={lane:T,revertLane:o.revertLane,gesture:o.gesture,action:o.action,hasEagerState:o.hasEagerState,eagerState:o.eagerState,next:null},i===null?(c=i=y,f=e):i=i.next=y,M.lanes|=T,du|=T;o=o.next}while(o!==null&&o!==t);if(i===null?f=e:i.next=c,!xl(e,l.memoizedState)&&(al=!0,g&&(u=ma,u!==null)))throw u;l.memoizedState=e,l.baseState=f,l.baseQueue=i,a.lastRenderedState=e}return n===null&&(a.lanes=0),[l.memoizedState,a.dispatch]}function oc(l){var t=P(),u=t.queue;if(u===null)throw Error(E(311));u.lastRenderedReducer=l;var a=u.dispatch,n=u.pending,e=t.memoizedState;if(n!==null){u.pending=null;var f=n=n.next;do e=l(e,f.action),f=f.next;while(f!==n);xl(e,t.memoizedState)||(al=!0),t.memoizedState=e,t.baseQueue===null&&(t.baseState=e),u.lastRenderedState=e}return[e,a]}function ny(l,t,u){var a=M,n=P(),e=U;if(e){if(u===void 0)throw Error(E(407));u=u()}else u=t();var f=!xl((Z||n).memoizedState,u);if(f&&(n.memoizedState=u,al=!0),n=n.queue,o0(cy.bind(null,a,n,l),[l]),l=n.getSnapshot!==t||f||tl!==null&&(tl.memoizedState.tag&1)!==0,Oa(l?9:8,{destroy:void 0},fy.bind(null,a,n,u,t),null),l){if(a.flags|=2048,x===null)throw Error(E(349));e||(Ht&127)!==0||ey(a,t,u)}return u}function ey(l,t,u){l.flags|=16384,l={getSnapshot:t,value:u},t=M.updateQueue,t===null?(t=zf(),M.updateQueue=t,t.stores=[l]):(u=t.stores,u===null?t.stores=[l]:u.push(l))}function fy(l,t,u,a){t.value=u,t.getSnapshot=a,iy(t)&&vy(l)}function cy(l,t,u){return u(function(){iy(t)&&vy(l)})}function iy(l){var t=l.getSnapshot;l=l.value;try{var u=t();return!xl(l,u)}catch{return!0}}function vy(l){var t=ju(l,2);t!==null&&Cl(t,l,2)}function kc(l){var t=Al();if(typeof l=="function"){var u=l;if(l=u(),Yu){wt(!0);try{u()}finally{wt(!1)}}}return t.memoizedState=t.baseState=l,t.queue={pending:null,lanes:0,dispatch:null,lastRenderedReducer:Rt,lastRenderedState:l},t}function oy(l,t,u,a){return l.baseState=u,v0(l,Z,typeof a=="function"?a:Rt)}function yg(l,t,u,a,n){if(_f(l))throw Error(E(485));if(l=t.action,l!==null){var e={payload:n,action:l,next:null,isTransition:!0,status:"pending",value:null,reason:null,listeners:[],then:function(f){e.listeners.push(f)}};A.T!==null?u(!0):e.isTransition=!1,a(e),u=t.pending,u===null?(e.next=t.pending=e,yy(t,e)):(e.next=u.next,t.pending=u.next=e)}}function yy(l,t){var u=t.action,a=t.payload,n=l.state;if(t.isTransition){var e=A.T,f={};f.types=e!==null?e.types:null,A.T=f;try{var c=u(n,a),i=A.S;i!==null&&i(f,c),I1(l,t,c)}catch(o){Pc(l,t,o)}finally{e!==null&&f.types!==null&&(e.types=f.types),A.T=e}}else try{e=u(n,a),I1(l,t,e)}catch(o){Pc(l,t,o)}}function I1(l,t,u){u!==null&&typeof u=="object"&&typeof u.then=="function"?u.then(function(a){k1(l,t,a)},function(a){return Pc(l,t,a)}):k1(l,t,u)}function k1(l,t,u){t.status="fulfilled",t.value=u,my(t),l.state=u,t=l.pending,t!==null&&(u=t.next,u===t?l.pending=null:(u=u.next,t.next=u,yy(l,u)))}function Pc(l,t,u){var a=l.pending;if(l.pending=null,a!==null){a=a.next;do t.status="rejected",t.reason=u,my(t),t=t.next;while(t!==a)}l.action=null}function my(l){l=l.listeners;for(var t=0;t<l.length;t++)(0,l[t])()}function dy(l,t){return t}function P1(l,t){if(U){var u=x.formState;if(u!==null){l:{var a=M;if(U){if(J){t:{for(var n=J,e=Il;n.nodeType!==8;){if(!e){n=null;break t}if(n=kl(n.nextSibling),n===null){n=null;break t}}e=n.data,n=e==="F!"||e==="F"?n:null}if(n){J=kl(n.nextSibling),a=n.data==="F!";break l}}ou(a)}a=!1}a&&(t=u[0])}}return u=Al(),u.memoizedState=u.baseState=t,a={pending:null,lanes:0,dispatch:null,lastRenderedReducer:dy,lastRenderedState:t},u.queue=a,u=ry.bind(null,M,a),a.dispatch=u,a=kc(!1),e=h0.bind(null,M,!1,a.queue),a=Al(),n={state:t,dispatch:null,action:l,pending:null},a.queue=n,u=yg.bind(null,M,n,e,u),n.dispatch=u,a.memoizedState=l,[t,u,!1]}function lv(l){var t=P();return hy(t,Z,l)}function hy(l,t,u){if(t=v0(l,t,dy)[0],l=_e(Rt)[0],typeof t=="object"&&t!==null&&typeof t.then=="function")try{var a=Xn(t)}catch(f){throw f===Ba?sf:f}else a=t;t=P();var n=t.queue,e=n.dispatch;return u!==t.memoizedState&&(M.flags|=2048,Oa(9,{destroy:void 0},mg.bind(null,n,u),null)),[a,e,l]}function mg(l,t){l.action=t}function tv(l){var t=P(),u=Z;if(u!==null)return hy(t,u,l);P(),t=t.memoizedState,u=P();var a=u.queue.dispatch;return u.memoizedState=l,[t,a,!1]}function Oa(l,t,u,a){return l={tag:l,create:u,deps:a,inst:t,next:null},t=M.updateQueue,t===null&&(t=zf(),M.updateQueue=t),u=t.lastEffect,u===null?t.lastEffect=l.next=l:(a=u.next,u.next=l,l.next=a,t.lastEffect=l),l}function gy(){return P().memoizedState}function Ae(l,t,u,a){var n=Al();M.flags|=l,n.memoizedState=Oa(1|t,{destroy:void 0},u,a===void 0?null:a)}function Of(l,t,u,a){var n=P();a=a===void 0?null:a;var e=n.memoizedState.inst;Z!==null&&a!==null&&a0(a,Z.memoizedState.deps)?n.memoizedState=Oa(t,e,u,a):(M.flags|=l,n.memoizedState=Oa(1|t,e,u,a))}function uv(l,t){Ae(8390656,8,l,t)}function o0(l,t){Of(2048,8,l,t)}function dg(l){M.flags|=4;var t=M.updateQueue;if(t===null)t=zf(),M.updateQueue=t,t.events=[l];else{var u=t.events;u===null?t.events=[l]:u.push(l)}}function Sy(l){var t=P().memoizedState;return dg({ref:t,nextImpl:l}),function(){if((q&2)!==0)throw Error(E(440));return t.impl.apply(void 0,arguments)}}function Ty(l,t){return Of(4,2,l,t)}function Ey(l,t){return Of(4,4,l,t)}function sy(l,t){if(typeof t=="function"){l=l();var u=t(l);return function(){typeof u=="function"?u():t(null)}}if(t!=null)return l=l(),t.current=l,function(){t.current=null}}function zy(l,t,u){u=u!=null?u.concat([l]):null,Of(4,4,sy.bind(null,t,l),u)}function y0(){}function by(l,t){var u=P();t=t===void 0?null:t;var a=u.memoizedState;return t!==null&&a0(t,a[1])?a[0]:(u.memoizedState=[l,t],l)}function Oy(l,t){var u=P();t=t===void 0?null:t;var a=u.memoizedState;if(t!==null&&a0(t,a[1]))return a[0];if(a=l(),Yu){wt(!0);try{l()}finally{wt(!1)}}return u.memoizedState=[a,t],a}function m0(l,t,u){return u===void 0||(Ht&1073741824)!==0&&(R&261930)===0?l.memoizedState=t:(l.memoizedState=u,l=hm(),M.lanes|=l,du|=l,u)}function _y(l,t,u,a){return xl(u,t)?u:yu.current!==null?(l=m0(l,u,a),xl(l,t)||(al=!0),l):(Ht&106)===0||(Ht&1073741824)!==0&&(R&261930)===0?(al=!0,l.memoizedState=u):(l=hm(),M.lanes|=l,du|=l,t)}function Ay(l,t,u,a,n){var e=G.p;G.p=e!==0&&8>e?e:8;var f=A.T,c={};c.types=f!==null?f.types:null,A.T=c,h0(l,!1,t,u);try{var i=n(),o=A.S;if(o!==null&&o(c,i),i!==null&&typeof i=="object"&&typeof i.then=="function"){var g=ig(i,a);cn(l,t,g,Zl(l))}else cn(l,t,a,Zl(l))}catch(T){cn(l,t,{then:function(){},status:"rejected",reason:T},Zl())}finally{G.p=e,f!==null&&c.types!==null&&(f.types=c.types),A.T=f}}function hg(){}function li(l,t,u,a){if(l.tag!==5)throw Error(E(476));var n=Ny(l).queue;Ay(l,n,t,_u,u===null?hg:function(){return Dy(l),u(a)})}function Ny(l){var t=l.memoizedState;if(t!==null)return t;t={memoizedState:_u,baseState:_u,baseQueue:null,queue:{pending:null,lanes:0,dispatch:null,lastRenderedReducer:Rt,lastRenderedState:_u},next:null};var u={};return t.next={memoizedState:u,baseState:u,baseQueue:null,queue:{pending:null,lanes:0,dispatch:null,lastRenderedReducer:Rt,lastRenderedState:u},next:null},l.memoizedState=t,l=l.alternate,l!==null&&(l.memoizedState=t),t}function Dy(l){var t=Ny(l);t.next===null&&(t=l.alternate.memoizedState),cn(l,t.next.queue,{},Zl())}function d0(){return dl(ra)}function My(){return P().memoizedState}function Uy(){return P().memoizedState}function gg(l){for(var t=l.return;t!==null;){switch(t.tag){case 24:case 3:var u=Zl();l=tu(u);var a=uu(t,l,u);a!==null&&(Cl(a,t,u),nn(a,t,u)),t={cache:Ii()},l.payload=t;return}t=t.return}}function Sg(l,t,u){var a=Zl();u={lane:a,revertLane:0,gesture:null,action:u,hasEagerState:!1,eagerState:null,next:null},_f(l)?Cy(t,u):(u=Wi(l,t,u,a),u!==null&&(Cl(u,l,a),Hy(u,t,a)))}function ry(l,t,u){var a=Zl();cn(l,t,u,a)}function cn(l,t,u,a){var n={lane:a,revertLane:0,gesture:null,action:u,hasEagerState:!1,eagerState:null,next:null};if(_f(l))Cy(t,n);else{var e=l.alternate;if(l.lanes===0&&(e===null||e.lanes===0)&&(e=t.lastRenderedReducer,e!==null))try{var f=t.lastRenderedState,c=e(f,u);if(n.hasEagerState=!0,n.eagerState=c,xl(c,f))return Tf(l,t,n,0),x===null&&Sf(),!1}catch{}if(u=Wi(l,t,n,a),u!==null)return Cl(u,l,a),Hy(u,t,a),!0}return!1}function h0(l,t,u,a){if(a={lane:2,revertLane:N0(),gesture:null,action:a,hasEagerState:!1,eagerState:null,next:null},_f(l)){if(t)throw Error(E(479))}else t=Wi(l,u,a,2),t!==null&&Cl(t,l,2)}function _f(l){var t=l.alternate;return l===M||t!==null&&t===M}function Cy(l,t){ha=We=!0;var u=l.pending;u===null?t.next=t:(t.next=u.next,u.next=t),l.pending=t}function Hy(l,t,u){if((u&4194048)!==0){var a=t.lanes;a&=l.pendingLanes,u|=a,t.lanes=u,go(l,u)}}var $e={readContext:dl,use:bf,useCallback:I,useContext:I,useEffect:I,useImperativeHandle:I,useLayoutEffect:I,useInsertionEffect:I,useMemo:I,useReducer:I,useRef:I,useState:I,useDebugValue:I,useDeferredValue:I,useTransition:I,useSyncExternalStore:I,useId:I,useHostTransitionStatus:I,useFormState:I,useActionState:I,useOptimistic:I,useMemoCache:I,useCacheRefresh:I,useEffectEvent:I},Ry={readContext:dl,use:bf,useCallback:function(l,t){return Al().memoizedState=[l,t===void 0?null:t],l},useContext:dl,useEffect:uv,useImperativeHandle:function(l,t,u){u=u!=null?u.concat([l]):null,Ae(4194308,4,sy.bind(null,t,l),u)},useLayoutEffect:function(l,t){return Ae(4194308,4,l,t)},useInsertionEffect:function(l,t){Ae(4,2,l,t)},useMemo:function(l,t){var u=Al();t=t===void 0?null:t;var a=l();if(Yu){wt(!0);try{l()}finally{wt(!1)}}return u.memoizedState=[a,t],a},useReducer:function(l,t,u){var a=Al();if(u!==void 0){var n=u(t);if(Yu){wt(!0);try{u(t)}finally{wt(!1)}}}else n=t;return a.memoizedState=a.baseState=n,l={pending:null,lanes:0,dispatch:null,lastRenderedReducer:l,lastRenderedState:n},a.queue=l,l=l.dispatch=Sg.bind(null,M,l),[a.memoizedState,l]},useRef:function(l){var t=Al();return l={current:l},t.memoizedState=l},useState:function(l){l=kc(l);var t=l.queue,u=ry.bind(null,M,t);return t.dispatch=u,[l.memoizedState,u]},useDebugValue:y0,useDeferredValue:function(l,t){var u=Al();return m0(u,l,t)},useTransition:function(){var l=kc(!1);return l=Ay.bind(null,M,l.queue,!0,!1),Al().memoizedState=l,[!1,l]},useSyncExternalStore:function(l,t,u){var a=M,n=Al();if(U){if(u===void 0)throw Error(E(407));u=u()}else{if(u=t(),x===null)throw Error(E(349));(R&127)!==0||ey(a,t,u)}n.memoizedState=u;var e={value:u,getSnapshot:t};return n.queue=e,uv(cy.bind(null,a,e,l),[l]),a.flags|=2048,Oa(9,{destroy:void 0},fy.bind(null,a,e,u,t),null),u},useId:function(){var l=Al(),t=x.identifierPrefix;if(U){var u=St,a=gt;u=(a&~(1<<32-Vl(a)-1)).toString(32)+u,t="_"+t+"R_"+u,u=Fe++,0<u&&(t+="H"+u.toString(32)),t+="_"}else u=vg++,t="_"+t+"r_"+u.toString(32)+"_";return l.memoizedState=t},useHostTransitionStatus:d0,useFormState:P1,useActionState:P1,useOptimistic:function(l){var t=Al();t.memoizedState=t.baseState=l;var u={pending:null,lanes:0,dispatch:null,lastRenderedReducer:null,lastRenderedState:null};return t.queue=u,t=h0.bind(null,M,!0,u),u.dispatch=t,[l,t]},useMemoCache:i0,useCacheRefresh:function(){return Al().memoizedState=gg.bind(null,M)},useEffectEvent:function(l){var t=Al(),u={impl:l};return t.memoizedState=u,function(){if((q&2)!==0)throw Error(E(440));return u.impl.apply(void 0,arguments)}}},Yy={readContext:dl,use:bf,useCallback:by,useContext:dl,useEffect:o0,useImperativeHandle:zy,useInsertionEffect:Ty,useLayoutEffect:Ey,useMemo:Oy,useReducer:_e,useRef:gy,useState:function(){return _e(Rt)},useDebugValue:y0,useDeferredValue:function(l,t){var u=P();return _y(u,Z.memoizedState,l,t)},useTransition:function(){var l=_e(Rt)[0],t=P().memoizedState;return[typeof l=="boolean"?l:Xn(l),t]},useSyncExternalStore:ny,useId:My,useHostTransitionStatus:d0,useFormState:lv,useActionState:lv,useOptimistic:function(l,t){var u=P();return oy(u,Z,l,t)},useMemoCache:i0,useCacheRefresh:Uy,useEffectEvent:Sy},Tg={readContext:dl,use:bf,useCallback:by,useContext:dl,useEffect:o0,useImperativeHandle:zy,useInsertionEffect:Ty,useLayoutEffect:Ey,useMemo:Oy,useReducer:oc,useRef:gy,useState:function(){return oc(Rt)},useDebugValue:y0,useDeferredValue:function(l,t){var u=P();return Z===null?m0(u,l,t):_y(u,Z.memoizedState,l,t)},useTransition:function(){var l=oc(Rt)[0],t=P().memoizedState;return[typeof l=="boolean"?l:Xn(l),t]},useSyncExternalStore:ny,useId:My,useHostTransitionStatus:d0,useFormState:tv,useActionState:tv,useOptimistic:function(l,t){var u=P();return Z!==null?oy(u,Z,l,t):(u.baseState=l,[l,u.queue.dispatch])},useMemoCache:i0,useCacheRefresh:Uy,useEffectEvent:Sy};function yc(l,t,u,a){t=l.memoizedState,u=u(a,t),u=u==null?t:L({},t,u),l.memoizedState=u,l.lanes===0&&(l.updateQueue.baseState=u)}var ti={enqueueSetState:function(l,t,u){l=l._reactInternals;var a=Zl(),n=tu(a);n.payload=t,u!=null&&(n.callback=u),t=uu(l,n,a),t!==null&&(Cl(t,l,a),nn(t,l,a))},enqueueReplaceState:function(l,t,u){l=l._reactInternals;var a=Zl(),n=tu(a);n.tag=1,n.payload=t,u!=null&&(n.callback=u),t=uu(l,n,a),t!==null&&(Cl(t,l,a),nn(t,l,a))},enqueueForceUpdate:function(l,t){l=l._reactInternals;var u=Zl(),a=tu(u);a.tag=2,t!=null&&(a.callback=t),t=uu(l,a,u),t!==null&&(Cl(t,l,u),nn(t,l,u))}};function av(l,t,u,a,n,e,f){return l=l.stateNode,typeof l.shouldComponentUpdate=="function"?l.shouldComponentUpdate(a,e,f):t.prototype&&t.prototype.isPureReactComponent?!Tn(u,a)||!Tn(n,e):!0}function nv(l,t,u,a){l=t.state,typeof t.componentWillReceiveProps=="function"&&t.componentWillReceiveProps(u,a),typeof t.UNSAFE_componentWillReceiveProps=="function"&&t.UNSAFE_componentWillReceiveProps(u,a),t.state!==l&&ti.enqueueReplaceState(t,t.state,null)}function Bu(l,t){var u=t;if("ref"in t){u={};for(var a in t)a!=="ref"&&(u[a]=t[a])}if(l=l.defaultProps){u===t&&(u=L({},u));for(var n in l)u[n]===void 0&&(u[n]=l[n])}return u}function By(l){Ve(l)}function qy(l){console.error(l)}function Gy(l){Ve(l)}function Ie(l,t){try{var u=l.onUncaughtError;u(t.value,{componentStack:t.stack})}catch(a){setTimeout(function(){throw a})}}function ev(l,t,u){try{var a=l.onCaughtError;a(u.value,{componentStack:u.stack,errorBoundary:t.tag===1?t.stateNode:null})}catch(n){setTimeout(function(){throw n})}}function ui(l,t,u){return u=tu(u),u.tag=3,u.payload={element:null},u.callback=function(){Ie(l,t)},u}function Xy(l){return l=tu(l),l.tag=3,l}function Qy(l,t,u,a){var n=u.type.getDerivedStateFromError;if(typeof n=="function"){var e=a.value;l.payload=function(){return n(e)},l.callback=function(){ev(t,u,a)}}var f=u.stateNode;f!==null&&typeof f.componentDidCatch=="function"&&(l.callback=function(){ev(t,u,a),typeof n!="function"&&(eu===null?eu=new Set([this]):eu.add(this));var c=a.stack;this.componentDidCatch(a.value,{componentStack:c!==null?c:""})})}function Eg(l,t,u,a,n){if(u.flags|=32768,a!==null&&typeof a=="object"&&typeof a.then=="function"){if(t=u.alternate,t!==null&&ru(t,u,n,!0),u=Tl.current,u!==null){switch(u.tag){case 31:case 13:case 19:return zl===null?ef():u.alternate===null&&k===0&&(k=3),u.flags&=-257,u.flags|=65536,u.lanes=n,a===Ke?u.flags|=16384:(t=u.updateQueue,t===null?u.updateQueue=new Set([a]):t.add(a),Ec(l,a,n)),!1;case 22:return u.flags|=65536,a===Ke?u.flags|=16384:(t=u.updateQueue,t===null?(t={transitions:null,markerInstances:null,retryQueue:new Set([a])},u.updateQueue=t):(u=t.retryQueue,u===null?t.retryQueue=new Set([a]):u.add(a)),Ec(l,a,n)),!1}throw Error(E(435,u.tag))}return Ec(l,a,n),ef(),!1}if(U)return t=Tl.current,t!==null?((t.flags&65536)===0&&(t.flags|=256),t.flags|=65536,t.lanes=n,a!==pc&&(l=Error(E(422),{cause:a}),sn($l(l,u)))):(a!==pc&&(t=Error(E(423),{cause:a}),sn($l(t,u))),l=l.current.alternate,l.flags|=65536,n&=-n,l.lanes|=n,a=$l(a,u),n=ui(l.stateNode,a,n),vc(l,n),k!==4&&(k=2)),!1;var e=Error(E(520),{cause:a});if(e=$l(e,u),mn===null?mn=[e]:mn.push(e),k!==4&&(k=2),t===null)return!0;a=$l(a,u),u=t;do{switch(u.tag){case 3:return u.flags|=65536,l=n&-n,u.lanes|=l,l=ui(u.stateNode,a,l),vc(u,l),!1;case 1:if(t=u.type,e=u.stateNode,(u.flags&128)===0&&(typeof t.getDerivedStateFromError=="function"||e!==null&&typeof e.componentDidCatch=="function"&&(eu===null||!eu.has(e))))return u.flags|=65536,n&=-n,u.lanes|=n,n=Xy(n),Qy(n,l,u,a),vc(u,n),!1;break;case 22:if(u.memoizedState!==null)return u.flags|=65536,!1}u=u.return}while(u!==null);return!1}var g0=Error(E(461)),al=!1;function nl(l,t,u,a){t.child=l===null?ko(t,null,u,a):Ru(t,l.child,u,a)}function fv(l,t,u,a,n){u=u.render;var e=t.ref;if("ref"in a){var f={};for(var c in a)c!=="ref"&&(f[c]=a[c])}else f=a;return Cu(t),a=n0(l,t,u,f,e,n),c=e0(),l!==null&&!al?(f0(l,t,n),Yt(l,t,n)):(U&&c&&Ef(t),t.flags|=1,nl(l,t,a,n),t.child)}function cv(l,t,u,a,n){if(l===null){var e=u.type;return typeof e=="function"&&!Fi(e)&&e.defaultProps===void 0&&u.compare===null?(t.tag=15,t.type=e,jy(l,t,e,a,n)):(l=ze(u.type,null,a,t,t.mode,n),l.ref=t.ref,l.return=t,t.child=l)}if(e=l.child,!T0(l,n)){var f=e.memoizedProps;if(u=u.compare,u=u!==null?u:Tn,u(f,a)&&l.ref===t.ref)return Yt(l,t,n)}return t.flags|=1,l=Mt(e,a),l.ref=t.ref,l.return=t,t.child=l}function jy(l,t,u,a,n){if(l!==null){var e=l.memoizedProps;if(Tn(e,a)&&l.ref===t.ref)if(al=!1,t.pendingProps=a=e,T0(l,n))(l.flags&131072)!==0&&(al=!0);else return t.lanes=l.lanes,Yt(l,t,n)}return ai(l,t,u,a,n)}function Vy(l,t,u,a){var n=a.children,e=l!==null?l.memoizedState:null;if(l===null&&t.stateNode===null&&(t.stateNode={_visibility:1,_pendingMarkers:null,_retryCache:null,_transitions:null}),a.mode==="hidden"){if((t.flags&128)!==0){if(e=e!==null?e.baseLanes|u:u,l!==null){for(a=t.child=l.child,n=0;a!==null;)n=n|a.lanes|a.childLanes,a=a.sibling;a=n&~e}else a=0,t.child=null;return iv(l,t,e,u,a)}if((u&536870912)!==0)t.memoizedState={baseLanes:0,cachePool:null},l!==null&&Oe(t,e!==null?e.cachePool:null),e!==null?$1(t,e):$c(),ty(t);else return a=t.lanes=536870912,iv(l,t,e!==null?e.baseLanes|u:u,u,a)}else e!==null?(Oe(t,e.cachePool),$1(t,e),nu(),t.memoizedState=null):(l!==null&&Oe(t,null),$c(),nu());return nl(l,t,n,u),t.child}function vn(l,t){return l!==null&&l.tag===22||t.stateNode!==null||(t.stateNode={_visibility:1,_pendingMarkers:null,_retryCache:null,_transitions:null}),t.sibling}function iv(l,t,u,a,n){var e=ki();return e=e===null?null:{parent:ul._currentValue,pool:e},t.memoizedState={baseLanes:u,cachePool:e},l!==null&&Oe(t,null),$c(),ty(t),l!==null&&ru(l,t,a,!0),t.childLanes=n,null}function Ne(l,t){return t=Af({mode:t.mode,children:t.children},l.mode),t.ref=l.ref,l.child=t,t.return=l,t}function vv(l,t,u){return Ru(t,l.child,null,u),l=Ne(t,t.pendingProps),l.flags|=2,ql(t),t.memoizedState=null,l}function sg(l,t,u){var a=t.pendingProps,n=(t.flags&128)!==0;if(t.flags&=-129,l===null){if(U){if(a.mode==="hidden")return l=Ne(t,a),t.lanes=536870912,l.memoizedState={baseLanes:0,cachePool:null},vn(null,l);if(Ic(t),(l=J)?(l=Vm(l,Il),l=l!==null&&l.data==="&"?l:null,l!==null&&(t.memoizedState={dehydrated:l,treeContext:vu!==null?{id:gt,overflow:St}:null,retryLane:536870912,hydrationErrors:null},u=Ko(l),u.return=t,t.child=u,ol=t,J=null)):l=null,l===null)throw ou(t);return t.lanes=536870912,null}return Ne(t,a)}var e=l.memoizedState;if(e!==null){var f=e.dehydrated;if(Ic(t),n)if(t.flags&256)t.flags&=-257,t=vv(l,t,u);else if(t.memoizedState!==null)t.child=l.child,t.flags|=128,t=null;else throw Error(E(558));else if(al||ru(l,t,u,!1),n=(u&l.childLanes)!==0,al||n){if(yu.current===null){if(a=x,a!==null&&(f=So(a,u),f!==0&&f!==e.retryLane))throw e.retryLane=f,ju(l,f),Cl(a,l,f),g0;ef()}t=vv(l,t,u)}else l=e.treeContext,J=kl(f.nextSibling),ol=t,U=!0,lu=null,Il=!1,l!==null&&wo(t,l),t=Ne(t,a),t.flags|=134221824;return t}return l=Mt(l.child,{mode:a.mode,children:a.children}),l.ref=t.ref,t.child=l,l.return=t,l}function Wu(l,t){var u=t.ref;if(u===null)l!==null&&l.ref!==null&&(t.flags|=4194816);else{if(typeof u!="function"&&typeof u!="object")throw Error(E(284));(l===null||l.ref!==u)&&(t.flags|=4194816)}}function ai(l,t,u,a,n){return Cu(t),u=n0(l,t,u,a,void 0,n),a=e0(),l!==null&&!al?(f0(l,t,n),Yt(l,t,n)):(U&&a&&Ef(t),t.flags|=1,nl(l,t,u,n),t.child)}function ov(l,t,u,a,n,e){return Cu(t),t.updateQueue=null,u=ay(t,a,u,n),uy(l),a=e0(),l!==null&&!al?(f0(l,t,e),Yt(l,t,e)):(U&&a&&Ef(t),t.flags|=1,nl(l,t,u,e),t.child)}function yv(l,t,u,a,n){if(Cu(t),t.stateNode===null){var e=ea,f=u.contextType;typeof f=="object"&&f!==null&&(e=dl(f)),e=new u(a,e),t.memoizedState=e.state!==null&&e.state!==void 0?e.state:null,e.updater=ti,t.stateNode=e,e._reactInternals=t,e=t.stateNode,e.props=a,e.state=t.memoizedState,e.refs={},l0(t),f=u.contextType,e.context=typeof f=="object"&&f!==null?dl(f):ea,e.state=t.memoizedState,f=u.getDerivedStateFromProps,typeof f=="function"&&(yc(t,u,f,a),e.state=t.memoizedState),typeof u.getDerivedStateFromProps=="function"||typeof e.getSnapshotBeforeUpdate=="function"||typeof e.UNSAFE_componentWillMount!="function"&&typeof e.componentWillMount!="function"||(f=e.state,typeof e.componentWillMount=="function"&&e.componentWillMount(),typeof e.UNSAFE_componentWillMount=="function"&&e.UNSAFE_componentWillMount(),f!==e.state&&ti.enqueueReplaceState(e,e.state,null),fn(t,a,e,n),en(),e.state=t.memoizedState),typeof e.componentDidMount=="function"&&(t.flags|=4194308),a=!0}else if(l===null){e=t.stateNode;var c=t.memoizedProps,i=Bu(u,c);e.props=i;var o=e.context,g=u.contextType;f=ea,typeof g=="object"&&g!==null&&(f=dl(g));var T=u.getDerivedStateFromProps;g=typeof T=="function"||typeof e.getSnapshotBeforeUpdate=="function",c=t.pendingProps!==c,g||typeof e.UNSAFE_componentWillReceiveProps!="function"&&typeof e.componentWillReceiveProps!="function"||(c||o!==f)&&nv(t,e,a,f),Kt=!1;var y=t.memoizedState;e.state=y,fn(t,a,e,n),en(),o=t.memoizedState,c||y!==o||Kt?(typeof T=="function"&&(yc(t,u,T,a),o=t.memoizedState),(i=Kt||av(t,u,i,a,y,o,f))?(g||typeof e.UNSAFE_componentWillMount!="function"&&typeof e.componentWillMount!="function"||(typeof e.componentWillMount=="function"&&e.componentWillMount(),typeof e.UNSAFE_componentWillMount=="function"&&e.UNSAFE_componentWillMount()),typeof e.componentDidMount=="function"&&(t.flags|=4194308)):(typeof e.componentDidMount=="function"&&(t.flags|=4194308),t.memoizedProps=a,t.memoizedState=o),e.props=a,e.state=o,e.context=f,a=i):(typeof e.componentDidMount=="function"&&(t.flags|=4194308),a=!1)}else{e=t.stateNode,Wc(l,t),f=t.memoizedProps,g=Bu(u,f),e.props=g,T=t.pendingProps,y=e.context,o=u.contextType,i=ea,typeof o=="object"&&o!==null&&(i=dl(o)),c=u.getDerivedStateFromProps,(o=typeof c=="function"||typeof e.getSnapshotBeforeUpdate=="function")||typeof e.UNSAFE_componentWillReceiveProps!="function"&&typeof e.componentWillReceiveProps!="function"||(f!==T||y!==i)&&nv(t,e,a,i),Kt=!1,y=t.memoizedState,e.state=y,fn(t,a,e,n),en();var h=t.memoizedState;f!==T||y!==h||Kt||l!==null&&l.dependencies!==null&&pe(l.dependencies)?(typeof c=="function"&&(yc(t,u,c,a),h=t.memoizedState),(g=Kt||av(t,u,g,a,y,h,i)||l!==null&&l.dependencies!==null&&pe(l.dependencies))?(o||typeof e.UNSAFE_componentWillUpdate!="function"&&typeof e.componentWillUpdate!="function"||(typeof e.componentWillUpdate=="function"&&e.componentWillUpdate(a,h,i),typeof e.UNSAFE_componentWillUpdate=="function"&&e.UNSAFE_componentWillUpdate(a,h,i)),typeof e.componentDidUpdate=="function"&&(t.flags|=4),typeof e.getSnapshotBeforeUpdate=="function"&&(t.flags|=1024)):(typeof e.componentDidUpdate!="function"||f===l.memoizedProps&&y===l.memoizedState||(t.flags|=4),typeof e.getSnapshotBeforeUpdate!="function"||f===l.memoizedProps&&y===l.memoizedState||(t.flags|=1024),t.memoizedProps=a,t.memoizedState=h),e.props=a,e.state=h,e.context=i,a=g):(typeof e.componentDidUpdate!="function"||f===l.memoizedProps&&y===l.memoizedState||(t.flags|=4),typeof e.getSnapshotBeforeUpdate!="function"||f===l.memoizedProps&&y===l.memoizedState||(t.flags|=1024),a=!1)}return e=a,Wu(l,t),a=(t.flags&128)!==0,e||a?(e=t.stateNode,u=a&&typeof u.getDerivedStateFromError!="function"?null:e.render(),t.flags|=1,l!==null&&a?(t.child=Ru(t,l.child,null,n),t.child=Ru(t,null,u,n)):nl(l,t,u,n),t.memoizedState=e.state,l=t.child):l=Yt(l,t,n),l}function mv(l,t,u,a){return Uu(),t.flags|=256,nl(l,t,u,a),t.child}var ni={dehydrated:null,treeContext:null,retryLane:0,hydrationErrors:null};function ei(l){return{baseLanes:l,cachePool:Fo()}}function fi(l,t,u){return l=l!==null?l.childLanes&~u:0,t&&(l|=Xl),l}function Zy(l,t,u){var a=t.pendingProps,n=!1,e=(t.flags&128)!==0,f;if((f=e)||(f=l!==null&&l.memoizedState===null?!1:(gl.current&2)!==0),f&&(n=!0,t.flags&=-129),f=(t.flags&32)!==0,t.flags&=-33,l===null){if(U){if(n?au(t):nu(),(l=J)?(l=Vm(l,Il),l=l!==null&&l.data!=="&"?l:null,l!==null&&(t.memoizedState={dehydrated:l,treeContext:vu!==null?{id:gt,overflow:St}:null,retryLane:536870912,hydrationErrors:null},u=Ko(l),u.return=t,t.child=u,ol=t,J=null)):l=null,l===null)throw ou(t);return r0(l)?t.lanes=32:t.lanes=536870912,null}return e=a.children,a=a.fallback,n?(nu(),n=t.mode,e=Af({mode:"hidden",children:e},n),a=Au(a,n,u,null),e.return=t,a.return=t,e.sibling=a,t.child=e,a=t.child,a.memoizedState=ei(u),a.childLanes=fi(l,f,u),t.memoizedState=ni,vn(null,a)):(au(t),S0(t,e))}var c=l.memoizedState;if(c!==null){var i=c.dehydrated;if(i!==null)return zg(l,t,e,f,a,i,c,u)}return n?(nu(),n=a.fallback,e=t.mode,c=l.child,i=c.sibling,a=Mt(c,{mode:"hidden",children:a.children}),a.subtreeFlags=c.subtreeFlags&1206910976,i!==null?n=Mt(i,n):(n=Au(n,e,u,null),n.flags|=2),n.return=t,a.return=t,a.sibling=n,t.child=a,vn(null,a),a=t.child,n=l.child.memoizedState,n===null?n=ei(u):(e=n.cachePool,e!==null?(c=ul._currentValue,e=e.parent!==c?{parent:c,pool:c}:e):e=Fo(),n={baseLanes:n.baseLanes|u,cachePool:e}),a.memoizedState=n,a.childLanes=fi(l,f,u),t.memoizedState=ni,vn(l.child,a)):(au(t),u=l.child,l=u.sibling,u=Mt(u,{mode:"visible",children:a.children}),u.return=t,u.sibling=null,l!==null&&(f=t.deletions,f===null?(t.deletions=[l],t.flags|=16):f.push(l)),t.child=u,t.memoizedState=null,u)}function S0(l,t){return t=Af({mode:"visible",children:t},l.mode),t.return=l,l.child=t}function Af(l,t){return l=rl(22,l,null,t),l.lanes=0,l}function fe(l,t,u){return Ru(t,l.child,null,u),l=S0(t,t.pendingProps.children),l.flags|=2,t.memoizedState=null,l}function zg(l,t,u,a,n,e,f,c){if(u)return t.flags&256?(au(t),t.flags&=-257,fe(l,t,c)):t.memoizedState!==null?(nu(),t.child=l.child,t.flags|=128,null):(nu(),e=n.fallback,f=t.mode,n=Af({mode:"visible",children:n.children},f),e=Au(e,f,c,null),e.flags|=2,n.return=t,e.return=t,n.sibling=e,t.child=n,Ru(t,l.child,null,c),n=t.child,n.memoizedState=ei(c),n.childLanes=fi(l,a,c),t.memoizedState=ni,vn(null,n));if(au(t),r0(e)){if(a=e.nextSibling&&e.nextSibling.dataset,a)var i=a.dgst;return a=i,a!==""&&(n=Error(E(419)),n.stack="",n.digest=a,sn({value:n,source:null,stack:null})),fe(l,t,c)}if(al||ru(l,t,c,!1),a=(c&l.childLanes)!==0,al||a){if(yu.current!==null)return fe(l,t,c);if(a=x,a!==null&&(n=So(a,c),n!==0&&n!==f.retryLane))throw f.retryLane=n,ju(l,n),Cl(a,l,n),g0;return Hi(e)||ef(),fe(l,t,c)}return Hi(e)?(t.flags|=192,t.child=l.child,null):(l=f.treeContext,J=kl(e.nextSibling),ol=t,U=!0,lu=null,Il=!1,l!==null&&wo(t,l),t=S0(t,n.children),t.flags|=134221824,t)}function dv(l,t,u){l.lanes|=t;var a=l.alternate;a!==null&&(a.lanes|=t),be(l.return,t,u)}function hv(l){for(var t=null;l!==null;){var u=l.alternate;u!==null&&we(u)===null&&(t=l),l=l.sibling}return t}function ce(l,t,u,a,n,e){var f=l.memoizedState;f===null?l.memoizedState={isBackwards:t,rendering:null,renderingStartTime:0,last:a,tail:u,tailMode:n,treeForkCount:e}:(f.isBackwards=t,f.rendering=null,f.renderingStartTime=0,f.last=a,f.tail=u,f.tailMode=n,f.treeForkCount=e)}function mc(l){var t=l.child;for(l.child=null;t!==null;){var u=t.sibling;t.sibling=l.child,l.child=t,t=u}}function ci(l,t,u){var a=t.pendingProps,n=a.revealOrder,e=a.tail;a=a.children;var f=gl.current;if(t.flags&128)return bn(t,f),null;var c=(f&2)!==0;if(c?(f=f&1|2,t.flags|=128):f&=1,bn(t,f),n==="backwards"&&l!==null?(mc(l),nl(l,t,a,u),mc(l)):nl(l,t,a,u),a=U?En:0,!c&&l!==null&&(l.flags&128)!==0)l:for(l=t.child;l!==null;){if(l.tag===13)l.memoizedState!==null&&dv(l,u,t);else if(l.tag===19)dv(l,u,t);else if(l.child!==null){l.child.return=l,l=l.child;continue}if(l===t)break l;for(;l.sibling===null;){if(l.return===null||l.return===t)break l;l=l.return}l.sibling.return=l.return,l=l.sibling}switch(n){case"backwards":u=hv(t.child),u===null?(n=t.child,t.child=null):(n=u.sibling,u.sibling=null,mc(t)),ce(t,!0,n,null,e,a);break;case"unstable_legacy-backwards":for(u=null,n=t.child,t.child=null;n!==null;){if(l=n.alternate,l!==null&&we(l)===null){t.child=n;break}l=n.sibling,n.sibling=u,u=n,n=l}ce(t,!0,u,null,e,a);break;case"together":ce(t,!1,null,null,void 0,a);break;case"independent":t.memoizedState=null;break;default:u=hv(t.child),u===null?(n=t.child,t.child=null):(n=u.sibling,u.sibling=null),ce(t,!1,n,u,e,a)}return t.child}function gv(l,t,u){var a=t.pendingProps;return Ft(t,t.type,a.value),nl(l,t,a.children,u),t.child}function Yt(l,t,u){if(l!==null&&(t.dependencies=l.dependencies),du|=t.lanes,(u&t.childLanes)===0)if(l!==null){if(ru(l,t,u,!1),(u&t.childLanes)===0)return null}else return null;if(l!==null&&t.child!==l.child)throw Error(E(153));if(t.child!==null){for(l=t.child,u=Mt(l,l.pendingProps),t.child=u,u.return=t;l.sibling!==null;)l=l.sibling,u=u.sibling=Mt(l,l.pendingProps),u.return=t;u.sibling=null}return t.child}function T0(l,t){return(l.lanes&t)!==0?!0:(l=l.dependencies,!!(l!==null&&pe(l)))}function bg(l,t,u){switch(t.tag){case 3:Ge(t,t.stateNode.containerInfo),Ft(t,ul,l.memoizedState.cache),Uu();break;case 27:case 5:Bc(t);break;case 4:Ge(t,t.stateNode.containerInfo);break;case 10:Ft(t,t.type,t.memoizedProps.value);break;case 31:if(t.memoizedState!==null)return t.flags|=128,Ic(t),null;break;case 13:var a=t.memoizedState;if(a!==null){if(a.dehydrated!==null)return au(t),t.flags|=128,null;a=ru(l,t,u,!1);var n=t.child.childLanes;return a||(u&n)!==0?Zy(l,t,u):(au(t),l=Yt(l,t,u),l!==null?l.sibling:null)}au(t);break;case 19:if(t.flags&128)return ci(l,t,u);if(n=(l.flags&128)!==0,a=(u&t.childLanes)!==0,a||(ru(l,t,u,!1),a=(u&t.childLanes)!==0),n){if(a)return ci(l,t,u);t.flags|=128}if(n=t.memoizedState,n!==null&&(n.rendering=null,n.tail=null,n.lastEffect=null),bn(t,gl.current),a)break;return null;case 22:return t.lanes=0,Vy(l,t,u,t.pendingProps);case 24:Ft(t,ul,l.memoizedState.cache)}return Yt(l,t,u)}function xy(l,t,u){if(l!==null)if(l.memoizedProps!==t.pendingProps)al=!0;else{if(!T0(l,u)&&(t.flags&128)===0)return al=!1,bg(l,t,u);al=(l.flags&131072)!==0}else al=!1,U&&(t.flags&1048576)!==0&&Jo(t,En,t.index);switch(t.lanes=0,t.tag){case 16:l:{var a=t.pendingProps;if(l=su(t.elementType),t.type=l,typeof l=="function")Fi(l)?(a=Bu(l,a),t.tag=1,t=yv(null,t,l,a,u)):(t.tag=0,t=ai(null,t,l,a,u));else{if(l!=null){var n=l.$$typeof;if(n===Gi){t.tag=11,t=fv(null,t,l,a,u);break l}else if(n===Xi){t.tag=14,t=cv(null,t,l,a,u);break l}else if(n===dt){t.tag=10,t.type=l,t=gv(null,t,u);break l}}throw t=Rc(l)||l,Error(E(306,t,""))}}return t;case 0:return ai(l,t,t.type,t.pendingProps,u);case 1:return a=t.type,n=Bu(a,t.pendingProps),yv(l,t,a,n,u);case 3:l:{if(Ge(t,t.stateNode.containerInfo),l===null)throw Error(E(387));a=t.pendingProps;var e=t.memoizedState;n=e.element,Wc(l,t),fn(t,a,null,u);var f=t.memoizedState;if(a=f.cache,Ft(t,ul,a),a!==e.cache&&Jc(t,[ul],u,!0),en(),a=f.element,e.isDehydrated)if(e={element:a,isDehydrated:!1,cache:f.cache},t.updateQueue.baseState=e,t.memoizedState=e,t.flags&256){t=mv(l,t,a,u);break l}else if(a!==n){n=$l(Error(E(424)),t),sn(n),t=mv(l,t,a,u);break l}else for(l=t.stateNode.containerInfo,l.nodeType===9?l=l.body:l=l.nodeName==="HTML"?l.ownerDocument.body:l,J=kl(l.firstChild),ol=t,U=!0,lu=null,Il=!0,u=ko(t,null,a,u),t.child=u;u;)u.flags=u.flags&-3|134221824,u=u.sibling;else{if(Uu(),a===n){t=Yt(l,t,u);break l}nl(l,t,a,u)}t=t.child}return t;case 26:return Wu(l,t),l===null?(u=xv(t.type,null,t.pendingProps,null))?t.memoizedState=u:U||(t.stateNode=Hm(t.type,t.pendingProps,Pt.current,t)):t.memoizedState=xv(t.type,l.memoizedProps,t.pendingProps,l.memoizedState),null;case 27:return Bc(t),l===null&&U&&(a=t.stateNode=Zm(t.type,t.pendingProps,Pt.current),ol=t,Il=!0,n=J,gu(t.type)?(Ri=n,J=kl(a.firstChild)):J=n),nl(l,t,t.pendingProps.children,u),Wu(l,t),l===null&&(t.flags|=4194304),t.child;case 5:return l===null&&U&&((n=a=J)&&(a=dS(a,t.type,t.pendingProps,Il),a!==null?(t.stateNode=a,ol=t,J=kl(a.firstChild),Il=!1,n=!0):n=!1),n||ou(t)),Bc(t),n=t.type,e=t.pendingProps,f=l!==null?l.memoizedProps:null,a=e.children,Ui(n,e)?a=null:f!==null&&Ui(n,f)&&(t.flags|=32),t.memoizedState!==null&&(n=n0(l,t,og,null,null,u),ra._currentValue=n),Wu(l,t),nl(l,t,a,u),t.child;case 6:return l===null&&U&&((l=u=J)&&(u=hS(u,t.pendingProps,Il),u!==null?(t.stateNode=u,ol=t,J=null,l=!0):l=!1),l||ou(t)),null;case 13:return Zy(l,t,u);case 4:return Ge(t,t.stateNode.containerInfo),a=t.pendingProps,l===null?t.child=Ru(t,null,a,u):nl(l,t,a,u),t.child;case 11:return fv(l,t,t.type,t.pendingProps,u);case 7:return a=t.pendingProps,Wu(l,t),nl(l,t,a,u),t.child;case 8:return nl(l,t,t.pendingProps.children,u),t.child;case 12:return nl(l,t,t.pendingProps.children,u),t.child;case 10:return gv(l,t,u);case 9:return n=t.type._context,a=t.pendingProps.children,Cu(t),n=dl(n),a=a(n),t.flags|=1,nl(l,t,a,u),t.child;case 14:return cv(l,t,t.type,t.pendingProps,u);case 15:return jy(l,t,t.type,t.pendingProps,u);case 19:return ci(l,t,u);case 31:return sg(l,t,u);case 22:return Vy(l,t,u,t.pendingProps);case 24:return Cu(t),a=dl(ul),l===null?(n=ki(),n===null&&(n=x,e=Ii(),n.pooledCache=e,e.refCount++,e!==null&&(n.pooledCacheLanes|=u),n=e),t.memoizedState={parent:a,cache:n},l0(t),Ft(t,ul,n)):((l.lanes&u)!==0&&(Wc(l,t),fn(t,null,null,u),en()),n=l.memoizedState,e=t.memoizedState,n.parent!==a?(n={parent:a,cache:a},t.memoizedState=n,t.lanes===0&&(t.memoizedState=t.updateQueue.baseState=n),Ft(t,ul,a)):(a=e.cache,Ft(t,ul,a),a!==n.cache&&Jc(t,[ul],u,!0))),nl(l,t,t.pendingProps.children,u),t.child;case 30:return t.stateNode===null&&(t.stateNode={autoName:null,paired:null,clones:null,ref:null}),a=t.pendingProps,a.name!=null&&a.name!=="auto"?t.flags|=l===null?18882560:18874368:U&&Ef(t),l!==null&&l.memoizedProps.name!==a.name?t.flags|=4194816:Wu(l,t),nl(l,t,a.children,u),t.child;case 29:throw t.pendingProps}throw Error(E(156,t.tag))}function At(l){l.flags|=4}function dc(l,t,u,a,n){var e;if((e=(l.mode&32)!==0)&&(e=u===null?Kv(t,a):Kv(t,a)&&(a.src!==u.src||a.srcSet!==u.srcSet)),e){if(l.flags|=16777216,(n&335544128)===n)if(l.stateNode.complete)l.flags|=8192;else if(Tm())l.flags|=8192;else throw Du=Ke,Pi}else l.flags&=-16777217}function Sv(l,t){if(t.type!=="stylesheet"||(t.state.loading&4)!==0)l.flags&=-16777217;else if(l.flags|=16777216,!Km(t))if(Tm())l.flags|=8192;else throw Du=Ke,Pi}function ie(l,t){t!==null&&(l.flags|=4),l.flags&16384&&(t=l.tag!==22?mo():536870912,l.lanes|=t,_a|=t)}function Ka(l,t){if(!U)switch(l.tailMode){case"visible":break;case"collapsed":for(var u=l.tail,a=null;u!==null;)u.alternate!==null&&(a=u),u=u.sibling;a===null?t||l.tail===null?l.tail=null:l.tail.sibling=null:a.sibling=null;break;default:for(t=l.tail,u=null;t!==null;)t.alternate!==null&&(u=t),t=t.sibling;u===null?l.tail=null:u.sibling=null}}function K(l){var t=l.alternate!==null&&l.alternate.child===l.child,u=0,a=0;if(t)for(var n=l.child;n!==null;)u|=n.lanes|n.childLanes,a|=n.subtreeFlags&1206910976,a|=n.flags&1206910976,n.return=l,n=n.sibling;else for(n=l.child;n!==null;)u|=n.lanes|n.childLanes,a|=n.subtreeFlags,a|=n.flags,n.return=l,n=n.sibling;return l.subtreeFlags|=a,l.childLanes=u,t}function Og(l,t,u){var a=t.pendingProps;switch($i(t),t.tag){case 16:case 15:case 0:case 11:case 7:case 8:case 12:case 9:case 14:return K(t),null;case 1:return K(t),null;case 3:return u=t.stateNode,a=null,l!==null&&(a=l.memoizedState.cache),t.memoizedState.cache!==a&&(t.flags|=2048),Ut(ul),sa(),u.pendingContext&&(u.context=u.pendingContext,u.pendingContext=null),(l===null||l.child===null)&&(Ju(t)?At(t):l===null||l.memoizedState.isDehydrated&&(t.flags&256)===0||(t.flags|=1024,ic())),K(t),null;case 26:var n=t.type,e=t.memoizedState;return l===null?(At(t),e!==null?(K(t),Sv(t,e)):(K(t),dc(t,n,null,a,u))):e?e!==l.memoizedState?(At(t),K(t),Sv(t,e)):(K(t),t.flags&=-16777217):(l=l.memoizedProps,l!==a&&At(t),K(t),dc(t,n,l,a,u)),null;case 27:if(Xe(t),u=Pt.current,n=t.type,l!==null&&t.stateNode!=null)l.memoizedProps!==a&&At(t);else{if(!a){if(t.stateNode===null)throw Error(E(166));return K(t),t.subtreeFlags&=-33554433,null}l=Tt.current,Ju(t)?L1(t,l):(l=Zm(n,a,u),t.stateNode=l,At(t))}return K(t),t.subtreeFlags&=-33554433,null;case 5:if(Xe(t),n=t.type,l!==null&&t.stateNode!=null)l.memoizedProps!==a&&At(t);else{if(!a){if(t.stateNode===null)throw Error(E(166));return K(t),t.subtreeFlags&=-33554433,null}if(e=Tt.current,Ju(t))L1(t,e);else{var f=Nn(Pt.current);switch(e){case 1:e=f.createElementNS("http://www.w3.org/2000/svg",n);break;case 2:e=f.createElementNS("http://www.w3.org/1998/Math/MathML",n);break;default:switch(n){case"svg":e=f.createElementNS("http://www.w3.org/2000/svg",n);break;case"math":e=f.createElementNS("http://www.w3.org/1998/Math/MathML",n);break;case"script":e=f.createElement("div"),e.innerHTML="<script><\/script>",e=e.removeChild(e.firstChild);break;case"select":e=typeof a.is=="string"?f.createElement("select",{is:a.is}):f.createElement("select"),a.multiple?e.multiple=!0:a.size&&(e.size=a.size);break;default:e=typeof a.is=="string"?f.createElement(n,{is:a.is}):f.createElement(n)}}e[ml]=t,e[Rl]=a;l:for(f=t.child;f!==null;){if(f.tag===5||f.tag===6)e.appendChild(f.stateNode);else if(f.tag!==4&&f.tag!==27&&f.child!==null){f.child.return=f,f=f.child;continue}if(f===t)break l;for(;f.sibling===null;){if(f.return===null||f.return===t)break l;f=f.return}f.sibling.return=f.return,f=f.sibling}t.stateNode=e;l:switch(Sl(e,n,a),n){case"button":case"input":case"select":case"textarea":a=!!a.autoFocus;break l;case"img":a=!0;break l;default:a=!1}a&&At(t)}}return K(t),t.subtreeFlags&=-33554433,dc(t,t.type,l===null?null:l.memoizedProps,t.pendingProps,u),null;case 6:if(l&&t.stateNode!=null)l.memoizedProps!==a&&At(t);else{if(typeof a!="string"&&t.stateNode===null)throw Error(E(166));if(l=Pt.current,Ju(t)){if(l=t.stateNode,u=t.memoizedProps,a=null,n=ol,n!==null)switch(n.tag){case 27:case 5:a=n.memoizedProps}l[ml]=t,l=!!(l.nodeValue===u||a!==null&&a.suppressHydrationWarning===!0||rm(l.nodeValue,u)),l||ou(t,!0)}else l=Nn(l).createTextNode(a),l[ml]=t,t.stateNode=l}return K(t),null;case 31:if(u=t.memoizedState,l===null||l.memoizedState!==null){if(a=Ju(t),u!==null){if(l===null){if(!a)throw Error(E(318));if(l=t.memoizedState,l=l!==null?l.dehydrated:null,!l)throw Error(E(557));l[ml]=t}else Uu(),(t.flags&128)===0&&(t.memoizedState=null),t.flags|=4;K(t),l=!1}else u=ic(),l!==null&&l.memoizedState!==null&&(l.memoizedState.hydrationErrors=u),l=!0;if(!l)return t.flags&256?(ql(t),t):(ql(t),null);if((t.flags&128)!==0)throw Error(E(558))}return K(t),null;case 13:if(a=t.memoizedState,l===null||l.memoizedState!==null&&l.memoizedState.dehydrated!==null){if(n=Ju(t),a!==null&&a.dehydrated!==null){if(l===null){if(!n)throw Error(E(318));if(n=t.memoizedState,n=n!==null?n.dehydrated:null,!n)throw Error(E(317));n[ml]=t}else Uu(),(t.flags&128)===0&&(t.memoizedState=null),t.flags|=4;K(t),n=!1}else n=ic(),l!==null&&l.memoizedState!==null&&(l.memoizedState.hydrationErrors=n),n=!0;if(!n)return t.flags&256?(ql(t),t):(ql(t),null)}return ql(t),(t.flags&128)!==0?(t.lanes=u,t):(u=a!==null,l=l!==null&&l.memoizedState!==null,u&&(a=t.child,n=null,a.alternate!==null&&a.alternate.memoizedState!==null&&a.alternate.memoizedState.cachePool!==null&&(n=a.alternate.memoizedState.cachePool.pool),e=null,a.memoizedState!==null&&a.memoizedState.cachePool!==null&&(e=a.memoizedState.cachePool.pool),e!==n&&(a.flags|=2048)),u!==l&&u&&(t.child.flags|=8192),ie(t,t.updateQueue),K(t),null);case 4:return sa(),l===null&&D0(t.stateNode.containerInfo),t.flags|=67108864,K(t),null;case 10:return Ut(t.type),K(t),null;case 19:if(u0(t),a=t.memoizedState,a===null)return K(t),null;if(n=(t.flags&128)!==0,e=a.rendering,e===null)if(n)Ka(a,!1);else{if(k!==0||l!==null&&(l.flags&128)!==0)for(l=t.child;l!==null;){if(e=we(l),e!==null){for(t.flags|=128,Ka(a,!1),l=e.updateQueue,t.updateQueue=l,ie(t,l),t.subtreeFlags=0,l=u,u=t.child;u!==null;)po(u,l),u=u.sibling;return bn(t,gl.current&1|2),U&&Nt(t,a.treeForkCount),t.child}l=l.sibling}a.tail!==null&&Ql()>af&&(t.flags|=128,n=!0,Ka(a,!1),t.lanes=4194304)}else{if(!n)if(l=we(e),l!==null){if(t.flags|=128,n=!0,l=l.updateQueue,t.updateQueue=l,ie(t,l),Ka(a,!0),a.tail===null&&a.tailMode!=="collapsed"&&a.tailMode!=="visible"&&!e.alternate&&!U)return K(t),null}else 2*Ql()-a.renderingStartTime>af&&u!==536870912&&(t.flags|=128,n=!0,Ka(a,!1),t.lanes=4194304);a.isBackwards?(e.sibling=t.child,t.child=e):(l=a.last,l!==null?l.sibling=e:t.child=e,a.last=e)}if(a.tail!==null){l=a.tail;l:{for(u=l;u!==null;){if(u.alternate!==null){u=!1;break l}u=u.sibling}u=!0}return a.rendering=l,a.tail=l.sibling,a.renderingStartTime=Ql(),l.sibling=null,e=gl.current,e=n?e&1|2:e&1,a.tailMode==="visible"||a.tailMode==="collapsed"||!u||U?bn(t,e):(u=e,w(Tl,t),w(gl,u),zl===null&&(zl=t)),U&&Nt(t,a.treeForkCount),l}return K(t),null;case 22:case 23:return ql(t),t0(),a=t.memoizedState!==null,l!==null?l.memoizedState!==null!==a&&(t.flags|=8192):a&&(t.flags|=8192),a?(u&536870912)!==0&&(t.flags&128)===0&&(K(t),t.subtreeFlags&6&&(t.flags|=8192)):K(t),u=t.updateQueue,u!==null&&ie(t,u.retryQueue),u=null,l!==null&&l.memoizedState!==null&&l.memoizedState.cachePool!==null&&(u=l.memoizedState.cachePool.pool),a=null,t.memoizedState!==null&&t.memoizedState.cachePool!==null&&(a=t.memoizedState.cachePool.pool),a!==u&&(t.flags|=2048),l!==null&&hl(Nu),null;case 24:return u=null,l!==null&&(u=l.memoizedState.cache),t.memoizedState.cache!==u&&(t.flags|=2048),Ut(ul),K(t),null;case 25:return null;case 30:return t.flags|=33554432,K(t),null}throw Error(E(156,t.tag))}function _g(l,t){switch($i(t),t.tag){case 1:return l=t.flags,l&65536?(t.flags=l&-65537|128,t):null;case 3:return Ut(ul),sa(),l=t.flags,(l&65536)!==0&&(l&128)===0?(t.flags=l&-65537|128,t):null;case 26:case 27:case 5:return Xe(t),null;case 31:if(t.memoizedState!==null){if(ql(t),t.alternate===null)throw Error(E(340));Uu()}return l=t.flags,l&65536?(t.flags=l&-65537|128,t):null;case 13:if(ql(t),l=t.memoizedState,l!==null&&l.dehydrated!==null){if(t.alternate===null)throw Error(E(340));Uu()}return l=t.flags,l&65536?(t.flags=l&-65537|128,t):null;case 19:return u0(t),l=t.flags,l&65536?(t.flags=l&-65537|128,l=t.memoizedState,l!==null&&(l.rendering=null,l.tail=null),t.flags|=4,t):null;case 4:return sa(),null;case 10:return Ut(t.type),null;case 22:case 23:return ql(t),t0(),l!==null&&hl(Nu),l=t.flags,l&65536?(t.flags=l&-65537|128,t):null;case 24:return Ut(ul),null;case 25:return null;default:return null}}function Ly(l,t){switch($i(t),t.tag){case 3:Ut(ul),sa();break;case 26:case 27:case 5:Xe(t);break;case 4:sa();break;case 31:t.memoizedState!==null&&ql(t);break;case 13:ql(t);break;case 19:u0(t);break;case 10:Ut(t.type);break;case 22:case 23:ql(t),t0(),l!==null&&hl(Nu);break;case 24:Ut(ul)}}function Qn(l,t){try{var u=t.updateQueue,a=u!==null?u.lastEffect:null;if(a!==null){var n=a.next;u=n;do{if((u.tag&l)===l){a=void 0;var e=u.create,f=u.inst;a=e(),f.destroy=a}u=u.next}while(u!==n)}}catch(c){V(t,t.return,c)}}function mu(l,t,u){try{var a=t.updateQueue,n=a!==null?a.lastEffect:null;if(n!==null){var e=n.next;a=e;do{if((a.tag&l)===l){var f=a.inst,c=f.destroy;if(c!==void 0){f.destroy=void 0,n=t;var i=u,o=c;try{o()}catch(g){V(n,i,g)}}}a=a.next}while(a!==e)}}catch(g){V(t,t.return,g)}}function py(l){var t=l.updateQueue;if(t!==null){var u=l.stateNode;try{ly(t,u)}catch(a){V(l,l.return,a)}}}function Ky(l,t,u){u.props=Bu(l.type,l.memoizedProps),u.state=l.memoizedState;try{u.componentWillUnmount()}catch(a){V(l,t,a)}}function yt(l,t){try{var u=l.ref;if(u!==null){switch(l.tag){case 26:case 27:case 5:var a=l.stateNode;break;case 30:var n=l.stateNode,e=Ct(l.memoizedProps,n);(n.ref===null||n.ref.name!==e)&&(n.ref=qm(e)),a=n.ref;break;case 7:if(l.stateNode===null){var f=new Ll(l);Hl(l.child,!1,yS,f,void 0,void 0),l.stateNode=f}a=l.stateNode;break;default:a=l.stateNode}typeof u=="function"?l.refCleanup=u(a):u.current=a}}catch(c){V(l,t,c)}}function yl(l,t){var u=l.ref,a=l.refCleanup;if(u!==null)if(typeof a=="function")try{a()}catch(n){V(l,t,n)}finally{l.refCleanup=null,l=l.alternate,l!=null&&(l.refCleanup=null)}else if(typeof u=="function")try{u(null)}catch(n){V(l,t,n)}else u.current=null}function ke(l,t){if((l.tag===5||l.tag===27||l.tag===6)&&l.alternate===null&&t!==null)for(var u=0;u<t.length;u++)jm(l.stateNode,t[u])}function Tv(l){for(var t=l.return;t!==null&&(s0(t)&&jm(l.stateNode,t.stateNode),!E0(t));)t=t.return}function on(l){for(var t=l.return;t!==null&&(s0(t)&&mS(l.stateNode,t.stateNode),!E0(t));)t=t.return}function E0(l){return l.tag===5||l.tag===3||l.tag===27}function s0(l){return l&&l.tag===7&&l.stateNode!==null}function ii(l){var t=l.type,u=l.memoizedProps,a=l.stateNode;try{l:switch(t){case"button":case"input":case"select":case"textarea":u.autoFocus&&a.focus();break l;case"img":u.src?a.src=u.src:u.srcSet&&(a.srcset=u.srcSet)}}catch(n){V(l,l.return,n)}}function hc(l,t,u){try{var a=l.stateNode;wg(a,l.type,u,t),a[Rl]=t}catch(n){V(l,l.return,n)}}function Jy(l){return l.tag===5||l.tag===3||l.tag===26||l.tag===27&&gu(l.type)||l.tag===4}function gc(l){l:for(;;){for(;l.sibling===null;){if(l.return===null||Jy(l.return))return null;l=l.return}for(l.sibling.return=l.return,l=l.sibling;l.tag!==5&&l.tag!==6&&l.tag!==18;){if(l.tag===27&&gu(l.type)||l.flags&2||l.child===null||l.tag===4)continue l;l.child.return=l,l=l.child}if(!(l.flags&2))return l.stateNode}}function vi(l,t,u,a){var n=l.tag;if(n===5||n===6)n=l.stateNode,t?(u.nodeType===9?u.body:u.nodeName==="HTML"?u.ownerDocument.body:u).insertBefore(n,t):(t=u.nodeType===9?u.body:u.nodeName==="HTML"?u.ownerDocument.body:u,t.appendChild(n),u=u._reactRootContainer,u!=null||t.onclick!==null||(t.onclick=ht)),ke(l,a),B=!0;else if(n!==4&&(n===27&&(ke(l,a),a=null,gu(l.type)&&(u=l.stateNode,t=null)),l=l.child,l!==null))for(vi(l,t,u,a),l=l.sibling;l!==null;)vi(l,t,u,a),l=l.sibling}function Pe(l,t,u,a){var n=l.tag;if(n===5||n===6)n=l.stateNode,t?u.insertBefore(n,t):u.appendChild(n),ke(l,a),B=!0;else if(n!==4&&(n===27&&(ke(l,a),a=null,gu(l.type)&&(u=l.stateNode)),l=l.child,l!==null))for(Pe(l,t,u,a),l=l.sibling;l!==null;)Pe(l,t,u,a),l=l.sibling}function wy(l){var t=l.stateNode,u=l.memoizedProps;try{for(var a=l.type,n=t.attributes;n.length;)t.removeAttributeNode(n[0]);Sl(t,a,u),t[ml]=l,t[Rl]=u}catch(e){V(l,l.return,e)}}var lf=!1,Gl=null;function Ev(l){(l.tag===30||(l.subtreeFlags&33554432)!==0)&&(lf=!0)}var mt=null;function sv(){var l=mt;return mt=null,l}var Ul=0;function qa(l,t,u,a,n){return Ul=0,Wy(l.child,t,u,a,n)}function Wy(l,t,u,a,n){for(var e=!1;l!==null;){if(l.tag===5){var f=l.stateNode;if(a!==null){var c=ri(f);a.push(c),c.view&&(e=!0)}else e||ri(f).view&&(e=!0);lf=!0,Rm(f,Ul===0?t:t+"_"+Ul,u),Ul++}else(l.tag!==22||l.memoizedState===null)&&(l.tag===30&&n||Wy(l.child,t,u,a,n)&&(e=!0));l=l.sibling}return e}function st(l,t){for(;l!==null;)l.tag===5?Ym(l.stateNode,l.memoizedProps):(l.tag!==22||l.memoizedState===null)&&(l.tag===30&&t||st(l.child,t)),l=l.sibling}function De(l){if((l.subtreeFlags&18874368)!==0)for(l=l.child;l!==null;){if((l.tag!==22||l.memoizedState===null)&&(De(l),l.tag===30&&(l.flags&18874368)!==0&&l.stateNode.paired)){var t=l.memoizedProps;if(t.name==null||t.name==="auto")throw Error(E(544));var u=t.name;t=Gt(t.default,t.share),t!=="none"&&(qa(l,u,t,null,!1)||st(l.child,!1))}l=l.sibling}}function oi(l,t){if(l.tag===30){var u=l.stateNode,a=l.memoizedProps,n=Ct(a,u),e=Gt(a.default,u.paired?a.share:a.enter);e!=="none"?qa(l,n,e,null,!1)?(De(l),u.paired||t||Aa(l,a.onEnter)):st(l.child,!1):De(l)}else if((l.subtreeFlags&33554432)!==0)for(l=l.child;l!==null;)oi(l,t),l=l.sibling;else De(l)}function yi(l){if(Gl!==null&&Gl.size!==0){var t=Gl;if((l.subtreeFlags&18874368)!==0)for(l=l.child;l!==null;){if(l.tag!==22||l.memoizedState===null){if(l.tag===30&&(l.flags&18874368)!==0){var u=l.memoizedProps,a=u.name;if(a!=null&&a!=="auto"){var n=t.get(a);if(n!==void 0){var e=Gt(u.default,u.share);if(e!=="none"&&(qa(l,a,e,null,!1)?(e=l.stateNode,n.paired=e,e.paired=n,Aa(l,u.onShare)):st(l.child,!1)),t.delete(a),t.size===0)break}}}yi(l)}l=l.sibling}}}function mi(l){if(l.tag===30){var t=l.memoizedProps,u=Ct(t,l.stateNode),a=Gl!==null?Gl.get(u):void 0,n=Gt(t.default,a!==void 0?t.share:t.exit);n!=="none"&&(qa(l,u,n,null,!1)?a!==void 0?(n=l.stateNode,a.paired=n,n.paired=a,Gl.delete(u),Aa(l,t.onShare)):Aa(l,t.onExit):st(l.child,!1)),Gl!==null&&yi(l)}else if((l.subtreeFlags&33554432)!==0)for(l=l.child;l!==null;)mi(l),l=l.sibling;else Gl!==null&&yi(l)}function Fy(l){for(l=l.child;l!==null;){if(l.tag===30){var t=l.memoizedProps,u=Ct(t,l.stateNode);t=Gt(t.default,t.update),l.flags&=-5,t!=="none"&&qa(l,u,t,l.memoizedState=[],!1)}else(l.subtreeFlags&33554432)!==0&&Fy(l);l=l.sibling}}function di(l){if((l.subtreeFlags&18874368)!==0)for(l=l.child;l!==null;){if(l.tag!==22||l.memoizedState===null){if(l.tag===30&&(l.flags&18874368)!==0){var t=l.stateNode;t.paired!==null&&(t.paired=null,st(l.child,!1))}di(l)}l=l.sibling}}function Me(l){if(l.tag===30)l.stateNode.paired=null,st(l.child,!1),di(l);else if((l.subtreeFlags&33554432)!==0)for(l=l.child;l!==null;)Me(l),l=l.sibling;else di(l)}function $y(l){for(l=l.child;l!==null;)l.tag===30?st(l.child,!1):(l.subtreeFlags&33554432)!==0&&$y(l),l=l.sibling}function z0(l,t,u,a,n,e,f){for(var c=!1;t!==null;){if(t.tag===5){var i=t.stateNode;if(e!==null&&Ul<e.length){var o=e[Ul],g=ri(i);(o.view||g.view)&&(c=!0);var T;if(T=(l.flags&4)===0)if(g.clip)T=!0;else{T=o.rect;var y=g.rect;T=T.y!==y.y||T.x!==y.x||T.height!==y.height||T.width!==y.width}T&&(l.flags|=4),g.abs?g=!o.abs:(o=o.rect,g=g.rect,g=o.height!==g.height||o.width!==g.width),g&&(l.flags|=32)}else l.flags|=32;(l.flags&4)!==0&&Rm(i,Ul===0?u:u+"_"+Ul,n),c&&(l.flags&4)!==0||(mt===null&&(mt=[]),mt.push(i,Ul===0?a:a+"_"+Ul,t.memoizedProps)),Ul++}else(t.tag!==22||t.memoizedState===null)&&(t.tag===30&&f?l.flags|=t.flags&32:z0(l,t.child,u,a,n,e,f)&&(c=!0));t=t.sibling}return c}function Iy(l,t){for(l=l.child;l!==null;){if(l.tag===30){var u=l.memoizedProps,a=l.stateNode,n=Ct(u,a),e=Gt(u.default,u.update);if(t){a=a.clones;var f=a===null?null:a.map(Pg)}else f=l.memoizedState,l.memoizedState=null;a=l;var c=l.child;Ul=0,n=z0(a,c,n,n,e,f,!1),(l.flags&4)!==0&&n&&(t||Aa(l,u.onUpdate))}else(l.subtreeFlags&33554432)!==0&&Iy(l,t);l=l.sibling}}var cl=!1,X=!1,it=!1,Sc=!1,zv=typeof WeakSet=="function"?WeakSet:Set,il=null,vt=!1,Pa=!1,tf=!1,hi=!1;function Ag(l,t,u){if(l=l.containerInfo,Di=Ca,l=Go(l),Ji(l)){if("selectionStart"in l)var a={start:l.selectionStart,end:l.selectionEnd};else l:{a=(a=l.ownerDocument)&&a.defaultView||window;var n=a.getSelection&&a.getSelection();if(n&&n.rangeCount!==0){a=n.anchorNode;var e=n.anchorOffset,f=n.focusNode;n=n.focusOffset;try{a.nodeType,f.nodeType}catch{a=null;break l}var c=0,i=-1,o=-1,g=0,T=0,y=l,h=null;t:for(;;){for(var s;y!==a||e!==0&&y.nodeType!==3||(i=c+e),y!==f||n!==0&&y.nodeType!==3||(o=c+n),y.nodeType===3&&(c+=y.nodeValue.length),(s=y.firstChild)!==null;)h=y,y=s;for(;;){if(y===l)break t;if(h===a&&++g===e&&(i=c),h===f&&++T===n&&(o=c),(s=y.nextSibling)!==null)break;y=h,h=y.parentNode}y=s}a=i===-1||o===-1?null:{start:i,end:o}}else a=null}a=a||{start:0,end:0}}else a=null;for(Mi={focusedElem:l,selectionRange:a},Ca=!1,u=(u&335544064)===u,il=t,t=u?9270:1024;il!==null;){if(l=il,u&&(a=l.deletions,a!==null))for(e=0;e<a.length;e++)u&&mi(a[e]);if(l.alternate===null&&(l.flags&2)!==0)u&&Ev(l),ve(u);else{if(l.tag===22){if(a=l.alternate,l.memoizedState!==null){a!==null&&a.memoizedState===null&&u&&mi(a),ve(u);continue}else if(a!==null&&a.memoizedState!==null){u&&Ev(l),ve(u);continue}}a=l.child,(l.subtreeFlags&t)!==0&&a!==null?(a.return=l,il=a):(u&&Fy(l),ve(u))}}Gl=null}function ve(l){for(;il!==null;){var t=il,u=l,a=t.alternate,n=t.flags;switch(t.tag){case 0:case 11:case 15:break;case 1:if((n&1024)!==0&&a!==null){u=void 0,n=a.memoizedProps,a=a.memoizedState;var e=t.stateNode;try{var f=Bu(t.type,n);u=e.getSnapshotBeforeUpdate(f,a),e.__reactInternalSnapshotBeforeUpdate=u}catch(c){V(t,t.return,c)}}break;case 3:if((n&1024)!==0){if(a=t.stateNode.containerInfo,u=a.nodeType,u===9)Ci(a);else if(u===1)switch(a.nodeName){case"HEAD":case"HTML":case"BODY":Ci(a);break;default:a.textContent=""}}break;case 5:case 26:case 27:case 6:case 4:case 17:break;case 30:u&&a!==null&&(u=Ct(a.memoizedProps,a.stateNode),n=t.memoizedProps,n=Gt(n.default,n.update),n!=="none"&&qa(a,u,n,a.memoizedState=[],!0));break;default:if((n&1024)!==0)throw Error(E(163))}if(a=t.sibling,a!==null){a.return=t.return,il=a;break}il=t.return}}function ky(l,t,u){var a=u.flags;switch(u.tag){case 0:case 11:case 15:ot(l,u),a&4&&Qn(5,u);break;case 1:if(ot(l,u),a&4)if(l=u.stateNode,t===null)try{l.componentDidMount()}catch(f){V(u,u.return,f)}else{var n=Bu(u.type,t.memoizedProps);t=t.memoizedState;try{l.componentDidUpdate(n,t,l.__reactInternalSnapshotBeforeUpdate)}catch(f){V(u,u.return,f)}}a&64&&py(u),a&512&&yt(u,u.return);break;case 3:if(ot(l,u),a&64&&(l=u.updateQueue,l!==null)){if(t=null,u.child!==null)switch(u.child.tag){case 27:case 5:t=u.child.stateNode;break;case 1:t=u.child.stateNode}try{ly(l,t)}catch(f){V(u,u.return,f)}}break;case 27:t===null&&a&4&&wy(u);case 26:case 5:ot(l,u),t===null&&a&4&&ii(u),a&512&&yt(u,u.return);break;case 12:ot(l,u);break;case 31:ot(l,u),a&4&&um(l,u);break;case 13:ot(l,u),a&4&&am(l,u),a&64&&(l=u.memoizedState,l!==null&&(l=l.dehydrated,l!==null&&(u=Gg.bind(null,u),gS(l,u))));break;case 22:if(a=u.memoizedState!==null||cl,!a){var e=t!==null&&t.memoizedState!==null||X;t=cl,n=X,cl=a,(X=e)&&!n?(a=2,(u.subtreeFlags&8772)!==0&&(a|=1),tt(l,u,a)):ot(l,u),cl=t,X=n}break;case 30:ot(l,u),a&512&&yt(u,u.return);break;case 7:a&512&&yt(u,u.return);default:ot(l,u)}}function gi(l,t){for(l=l.child;l!==null;)Py(l,t),l=l.sibling}function Py(l,t){switch(l.tag){case 5:case 26:try{var u=l.stateNode;if(t){var a=u.style;typeof a.setProperty=="function"?a.setProperty("display","none","important"):a.display="none"}else{var n=l.stateNode,e=l.memoizedProps.style,f=e!=null&&e.hasOwnProperty("display")?e.display:null;n.style.display=f==null||typeof f=="boolean"?"":(""+f).trim()}}catch(i){V(l,l.return,i)}Si(l,t);break;case 6:try{l.stateNode.nodeValue=t?"":l.memoizedProps,B=!0}catch(i){V(l,l.return,i)}break;case 18:try{var c=l.stateNode;t?Gv(c,!0):Gv(l.stateNode,!1)}catch(i){V(l,l.return,i)}break;case 22:case 23:l.memoizedState===null&&gi(l,t);break;default:gi(l,t)}}function Si(l,t){if(l.subtreeFlags&67108864)for(l=l.child;l!==null;){l:{var u=l,a=t;switch(u.tag){case 4:Py(u,a);break l;case 22:u.memoizedState===null&&Si(u,a);break l;default:Si(u,a)}}l=l.sibling}}function lm(l){var t=l.alternate;t!==null&&(l.alternate=null,lm(t)),l.child=null,l.deletions=null,l.sibling=null,l.tag===5&&(t=l.stateNode,t!==null&&mf(t)),l.stateNode=null,l.return=null,l.dependencies=null,l.memoizedProps=null,l.memoizedState=null,l.pendingProps=null,l.stateNode=null,l.updateQueue=null}var F=null,Dl=!1;function lt(l,t,u){for(u=u.child;u!==null;)tm(l,t,u),u=u.sibling}function tm(l,t,u){if(jl&&typeof jl.onCommitFiberUnmount=="function")try{jl.onCommitFiberUnmount(Hn,u)}catch{}switch(u.tag){case 26:X||yl(u,t),lt(l,t,u),u.memoizedState?u.memoizedState.count--:u.stateNode&&!X&&(u=u.stateNode,u.parentNode.removeChild(u));break;case 27:X||yl(u,t),on(u);var a=F,n=Dl;gu(u.type)&&(F=u.stateNode,Dl=!1),lt(l,t,u),xm(u.stateNode,u.type,u.memoizedProps),F=a,Dl=n;break;case 5:X||yl(u,t),on(u);case 6:if(u.tag===6&&on(u),a=F,n=Dl,F=null,lt(l,t,u),F=a,Dl=n,F!==null)if(Dl)try{(F.nodeType===9?F.body:F.nodeName==="HTML"?F.ownerDocument.body:F).removeChild(u.stateNode),B=!0}catch(e){V(u,t,e)}else try{F.removeChild(u.stateNode),B=!0}catch(e){V(u,t,e)}break;case 18:F!==null&&(Dl?(l=F,qv(l.nodeType===9?l.body:l.nodeName==="HTML"?l.ownerDocument.body:l,u.stateNode),Ha(l)):qv(F,u.stateNode));break;case 4:a=F,n=Dl,F=u.stateNode.containerInfo,Dl=!0,lt(l,t,u),F=a,Dl=n;break;case 0:case 11:case 14:case 15:mu(2,u,t),X||mu(4,u,t),lt(l,t,u);break;case 1:X||(yl(u,t),a=u.stateNode,typeof a.componentWillUnmount=="function"&&Ky(u,t,a)),lt(l,t,u);break;case 21:lt(l,t,u);break;case 22:X=(a=X)||u.memoizedState!==null,lt(l,t,u),X=a;break;case 30:yl(u,t),lt(l,t,u);break;case 7:X||yl(u,t),lt(l,t,u);break;default:lt(l,t,u)}}function um(l,t){if(t.memoizedState===null&&(l=t.alternate,l!==null&&(l=l.memoizedState,l!==null))){l=l.dehydrated;try{Ha(l)}catch(u){V(t,t.return,u)}}}function am(l,t){if(t.memoizedState===null&&(l=t.alternate,l!==null&&(l=l.memoizedState,l!==null&&(l=l.dehydrated,l!==null))))try{Ha(l)}catch(u){V(t,t.return,u)}}function Ng(l){switch(l.tag){case 31:case 13:case 19:var t=l.stateNode;return t===null&&(t=l.stateNode=new zv),t;case 22:return l=l.stateNode,t=l._retryCache,t===null&&(t=l._retryCache=new zv),t;default:throw Error(E(435,l.tag))}}function oe(l,t){var u=Ng(l);t.forEach(function(a){if(!u.has(a)){u.add(a);var n=Xg.bind(null,l,a);a.then(n,n)}})}function Ol(l,t,u){var a=t.deletions;if(a!==null)for(var n=0;n<a.length;n++){var e=a[n],f=l,c=t,i=c;l:for(;i!==null;){switch(i.tag){case 27:if(gu(i.type)){F=i.stateNode,Dl=!1;break l}break;case 5:F=i.stateNode,Dl=!1;break l;case 3:case 4:F=i.stateNode.containerInfo,Dl=!0;break l}i=i.return}if(F===null)throw Error(E(160));tm(f,c,e),F=null,Dl=!1,f=e.alternate,f!==null&&(f.return=null),e.return=null}if(t.subtreeFlags&13886)for(t=t.child;t!==null;)nm(t,l,u),t=t.sibling}var ut=null;function nm(l,t,u){var a=l.alternate,n=l.flags;switch(l.tag){case 0:case 11:case 14:case 15:if(n&4&&(a=l.updateQueue,a=a!==null?a.events:null,a!==null))for(var e=0;e<a.length;e++){var f=a[e];f.ref.impl=f.nextImpl}Ol(t,l,u),_l(l),n&4&&(mu(3,l,l.return),Qn(3,l),mu(5,l,l.return));break;case 1:Ol(t,l,u),_l(l),n&512&&(X||a===null||yl(a,a.return)),n&64&&cl&&(l=l.updateQueue,l!==null&&(t=l.callbacks,t!==null&&(u=l.shared.hiddenCallbacks,l.shared.hiddenCallbacks=u===null?t:u.concat(t))));break;case 26:if(e=ut,Ol(t,l,u),_l(l),n&512&&(X||a===null||yl(a,a.return)),n&4)if(n=a!==null?a.memoizedState:null,u=l.memoizedState,a===null)if(u===null)if(l.stateNode===null)if(cl)l.stateNode=Hm(l.type,l.memoizedProps,t.containerInfo,l);else{l:{t=l.type,u=l.memoizedProps,n=e.ownerDocument||e;t:switch(t){case"title":a=n.getElementsByTagName("title")[0],(!a||a[Bn]||a[ml]||a.namespaceURI==="http://www.w3.org/2000/svg"||a.hasAttribute("itemprop"))&&(a=n.createElement(t),n.head.insertBefore(a,n.querySelector("head > title"))),Sl(a,t,u),a[ml]=l,vl(a),t=a;break l;case"link":if(e=pv("link","href",n).get(t+(u.href||""))){for(f=0;f<e.length;f++)if(a=e[f],a.getAttribute("href")===(u.href==null||u.href===""?null:u.href)&&a.getAttribute("rel")===(u.rel==null?null:u.rel)&&a.getAttribute("title")===(u.title==null?null:u.title)&&a.getAttribute("crossorigin")===(u.crossOrigin==null?null:u.crossOrigin)){e.splice(f,1);break t}}a=n.createElement(t),Sl(a,t,u),n.head.appendChild(a);break;case"meta":if(e=pv("meta","content",n).get(t+(u.content||""))){for(f=0;f<e.length;f++)if(a=e[f],a.getAttribute("content")===(u.content==null?null:""+u.content)&&a.getAttribute("name")===(u.name==null?null:u.name)&&a.getAttribute("property")===(u.property==null?null:u.property)&&a.getAttribute("http-equiv")===(u.httpEquiv==null?null:u.httpEquiv)&&a.getAttribute("charset")===(u.charSet==null?null:u.charSet)){e.splice(f,1);break t}}a=n.createElement(t),Sl(a,t,u),n.head.appendChild(a);break;default:throw Error(E(468,t))}a[ml]=l,vl(a),t=a}l.stateNode=t}else cl||Yi(e,l.type,l.stateNode);else l.stateNode=Lv(e,u,l.memoizedProps);else n!==u?(n===null?(t=a.stateNode,t===null||X||t.parentNode.removeChild(t)):n.count--,u===null?cl||Yi(e,l.type,l.stateNode):Lv(e,u,l.memoizedProps)):u===null&&l.stateNode!==null&&hc(l,l.memoizedProps,a.memoizedProps);break;case 27:Ol(t,l,u),_l(l),n&512&&(X||a===null||yl(a,a.return)),a!==null&&n&4&&hc(l,l.memoizedProps,a.memoizedProps);break;case 5:if(e=it,it=!1,Ol(t,l,u),it=e,_l(l),n&512&&(X||a===null||yl(a,a.return)),l.flags&32){t=l.stateNode;try{ba(t,""),B=!0}catch(g){V(l,l.return,g)}}n&4&&l.stateNode!=null&&(t=l.memoizedProps,hc(l,t,a!==null?a.memoizedProps:t)),n&1024&&(Sc=!0);break;case 6:if(Ol(t,l,u),_l(l),n&4){if(l.stateNode===null)throw Error(E(162));t=l.memoizedProps,u=l.stateNode;try{u.nodeValue=t,B=!0}catch(g){V(l,l.return,g)}}break;case 3:if(B=!1,He=null,e=ut,ut=Dn(t.containerInfo),Ol(t,l,u),ut=e,_l(l),n&4&&a!==null&&a.memoizedState.isDehydrated)try{Ha(t.containerInfo)}catch(g){V(l,l.return,g)}Sc&&(Sc=!1,em(l)),B=!1;break;case 4:n=it,it=cl,a=D1(),e=ut,ut=Dn(l.stateNode.containerInfo),Ol(t,l,u),_l(l),ut=e,B&&Pa&&(tf=!0),B=a,it=n;break;case 12:Ol(t,l,u),_l(l);break;case 31:Ol(t,l,u),_l(l),n&4&&(t=l.updateQueue,t!==null&&(l.updateQueue=null,oe(l,t)));break;case 13:Ol(t,l,u),_l(l),l.child.flags&8192&&l.memoizedState!==null!=(a!==null&&a.memoizedState!==null)&&(Nf=Ql()),n&4&&(t=l.updateQueue,t!==null&&(l.updateQueue=null,oe(l,t)));break;case 22:e=l.memoizedState!==null,f=a!==null&&a.memoizedState!==null;var c=cl,i=X,o=it;cl=c||e,it=o||e,X=i||f,Ol(t,l,u),X=i,it=o,cl=c,_l(l),n&8192&&(t=l.stateNode,t._visibility=e?t._visibility&-2:t._visibility|1,!e||a===null||f||cl||X||(t=f||X,u=cl,a=X,cl=e||cl,X=t,Lt(l,2),cl=u,X=a),!e&&it||gi(l,e)),n&4&&(t=l.updateQueue,t!==null&&(u=t.retryQueue,u!==null&&(t.retryQueue=null,oe(l,u))));break;case 19:Ol(t,l,u),_l(l),n&4&&(t=l.updateQueue,t!==null&&(l.updateQueue=null,oe(l,t)));break;case 30:n&512&&(X||a===null||yl(a,a.return)),n=D1(),e=Pa,f=(u&335544064)===u,c=l.memoizedProps,Pa=f&&Gt(c.default,c.update)!=="none",Ol(t,l,u),_l(l),f&&a!==null&&B&&(l.flags|=4),Pa=e,B=n;break;case 21:break;case 7:n&512&&(X||a===null||yl(a,a.return)),a&&a.stateNode!==null&&(a.stateNode._fragmentFiber=l);default:Ol(t,l,u),_l(l)}}function _l(l){var t=l.flags;if(t&2){try{for(var u,a=l.return;a!==null;){if(Jy(a)){u=a;break}a=a.return}a=null;for(var n=l.return;n!==null;){if(s0(n)){var e=n.stateNode;a===null?a=[e]:a.push(e)}if(E0(n))break;n=n.return}var f=a;if(u==null)throw Error(E(160));switch(u.tag){case 27:var c=u.stateNode,i=gc(l);Pe(l,i,c,f);break;case 5:var o=u.stateNode;u.flags&32&&(ba(o,""),u.flags&=-33);var g=gc(l);Pe(l,g,o,f);break;case 3:case 4:var T=u.stateNode.containerInfo,y=gc(l);vi(l,y,T,f);break;default:throw Error(E(161))}}catch(h){V(l,l.return,h)}l.flags&=-3}t&4096&&(l.flags&=-4097)}function em(l){if(l.subtreeFlags&1024)for(l=l.child;l!==null;){var t=l;em(t),t.tag===5&&t.flags&1024&&(t=t.stateNode,Ca=!0,t.reset(),Ca=!1),l=l.sibling}}function wu(l,t){if(t.subtreeFlags&9270)for(t=t.child;t!==null;)fm(t,l),t=t.sibling;else Iy(t,!1)}function fm(l,t){var u=l.alternate;if(u===null)oi(l,!1);else switch(l.tag){case 3:if(hi=vt=!1,sv(),wu(t,l),!vt&&!tf){if(l=mt,l!==null)for(var a=0;a<l.length;a+=3){u=l[a];var n=l[a+1];Ym(u,l[a+2]),u=u.ownerDocument.documentElement,u!==null&&u.animate({opacity:[0,0],pointerEvents:["none","none"]},{duration:0,fill:"forwards",pseudoElement:"::view-transition-group("+n+")"})}l=t.containerInfo,l=l.nodeType===9?l.documentElement:l.ownerDocument.documentElement,l!==null&&l.style.viewTransitionName===""&&(l.style.viewTransitionName="none",l.animate({opacity:[0,0],pointerEvents:["none","none"]},{duration:0,fill:"forwards",pseudoElement:"::view-transition-group(root)"}),l.animate({width:[0,0],height:[0,0]},{duration:0,fill:"forwards",pseudoElement:"::view-transition"})),hi=!0}mt=null;break;case 5:wu(t,l);break;case 4:a=vt,vt=!1,wu(t,l),vt&&(tf=!0),vt=a;break;case 22:l.memoizedState===null&&(u.memoizedState!==null?oi(l,!1):wu(t,l));break;case 30:a=vt,n=sv(),vt=!1,wu(t,l),vt&&(l.flags|=4);var e=l.memoizedProps,f=l.stateNode;t=Ct(e,f),f=Ct(u.memoizedProps,f);var c=Gt(e.default,e.update);c==="none"?t=!1:(e=u.memoizedState,u.memoizedState=null,u=l.child,Ul=0,t=z0(l,u,t,f,c,e,!0),Ul!==(e===null?0:e.length)&&(l.flags|=32)),(l.flags&4)!==0&&t?(Aa(l,l.memoizedProps.onUpdate),mt=n):n!==null&&(n.push.apply(n,mt),mt=n),vt=(l.flags&32)!==0?!0:a;break;default:wu(t,l)}}function ot(l,t){if(t.subtreeFlags&8772)for(t=t.child;t!==null;)ky(l,t.alternate,t),t=t.sibling}function Lt(l,t){for(l=l.child;l!==null;){var u=l,a=t;switch(u.tag){case 0:case 11:case 14:case 15:mu(4,u,u.return),Lt(u,a);break;case 1:yl(u,u.return);var n=u.stateNode;typeof n.componentWillUnmount=="function"&&Ky(u,u.return,n),Lt(u,a);break;case 27:(a&2)!==0&&xm(u.stateNode,u.type,u.memoizedProps);case 5:yl(u,u.return),u.tag!==5&&u.tag!==27||on(u),Lt(u,a);break;case 6:on(u);break;case 26:yl(u,u.return),n=u.stateNode,u.memoizedState!==null||n===null||X||n.parentNode.removeChild(n),Lt(u,a);break;case 22:u.memoizedState===null&&Lt(u,a);break;case 30:yl(u,u.return),Lt(u,a);break;case 7:yl(u,u.return);default:Lt(u,a)}l=l.sibling}}function tt(l,t,u){for(u=(t.subtreeFlags&8772)!==0?u:u&-2,t=t.child;t!==null;){var a=t.alternate,n=l,e=t,f=e.flags,c=(u&1)!==0;switch(e.tag){case 0:case 11:case 15:tt(n,e,u),Qn(4,e);break;case 1:if(tt(n,e,u),a=e,n=a.stateNode,typeof n.componentDidMount=="function")try{n.componentDidMount()}catch(g){V(a,a.return,g)}if(a=e,n=a.updateQueue,n!==null){var i=a.stateNode;try{var o=n.shared.hiddenCallbacks;if(o!==null)for(n.shared.hiddenCallbacks=null,n=0;n<o.length;n++)Po(o[n],i)}catch(g){V(a,a.return,g)}}c&&f&64&&py(e),yt(e,e.return);break;case 27:(u&2)!==0&&wy(e);case 5:e.tag!==5&&e.tag!==27||Tv(e),tt(n,e,u),c&&a===null&&f&4&&ii(e),yt(e,e.return);break;case 6:Tv(e);break;case 26:i=e.stateNode,e.memoizedState!==null||i===null||cl||Yi(Dn(i.ownerDocument),e.type,i),tt(n,e,u),c&&a===null&&f&4&&ii(e),yt(e,e.return);break;case 12:tt(n,e,u);break;case 31:tt(n,e,u),c&&f&4&&um(n,e);break;case 13:tt(n,e,u),c&&f&4&&am(n,e);break;case 22:e.memoizedState===null&&tt(n,e,u),yt(e,e.return);break;case 30:tt(n,e,u),yt(e,e.return);break;case 7:yt(e,e.return);default:tt(n,e,u)}t=t.sibling}}function b0(l,t){var u=null;l!==null&&l.memoizedState!==null&&l.memoizedState.cachePool!==null&&(u=l.memoizedState.cachePool.pool),l=null,t.memoizedState!==null&&t.memoizedState.cachePool!==null&&(l=t.memoizedState.cachePool.pool),l!==u&&(l!=null&&l.refCount++,u!=null&&Gn(u))}function O0(l,t){l=null,t.alternate!==null&&(l=t.alternate.memoizedState.cache),t=t.memoizedState.cache,t!==l&&(t.refCount++,l!=null&&Gn(l))}function Kl(l,t,u,a){var n=(u&335544064)===u;if(t.subtreeFlags&(n?10262:10256))for(t=t.child;t!==null;)cm(l,t,u,a),t=t.sibling;else n&&$y(t)}function cm(l,t,u,a){var n=(u&335544064)===u;n&&t.alternate===null&&t.return!==null&&t.return.alternate!==null&&Me(t);var e=t.flags;switch(t.tag){case 0:case 11:case 15:Kl(l,t,u,a),e&2048&&Qn(9,t);break;case 1:Kl(l,t,u,a);break;case 3:Kl(l,t,u,a),n&&hi&&(l=l.containerInfo,l=l.nodeType===9?l.body:l.nodeName==="HTML"?l.ownerDocument.body:l,l.style.viewTransitionName==="root"&&(l.style.viewTransitionName=""),l=l.ownerDocument.documentElement,l!==null&&l.style.viewTransitionName==="none"&&(l.style.viewTransitionName="")),e&2048&&(e=null,t.alternate!==null&&(e=t.alternate.memoizedState.cache),t=t.memoizedState.cache,t!==e&&(t.refCount++,e!=null&&Gn(e)));break;case 12:if(e&2048){Kl(l,t,u,a),e=t.stateNode;try{var f=t.memoizedProps,c=f.id,i=f.onPostCommit;typeof i=="function"&&i(c,t.alternate===null?"mount":"update",e.passiveEffectDuration,-0)}catch(o){V(t,t.return,o)}}else Kl(l,t,u,a);break;case 31:Kl(l,t,u,a);break;case 13:Kl(l,t,u,a);break;case 23:break;case 22:f=t.stateNode,c=t.alternate,t.memoizedState!==null?(n&&c!==null&&c.memoizedState===null&&Me(c),f._visibility&2?Kl(l,t,u,a):yn(l,t)):(n&&c!==null&&c.memoizedState!==null&&Me(t),f._visibility&2?Kl(l,t,u,a):(f._visibility|=2,Fu(l,t,u,a,(t.subtreeFlags&10256)!==0||!1))),e&2048&&b0(c,t);break;case 24:Kl(l,t,u,a),e&2048&&O0(t.alternate,t);break;case 30:n&&(e=t.alternate,e!==null&&(st(e.child,!0),st(t.child,!0))),Kl(l,t,u,a);break;default:Kl(l,t,u,a)}}function Fu(l,t,u,a,n){for(n=n&&((t.subtreeFlags&10256)!==0||!1),t=t.child;t!==null;){var e=l,f=t,c=u,i=a,o=f.flags;switch(f.tag){case 0:case 11:case 15:Fu(e,f,c,i,n),Qn(8,f);break;case 23:break;case 22:var g=f.stateNode;f.memoizedState!==null?g._visibility&2?Fu(e,f,c,i,n):yn(e,f):(g._visibility|=2,Fu(e,f,c,i,n)),n&&o&2048&&b0(f.alternate,f);break;case 24:Fu(e,f,c,i,n),n&&o&2048&&O0(f.alternate,f);break;default:Fu(e,f,c,i,n)}t=t.sibling}}function yn(l,t){if(t.subtreeFlags&10256)for(t=t.child;t!==null;){var u=l,a=t,n=a.flags;switch(a.tag){case 22:yn(u,a),n&2048&&b0(a.alternate,a);break;case 24:yn(u,a),n&2048&&O0(a.alternate,a);break;default:yn(u,a)}t=t.sibling}}var zu=8192;function Tu(l,t,u){if(l.subtreeFlags&zu)for(l=l.child;l!==null;)im(l,t,u),l=l.sibling}function im(l,t,u){switch(l.tag){case 26:Tu(l,t,u),l.flags&zu&&(l.memoizedState!==null?rS(u,ut,l.memoizedState,l.memoizedProps):(l=l.stateNode,(t&335544128)===t&&Jv(u,l)));break;case 5:Tu(l,t,u),l.flags&zu&&(l=l.stateNode,(t&335544128)===t&&Jv(u,l));break;case 3:case 4:var a=ut;ut=Dn(l.stateNode.containerInfo),Tu(l,t,u),ut=a;break;case 22:l.memoizedState===null&&(a=l.alternate,a!==null&&a.memoizedState!==null?(a=zu,zu=16777216,Tu(l,t,u),zu=a):Tu(l,t,u));break;case 30:if((l.flags&zu)!==0&&(a=l.memoizedProps.name,a!=null&&a!=="auto")){var n=l.stateNode;n.paired=null,Gl===null&&(Gl=new Map),Gl.set(a,n)}Tu(l,t,u);break;default:Tu(l,t,u)}}function vm(l){var t=l.alternate;if(t!==null&&(l=t.child,l!==null)){t.child=null;do t=l.sibling,l.sibling=null,l=t;while(l!==null)}}function Ja(l){var t=l.deletions;if((l.flags&16)!==0){if(t!==null)for(var u=0;u<t.length;u++){var a=t[u];il=a,ym(a,l)}vm(l)}if(l.subtreeFlags&10256)for(l=l.child;l!==null;)om(l),l=l.sibling}function om(l){switch(l.tag){case 0:case 11:case 15:Ja(l),l.flags&2048&&mu(9,l,l.return);break;case 3:Ja(l);break;case 12:Ja(l);break;case 22:var t=l.stateNode;l.memoizedState!==null&&t._visibility&2&&(l.return===null||l.return.tag!==13)?(t._visibility&=-3,Ue(l)):Ja(l);break;default:Ja(l)}}function Ue(l){var t=l.deletions;if((l.flags&16)!==0){if(t!==null)for(var u=0;u<t.length;u++){var a=t[u];il=a,ym(a,l)}vm(l)}for(l=l.child;l!==null;){switch(t=l,t.tag){case 0:case 11:case 15:mu(8,t,t.return),Ue(t);break;case 22:u=t.stateNode,u._visibility&2&&(u._visibility&=-3,Ue(t));break;default:Ue(t)}l=l.sibling}}function ym(l,t){for(;il!==null;){var u=il;switch(u.tag){case 0:case 11:case 15:mu(8,u,t);break;case 23:case 22:if(u.memoizedState!==null&&u.memoizedState.cachePool!==null){var a=u.memoizedState.cachePool.pool;a!=null&&a.refCount++}break;case 24:Gn(u.memoizedState.cache)}if(a=u.child,a!==null)a.return=u,il=a;else l:for(u=l;il!==null;){a=il;var n=a.sibling,e=a.return;if(lm(a),a===u){il=null;break l}if(n!==null){n.return=e,il=n;break l}il=e}}}var Dg={getCacheForType:function(l){var t=dl(ul),u=t.data.get(l);return u===void 0&&(u=l(),t.data.set(l,u)),u},cacheSignal:function(){return dl(ul).controller.signal}},Mg=typeof WeakMap=="function"?WeakMap:Map,q=0,x=null,C=null,R=0,Q=0,Yl=null,$t=!1,Ga=!1,_0=!1,Bt=0,k=0,du=0,Mu=0,uf=0,Xl=0,_a=0,mn=null,Ml=null,Ti=!1,Nf=0,mm=0,af=1/0,nf=null,eu=null,$=0,nt=null,qu=null,Et=0,Ei=0,si=null,dm=null,Sa=null,Ta=null,Ea=null,dn=0,re=null;function Zl(){return(q&2)!==0&&R!==0?R&-R:A.T!==null?N0():To()}function hm(){if(Xl===0)if((R&536870912)===0||U){var l=In;In<<=1,(In&3932160)===0&&(In=262144),Xl=l}else Xl=536870912;return l=Tl.current,l!==null&&(l.flags|=32),Xl}function Aa(l,t){if(t!=null){var u=l.stateNode,a=u.ref;a===null&&(a=u.ref=qm(Ct(l.memoizedProps,u))),Ta===null&&(Ta=[]),Ta.push(t.bind(null,a))}}function Cl(l,t,u){(l===x&&(Q===2||Q===9)||l.cancelPendingCommit!==null)&&(Na(l,0),It(l,R,Xl,!1)),Yn(l,u),((q&2)===0||l!==x)&&(l===x&&((q&2)===0&&(Mu|=u),k===4&&It(l,R,Xl,!1)),bt(l))}function gm(l,t,u){if((q&6)!==0)throw Error(E(327));var a=!u&&(t&127)===0&&(t&l.expiredLanes)===0||Rn(l,t),n=a?Cg(l,t):Tc(l,t,!0),e=a;do{if(n===0){Ga&&!a&&It(l,t,0,!1);break}else{if(u=l.current.alternate,e&&!Ug(u)){n=Tc(l,t,!1),e=!1;continue}if(n===2){if(e=t,l.errorRecoveryDisabledLanes&e)var f=0;else f=l.pendingLanes&-536870913,f=f!==0?f:f&536870912?536870912:0;if(f!==0){t=f;l:{var c=l;n=mn;var i=c.current.memoizedState.isDehydrated;if(i&&(Na(c,f).flags|=256),f=Tc(c,f,!1),f!==2&&f!==6){if(_0&&!i){c.errorRecoveryDisabledLanes|=e,Mu|=e,n=4;break l}e=Ml,Ml=n,e!==null&&(Ml===null?Ml=e:Ml.push.apply(Ml,e))}n=f}if(e=!1,n!==2)continue}}if(n===1){Na(l,0),It(l,t,0,!0);break}l:{switch(a=l,e=n,e){case 0:case 1:throw Error(E(345));case 4:if((t&4194048)!==t&&(t&62914560)!==t)break;case 6:It(a,t,Xl,!$t);break l;case 2:Ml=null;break;case 3:case 5:break;default:throw Error(E(329))}if((t&62914560)===t&&(n=Nf+300-Ql(),10<n)){if(It(a,t,Xl,!$t),yf(a,0,!0)!==0)break l;Et=t,a.timeoutHandle=M0(bv.bind(null,a,u,Ml,nf,Ti,t,Xl,Mu,_a,$t,e,"Throttled",-0,0),n);break l}bv(a,u,Ml,nf,Ti,t,Xl,Mu,_a,$t,e,null,-0,0)}}break}while(!0);bt(l)}function bv(l,t,u,a,n,e,f,c,i,o,g,T,y,h){l.timeoutHandle=-1;var s=t.subtreeFlags,b=(e&335544064)===e;if(T=null,(b||s&8192||(s&16785408)===16785408)&&(T={stylesheets:null,count:0,imgCount:0,imgBytes:0,suspenseyImages:[],waitingForImages:!0,waitingForViewTransition:!1,unsuspend:ht},Gl=null,im(t,e,T),b&&(s=T,b=l.containerInfo,b=(b.nodeType===9?b:b.ownerDocument).__reactViewTransition,b!=null&&(s.count++,s.waitingForViewTransition=!0,s=Mn.bind(s),b.finished.then(s,s))),s=(e&62914560)===e?Nf-Ql():(e&4194048)===e?mm-Ql():0,s=CS(T,s),s!==null)){Et=e,l.cancelPendingCommit=s(_v.bind(null,l,t,e,u,a,n,f,c,i,o,g,T,null,y,h)),It(l,e,f,!o);return}_v(l,t,e,u,a,n,f,c,i,o,g,T)}function Ug(l){for(var t=l;;){var u=t.tag;if((u===0||u===11||u===15)&&t.flags&16384&&(u=t.updateQueue,u!==null&&(u=u.stores,u!==null)))for(var a=0;a<u.length;a++){var n=u[a],e=n.getSnapshot;n=n.value;try{if(!xl(e(),n))return!1}catch{return!1}}if(u=t.child,t.subtreeFlags&16384&&u!==null)u.return=t,t=u;else{if(t===l)break;for(;t.sibling===null;){if(t.return===null||t.return===l)return!0;t=t.return}t.sibling.return=t.return,t=t.sibling}}return!0}function It(l,t,u,a){t=yo(l,t),t&=~uf,t&=~Mu,l.suspendedLanes|=t,l.pingedLanes&=~t,a&&(l.warmLanes|=t),a=l.expirationTimes;for(var n=t;0<n;){var e=31-Vl(n),f=1<<e;a[e]=-1,n&=~f}u!==0&&ho(l,u,t)}function Df(){return(q&6)===0?(jn(0,!1),!1):!0}function A0(){if(C!==null){if(Q===0)var l=C.return;else l=C,Dt=Vu=null,c0(l),da=null,zn=0,l=C;for(;l!==null;)Ly(l.alternate,l),l=l.return;C=null}}function Na(l,t){var u=l.timeoutHandle;return u!==-1&&(l.timeoutHandle=-1,$g(u)),u=l.cancelPendingCommit,u!==null&&(l.cancelPendingCommit=null,u()),Et=0,A0(),x=l,C=u=Mt(l.current,null),R=t,Q=0,Yl=null,$t=!1,Ga=Rn(l,t),_0=!1,_a=Xl=uf=Mu=du=k=0,Ml=mn=null,Ti=!1,Bt=yo(l,t),Sf(),u}function Sm(l,t){M=null,A.H=$e,t===Ba||t===sf?(t=W1(),Q=3):t===Pi?(t=W1(),Q=4):Q=t===g0?8:t!==null&&typeof t=="object"&&typeof t.then=="function"?6:1,Yl=t,C===null&&(k=1,Ie(l,$l(t,l.current)))}function Tm(){var l=Tl.current;return l===null?!0:(R&4194048)===R?zl===null:(R&62914560)===R||(R&536870912)!==0?l===zl:!1}function Em(){var l=A.H;return A.H=$e,l===null?$e:l}function sm(){var l=A.A;return A.A=Dg,l}function ef(){k=4,$t||(R&4194048)!==R&&Tl.current!==null||(Ga=!0),(du&134217727)===0&&(Mu&134217727)===0||x===null||It(x,R,Xl,!1)}function Tc(l,t,u){var a=q;q|=2;var n=Em(),e=sm();(x!==l||R!==t)&&(nf=null,Na(l,t)),t=!1;var f=k;l:do try{if(Q!==0&&C!==null){var c=C,i=Yl;switch(Q){case 8:A0(),f=6;break l;case 3:case 2:case 9:case 6:Tl.current===null&&(t=!0);var o=Q;if(Q=0,Yl=null,ia(l,c,i,o),u&&Ga){f=0;break l}break;default:o=Q,Q=0,Yl=null,ia(l,c,i,o)}}rg(),f=k;break}catch(g){Sm(l,g)}while(!0);return t&&l.shellSuspendCounter++,Dt=Vu=null,q=a,A.H=n,A.A=e,C===null&&(x=null,R=0,Sf()),f}function rg(){for(;C!==null;)zm(C)}function Cg(l,t){var u=q;q|=2;var a=Em(),n=sm();x!==l||R!==t?(nf=null,af=Ql()+500,Na(l,t)):Ga=Rn(l,t);l:do try{if(Q!==0&&C!==null){t=C;var e=Yl;t:switch(Q){case 1:Q=0,Yl=null,ia(l,t,e,1);break;case 2:case 9:if(w1(e)){Q=0,Yl=null,Ov(t);break}t=function(){Q!==2&&Q!==9||x!==l||(Q=7),bt(l)},e.then(t,t);break l;case 3:Q=7;break l;case 4:Q=5;break l;case 7:w1(e)?(Q=0,Yl=null,Ov(t)):(Q=0,Yl=null,ia(l,t,e,7));break;case 5:var f=null;switch(C.tag){case 26:f=C.memoizedState;case 5:case 27:var c=C;if(f?Km(f):c.stateNode.complete){Q=0,Yl=null;var i=c.sibling;if(i!==null)C=i;else{var o=c.return;o!==null?(C=o,Mf(o)):C=null}break t}}Q=0,Yl=null,ia(l,t,e,5);break;case 6:Q=0,Yl=null,ia(l,t,e,6);break;case 8:A0(),k=6;break l;default:throw Error(E(462))}}Hg();break}catch(g){Sm(l,g)}while(!0);return Dt=Vu=null,A.H=a,A.A=n,q=u,C!==null?0:(x=null,R=0,Sf(),k)}function Hg(){for(;C!==null&&!Wd();)zm(C)}function zm(l){var t=xy(l.alternate,l,Bt);l.memoizedProps=l.pendingProps,t===null?Mf(l):C=t}function Ov(l){var t=l,u=t.alternate;switch(t.tag){case 15:case 0:t=ov(u,t,t.pendingProps,t.type,void 0,R);break;case 11:t=ov(u,t,t.pendingProps,t.type.render,t.ref,R);break;case 5:c0(t);var a=t;a===ol&&(U?(Le(a),a.tag===5&&a.stateNode!=null&&(J=a.stateNode)):(Le(a),U=!0));default:Ly(u,t),t=C=po(t,Bt),t=xy(u,t,Bt)}l.memoizedProps=l.pendingProps,t===null?Mf(l):C=t}function ia(l,t,u,a){Dt=Vu=null,c0(t),da=null,zn=0;var n=t.return;try{if(Eg(l,n,t,u,R)){k=1,Ie(l,$l(u,l.current)),C=null;return}}catch(e){if(n!==null)throw C=n,e;k=1,Ie(l,$l(u,l.current)),C=null;return}t.flags&32768?(U||a===1?l=!0:Ga||(R&536870912)!==0?l=!1:($t=l=!0,(a===2||a===9||a===3||a===6)&&(a=Tl.current,a!==null&&a.tag===13&&(a.flags|=16384))),bm(t,l)):Mf(t)}function Mf(l){var t=l;do{if((t.flags&32768)!==0){bm(t,$t);return}l=t.return;var u=Og(t.alternate,t,Bt);if(u!==null){C=u;return}if(t=t.sibling,t!==null){C=t;return}C=t=l}while(t!==null);k===0&&(k=5)}function bm(l,t){do{var u=_g(l.alternate,l);if(u!==null){u.flags&=32767,C=u;return}if(u=l.return,u!==null&&(u.flags|=32768,u.subtreeFlags=0,u.deletions=null),!t&&(l=l.sibling,l!==null)){C=l;return}C=l=u}while(l!==null);k=6,C=null}function _v(l,t,u,a,n,e,f,c,i,o,g,T){l.cancelPendingCommit=null;do Uf();while($!==0);if((q&6)!==0)throw Error(E(327));if(t!==null){if(t===l.current)throw Error(E(177));l===x&&(C=x=null,R=0),qu=t,nt=l,Et=u,si=n,dm=a,Rg(l,t,u,f,c,i,T)}}function Rg(l,t,u,a,n,e,f){var c=t.lanes|t.childLanes;if(Ei=c,c|=wi,nh(l,u,c,a,n,e),Ta=null,(u&335544064)===u?(Ea=fg(l),a=10262):(Ea=null,a=10256),(t.subtreeFlags&a)!==0||(t.flags&a)!==0?(l.callbackNode=null,l.callbackPriority=0,Qg(Qe,function(){return _i(),null})):(l.callbackNode=null,l.callbackPriority=0),lf=!1,a=(t.flags&13878)!==0,(t.subtreeFlags&13878)!==0||a){a=A.T,A.T=null,n=G.p,G.p=2,e=q,q|=4;try{Ag(l,t,u)}finally{q=e,G.p=n,A.T=a}}$=1,lf?Sa=uS(f,l.containerInfo,Ea,zi,bi,Bg,Oi,_i,Yg,null,null):(zi(),bi(),Oi())}function Yg(l){if($!==0){var t=nt.onRecoverableError;t(l,{componentStack:null})}}function Bg(){$===3&&($=0,fm(qu,nt),$=4)}function zi(){if($===1){$=0;var l=nt,t=qu,u=Et,a=(t.flags&13878)!==0;if((t.subtreeFlags&13878)!==0||a){a=A.T,A.T=null;var n=G.p;G.p=2;var e=q;q|=4;try{Pa=tf=!1,nm(t,l,u),u=Mi;var f=Go(l.containerInfo),c=u.focusedElem,i=u.selectionRange;if(f!==c&&c&&c.ownerDocument&&qo(c.ownerDocument.documentElement,c)){if(i!==null&&Ji(c)){var o=i.start,g=i.end;if(g===void 0&&(g=o),"selectionStart"in c)c.selectionStart=o,c.selectionEnd=Math.min(g,c.value.length);else{var T=c.ownerDocument||document,y=T&&T.defaultView||window;if(y.getSelection){var h=y.getSelection(),s=c.textContent.length,b=Math.min(i.start,s),D=i.end===void 0?b:Math.min(i.end,s);!h.extend&&b>D&&(f=D,D=b,b=f);var m=j1(c,b),v=j1(c,D);if(m&&v&&(h.rangeCount!==1||h.anchorNode!==m.node||h.anchorOffset!==m.offset||h.focusNode!==v.node||h.focusOffset!==v.offset)){var d=T.createRange();d.setStart(m.node,m.offset),h.removeAllRanges(),b>D?(h.addRange(d),h.extend(v.node,v.offset)):(d.setEnd(v.node,v.offset),h.addRange(d))}}}}for(T=[],h=c;h=h.parentNode;)h.nodeType===1&&T.push({element:h,left:h.scrollLeft,top:h.scrollTop});for(typeof c.focus=="function"&&c.focus(),c=0;c<T.length;c++){var S=T[c];S.element.scrollLeft=S.left,S.element.scrollTop=S.top}}Ca=!!Di,Mi=Di=null}finally{q=e,G.p=n,A.T=a}}l.current=t,$=2}}function bi(){if($===2){$=0;var l=nt,t=qu,u=(t.flags&8772)!==0;if((t.subtreeFlags&8772)!==0||u){u=A.T,A.T=null;var a=G.p;G.p=2;var n=q;q|=4;try{ky(l,t.alternate,t)}finally{q=n,G.p=a,A.T=u}}$=3}}function Oi(){if($===4||$===3){$=0;var l=Sa;Sa=null,Fd();var t=nt,u=qu,a=Et,n=dm,e=(a&335544064)===a?10262:10256;if((u.subtreeFlags&e)!==0||(u.flags&e)!==0?$=5:($=0,qu=nt=null,Om(t,t.pendingLanes)),e=t.pendingLanes,e===0&&(eu=null),Vi(a),u=u.stateNode,jl&&typeof jl.onCommitFiberRoot=="function")try{jl.onCommitFiberRoot(Hn,u,void 0,(u.current.flags&128)===128)}catch{}if(n!==null){u=A.T,e=G.p,G.p=2,A.T=null;try{for(var f=t.onRecoverableError,c=0;c<n.length;c++){var i=n[c];f(i.value,{componentStack:i.stack})}}finally{A.T=u,G.p=e}}if(n=Ta,f=Ea,Ea=null,n!==null&&(Ta=null,f===null&&(f=[]),l!==null))for(i=0;i<n.length;i++)u=(0,n[i])(f),u!==void 0&&l.finished.finally(u);(Et&3)!==0&&Uf(),bt(t),e=t.pendingLanes,(a&261930)!==0&&(e&42)!==0?t===re?dn++:(dn=0,re=t):(dn=0,re=null),jn(0,!1)}}function Om(l,t){(l.pooledCacheLanes&=t)===0&&(t=l.pooledCache,t!=null&&(l.pooledCache=null,Gn(t)))}function Uf(){return Sa!==null&&(Sa.skipTransition(),Sa=null),zi(),bi(),Oi(),_i()}function _i(){if($!==5)return!1;var l=nt,t=Ei;Ei=0;var u=Vi(Et),a=A.T,n=G.p;try{G.p=32>u?32:u,A.T=null,u=si,si=null;var e=nt,f=Et;if($=0,qu=nt=null,Et=0,(q&6)!==0)throw Error(E(331));var c=q;if(q|=4,om(e.current),cm(e,e.current,f,u),q=c,jn(0,!1),jl&&typeof jl.onPostCommitFiberRoot=="function")try{jl.onPostCommitFiberRoot(Hn,e)}catch{}return!0}finally{G.p=n,A.T=a,Om(l,t)}}function Av(l,t,u){t=$l(u,t),t=ui(l.stateNode,t,2),l=uu(l,t,2),l!==null&&(Yn(l,2),bt(l))}function V(l,t,u){if(l.tag===3)Av(l,l,u);else for(;t!==null;){if(t.tag===3){Av(t,l,u);break}else if(t.tag===1){var a=t.stateNode;if(typeof t.type.getDerivedStateFromError=="function"||typeof a.componentDidCatch=="function"&&(eu===null||!eu.has(a))){l=$l(u,l),u=Xy(2),a=uu(t,u,2),a!==null&&(Qy(u,a,t,l),Yn(a,2),bt(a));break}}t=t.return}}function Ec(l,t,u){var a=l.pingCache;if(a===null){a=l.pingCache=new Mg;var n=new Set;a.set(t,n)}else n=a.get(t),n===void 0&&(n=new Set,a.set(t,n));n.has(u)||(_0=!0,n.add(u),l=qg.bind(null,l,t,u),t.then(l,l))}function qg(l,t,u){var a=l.pingCache;a!==null&&a.delete(t),l.pingedLanes|=l.suspendedLanes&u,l.warmLanes&=~u,x===l&&(R&u)===u&&((k===4||k===3&&(R&62914560)===R&&300>Ql()-Nf)&&(q&2)===0?Na(l,0):uf|=u,_a===R&&(_a=0)),bt(l)}function _m(l,t){t===0&&(t=mo()),l=ju(l,t),l!==null&&(Yn(l,t),bt(l))}function Gg(l){var t=l.memoizedState,u=0;t!==null&&(u=t.retryLane),_m(l,u)}function Xg(l,t){var u=0;switch(l.tag){case 31:case 13:var a=l.stateNode,n=l.memoizedState;n!==null&&(u=n.retryLane);break;case 19:a=l.stateNode;break;case 22:a=l.stateNode._retryCache;break;default:throw Error(E(314))}a!==null&&a.delete(t),_m(l,u)}function Qg(l,t){return Qi(l,t)}var Da=null,$u=null,Ai=!1,ff=!1,sc=!1,kt=0;function bt(l){l!==$u&&l.next===null&&($u===null?Da=$u=l:$u=$u.next=l),ff=!0,Ai||(Ai=!0,Vg())}function jn(l,t){if(!sc&&ff){sc=!0;do for(var u=!1,a=Da;a!==null;){if(!t)if(l!==0){var n=a.pendingLanes;if(n===0)var e=0;else{var f=a.suspendedLanes,c=a.pingedLanes;e=(1<<31-Vl(42|l)+1)-1,e&=n&~(f&~c),e=e&201326741?e&201326741|1:e?e|2:0}e!==0&&(u=!0,Nv(a,e))}else e=R,e=yf(a,a===x?e:0,a.cancelPendingCommit!==null||a.timeoutHandle!==-1),(e&3)===0||Rn(a,e)||(u=!0,Nv(a,e));a=a.next}while(u);sc=!1}}function jg(){Am()}function Am(){ff=Ai=!1;var l=0;kt!==0&&Fg()&&(l=kt);for(var t=Ql(),u=null,a=Da;a!==null;){var n=a.next,e=Nm(a,t);e===0?(a.next=null,u===null?Da=n:u.next=n,n===null&&($u=u)):(u=a,(l!==0||(e&3)!==0)&&(ff=!0)),a=n}$!==0&&$!==5||jn(l,!1),kt!==0&&(kt=0)}function Nm(l,t){for(var u=l.suspendedLanes,a=l.pingedLanes,n=l.expirationTimes,e=l.pendingLanes&-62914561;0<e;){var f=31-Vl(e),c=1<<f,i=n[f];i===-1?((c&u)===0||(c&a)!==0)&&(n[f]=ah(c,t)):i<=t&&(l.expiredLanes|=c),e&=~c}if(t=x,u=R,u=yf(l,l===t?u:0,l.cancelPendingCommit!==null||l.timeoutHandle!==-1),a=l.callbackNode,u===0||l===t&&(Q===2||Q===9)||l.cancelPendingCommit!==null)return a!==null&&a!==null&&If(a),l.callbackNode=null,l.callbackPriority=0;if((u&3)===0||Rn(l,u)){if(t=u&-u,t===l.callbackPriority)return t;switch(a!==null&&If(a),Vi(u)){case 2:case 8:u=vo;break;case 32:u=Qe;break;case 268435456:u=oo;break;default:u=Qe}return a=Dm.bind(null,l),u=Qi(u,a),l.callbackPriority=t,l.callbackNode=u,t}return a!==null&&a!==null&&If(a),l.callbackPriority=2,l.callbackNode=null,2}function Dm(l,t){if($!==0&&$!==5)return l.callbackNode=null,l.callbackPriority=0,null;var u=l.callbackNode;if(Uf()&&l.callbackNode!==u)return null;var a=R;return a=yf(l,l===x?a:0,l.cancelPendingCommit!==null||l.timeoutHandle!==-1),a===0?null:(gm(l,a,t),Nm(l,Ql()),l.callbackNode!=null&&l.callbackNode===u?Dm.bind(null,l):null)}function Nv(l,t){if(Uf())return null;gm(l,t,!0)}function Vg(){Ig(function(){(q&6)!==0?Qi(io,jg):Am()})}function N0(){if(kt===0){var l=Hu;l===0&&(l=$n,$n<<=1,($n&261888)===0&&($n=256)),kt=l}return kt}function Dv(l){return l==null||typeof l=="symbol"||typeof l=="boolean"?null:typeof l=="function"?l:Te(l)}function Zg(l,t,u,a,n){if(t==="submit"&&u&&u.stateNode===n){var e=Dv((n[Rl]||null).action),f=a.submitter;f&&(t=(t=f[Rl]||null)?Dv(t.formAction):f.getAttribute("formAction"),t!==null&&(e=t,f=null));var c=new df("action","action",null,a,n);l.push({event:c,listeners:[{instance:null,listener:function(){if(a.defaultPrevented){if(kt!==0){var i=new FormData(n,f);li(u,{pending:!0,data:i,method:n.method,action:e},null,i)}}else typeof e=="function"&&(c.preventDefault(),i=new FormData(n,f),li(u,{pending:!0,data:i,method:n.method,action:e},e,i))},currentTarget:n}]})}}for(ye=0;ye<Lc.length;ye++)me=Lc[ye],Mv=me.toLowerCase(),Uv=me[0].toUpperCase()+me.slice(1),et(Mv,"on"+Uv);var me,Mv,Uv,ye;et(Qo,"onAnimationEnd");et(jo,"onAnimationIteration");et(Vo,"onAnimationStart");et("dblclick","onDoubleClick");et("focusin","onFocus");et("focusout","onBlur");et(kh,"onTransitionRun");et(Ph,"onTransitionStart");et(lg,"onTransitionCancel");et(Zo,"onTransitionEnd");za("onMouseEnter",["mouseout","mouseover"]);za("onMouseLeave",["mouseout","mouseover"]);za("onPointerEnter",["pointerout","pointerover"]);za("onPointerLeave",["pointerout","pointerover"]);Xu("onChange","change click focusin focusout input keydown keyup selectionchange".split(" "));Xu("onSelect","focusout contextmenu dragend focusin keydown keyup mousedown mouseup selectionchange".split(" "));Xu("onBeforeInput",["compositionend","keypress","textInput","paste"]);Xu("onCompositionEnd","compositionend focusout keydown keypress keyup mousedown".split(" "));Xu("onCompositionStart","compositionstart focusout keydown keypress keyup mousedown".split(" "));Xu("onCompositionUpdate","compositionupdate focusout keydown keypress keyup mousedown".split(" "));var _n="abort canplay canplaythrough durationchange emptied encrypted ended error loadeddata loadedmetadata loadstart pause play playing progress ratechange resize seeked seeking stalled suspend timeupdate volumechange waiting".split(" "),xg=new Set("beforetoggle cancel close invalid load scroll scrollend toggle".split(" ").concat(_n));function Mm(l,t){t=(t&4)!==0;for(var u=0;u<l.length;u++){var a=l[u],n=a.event;a=a.listeners;l:{var e=void 0;if(t)for(var f=a.length-1;0<=f;f--){var c=a[f],i=c.instance,o=c.currentTarget;if(c=c.listener,i!==e&&n.isPropagationStopped())break l;e=c,n.currentTarget=o;try{e(n)}catch(g){Ve(g)}n.currentTarget=null,e=i}else for(f=0;f<a.length;f++){if(c=a[f],i=c.instance,o=c.currentTarget,c=c.listener,i!==e&&n.isPropagationStopped())break l;e=c,n.currentTarget=o;try{e(n)}catch(g){Ve(g)}n.currentTarget=null,e=i}}}}function r(l,t){var u=t[O1];u===void 0&&(u=t[O1]=new Set);var a=l+"__bubble";u.has(a)||(Um(t,l,2,!1),u.add(a))}function zc(l,t,u){var a=0;t&&(a|=4),Um(u,l,a,t)}var de="_reactListening"+Math.random().toString(36).slice(2);function D0(l){if(!l[de]){l[de]=!0,so.forEach(function(u){u!=="selectionchange"&&(xg.has(u)||zc(u,!1,l),zc(u,!0,l))});var t=l.nodeType===9?l:l.ownerDocument;t===null||t[de]||(t[de]=!0,zc("selectionchange",!1,t))}}function Um(l,t,u,a){switch(km(t)){case 2:var n=BS;break;case 8:n=qS;break;default:n=Y0}u=n.bind(null,t,u,l),n=void 0,!jc||t!=="touchstart"&&t!=="touchmove"&&t!=="wheel"||(n=!0),a?n!==void 0?l.addEventListener(t,u,{capture:!0,passive:n}):l.addEventListener(t,u,!0):n!==void 0?l.addEventListener(t,u,{passive:n}):l.addEventListener(t,u,!1)}function bc(l,t,u,a,n){var e=a;if((t&1)===0&&(t&2)===0&&a!==null)l:for(;;){if(a===null)return;var f=a.tag;if(f===3||f===4){var c=a.stateNode.containerInfo;if(c===n)break;if(f===4)for(f=a.return;f!==null;){var i=f.tag;if((i===3||i===4)&&f.stateNode.containerInfo===n)return;f=f.return}for(;c!==null;){if(f=bu(c),f===null)return;if(i=f.tag,i===5||i===6||i===26||i===27){a=e=f;continue l}c=c.parentNode}}a=a.return}Mo(function(){var o=e,g=xi(u),T=[];l:{var y=xo.get(l);if(y!==void 0){var h=df,s=l;switch(l){case"keypress":if(se(u)===0)break l;case"keydown":case"keyup":h=Uh;break;case"focusin":s="focus",h=ac;break;case"focusout":s="blur",h=ac;break;case"beforeblur":case"afterblur":h=ac;break;case"click":if(u.button===2)break l;case"auxclick":case"dblclick":case"mousedown":case"mousemove":case"mouseup":case"mouseout":case"mouseover":case"contextmenu":h=C1;break;case"drag":case"dragend":case"dragenter":case"dragexit":case"dragleave":case"dragover":case"dragstart":case"drop":h=Sh;break;case"touchcancel":case"touchend":case"touchmove":case"touchstart":h=Yh;break;case Qo:case jo:case Vo:h=sh;break;case Zo:h=qh;break;case"scroll":case"scrollend":h=hh;break;case"wheel":h=Xh;break;case"copy":case"cut":case"paste":h=bh;break;case"gotpointercapture":case"lostpointercapture":case"pointercancel":case"pointerdown":case"pointermove":case"pointerout":case"pointerover":case"pointerup":h=R1;break;case"submit":h=Hh;break;case"toggle":case"beforetoggle":h=jh}var b=(t&4)!==0,D=!b&&(l==="scroll"||l==="scrollend"),m=b?y!==null?y+"Capture":null:y;b=[];for(var v=o,d;v!==null;){var S=v;if(d=S.stateNode,S=S.tag,S!==5&&S!==26&&S!==27||d===null||m===null||(S=gn(v,m),S!=null&&b.push(An(v,S,d))),D)break;v=v.return}0<b.length&&(y=new h(y,s,null,u,g),T.push({event:y,listeners:b}))}}if((t&7)===0){l:{if(h=l==="mouseover"||l==="pointerover",y=l==="mouseout"||l==="pointerout",h&&u!==Qc&&(s=u.relatedTarget||u.fromElement)&&(bu(s)||s[Ra]))break l;(y||h)&&(s=g.window===g?g:(h=g.ownerDocument)?h.defaultView||h.parentWindow:window,y?(h=u.relatedTarget||u.toElement,y=o,h=h?bu(h):null,h!==null&&(D=Cn(h),b=h.tag,h!==D||b!==5&&b!==27&&b!==6)&&(h=null)):(y=null,h=o),y!==h&&(b=C1,S="onMouseLeave",m="onMouseEnter",v="mouse",(l==="pointerout"||l==="pointerover")&&(b=R1,S="onPointerLeave",m="onPointerEnter",v="pointer"),D=y==null?s:Ia(y),d=h==null?s:Ia(h),s=new b(S,v+"leave",y,u,g),s.target=D,s.relatedTarget=d,S=null,bu(g)===o&&(b=new b(m,v+"enter",h,u,g),b.target=d,b.relatedTarget=D,S=b),D=S,b=y&&h?Dc(y,h,Lg):null,y!==null&&rv(T,s,y,b,!1),h!==null&&D!==null&&rv(T,D,h,b,!0)))}l:{if(y=o?Ia(o):window,h=y.nodeName&&y.nodeName.toLowerCase(),h==="select"||h==="input"&&y.type==="file")var z=G1;else if(q1(y))if(Yo)z=Fh;else{z=wh;var H=Jh}else h=y.nodeName,!h||h.toLowerCase()!=="input"||y.type!=="checkbox"&&y.type!=="radio"?o&&Zi(o.elementType)&&(z=G1):z=Wh;if(z&&(z=z(l,o))){Ro(T,z,u,g);break l}H&&H(l,y,o)}switch(H=o?Ia(o):window,l){case"focusin":(q1(H)||H.contentEditable==="true")&&(ua=H,Zc=o,un=null);break;case"focusout":un=Zc=ua=null;break;case"mousedown":xc=!0;break;case"contextmenu":case"mouseup":case"dragend":xc=!1,V1(T,u,g);break;case"selectionchange":if(Ih)break;case"keydown":case"keyup":V1(T,u,g)}var O;if(Ki)l:{switch(l){case"compositionstart":var _="onCompositionStart";break l;case"compositionend":_="onCompositionEnd";break l;case"compositionupdate":_="onCompositionUpdate";break l}_=void 0}else ta?Co(l,u)&&(_="onCompositionEnd"):l==="keydown"&&u.keyCode===229&&(_="onCompositionStart");_&&(ro&&u.locale!=="ko"&&(ta||_!=="onCompositionStart"?_==="onCompositionEnd"&&ta&&(O=Uo()):(Wt=g,Li="value"in Wt?Wt.value:Wt.textContent,ta=!0)),H=cf(o,_),0<H.length&&(_=new H1(_,l,null,u,g),T.push({event:_,listeners:H}),O?_.data=O:(O=Ho(u),O!==null&&(_.data=O)))),(O=Zh?xh(l,u):Lh(l,u))&&(_=cf(o,"onBeforeInput"),0<_.length&&(H=new H1("onBeforeInput","beforeinput",null,u,g),T.push({event:H,listeners:_}),H.data=O)),Zg(T,l,o,u,g)}Mm(T,t)})}function An(l,t,u){return{instance:l,listener:t,currentTarget:u}}function cf(l,t){for(var u=t+"Capture",a=[];l!==null;){var n=l,e=n.stateNode;if(n=n.tag,n!==5&&n!==26&&n!==27||e===null||(n=gn(l,u),n!=null&&a.unshift(An(l,n,e)),n=gn(l,t),n!=null&&a.push(An(l,n,e))),l.tag===3)return a;l=l.return}return[]}function Lg(l){if(l===null)return null;do l=l.return;while(l&&l.tag!==5&&l.tag!==27);return l||null}function rv(l,t,u,a,n){for(var e=t._reactName,f=[];u!==null&&u!==a;){var c=u,i=c.alternate,o=c.stateNode;if(c=c.tag,i!==null&&i===a)break;c!==5&&c!==26&&c!==27||o===null||(i=o,n?(o=gn(u,e),o!=null&&f.unshift(An(u,o,i))):n||(o=gn(u,e),o!=null&&f.push(An(u,o,i)))),u=u.return}f.length!==0&&l.push({event:t,listeners:f})}var pg=/\r\n?/g,Kg=/\u0000|\uFFFD/g;function Cv(l){return(typeof l=="string"?l:""+l).replace(pg,`
`).replace(Kg,"")}function rm(l,t){return t=Cv(t),Cv(l)===t}function j(l,t,u,a,n,e){switch(u){case"children":if(typeof a=="string")t==="body"||t==="textarea"&&a===""||ba(l,a);else if(typeof a=="number"||typeof a=="bigint")t!=="body"&&ba(l,""+a);else return;break;case"className":Pn(l,"class",a);break;case"tabIndex":Pn(l,"tabindex",a);break;case"dir":case"role":case"viewBox":case"width":case"height":Pn(l,u,a);break;case"style":Do(l,a,e);return;case"data":if(t!=="object"){Pn(l,"data",a);break}case"src":case"href":if(a===""&&(t!=="a"||u!=="href")){l.removeAttribute(u);break}if(a==null||typeof a=="function"||typeof a=="symbol"||typeof a=="boolean"){l.removeAttribute(u);break}a=Te(a),l.setAttribute(u,a);break;case"action":case"formAction":if(typeof a=="function"){l.setAttribute(u,"javascript:throw new Error('A React form was unexpectedly submitted. If you called form.submit() manually, consider using form.requestSubmit() instead. If you\\'re trying to use event.stopPropagation() in a submit event handler, consider also calling event.preventDefault().')");break}else typeof e=="function"&&(u==="formAction"?(t!=="input"&&j(l,t,"name",n.name,n,null),j(l,t,"formEncType",n.formEncType,n,null),j(l,t,"formMethod",n.formMethod,n,null),j(l,t,"formTarget",n.formTarget,n,null)):(j(l,t,"encType",n.encType,n,null),j(l,t,"method",n.method,n,null),j(l,t,"target",n.target,n,null)));if(a==null||typeof a=="symbol"||typeof a=="boolean"){l.removeAttribute(u);break}a=Te(a),l.setAttribute(u,a);break;case"onClick":a!=null&&(l.onclick=ht);return;case"onScroll":a!=null&&r("scroll",l);return;case"onScrollEnd":a!=null&&r("scrollend",l);return;case"dangerouslySetInnerHTML":if(a!=null){if(typeof a!="object"||!("__html"in a))throw Error(E(61));if(u=a.__html,u!=null){if(n.children!=null)throw Error(E(60));e?.__html!==u&&(l.innerHTML=u)}}break;case"multiple":l.multiple=a&&typeof a!="function"&&typeof a!="symbol";break;case"muted":l.muted=a&&typeof a!="function"&&typeof a!="symbol";break;case"suppressContentEditableWarning":case"suppressHydrationWarning":case"defaultValue":case"defaultChecked":case"innerHTML":case"ref":break;case"autoFocus":break;case"xlinkHref":if(a==null||typeof a=="function"||typeof a=="boolean"||typeof a=="symbol"){l.removeAttribute("xlink:href");break}u=Te(a),l.setAttributeNS("http://www.w3.org/1999/xlink","xlink:href",u);break;case"contentEditable":case"spellCheck":case"draggable":case"value":case"autoReverse":case"externalResourcesRequired":case"focusable":case"preserveAlpha":a!=null&&typeof a!="function"&&typeof a!="symbol"?l.setAttribute(u,a):l.removeAttribute(u);break;case"inert":case"allowFullScreen":case"async":case"autoPlay":case"controls":case"credentialless":case"default":case"defer":case"disabled":case"disablePictureInPicture":case"disableRemotePlayback":case"formNoValidate":case"hidden":case"loop":case"noModule":case"noValidate":case"open":case"playsInline":case"readOnly":case"required":case"reversed":case"scoped":case"seamless":case"itemScope":a&&typeof a!="function"&&typeof a!="symbol"?l.setAttribute(u,""):l.removeAttribute(u);break;case"capture":case"download":a===!0?l.setAttribute(u,""):a!==!1&&a!=null&&typeof a!="function"&&typeof a!="symbol"?l.setAttribute(u,a):l.removeAttribute(u);break;case"cols":case"rows":case"size":case"span":a!=null&&typeof a!="function"&&typeof a!="symbol"&&!isNaN(a)&&1<=a?l.setAttribute(u,a):l.removeAttribute(u);break;case"rowSpan":case"start":a==null||typeof a=="function"||typeof a=="symbol"||isNaN(a)?l.removeAttribute(u):l.setAttribute(u,a);break;case"popover":r("beforetoggle",l),r("toggle",l),Se(l,"popover",a);break;case"xlinkActuate":_t(l,"http://www.w3.org/1999/xlink","xlink:actuate",a);break;case"xlinkArcrole":_t(l,"http://www.w3.org/1999/xlink","xlink:arcrole",a);break;case"xlinkRole":_t(l,"http://www.w3.org/1999/xlink","xlink:role",a);break;case"xlinkShow":_t(l,"http://www.w3.org/1999/xlink","xlink:show",a);break;case"xlinkTitle":_t(l,"http://www.w3.org/1999/xlink","xlink:title",a);break;case"xlinkType":_t(l,"http://www.w3.org/1999/xlink","xlink:type",a);break;case"xmlBase":_t(l,"http://www.w3.org/XML/1998/namespace","xml:base",a);break;case"xmlLang":_t(l,"http://www.w3.org/XML/1998/namespace","xml:lang",a);break;case"xmlSpace":_t(l,"http://www.w3.org/XML/1998/namespace","xml:space",a);break;case"is":Se(l,"is",a);break;case"innerText":case"textContent":return;default:if(!(2<u.length)||u[0]!=="o"&&u[0]!=="O"||u[1]!=="n"&&u[1]!=="N")u=mh.get(u)||u,Se(l,u,a);else return}B=!0}function Ni(l,t,u,a,n,e){switch(u){case"style":Do(l,a,e);return;case"dangerouslySetInnerHTML":if(a!=null){if(typeof a!="object"||!("__html"in a))throw Error(E(61));if(u=a.__html,u!=null){if(n.children!=null)throw Error(E(60));e?.__html!==u&&(l.innerHTML=u)}}break;case"children":if(typeof a=="string")ba(l,a);else if(typeof a=="number"||typeof a=="bigint")ba(l,""+a);else return;break;case"onScroll":a!=null&&r("scroll",l);return;case"onScrollEnd":a!=null&&r("scrollend",l);return;case"onClick":a!=null&&(l.onclick=ht);return;case"suppressContentEditableWarning":case"suppressHydrationWarning":case"innerHTML":case"ref":return;case"innerText":case"textContent":return;default:if(!zo.hasOwnProperty(u))l:{if(u[0]==="o"&&u[1]==="n"&&(n=u.endsWith("Capture"),e=u.slice(2,n?u.length-7:void 0),t=l[Rl]||null,t=t!=null?t[u]:null,typeof t=="function"&&l.removeEventListener(e,t,n),typeof a=="function")){typeof t!="function"&&t!==null&&(u in l?l[u]=null:l.hasAttribute(u)&&l.removeAttribute(u)),l.addEventListener(e,a,n);break l}B=!0,u in l?l[u]=a:a===!0?l.setAttribute(u,""):Se(l,u,a)}return}B=!0}function Sl(l,t,u){switch(t){case"div":case"span":case"svg":case"path":case"a":case"g":case"p":case"li":break;case"img":r("error",l),r("load",l);var a=!1,n=!1,e;for(e in u)if(u.hasOwnProperty(e)){var f=u[e];if(f!=null)switch(e){case"src":a=!0;break;case"srcSet":n=!0;break;case"children":case"dangerouslySetInnerHTML":throw Error(E(137,t));default:j(l,t,e,f,u,null)}}n&&j(l,t,"srcSet",u.srcSet,u,null),a&&j(l,t,"src",u.src,u,null);return;case"input":r("invalid",l);var c=e=f=n=null,i=null,o=null;for(a in u)if(u.hasOwnProperty(a)){var g=u[a];if(g!=null)switch(a){case"name":n=g;break;case"type":f=g;break;case"checked":i=g;break;case"defaultChecked":o=g;break;case"value":e=g;break;case"defaultValue":c=g;break;case"children":case"dangerouslySetInnerHTML":if(g!=null)throw Error(E(137,t));break;default:j(l,t,a,g,u,null)}}_o(l,e,c,i,o,f,n,!1);return;case"select":r("invalid",l),a=f=e=null;for(n in u)if(u.hasOwnProperty(n)&&(c=u[n],c!=null))switch(n){case"value":e=c;break;case"defaultValue":f=c;break;case"multiple":a=c;default:j(l,t,n,c,u,null)}t=e,u=f,l.multiple=!!a,t!=null?oa(l,!!a,t,!1):u!=null&&oa(l,!!a,u,!0);return;case"textarea":r("invalid",l),e=n=a=null;for(f in u)if(u.hasOwnProperty(f)&&(c=u[f],c!=null))switch(f){case"value":a=c;break;case"defaultValue":n=c;break;case"children":e=c;break;case"dangerouslySetInnerHTML":if(c!=null)throw Error(E(91));break;default:j(l,t,f,c,u,null)}No(l,a,n,e);return;case"option":for(i in u)u.hasOwnProperty(i)&&(a=u[i],a!=null)&&(i==="selected"?l.selected=a&&typeof a!="function"&&typeof a!="symbol":j(l,t,i,a,u,null));return;case"dialog":r("beforetoggle",l),r("toggle",l),r("cancel",l),r("close",l);break;case"iframe":case"object":r("load",l);break;case"video":case"audio":for(a=0;a<_n.length;a++)r(_n[a],l);break;case"image":r("error",l),r("load",l);break;case"details":r("toggle",l);break;case"embed":case"source":case"link":r("error",l),r("load",l);case"area":case"base":case"br":case"col":case"hr":case"keygen":case"meta":case"param":case"track":case"wbr":case"menuitem":for(o in u)if(u.hasOwnProperty(o)&&(a=u[o],a!=null))switch(o){case"children":case"dangerouslySetInnerHTML":throw Error(E(137,t));default:j(l,t,o,a,u,null)}return;default:if(Zi(t)){for(g in u)u.hasOwnProperty(g)&&(a=u[g],a!==void 0&&Ni(l,t,g,a,u,void 0));return}}for(c in u)u.hasOwnProperty(c)&&(a=u[c],a!=null&&j(l,t,c,a,u,null))}var Jg={};function wg(l,t,u,a){switch(t){case"div":case"span":case"svg":case"path":case"a":case"g":case"p":case"li":break;case"input":var n=null,e=null,f=null,c=null,i=null,o=null,g=null;for(h in u){var T=u[h];if(u.hasOwnProperty(h)&&T!=null)switch(h){case"checked":break;case"value":break;case"defaultValue":i=T;default:a.hasOwnProperty(h)||j(l,t,h,null,a,T)}}for(var y in a){var h=a[y];if(T=u[y],a.hasOwnProperty(y)&&(h!=null||T!=null))switch(y){case"type":h!==T&&(B=!0),e=h;break;case"name":h!==T&&(B=!0),n=h;break;case"checked":h!==T&&(B=!0),o=h;break;case"defaultChecked":h!==T&&(B=!0),g=h;break;case"value":h!==T&&(B=!0),f=h;break;case"defaultValue":h!==T&&(B=!0),c=h;break;case"children":case"dangerouslySetInnerHTML":if(h!=null)throw Error(E(137,t));break;default:h!==T&&j(l,t,y,h,a,T)}}Xc(l,f,c,i,o,g,e,n);return;case"select":h=f=c=y=null;for(e in u)if(i=u[e],u.hasOwnProperty(e)&&i!=null)switch(e){case"value":break;case"multiple":h=i;default:a.hasOwnProperty(e)||j(l,t,e,null,a,i)}for(n in a)if(e=a[n],i=u[n],a.hasOwnProperty(n)&&(e!=null||i!=null))switch(n){case"value":e!==i&&(B=!0),y=e;break;case"defaultValue":e!==i&&(B=!0),c=e;break;case"multiple":e!==i&&(B=!0),f=e;default:e!==i&&j(l,t,n,e,a,i)}t=c,u=f,a=h,y!=null?oa(l,!!u,y,!1):!!a!=!!u&&(t!=null?oa(l,!!u,t,!0):oa(l,!!u,u?[]:"",!1));return;case"textarea":h=y=null;for(c in u)if(n=u[c],u.hasOwnProperty(c)&&n!=null&&!a.hasOwnProperty(c))switch(c){case"value":break;case"children":break;default:j(l,t,c,null,a,n)}for(f in a)if(n=a[f],e=u[f],a.hasOwnProperty(f)&&(n!=null||e!=null))switch(f){case"value":n!==e&&(B=!0),y=n;break;case"defaultValue":n!==e&&(B=!0),h=n;break;case"children":break;case"dangerouslySetInnerHTML":if(n!=null)throw Error(E(91));break;default:n!==e&&j(l,t,f,n,a,e)}Ao(l,y,h);return;case"option":for(var s in u)y=u[s],u.hasOwnProperty(s)&&y!=null&&!a.hasOwnProperty(s)&&(s==="selected"?l.selected=!1:j(l,t,s,null,a,y));for(i in a)y=a[i],h=u[i],a.hasOwnProperty(i)&&y!==h&&(y!=null||h!=null)&&(i==="selected"?(y!==h&&(B=!0),l.selected=y&&typeof y!="function"&&typeof y!="symbol"):j(l,t,i,y,a,h));return;case"img":case"link":case"area":case"base":case"br":case"col":case"embed":case"hr":case"keygen":case"meta":case"param":case"source":case"track":case"wbr":case"menuitem":for(var b in u)y=u[b],u.hasOwnProperty(b)&&y!=null&&!a.hasOwnProperty(b)&&j(l,t,b,null,a,y);for(o in a)if(y=a[o],h=u[o],a.hasOwnProperty(o)&&y!==h&&(y!=null||h!=null))switch(o){case"children":case"dangerouslySetInnerHTML":if(y!=null)throw Error(E(137,t));break;default:j(l,t,o,y,a,h)}return;default:if(Zi(t)){for(var D in u)y=u[D],u.hasOwnProperty(D)&&y!==void 0&&!a.hasOwnProperty(D)&&Ni(l,t,D,void 0,a,y);for(g in a)y=a[g],h=u[g],!a.hasOwnProperty(g)||y===h||y===void 0&&h===void 0||Ni(l,t,g,y,a,h);return}}for(var m in u)y=u[m],u.hasOwnProperty(m)&&y!=null&&!a.hasOwnProperty(m)&&j(l,t,m,null,a,y);for(T in a)y=a[T],h=u[T],!a.hasOwnProperty(T)||y===h||y==null&&h==null||j(l,t,T,y,a,h)}function Hv(l){switch(l){case"css":case"script":case"font":case"img":case"image":case"input":case"link":return!0;default:return!1}}function Wg(){if(typeof performance.getEntriesByType=="function"){for(var l=0,t=0,u=performance.getEntriesByType("resource"),a=0;a<u.length;a++){var n=u[a],e=n.transferSize,f=n.initiatorType,c=n.duration;if(e&&c&&Hv(f)){for(f=0,c=n.responseEnd,a+=1;a<u.length;a++){var i=u[a],o=i.startTime;if(o>c)break;var g=i.transferSize,T=i.initiatorType;g&&Hv(T)&&(i=i.responseEnd,f+=g*(i<c?1:(c-o)/(i-o)))}if(--a,t+=8*(e+f)/(n.duration/1e3),l++,10<l)break}}if(0<l)return t/l/1e6}return navigator.connection&&(l=navigator.connection.downlink,typeof l=="number")?l:5}var Di=null,Mi=null;function Nn(l){return l.nodeType===9?l:l.ownerDocument}function Rv(l){switch(l){case"http://www.w3.org/2000/svg":return 1;case"http://www.w3.org/1998/Math/MathML":return 2;default:return 0}}function Cm(l,t){if(l===0)switch(t){case"svg":return 1;case"math":return 2;default:return 0}return l===1&&t==="foreignObject"?0:l}function Hm(l,t,u,a){return u=Nn(u).createElement(l),u[ml]=a,u[Rl]=t,Sl(u,l,t),vl(u),u}function Ui(l,t){return l==="textarea"||l==="noscript"||typeof t.children=="string"||typeof t.children=="number"||typeof t.children=="bigint"||typeof t.dangerouslySetInnerHTML=="object"&&t.dangerouslySetInnerHTML!==null&&t.dangerouslySetInnerHTML.__html!=null}var Oc=null;function Fg(){var l=window.event;return l&&l.type==="popstate"?l===Oc?!1:(Oc=l,!0):(Oc=null,!1)}var M0=typeof setTimeout=="function"?setTimeout:void 0,$g=typeof clearTimeout=="function"?clearTimeout:void 0,Yv=typeof Promise=="function"?Promise:void 0,Bv=typeof requestAnimationFrame=="function"?requestAnimationFrame:M0,Ig=typeof queueMicrotask=="function"?queueMicrotask:typeof Yv<"u"?function(l){return Yv.resolve(null).then(l).catch(kg)}:M0;function kg(l){setTimeout(function(){throw l})}function gu(l){return l==="head"}function qv(l,t){var u=t,a=0;do{var n=u.nextSibling;if(l.removeChild(u),n&&n.nodeType===8)if(u=n.data,u==="/$"||u==="/&"){if(a===0){l.removeChild(n),Ha(t);return}a--}else if(u==="$"||u==="$?"||u==="$~"||u==="$!"||u==="&")a++;else if(u==="html")Ac(l.ownerDocument.documentElement);else if(u==="head"){u=l.ownerDocument.head,Ac(u);for(var e=u.firstChild;e;){var f=e.nextSibling,c=e.nodeName;e[Bn]||c==="SCRIPT"||c==="STYLE"||c==="LINK"&&e.rel.toLowerCase()==="stylesheet"||u.removeChild(e),e=f}}else u==="body"&&Ac(l.ownerDocument.body);u=n}while(u);Ha(t)}function Gv(l,t){var u=l;l=0;do{var a=u.nextSibling;if(u.nodeType===1?t?(u._stashedDisplay=u.style.display,u.style.display="none"):(u.style.display=u._stashedDisplay||"",u.getAttribute("style")===""&&u.removeAttribute("style")):u.nodeType===3&&(t?(u._stashedText=u.nodeValue,u.nodeValue=""):u.nodeValue=u._stashedText||""),a&&a.nodeType===8)if(u=a.data,u==="/$"){if(l===0)break;l--}else u!=="$"&&u!=="$?"&&u!=="$~"&&u!=="$!"||l++;u=a}while(u)}function Rm(l,t,u){if(t=CSS.escape(t)!==t?"r-"+btoa(t).replace(/=/g,""):t,l.style.viewTransitionName=t,u!=null&&(l.style.viewTransitionClass=u),u=getComputedStyle(l),u.display==="inline"){if(t=l.getClientRects(),t.length===1)var a=1;else for(var n=a=0;n<t.length;n++){var e=t[n];0<e.width&&0<e.height&&a++}a===1&&(l=l.style,l.display=t.length===1?"inline-block":"block",l.marginTop="-"+u.paddingTop,l.marginBottom="-"+u.paddingBottom)}}function Ym(l,t){l=l.style,t=t.style;var u=t!=null?t.hasOwnProperty("viewTransitionName")?t.viewTransitionName:t.hasOwnProperty("view-transition-name")?t["view-transition-name"]:null:null;l.viewTransitionName=u==null||typeof u=="boolean"?"":(""+u).trim(),u=t!=null?t.hasOwnProperty("viewTransitionClass")?t.viewTransitionClass:t.hasOwnProperty("view-transition-class")?t["view-transition-class"]:null:null,l.viewTransitionClass=u==null||typeof u=="boolean"?"":(""+u).trim(),l.display==="inline-block"&&(t==null?l.display=l.margin="":(u=t.display,l.display=u==null||typeof u=="boolean"?"":u,u=t.margin,u!=null?l.margin=u:(u=t.hasOwnProperty("marginTop")?t.marginTop:t["margin-top"],l.marginTop=u==null||typeof u=="boolean"?"":u,t=t.hasOwnProperty("marginBottom")?t.marginBottom:t["margin-bottom"],l.marginBottom=t==null||typeof t=="boolean"?"":t)))}function Bm(l,t,u){return u=u.ownerDocument.defaultView,{rect:l,abs:t.position==="absolute"||t.position==="fixed",clip:t.clipPath!=="none"||t.overflow!=="visible"||t.filter!=="none"||t.mask!=="none"||t.mask!=="none"||t.borderRadius!=="0px",view:0<=l.bottom&&0<=l.right&&l.top<=u.innerHeight&&l.left<=u.innerWidth}}function ri(l){var t=l.getBoundingClientRect(),u=getComputedStyle(l);return Bm(t,u,l)}function Pg(l){var t=l.getBoundingClientRect();t=new DOMRect(t.x+2e4,t.y+2e4,t.width,t.height);var u=getComputedStyle(l);return Bm(t,u,l)}function lS(l){return l.documentElement.clientHeight}function tS(l){this.addEventListener("load",l),this.addEventListener("error",l)}function uS(l,t,u,a,n,e,f,c,i){var o=t.nodeType===9?t:t.ownerDocument;try{var g=o.startViewTransition({update:function(){var y=o.defaultView,h=y.navigation&&y.navigation.transition,s=o.fonts.status;a();var b=[];if(s==="loaded"&&(lS(o),o.fonts.status==="loading"&&b.push(o.fonts.ready)),s=b.length,l!==null)for(var D=l.suspenseyImages,m=0,v=0;v<D.length;v++){var d=D[v];if(!d.complete){var S=d.getBoundingClientRect();if(0<S.bottom&&0<S.right&&S.top<y.innerHeight&&S.left<y.innerWidth){if(m+=Jm(d),m>Re){b.length=s;break}d=new Promise(tS.bind(d)),b.push(d)}}}if(0<b.length)return y=Promise.race([Promise.all(b),new Promise(function(z){return setTimeout(z,500)})]).then(n,n),(h?Promise.allSettled([h.finished,y]):y).then(e,e);if(n(),h)return h.finished.then(e,e);e()},types:u});o.__reactViewTransition=g;var T=[];return g.ready.then(function(){for(var y=o.documentElement.getAnimations({subtree:!0}),h=0;h<y.length;h++){var s=y[h],b=s.effect,D=b.pseudoElement;if(D!=null&&D.startsWith("::view-transition")){T.push(s),s=b.getKeyframes();for(var m=D=void 0,v=!0,d=0;d<s.length;d++){var S=s[d],z=S.width;if(D===void 0)D=z;else if(D!==z){v=!1;break}if(z=S.height,m===void 0)m=z;else if(m!==z){v=!1;break}delete S.width,delete S.height,S.transform==="none"&&delete S.transform}v&&D!==void 0&&m!==void 0&&(b.setKeyframes(s),v=getComputedStyle(b.target,b.pseudoElement),v.width!==D||v.height!==m)&&(v=s[0],v.width=D,v.height=m,v=s[s.length-1],v.width=D,v.height=m,b.setKeyframes(s))}}f()},function(y){o.__reactViewTransition===g&&(o.__reactViewTransition=null);try{typeof y=="object"&&y!==null&&y.name==="InvalidStateError"&&(y.message==="View transition was skipped because document visibility state is hidden."||y.message==="Skipping view transition because document visibility state has become hidden."||y.message==="Skipping view transition because viewport size changed."||y.message==="Transition was aborted because of invalid state")&&(y=null),y!==null&&i(y)}finally{a(),n(),f()}}),g.finished.finally(function(){for(var y=0;y<T.length;y++)T[y].cancel();o.__reactViewTransition===g&&(o.__reactViewTransition=null),c()}),g}catch{return a(),n(),f(),null}}function Ou(l,t){this._scope=document.documentElement,this._selector="::view-transition-"+l+"("+t+")"}Ou.prototype.animate=function(l,t){return t=typeof t=="number"?{duration:t}:L({},t),t.pseudoElement=this._selector,this._scope.animate(l,t)};Ou.prototype.getAnimations=function(){for(var l=this._scope,t=this._selector,u=l.getAnimations({subtree:!0}),a=[],n=0;n<u.length;n++){var e=u[n].effect;e!==null&&e.target===l&&e.pseudoElement===t&&a.push(u[n])}return a};Ou.prototype.getComputedStyle=function(){return getComputedStyle(this._scope,this._selector)};function qm(l){return{name:l,group:new Ou("group",l),imagePair:new Ou("image-pair",l),old:new Ou("old",l),new:new Ou("new",l)}}function Ll(l){this._fragmentFiber=l,this._observers=this._eventListeners=null}Ll.prototype.addEventListener=function(l,t,u){var a=null,n=null;if(!(u!=null&&typeof u!="boolean"&&(a=u.signal||null,a!==null&&a.aborted))){this._eventListeners===null&&(this._eventListeners=[]);var e=this._eventListeners;if(Gm(e,l,t,u)===-1){var f=this,c=t;u!=null&&typeof u!="boolean"&&u.once===!0&&(c=function(i){f.removeEventListener(l,t,u),typeof t=="function"?t.call(this,i):t.handleEvent(i)}),a!==null&&(n=f.removeEventListener.bind(f,l,t,u),a.addEventListener("abort",n,{once:!0}),n=a.removeEventListener.bind(a,"abort",n)),a=Ma(u),e.push({type:l,listener:t,optionsOrUseCapture:u,attachedListener:c,cleanup:n}),Hl(this._fragmentFiber.child,!1,aS,l,c,a)}this._eventListeners=e}};function aS(l,t,u,a){return el(l).addEventListener(t,u,a),!1}Ll.prototype.removeEventListener=function(l,t,u){var a=this._eventListeners;if(a!==null&&(t=Gm(a,l,t,u),t!==-1)){var n=a[t];u=n.attachedListener;var e=n.cleanup;n=Ma(n.optionsOrUseCapture),Hl(this._fragmentFiber.child,!1,nS,l,u,n),a.splice(t,1),e!==null&&e()}};function nS(l,t,u,a){return el(l).removeEventListener(t,u,a),!1}function Ma(l){return l!=null&&typeof l!="boolean"&&(l.once===!0||l.signal instanceof AbortSignal)?{capture:l.capture,passive:l.passive}:l}function Xv(l){return l==null?"c=0":typeof l=="boolean"?"c="+(l?"1":"0"):"c="+(l.capture?"1":"0")}function Gm(l,t,u,a){if(l.length===0)return-1;a=Xv(a);for(var n=0;n<l.length;n++){var e=l[n];if(e.type===t&&e.listener===u&&Xv(e.optionsOrUseCapture)===a)return n}return-1}Ll.prototype.dispatchEvent=function(l){var t=Gu(this._fragmentFiber);if(t===null)return!0;t=el(t);var u=this._eventListeners;if(u!==null&&0<u.length||!l.bubbles){var a=t.nodeType===9?t.createComment(""):document.createTextNode("");if(u)for(var n=0;n<u.length;n++){var e=u[n];a.addEventListener(e.type,e.attachedListener,Ma(e.optionsOrUseCapture))}if(t.appendChild(a),l=a.dispatchEvent(l),u)for(n=0;n<u.length;n++)e=u[n],a.removeEventListener(e.type,e.attachedListener,Ma(e.optionsOrUseCapture));return t.removeChild(a),l}return t.dispatchEvent(l)};Ll.prototype.focus=function(l){Hl(this._fragmentFiber.child,!0,Xm,l,void 0,void 0)};function Xm(l,t){return l.tag===6?!1:(l=el(l),SS(l,t))}Ll.prototype.focusLast=function(l){var t=[];Hl(this._fragmentFiber.child,!0,U0,t,void 0,void 0);for(var u=t.length-1;0<=u&&!Xm(t[u],l);u--);};function U0(l,t){return t.push(l),!1}Ll.prototype.blur=function(){var l=Gu(this._fragmentFiber);l!==null&&(l=el(l),l=Nn(l).activeElement,l!==null&&Hl(this._fragmentFiber.child,!1,eS,l,void 0,void 0))};function eS(l,t){return l.tag===6?!1:(l=el(l),l===t||l.contains(t)?(t.blur(),!0):!1)}Ll.prototype.observeUsing=function(l){this._observers===null&&(this._observers=new Set),this._observers.add(l),Hl(this._fragmentFiber.child,!1,fS,l,void 0,void 0)};function fS(l,t){return l.tag===6||(l=el(l),t.observe(l)),!1}Ll.prototype.unobserveUsing=function(l){var t=this._observers;if(t!==null&&t.has(l)){t.delete(l),Hl(this._fragmentFiber.child,!1,cS,l,void 0,void 0);for(var u=t=0;u<at.length;u++){var a=at[u];a.fragmentInstance===this&&a.observer===l?l.unobserve(a.instance):at[t++]=a}at.length=t}};function cS(l,t){return l.tag===6||(l=el(l),t.unobserve(l)),!1}var at=[],_c=!1;function iS(l,t,u){at.push({fragmentInstance:l,observer:t,instance:u}),_c||(_c=!0,TS(function(){_c=!1;var a=at;at=[];for(var n=0;n<a.length;n++){var e=a[n];e.observer.unobserve(e.instance)}}))}Ll.prototype.getClientRects=function(){var l=[];return Hl(this._fragmentFiber.child,!1,vS,l,void 0,void 0),l};function vS(l,t){if(l.tag===6){l=l.stateNode;var u=l.ownerDocument.createRange();u.selectNodeContents(l),t.push.apply(t,u.getClientRects())}else l=el(l),t.push.apply(t,l.getClientRects());return!1}Ll.prototype.getRootNode=function(l){var t=Gu(this._fragmentFiber);return t===null?this:el(t).getRootNode(l)};Ll.prototype.compareDocumentPosition=function(l){var t=Gu(this._fragmentFiber);if(t===null)return Node.DOCUMENT_POSITION_DISCONNECTED;var u=[];Hl(this._fragmentFiber.child,!1,U0,u,void 0,void 0);var a=el(t);if(u.length===0){if(u=a,S1(this._fragmentFiber)){l:{for(t=this._fragmentFiber.return;t!==null;){if(t.tag===4){t=t.stateNode.containerInfo;break l}if(t.tag===3||t.tag===5||t.tag===27)break;t=t.return}t=null}t!=null&&(u=t)}t=this._fragmentFiber;var n=a=u.compareDocumentPosition(l);return u===l?n=Node.DOCUMENT_POSITION_CONTAINS:a&Node.DOCUMENT_POSITION_CONTAINED_BY&&(u=no(t)[1],u===null?n=Node.DOCUMENT_POSITION_PRECEDING:(l=el(u).compareDocumentPosition(l),n=l===0||l&Node.DOCUMENT_POSITION_FOLLOWING?Node.DOCUMENT_POSITION_FOLLOWING:Node.DOCUMENT_POSITION_PRECEDING)),n|=Node.DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC}t=el(u[0]),n=el(u[u.length-1]);var e=S1(this._fragmentFiber)?t.parentElement:a;if(e==null)return Node.DOCUMENT_POSITION_DISCONNECTED;a=e.compareDocumentPosition(t)&Node.DOCUMENT_POSITION_CONTAINED_BY,e=e.compareDocumentPosition(n)&Node.DOCUMENT_POSITION_CONTAINED_BY;var f=t.compareDocumentPosition(l),c=n.compareDocumentPosition(l),i=f&Node.DOCUMENT_POSITION_CONTAINED_BY||c&Node.DOCUMENT_POSITION_CONTAINED_BY;return c=a&&e&&f&Node.DOCUMENT_POSITION_FOLLOWING&&c&Node.DOCUMENT_POSITION_PRECEDING,t=a&&t===l||e&&n===l||i||c?Node.DOCUMENT_POSITION_CONTAINED_BY:!a&&t===l||!e&&n===l?Node.DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC:f,t&Node.DOCUMENT_POSITION_DISCONNECTED||t&Node.DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC||oS(t,this._fragmentFiber,u[0],u[u.length-1],l)?t:Node.DOCUMENT_POSITION_IMPLEMENTATION_SPECIFIC};function oS(l,t,u,a,n){var e=bu(n);if(l&Node.DOCUMENT_POSITION_CONTAINED_BY){if(u=!!e)l:{for(;e!==null;){if(e.tag===7&&(e===t||e.alternate===t)){u=!0;break l}e=e.return}u=!1}return u}if(l&Node.DOCUMENT_POSITION_CONTAINS){if(e===null)return e=n.ownerDocument,n===e||n===e.documentElement||n===e.body;l:{for(e=t,t=Gu(t);e!==null;){if(!(e.tag!==5&&e.tag!==3&&e.tag!==27||e!==t&&e.alternate!==t)){e=!0;break l}e=e.return}e=!1}return e}return l&Node.DOCUMENT_POSITION_PRECEDING?((t=!!e)&&!(t=e===u)&&(t=Dc(u,e,T1),t===null?t=!1:(Hl(t,!0,Vd,e,u),e=Iu,Iu=null,t=e!==null)),t):l&Node.DOCUMENT_POSITION_FOLLOWING?((t=!!e)&&!(t=e===a)&&(t=Dc(a,e,T1),t===null?t=!1:(Hl(t,!0,Zd,e,a),e=Iu,Nc=Iu=null,t=e!==null)),t):!1}function Qv(l,t){var u=l.ownerDocument.createRange();u.selectNodeContents(l),l=u.getBoundingClientRect(),window.scrollTo(window.scrollX+l.left,t?window.scrollY+l.top:window.scrollY+l.bottom-window.innerHeight)}Ll.prototype.scrollIntoView=function(l){if(typeof l=="object")throw Error(E(566));var t=[];Hl(this._fragmentFiber.child,!1,U0,t,void 0,void 0);var u=l!==!1;if(t.length===0){var a=no(this._fragmentFiber);if(a=u?a[1]||a[0]||Gu(this._fragmentFiber):a[0]||a[1],a===null)return;if(a.tag===6){l=el(a),Qv(l,u);return}if(a=el(a),a.nodeType!==9){if(a.nodeType===11){u="host"in a?a.host:null,u!==null&&u.scrollIntoView(l);return}a.scrollIntoView(l)}}for(a=u?t.length-1:0;a!==(u?-1:t.length);){var n=t[a];n.tag===6?(n=el(n),Qv(n,u)):el(n).scrollIntoView(l),a+=u?-1:1}};function yS(l,t){return l=el(l),Qm(l,t),!1}function Qm(l,t){l.reactFragments==null&&(l.reactFragments=new Set),l.reactFragments.add(t)}function jm(l,t){var u=t._eventListeners;if(u!==null)for(var a=0;a<u.length;a++){var n=u[a];l.addEventListener(n.type,n.attachedListener,Ma(n.optionsOrUseCapture))}l.nodeType!==3&&(u=t._observers,u!==null&&u.forEach(function(e){for(var f=0,c=0;c<at.length;c++){var i=at[c];(i.fragmentInstance!==t||i.observer!==e||i.instance!==l)&&(at[f++]=i)}at.length=f,e.observe(l)}),Qm(l,t))}function mS(l,t){var u=t._eventListeners;if(u!==null)for(var a=0;a<u.length;a++){var n=u[a];l.removeEventListener(n.type,n.attachedListener,Ma(n.optionsOrUseCapture))}l.nodeType!==3&&(u=t._observers,u!==null&&u.forEach(function(e){typeof e.rootMargin=="string"?iS(t,e,l):e.unobserve(l)}),l.reactFragments!=null&&l.reactFragments.delete(t))}function Ci(l){var t=l.firstChild;for(t&&t.nodeType===10&&(t=t.nextSibling);t;){var u=t;switch(t=t.nextSibling,u.nodeName){case"HTML":case"HEAD":case"BODY":Ci(u),mf(u);continue;case"SCRIPT":case"STYLE":continue;case"LINK":if(u.rel.toLowerCase()==="stylesheet")continue}l.removeChild(u)}}function dS(l,t,u,a){for(;l.nodeType===1;){var n=u;if(l.nodeName.toLowerCase()!==t.toLowerCase()){if(!a&&(l.nodeName!=="INPUT"||l.type!=="hidden"))break}else if(a){if(!l[Bn])switch(t){case"meta":if(!l.hasAttribute("itemprop"))break;return l;case"link":if(e=l.getAttribute("rel"),e==="stylesheet"&&l.hasAttribute("data-precedence"))break;if(e!==n.rel||l.getAttribute("href")!==(n.href==null||n.href===""?null:n.href)||l.getAttribute("crossorigin")!==(n.crossOrigin==null?null:n.crossOrigin)||l.getAttribute("title")!==(n.title==null?null:n.title))break;return l;case"style":if(l.hasAttribute("data-precedence"))break;return l;case"script":if(e=l.getAttribute("src"),(e!==(n.src==null?null:n.src)||l.getAttribute("type")!==(n.type==null?null:n.type)||l.getAttribute("crossorigin")!==(n.crossOrigin==null?null:n.crossOrigin))&&e&&l.hasAttribute("async")&&!l.hasAttribute("itemprop"))break;return l;default:return l}}else if(t==="input"&&l.type==="hidden"){var e=n.name==null?null:""+n.name;if(n.type==="hidden"&&l.getAttribute("name")===e)return l}else return l;if(l=kl(l.nextSibling),l===null)break}return null}function hS(l,t,u){if(t==="")return null;for(;l.nodeType!==3;)if((l.nodeType!==1||l.nodeName!=="INPUT"||l.type!=="hidden")&&!u||(l=kl(l.nextSibling),l===null))return null;return l}function Vm(l,t){for(;l.nodeType!==8;)if((l.nodeType!==1||l.nodeName!=="INPUT"||l.type!=="hidden")&&!t||(l=kl(l.nextSibling),l===null))return null;return l}function Hi(l){return l.data==="$?"||l.data==="$~"}function r0(l){return l.data==="$!"||l.data==="$?"&&l.ownerDocument.readyState!=="loading"}function gS(l,t){var u=l.ownerDocument;if(l.data==="$~")l._reactRetry=t;else if(l.data!=="$?"||u.readyState!=="loading")t();else{var a=function(){t(),u.removeEventListener("DOMContentLoaded",a)};u.addEventListener("DOMContentLoaded",a),l._reactRetry=a}}function kl(l){for(;l!=null;l=l.nextSibling){var t=l.nodeType;if(t===1||t===3)break;if(t===8){if(t=l.data,t==="$"||t==="$!"||t==="$?"||t==="$~"||t==="&"||t==="F!"||t==="F")break;if(t==="/$"||t==="/&")return null}}return l}var Ri=null;function jv(l){l=l.nextSibling;for(var t=0;l;){if(l.nodeType===8){var u=l.data;if(u==="/$"||u==="/&"){if(t===0)return kl(l.nextSibling);t--}else u!=="$"&&u!=="$!"&&u!=="$?"&&u!=="$~"&&u!=="&"||t++}l=l.nextSibling}return null}function Vv(l){l=l.previousSibling;for(var t=0;l;){if(l.nodeType===8){var u=l.data;if(u==="$"||u==="$!"||u==="$?"||u==="$~"||u==="&"){if(t===0)return l;t--}else u!=="/$"&&u!=="/&"||t++}l=l.previousSibling}return null}function SS(l,t){function u(){a=!0}if(l.ownerDocument.activeElement===l)return!0;var a=!1;try{l.ownerDocument.addEventListener("focus",u,!0),(l.focus||HTMLElement.prototype.focus).call(l,t)}finally{l.ownerDocument.removeEventListener("focus",u,!0)}return a}function TS(l){Bv(function(){Bv(function(t){return l(t)})})}function Zm(l,t,u){switch(t=Nn(u),l){case"html":if(l=t.documentElement,!l)throw Error(E(452));return l;case"head":if(l=t.head,!l)throw Error(E(453));return l;case"body":if(l=t.body,!l)throw Error(E(454));return l;default:throw Error(E(451))}}function xm(l,t,u){for(var a in u){var n=u[a];u.hasOwnProperty(a)&&n!=null&&j(l,t,a,null,Jg,n)}u.dangerouslySetInnerHTML!=null&&(l.textContent=""),l.onclick===ht&&(l.onclick=null),mf(l)}function Ac(l){for(var t=l.attributes;t.length;)l.removeAttributeNode(t[0]);mf(l)}var Pl=new Map,Zv=new Set;function Dn(l){if(typeof l.getRootNode=="function"){var t=l.getRootNode();if(t.nodeType===9||t.nodeType===11)return t}return l.nodeType===9?l:l.ownerDocument}var Xt=G.d;G.d={f:ES,r:sS,D:zS,C:bS,L:OS,m:_S,X:NS,S:AS,M:DS};function ES(){var l=Xt.f(),t=Df();return l||t}function sS(l){var t=Ya(l);t!==null&&t.tag===5&&t.type==="form"?Dy(t):Xt.r(l)}var Xa=typeof document>"u"?null:document;function Lm(l,t,u){var a=Xa;if(a&&typeof t=="string"&&t){var n=Fl(t);n='link[rel="'+l+'"][href="'+n+'"]',typeof u=="string"&&(n+='[crossorigin="'+u+'"]'),Zv.has(n)||(Zv.add(n),l={rel:l,crossOrigin:u,href:t},a.querySelector(n)===null&&(t=a.createElement("link"),Sl(t,"link",l),vl(t),a.head.appendChild(t)))}}function zS(l){Xt.D(l),Lm("dns-prefetch",l,null)}function bS(l,t){Xt.C(l,t),Lm("preconnect",l,t)}function OS(l,t,u){Xt.L(l,t,u);var a=Xa;if(a&&l&&t){var n='link[rel="preload"][as="'+Fl(t)+'"]';t==="image"&&u&&u.imageSrcSet?(n+='[imagesrcset="'+Fl(u.imageSrcSet)+'"]',typeof u.imageSizes=="string"&&(n+='[imagesizes="'+Fl(u.imageSizes)+'"]')):n+='[href="'+Fl(l)+'"]';var e=n;switch(t){case"style":e=Ua(l);break;case"script":e=Qa(l)}if(!(Pl.has(e)||(l=L({rel:"preload",href:t==="image"&&u&&u.imageSrcSet?void 0:l,as:t},u),Pl.set(e,l),a.querySelector(n)!==null||t==="style"&&a.querySelector(Vn(e))||t==="script"&&a.querySelector(Zn(e))))){var f=a.createElement("link");Sl(f,"link",l),t==="style"&&(f[je]=!0,f.onload=f.onerror=function(){Eo(f)}),vl(f),a.head.appendChild(f)}}}function _S(l,t){Xt.m(l,t);var u=Xa;if(u&&l){var a=t&&typeof t.as=="string"?t.as:"script",n='link[rel="modulepreload"][as="'+Fl(a)+'"][href="'+Fl(l)+'"]',e=n;switch(a){case"audioworklet":case"paintworklet":case"serviceworker":case"sharedworker":case"worker":case"script":e=Qa(l)}if(!Pl.has(e)&&(l=L({rel:"modulepreload",href:l},t),Pl.set(e,l),u.querySelector(n)===null)){switch(a){case"audioworklet":case"paintworklet":case"serviceworker":case"sharedworker":case"worker":case"script":if(u.querySelector(Zn(e)))return}a=u.createElement("link"),Sl(a,"link",l),vl(a),u.head.appendChild(a)}}}function AS(l,t,u){Xt.S(l,t,u);var a=Xa;if(a&&l){var n=va(a).hoistableStyles,e=Ua(l);t=t||"default";var f=n.get(e);if(!f){var c={loading:0,preload:null};if(f=a.querySelector(Vn(e)))c.loading=5;else{l=L({rel:"stylesheet",href:l,"data-precedence":t},u),(u=Pl.get(e))&&C0(l,u);var i=f=a.createElement("link");vl(i),Sl(i,"link",l),i._p=new Promise(function(o,g){i.onload=o,i.onerror=g}),i.addEventListener("load",function(){c.loading|=1}),i.addEventListener("error",function(){c.loading|=2}),c.loading|=4,Ce(f,t,a)}f={type:"stylesheet",instance:f,count:1,state:c},n.set(e,f)}}}function NS(l,t){Xt.X(l,t);var u=Xa;if(u&&l){var a=va(u).hoistableScripts,n=Qa(l),e=a.get(n);e||(e=u.querySelector(Zn(n)),e||(l=L({src:l,async:!0},t),(t=Pl.get(n))&&H0(l,t),e=u.createElement("script"),vl(e),Sl(e,"link",l),u.head.appendChild(e)),e={type:"script",instance:e,count:1,state:null},a.set(n,e))}}function DS(l,t){Xt.M(l,t);var u=Xa;if(u&&l){var a=va(u).hoistableScripts,n=Qa(l),e=a.get(n);e||(e=u.querySelector(Zn(n)),e||(l=L({src:l,async:!0,type:"module"},t),(t=Pl.get(n))&&H0(l,t),e=u.createElement("script"),vl(e),Sl(e,"link",l),u.head.appendChild(e)),e={type:"script",instance:e,count:1,state:null},a.set(n,e))}}function xv(l,t,u,a){var n=(n=Pt.current)?Dn(n):null;if(!n)throw Error(E(446));switch(l){case"meta":case"title":return null;case"style":return typeof u.precedence=="string"&&typeof u.href=="string"?(u=Ua(u.href),t=va(n).hoistableStyles,a=t.get(u),a||(a={type:"style",instance:null,count:0,state:null},t.set(u,a)),a):{type:"void",instance:null,count:0,state:null};case"link":if(u.rel==="stylesheet"&&typeof u.href=="string"&&typeof u.precedence=="string"){l=Ua(u.href);var e=va(n).hoistableStyles,f=e.get(l);if(f||(n=n.ownerDocument||n,f={type:"stylesheet",instance:null,count:0,state:{loading:0,preload:null}},e.set(l,f),(e=n.querySelector(Vn(l)))?e._p||(f.instance=e,f.state.loading=5):(e=Pl.get(l),e||(e={rel:"preload",as:"style",href:u.href,crossOrigin:u.crossOrigin,integrity:u.integrity,media:u.media,hrefLang:u.hrefLang,referrerPolicy:u.referrerPolicy},Pl.set(l,e)),MS(n,l,e,f.state))),t&&a===null)throw Error(E(528,""));return f}if(t&&a!==null)throw Error(E(529,""));return null;case"script":return t=u.async,u=u.src,typeof u=="string"&&t&&typeof t!="function"&&typeof t!="symbol"?(u=Qa(u),t=va(n).hoistableScripts,a=t.get(u),a||(a={type:"script",instance:null,count:0,state:null},t.set(u,a)),a):{type:"void",instance:null,count:0,state:null};default:throw Error(E(444,l))}}function Ua(l){return'href="'+Fl(l)+'"'}function Vn(l){return'link[rel="stylesheet"]['+l+"]"}function pm(l){return L({},l,{"data-precedence":l.precedence,precedence:null})}function MS(l,t,u,a){if(t=l.querySelector('link[rel="preload"][as="style"]['+t+"]")){if(t[je]!==!0){a.loading=1;return}}else t=l.createElement("link"),t[je]=!0,t.onload=t.onerror=Eo.bind(null,t),Sl(t,"link",u),vl(t),l.head.appendChild(t);a.preload=t,t.addEventListener("load",function(){return a.loading|=1}),t.addEventListener("error",function(){return a.loading|=2})}function Qa(l){return'[src="'+Fl(l)+'"]'}function Zn(l){return"script[async]"+l}function Lv(l,t,u){if(t.count++,t.instance===null)switch(t.type){case"style":var a=l.querySelector('style[data-href~="'+Fl(u.href)+'"]');if(a)return t.instance=a,vl(a),a;var n=L({},u,{"data-href":u.href,"data-precedence":u.precedence,href:null,precedence:null});return a=(l.ownerDocument||l).createElement("style"),vl(a),Sl(a,"style",n),Ce(a,u.precedence,l),t.instance=a;case"stylesheet":n=Ua(u.href);var e=l.querySelector(Vn(n));if(e)return t.state.loading|=4,t.instance=e,vl(e),e;a=pm(u),(n=Pl.get(n))&&C0(a,n),e=(l.ownerDocument||l).createElement("link"),vl(e);var f=e;return f._p=new Promise(function(c,i){f.onload=c,f.onerror=i}),Sl(e,"link",a),t.state.loading|=4,Ce(e,u.precedence,l),t.instance=e;case"script":return e=Qa(u.src),(n=l.querySelector(Zn(e)))?(t.instance=n,vl(n),n):(a=u,(n=Pl.get(e))&&(a=L({},u),H0(a,n)),l=l.ownerDocument||l,n=l.createElement("script"),vl(n),Sl(n,"link",a),l.head.appendChild(n),t.instance=n);case"void":return null;default:throw Error(E(443,t.type))}else t.type==="stylesheet"&&(t.state.loading&4)===0&&(a=t.instance,t.state.loading|=4,Ce(a,u.precedence,l));return t.instance}function Ce(l,t,u){for(var a=u.querySelectorAll('link[rel="stylesheet"][data-precedence],style[data-precedence]'),n=a.length?a[a.length-1]:null,e=n,f=0;f<a.length;f++){var c=a[f];if(c.dataset.precedence===t)e=c;else if(e!==n)break}e?e.parentNode.insertBefore(l,e.nextSibling):(t=u.nodeType===9?u.head:u,t.insertBefore(l,t.firstChild))}function C0(l,t){l.crossOrigin==null&&(l.crossOrigin=t.crossOrigin),l.referrerPolicy==null&&(l.referrerPolicy=t.referrerPolicy),l.title==null&&(l.title=t.title)}function H0(l,t){l.crossOrigin==null&&(l.crossOrigin=t.crossOrigin),l.referrerPolicy==null&&(l.referrerPolicy=t.referrerPolicy),l.integrity==null&&(l.integrity=t.integrity)}var He=null;function pv(l,t,u){if(He===null){var a=new Map,n=He=new Map;n.set(u,a)}else n=He,a=n.get(u),a||(a=new Map,n.set(u,a));if(a.has(l))return a;for(a.set(l,null),u=u.getElementsByTagName(l),n=0;n<u.length;n++){var e=u[n];if(!(e[Bn]||e[ml]||l==="link"&&e.getAttribute("rel")==="stylesheet")&&e.namespaceURI!=="http://www.w3.org/2000/svg"){var f=e.getAttribute(t)||"";f=l+f;var c=a.get(f);c?c.push(e):a.set(f,[e])}}return a}function Yi(l,t,u){l=l.ownerDocument||l,l.head.insertBefore(u,t==="title"?l.querySelector("head > title"):null)}function US(l,t,u){if(u===1||t.itemProp!=null)return!1;switch(l){case"meta":case"title":return!0;case"style":if(typeof t.precedence!="string"||typeof t.href!="string"||t.href==="")break;return!0;case"link":if(typeof t.rel!="string"||typeof t.href!="string"||t.href===""||t.onLoad||t.onError)break;return t.rel==="stylesheet"?(l=t.disabled,typeof t.precedence=="string"&&l==null):!0;case"script":if(t.async&&typeof t.async!="function"&&typeof t.async!="symbol"&&!t.onLoad&&!t.onError&&t.src&&typeof t.src=="string")return!0}return!1}function Kv(l,t){return l==="img"&&t.src!=null&&t.src!==""&&t.onLoad==null&&t.loading!=="lazy"}function Km(l){return!(l.type==="stylesheet"&&(l.state.loading&3)===0)}function Jm(l){return(l.width||100)*(l.height||100)*(typeof devicePixelRatio=="number"?devicePixelRatio:1)*.25}function Jv(l,t){typeof t.decode=="function"&&(l.imgCount++,t.complete||(l.imgBytes+=Jm(t),l.suspenseyImages.push(t)),l=HS.bind(l),t.decode().then(l,l))}function rS(l,t,u,a){if(u.type==="stylesheet"&&(typeof a.media!="string"||matchMedia(a.media).matches!==!1)&&(u.state.loading&4)===0){if(u.instance===null){var n=Ua(a.href),e=t.querySelector(Vn(n));if(e){t=e._p,t!==null&&typeof t=="object"&&typeof t.then=="function"&&(l.count++,l=Mn.bind(l),t.then(l,l)),u.state.loading|=4,u.instance=e,vl(e);return}e=t.ownerDocument||t,a=pm(a),(n=Pl.get(n))&&C0(a,n),e=e.createElement("link"),vl(e);var f=e;f._p=new Promise(function(c,i){f.onload=c,f.onerror=i}),Sl(e,"link",a),u.instance=e}l.stylesheets===null&&(l.stylesheets=new Map),l.stylesheets.set(u,t),(t=u.state.preload)&&(u.state.loading&3)===0&&(l.count++,u=Mn.bind(l),t.addEventListener("load",u),t.addEventListener("error",u))}}var Re=0;function CS(l,t){return l.stylesheets&&l.count===0&&Ye(l,l.stylesheets),0<l.count||0<l.imgCount?function(u){var a=setTimeout(function(){if(l.stylesheets&&Ye(l,l.stylesheets),l.unsuspend){var e=l.unsuspend;l.unsuspend=null,e()}},6e4+t);0<l.imgBytes&&Re===0&&(Re=62500*Wg());var n=setTimeout(function(){if(l.waitingForImages=!1,l.count===0&&(l.stylesheets&&Ye(l,l.stylesheets),l.unsuspend)){var e=l.unsuspend;l.unsuspend=null,e()}},(l.imgBytes>Re?50:800)+t);return l.unsuspend=u,function(){l.unsuspend=null,clearTimeout(a),clearTimeout(n)}}:null}function wm(l){if(l.count===0&&(l.imgCount===0||!l.waitingForImages)){if(l.stylesheets)Ye(l,l.stylesheets);else if(l.unsuspend){var t=l.unsuspend;l.unsuspend=null,t()}}}function Mn(){this.count--,wm(this)}function HS(){this.imgCount--,wm(this)}var vf=null;function Ye(l,t){l.stylesheets=null,l.unsuspend!==null&&(l.count++,vf=new Map,t.forEach(RS,l),vf=null,Mn.call(l))}function RS(l,t){if(!(t.state.loading&4)){var u=vf.get(l);if(u)var a=u.get(null);else{u=new Map,vf.set(l,u);for(var n=l.querySelectorAll("link[data-precedence],style[data-precedence]"),e=0;e<n.length;e++){var f=n[e];(f.nodeName==="LINK"||f.getAttribute("media")!=="not all")&&(u.set(f.dataset.precedence,f),a=f)}a&&u.set(null,a)}n=t.instance,f=n.getAttribute("data-precedence"),e=u.get(f)||a,e===a&&u.set(null,n),u.set(f,n),this.count++,a=Mn.bind(this),n.addEventListener("load",a),n.addEventListener("error",a),e?e.parentNode.insertBefore(n,e.nextSibling):(l=l.nodeType===9?l.head:l,l.insertBefore(n,l.firstChild)),t.state.loading|=4}}var ra={$$typeof:dt,Provider:null,Consumer:null,_currentValue:_u,_currentValue2:_u,_threadCount:0};function YS(l,t,u,a,n,e,f,c,i){this.tag=1,this.containerInfo=l,this.pingCache=this.current=this.pendingChildren=null,this.timeoutHandle=-1,this.callbackNode=this.next=this.pendingContext=this.context=this.cancelPendingCommit=null,this.callbackPriority=0,this.expirationTimes=kf(-1),this.entangledLanes=this.shellSuspendCounter=this.errorRecoveryDisabledLanes=this.expiredLanes=this.warmLanes=this.pingedLanes=this.suspendedLanes=this.pendingLanes=0,this.entanglements=kf(0),this.hiddenUpdates=kf(null),this.identifierPrefix=a,this.onUncaughtError=n,this.onCaughtError=e,this.onRecoverableError=f,this.pooledCache=null,this.pooledCacheLanes=0,this.formState=i,this.transitionTypes=null,this.incompleteTransitions=new Map}function Wm(l,t,u,a,n,e,f,c,i,o,g,T){return l=new YS(l,t,u,f,i,o,g,T,c),t=1,e===!0&&(t|=24),e=rl(3,null,null,t),l.current=e,e.stateNode=l,t=Ii(),t.refCount++,l.pooledCache=t,t.refCount++,e.memoizedState={element:a,isDehydrated:u,cache:t},l0(e),l}function Fm(l){return l?(l=ea,l):ea}function $m(l,t,u,a,n,e){n=Fm(n),a.context===null?a.context=n:a.pendingContext=n,a=tu(t),a.payload={element:u},e=e===void 0?null:e,e!==null&&(a.callback=e),u=uu(l,a,t),u!==null&&(Cl(u,l,t),nn(u,l,t))}function wv(l,t){if(l=l.memoizedState,l!==null&&l.dehydrated!==null){var u=l.retryLane;l.retryLane=u!==0&&u<t?u:t}}function R0(l,t){wv(l,t),(l=l.alternate)&&wv(l,t)}function Im(l){if(l.tag===13||l.tag===31){var t=ju(l,67108864);t!==null&&Cl(t,l,67108864),R0(l,67108864)}}function Wv(l){if(l.tag===13||l.tag===31){var t=Zl();t=ji(t);var u=ju(l,t);u!==null&&Cl(u,l,t),R0(l,t)}}var Ca=!0;function BS(l,t,u,a){var n=A.T;A.T=null;var e=G.p;try{G.p=2,Y0(l,t,u,a)}finally{G.p=e,A.T=n}}function qS(l,t,u,a){var n=A.T;A.T=null;var e=G.p;try{G.p=8,Y0(l,t,u,a)}finally{G.p=e,A.T=n}}function Y0(l,t,u,a){if(Ca){var n=Bi(a);if(n===null)bc(l,t,a,of,u),Fv(l,a);else if(XS(n,l,t,u,a))a.stopPropagation();else if(Fv(l,a),t&4&&-1<GS.indexOf(l)){for(;n!==null;){var e=Ya(n);if(e!==null)switch(e.tag){case 3:if(e=e.stateNode,e.current.memoizedState.isDehydrated){var f=Eu(e.pendingLanes);if(f!==0){var c=e;for(c.pendingLanes|=2,c.entangledLanes|=2;f;){var i=1<<31-Vl(f);c.entanglements[1]|=i,f&=~i}bt(e),(q&6)===0&&(af=Ql()+500,jn(0,!1))}}break;case 31:case 13:c=ju(e,2),c!==null&&Cl(c,e,2),Df(),R0(e,2)}if(e=Bi(a),e===null&&bc(l,t,a,of,u),e===n)break;n=e}n!==null&&a.stopPropagation()}else bc(l,t,a,null,u)}}function Bi(l){return l=xi(l),B0(l)}var of=null;function B0(l){if(of=null,l=bu(l),l!==null){var t=Cn(l);if(t===null)l=null;else{var u=t.tag;if(u===13){if(l=to(t),l!==null)return l;l=null}else if(u===31){if(l=uo(t),l!==null)return l;l=null}else if(u===3){if(t.stateNode.current.memoizedState.isDehydrated)return t.tag===3?t.stateNode.containerInfo:null;l=null}else t!==l&&(l=null)}}return of=l,null}function km(l){switch(l){case"beforetoggle":case"cancel":case"click":case"close":case"contextmenu":case"copy":case"cut":case"auxclick":case"dblclick":case"dragend":case"dragstart":case"drop":case"focusin":case"focusout":case"input":case"invalid":case"keydown":case"keypress":case"keyup":case"mousedown":case"mouseup":case"paste":case"pause":case"play":case"pointercancel":case"pointerdown":case"pointerup":case"ratechange":case"reset":case"seeked":case"submit":case"toggle":case"touchcancel":case"touchend":case"touchstart":case"volumechange":case"change":case"selectionchange":case"textInput":case"compositionstart":case"compositionend":case"compositionupdate":case"beforeblur":case"afterblur":case"beforeinput":case"blur":case"fullscreenchange":case"fullscreenerror":case"focus":case"hashchange":case"popstate":case"select":case"selectstart":return 2;case"drag":case"dragenter":case"dragexit":case"dragleave":case"dragover":case"mousemove":case"mouseout":case"mouseover":case"pointermove":case"pointerout":case"pointerover":case"resize":case"scroll":case"touchmove":case"wheel":case"mouseenter":case"mouseleave":case"pointerenter":case"pointerleave":return 8;case"message":switch($d()){case io:return 2;case vo:return 8;case Qe:case Id:return 32;case oo:return 268435456;default:return 32}default:return 32}}var qi=!1,fu=null,cu=null,iu=null,Un=new Map,rn=new Map,Jt=[],GS="mousedown mouseup touchcancel touchend touchstart auxclick dblclick pointercancel pointerdown pointerup dragend dragstart drop compositionend compositionstart keydown keypress keyup input textInput copy cut paste click change contextmenu reset".split(" ");function Fv(l,t){switch(l){case"focusin":case"focusout":fu=null;break;case"dragenter":case"dragleave":cu=null;break;case"mouseover":case"mouseout":iu=null;break;case"pointerover":case"pointerout":Un.delete(t.pointerId);break;case"gotpointercapture":case"lostpointercapture":rn.delete(t.pointerId)}}function wa(l,t,u,a,n,e){return l===null||l.nativeEvent!==e?(l={blockedOn:t,domEventName:u,eventSystemFlags:a,nativeEvent:e,targetContainers:[n]},t!==null&&(t=Ya(t),t!==null&&Im(t)),l):(l.eventSystemFlags|=a,t=l.targetContainers,n!==null&&t.indexOf(n)===-1&&t.push(n),l)}function XS(l,t,u,a,n){switch(t){case"focusin":return fu=wa(fu,l,t,u,a,n),!0;case"dragenter":return cu=wa(cu,l,t,u,a,n),!0;case"mouseover":return iu=wa(iu,l,t,u,a,n),!0;case"pointerover":var e=n.pointerId;return Un.set(e,wa(Un.get(e)||null,l,t,u,a,n)),!0;case"gotpointercapture":return e=n.pointerId,rn.set(e,wa(rn.get(e)||null,l,t,u,a,n)),!0}return!1}function Pm(l){var t=bu(l.target);if(t!==null){var u=Cn(t);if(u!==null){if(t=u.tag,t===13){if(t=to(u),t!==null){l.blockedOn=t,b1(l.priority,function(){Wv(u)});return}}else if(t===31){if(t=uo(u),t!==null){l.blockedOn=t,b1(l.priority,function(){Wv(u)});return}}else if(t===3&&u.stateNode.current.memoizedState.isDehydrated){l.blockedOn=u.tag===3?u.stateNode.containerInfo:null;return}}}l.blockedOn=null}function Be(l){if(l.blockedOn!==null)return!1;for(var t=l.targetContainers;0<t.length;){var u=Bi(l.nativeEvent);if(u===null){u=l.nativeEvent;var a=new u.constructor(u.type,u);Qc=a,u.target.dispatchEvent(a),Qc=null}else return t=Ya(u),t!==null&&Im(t),l.blockedOn=u,!1;t.shift()}return!0}function $v(l,t,u){Be(l)&&u.delete(t)}function QS(){qi=!1,fu!==null&&Be(fu)&&(fu=null),cu!==null&&Be(cu)&&(cu=null),iu!==null&&Be(iu)&&(iu=null),Un.forEach($v),rn.forEach($v)}function he(l,t){l.blockedOn===t&&(l.blockedOn=null,qi||(qi=!0,fl.unstable_scheduleCallback(fl.unstable_NormalPriority,QS)))}var ge=null;function Iv(l){ge!==l&&(ge=l,fl.unstable_scheduleCallback(fl.unstable_NormalPriority,function(){ge===l&&(ge=null);for(var t=0;t<l.length;t+=3){var u=l[t],a=l[t+1],n=l[t+2];if(typeof a!="function"){if(B0(a||u)===null)continue;break}var e=Ya(u);e!==null&&(l.splice(t,3),t-=3,li(e,{pending:!0,data:n,method:u.method,action:a},a,n))}}))}function Ha(l){function t(i){return he(i,l)}fu!==null&&he(fu,l),cu!==null&&he(cu,l),iu!==null&&he(iu,l),Un.forEach(t),rn.forEach(t);for(var u=0;u<Jt.length;u++){var a=Jt[u];a.blockedOn===l&&(a.blockedOn=null)}for(;0<Jt.length&&(u=Jt[0],u.blockedOn===null);)Pm(u),u.blockedOn===null&&Jt.shift();if(u=(l.ownerDocument||l).$$reactFormReplay,u!=null)for(a=0;a<u.length;a+=3){var n=u[a],e=u[a+1],f=n[Rl]||null;if(typeof e=="function")f||Iv(u);else if(f){var c=null;if(e&&e.hasAttribute("formAction")){if(n=e,f=e[Rl]||null)c=f.formAction;else if(B0(n)!==null)continue}else c=f.action;typeof c=="function"?u[a+1]=c:(u.splice(a,3),a-=3),Iv(u)}}}function ld(){function l(e){e.canIntercept&&e.info==="react-transition"&&e.intercept({handler:function(){return new Promise(function(f){return n=f})},focusReset:"manual",scroll:"manual"})}function t(){n!==null&&(n(),n=null),a||setTimeout(u,20)}function u(){if(!a&&!navigation.transition){var e=navigation.currentEntry;e&&e.url!=null&&navigation.navigate(e.url,{state:e.getState(),info:"react-transition",history:"replace"})}}if(typeof navigation=="object"){var a=!1,n=null;return navigation.addEventListener("navigate",l),navigation.addEventListener("navigatesuccess",t),navigation.addEventListener("navigateerror",t),setTimeout(u,100),function(){a=!0,navigation.removeEventListener("navigate",l),navigation.removeEventListener("navigatesuccess",t),navigation.removeEventListener("navigateerror",t),n!==null&&(n(),n=null)}}}function q0(l){this._internalRoot=l}rf.prototype.render=q0.prototype.render=function(l){var t=this._internalRoot;if(t===null)throw Error(E(409));var u=t.current,a=Zl();$m(u,a,l,t,null,null)};rf.prototype.unmount=q0.prototype.unmount=function(){var l=this._internalRoot;if(l!==null){this._internalRoot=null;var t=l.containerInfo;$m(l.current,2,null,l,null,null),Df(),t[Ra]=null}};function rf(l){this._internalRoot=l}rf.prototype.unstable_scheduleHydration=function(l){if(l){var t=To();l={blockedOn:null,target:l,priority:t};for(var u=0;u<Jt.length&&t!==0&&t<Jt[u].priority;u++);Jt.splice(u,0,l),u===0&&Pm(l)}};var kv=Pv.version;if(kv!=="19.3.1")throw Error(E(527,kv,"19.3.1"));G.findDOMNode=function(l){var t=l._reactInternals;if(t===void 0)throw typeof l.render=="function"?Error(E(188)):(l=Object.keys(l).join(","),Error(E(268,l)));return l=jd(t),l=l!==null?ao(l):null,l=l===null?null:l.stateNode,l};var jS={bundleType:0,version:"19.3.1",rendererPackageName:"react-dom",currentDispatcherRef:A,reconcilerVersion:"19.3.1"};if(typeof __REACT_DEVTOOLS_GLOBAL_HOOK__<"u"&&(Wa=__REACT_DEVTOOLS_GLOBAL_HOOK__,!Wa.isDisabled&&Wa.supportsFiber))try{Hn=Wa.inject(jS),jl=Wa}catch{}var Wa;Cf.createRoot=function(l,t){if(!lo(l))throw Error(E(299));var u=!1,a="",n=By,e=qy,f=Gy;return t!=null&&(t.unstable_strictMode===!0&&(u=!0),t.identifierPrefix!==void 0&&(a=t.identifierPrefix),t.onUncaughtError!==void 0&&(n=t.onUncaughtError),t.onCaughtError!==void 0&&(e=t.onCaughtError),t.onRecoverableError!==void 0&&(f=t.onRecoverableError)),t=Wm(l,1,!1,null,null,u,a,null,n,e,f,ld),l[Ra]=t.current,D0(l),new q0(t)};Cf.hydrateRoot=function(l,t,u){if(!lo(l))throw Error(E(299));var a=!1,n="",e=By,f=qy,c=Gy,i=null;return u!=null&&(u.unstable_strictMode===!0&&(a=!0),u.identifierPrefix!==void 0&&(n=u.identifierPrefix),u.onUncaughtError!==void 0&&(e=u.onUncaughtError),u.onCaughtError!==void 0&&(f=u.onCaughtError),u.onRecoverableError!==void 0&&(c=u.onRecoverableError),u.formState!==void 0&&(i=u.formState)),t=Wm(l,1,!0,t,u??null,a,n,i,e,f,c,ld),t.context=Fm(null),u=t.current,a=Zl(),a=ji(a),n=tu(a),n.callback=null,uu(u,n,a),u=a,t.current.lanes=u,Yn(t,u),bt(t),l[Ra]=t.current,D0(l),new rf(t)};Cf.version="19.3.1"});var nd=ft((t2,ad)=>{"use strict";function ud(){if(!(typeof __REACT_DEVTOOLS_GLOBAL_HOOK__>"u"||typeof __REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE!="function"))try{__REACT_DEVTOOLS_GLOBAL_HOOK__.checkDCE(ud)}catch(l){console.error(l)}}ud(),ad.exports=td()});var fd=ft(Hf=>{"use strict";/**
 * @license React
 * react-jsx-runtime.production.js
 *
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * This source code is licensed under the MIT license found in the
 * LICENSE file in the root directory of this source tree.
 */var VS=Symbol.for("react.transitional.element"),ZS=Symbol.for("react.fragment");function ed(l,t,u){var a=null;if(u!==void 0&&(a=""+u),t.key!==void 0&&(a=""+t.key),"key"in t){u={};for(var n in t)n!=="key"&&(u[n]=t[n])}else u=t;return t=u.ref,{$$typeof:VS,type:l,key:a,ref:t!==void 0?t:null,props:u}}Hf.Fragment=ZS;Hf.jsx=ed;Hf.jsxs=ed});var id=ft((a2,cd)=>{"use strict";cd.exports=fd()});var xS=xn(pn(),1),LS=xn(jf(),1),pS=xn(nd(),1),KS=xn(id(),1);var G0="blockr.core-react";var JS={react:xS,"react/jsx-runtime":KS,"react-dom":LS,"react-dom/client":pS};Object.hasOwn(globalThis,G0)||Object.defineProperty(globalThis,G0,{value:Object.freeze(JS)});})();
var __create = Object.create;
var __defProp = Object.defineProperty;
var __getOwnPropDesc = Object.getOwnPropertyDescriptor;
var __getOwnPropNames = Object.getOwnPropertyNames;
var __getProtoOf = Object.getPrototypeOf;
var __hasOwnProp = Object.prototype.hasOwnProperty;
var __commonJS = (cb, mod) => function __require() {
  try {
    return mod || (0, cb[__getOwnPropNames(cb)[0]])((mod = { exports: {} }).exports, mod), mod.exports;
  } catch (e2) {
    throw mod = 0, e2;
  }
};
var __copyProps = (to, from, except, desc) => {
  if (from && typeof from === "object" || typeof from === "function") {
    for (let key of __getOwnPropNames(from))
      if (!__hasOwnProp.call(to, key) && key !== except)
        __defProp(to, key, { get: () => from[key], enumerable: !(desc = __getOwnPropDesc(from, key)) || desc.enumerable });
  }
  return to;
};
var __toESM = (mod, isNodeMode, target2) => (target2 = mod != null ? __create(__getProtoOf(mod)) : {}, __copyProps(
  // If the importer is in node compatibility mode or this is not an ESM
  // file that has been converted to a CommonJS file using a Babel-
  // compatible transform (i.e. "__esModule" has not been set), then set
  // "default" to the CommonJS "module.exports" for node compatibility.
  isNodeMode || !mod || !mod.__esModule ? __defProp(target2, "default", { value: mod, enumerable: true }) : target2,
  mod
));

// shared-react:react
var require_react = __commonJS({
  "shared-react:react"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react"];
  }
});

// shared-react:react/jsx-runtime
var require_jsx_runtime = __commonJS({
  "shared-react:react/jsx-runtime"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react/jsx-runtime"];
  }
});

// shared-react:react-dom
var require_react_dom = __commonJS({
  "shared-react:react-dom"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react-dom"];
  }
});

// shared-react:react-dom/client
var require_client = __commonJS({
  "shared-react:react-dom/client"(exports, module) {
    var shared = globalThis["blockr.core-react"];
    if (shared === void 0) throw new Error("React is not on the page: core's runtime puts it there before any module loads.");
    module.exports = shared["react-dom/client"];
  }
});

// node_modules/@preact/signals-core/dist/signals-core.module.js
var i = /* @__PURE__ */ Symbol.for("preact-signals");
function t() {
  if (!(v > 1)) {
    var i2, t2 = false;
    !(function() {
      var i3 = c;
      c = void 0;
      while (void 0 !== i3) {
        var t3 = i3.S;
        if (t3.v === i3.v) {
          for (var r3 = t3.t; void 0 !== r3; r3 = r3.x) if (r3.i === i3.i) r3.i = t3.i;
        }
        i3 = i3.o;
      }
    })();
    while (void 0 !== h) {
      var r2 = h;
      h = void 0;
      s++;
      while (void 0 !== r2) {
        var n2 = r2.u;
        r2.u = void 0;
        r2.f &= -3;
        if (!(8 & r2.f) && w(r2)) try {
          r2.c();
        } catch (r3) {
          if (!t2) {
            i2 = r3;
            t2 = true;
          }
        }
        r2 = n2;
      }
    }
    s = 0;
    v--;
    if (t2) throw i2;
  } else v--;
}
function r(i2) {
  if (v > 0) return i2();
  u = ++e;
  v++;
  try {
    return i2();
  } finally {
    t();
  }
}
var n;
var o = void 0;
function f(i2) {
  var t2 = o, r2 = n;
  o = void 0;
  n = void 0;
  try {
    return i2();
  } finally {
    o = t2;
    n = r2;
  }
}
var h = void 0;
var v = 0;
var s = 0;
var e = 0;
var u = 0;
var c = void 0;
var a = 0;
function d(i2) {
  if (void 0 !== o) {
    var t2 = i2.n;
    if (void 0 === t2 || t2.t !== o) {
      t2 = { i: 0, S: i2, p: o.s, n: void 0, t: o, e: void 0, x: void 0, r: t2 };
      if (void 0 !== o.s) o.s.n = t2;
      o.s = t2;
      i2.n = t2;
      if (32 & o.f) i2.S(t2);
      return t2;
    } else if (-1 === t2.i) {
      t2.i = 0;
      if (void 0 !== t2.n) {
        t2.n.p = t2.p;
        if (void 0 !== t2.p) t2.p.n = t2.n;
        t2.p = o.s;
        t2.n = void 0;
        o.s.n = t2;
        o.s = t2;
      }
      return t2;
    }
  }
}
function l(i2, t2) {
  this.v = i2;
  this.i = 0;
  this.n = void 0;
  this.t = void 0;
  this.l = 0;
  this.W = null == t2 ? void 0 : t2.watched;
  this.Z = null == t2 ? void 0 : t2.unwatched;
  this.name = null == t2 ? void 0 : t2.name;
}
l.prototype.brand = i;
l.prototype.h = function() {
  return true;
};
l.prototype.S = function(i2) {
  var t2 = this, r2 = this.t;
  if (r2 !== i2 && void 0 === i2.e) {
    i2.x = r2;
    this.t = i2;
    if (void 0 !== r2) r2.e = i2;
    else f(function() {
      var i3;
      null == (i3 = t2.W) || i3.call(t2);
    });
  }
};
l.prototype.U = function(i2) {
  var t2 = this;
  if (void 0 !== this.t) {
    var r2 = i2.e, n2 = i2.x;
    if (void 0 !== r2) {
      r2.x = n2;
      i2.e = void 0;
    }
    if (void 0 !== n2) {
      n2.e = r2;
      i2.x = void 0;
    }
    if (i2 === this.t) {
      this.t = n2;
      if (void 0 === n2) f(function() {
        var i3;
        null == (i3 = t2.Z) || i3.call(t2);
      });
    }
  }
};
l.prototype.subscribe = function(i2) {
  var t2 = this;
  return x(function() {
    var r2 = t2.value;
    f(function() {
      return i2(r2);
    });
  }, { name: "sub" });
};
l.prototype.valueOf = function() {
  return this.value;
};
l.prototype.toString = function() {
  return this.value + "";
};
l.prototype.toJSON = function() {
  return this.value;
};
l.prototype.peek = function() {
  var i2 = this;
  return f(function() {
    return i2.value;
  });
};
Object.defineProperty(l.prototype, "value", { get: function() {
  var i2 = d(this);
  if (void 0 !== i2) i2.i = this.i;
  return this.v;
}, set: function(i2) {
  if (i2 !== this.v) {
    if (s > 100) throw new Error("Cycle detected");
    !(function(i3) {
      if (0 !== v && 0 === s) {
        if (i3.l !== u) {
          i3.l = u;
          c = { S: i3, v: i3.v, i: i3.i, o: c };
        }
      }
    })(this);
    this.v = i2;
    this.i++;
    a++;
    v++;
    try {
      for (var r2 = this.t; void 0 !== r2; r2 = r2.x) r2.t.N();
    } finally {
      t();
    }
  }
} });
function y(i2, t2) {
  return new l(i2, t2);
}
function w(i2) {
  for (var t2 = i2.s; void 0 !== t2; t2 = t2.n) if (t2.S.i !== t2.i || !t2.S.h() || t2.S.i !== t2.i) return true;
  return false;
}
function b(i2) {
  for (var t2 = i2.s; void 0 !== t2; t2 = t2.n) {
    var r2 = t2.S.n;
    if (void 0 !== r2) t2.r = r2;
    t2.S.n = t2;
    t2.i = -1;
    if (void 0 === t2.n) {
      i2.s = t2;
      break;
    }
  }
}
function _(i2) {
  var t2 = i2.s, r2 = void 0;
  while (void 0 !== t2) {
    var n2 = t2.p;
    if (-1 === t2.i) {
      t2.S.U(t2);
      if (void 0 !== n2) n2.n = t2.n;
      if (void 0 !== t2.n) t2.n.p = n2;
    } else r2 = t2;
    t2.S.n = t2.r;
    if (void 0 !== t2.r) t2.r = void 0;
    t2 = n2;
  }
  i2.s = r2;
}
function j(i2, t2) {
  l.call(this, void 0, t2);
  this.x = i2;
  this.s = void 0;
  this.g = a - 1;
  this.f = 4;
}
j.prototype = new l();
j.prototype.h = function() {
  this.f &= -3;
  if (1 & this.f) return false;
  if (32 == (36 & this.f)) return true;
  this.f &= -5;
  if (this.g === a) return true;
  this.g = a;
  this.f |= 1;
  if (this.i > 0 && !w(this)) {
    this.f &= -2;
    return true;
  }
  var i2 = o;
  try {
    b(this);
    o = this;
    var t2 = this.x();
    if (16 & this.f || this.v !== t2 || 0 === this.i) {
      this.v = t2;
      this.f &= -17;
      this.i++;
    }
  } catch (i3) {
    this.v = i3;
    this.f |= 16;
    this.i++;
  }
  o = i2;
  _(this);
  this.f &= -2;
  return true;
};
j.prototype.S = function(i2) {
  if (void 0 === this.t) {
    this.f |= 36;
    for (var t2 = this.s; void 0 !== t2; t2 = t2.n) t2.S.S(t2);
  }
  l.prototype.S.call(this, i2);
};
j.prototype.U = function(i2) {
  if (void 0 !== this.t) {
    l.prototype.U.call(this, i2);
    if (void 0 === this.t) {
      this.f &= -33;
      for (var t2 = this.s; void 0 !== t2; t2 = t2.n) t2.S.U(t2);
    }
  }
};
j.prototype.N = function() {
  if (!(2 & this.f)) {
    this.f |= 6;
    for (var i2 = this.t; void 0 !== i2; i2 = i2.x) i2.t.N();
  }
};
Object.defineProperty(j.prototype, "value", { get: function() {
  if (1 & this.f) throw new Error("Cycle detected");
  var i2 = d(this);
  this.h();
  if (void 0 !== i2) i2.i = this.i;
  if (16 & this.f) throw this.v;
  return this.v;
} });
function O(i2, t2) {
  return new j(i2, t2);
}
function p(i2) {
  var r2 = i2.j;
  i2.j = void 0;
  if ("function" == typeof r2) {
    v++;
    var n2 = o;
    o = void 0;
    try {
      r2();
    } catch (t2) {
      i2.f &= -2;
      i2.f |= 8;
      g(i2);
      throw t2;
    } finally {
      o = n2;
      t();
    }
  }
}
function g(i2) {
  for (var t2 = i2.s; void 0 !== t2; t2 = t2.n) t2.S.U(t2);
  i2.x = void 0;
  i2.s = void 0;
  p(i2);
}
function S(i2) {
  if (o !== this) throw new Error("Out-of-order effect");
  _(this);
  o = i2;
  this.f &= -2;
  try {
    if (8 & this.f) g(this);
  } finally {
    t();
  }
}
function m(i2, t2) {
  this.x = i2;
  this.j = void 0;
  this.s = void 0;
  this.u = void 0;
  this.f = 32;
  this.name = null == t2 ? void 0 : t2.name;
  if (n) n.push(this);
}
m.prototype.c = function() {
  var i2 = this.S();
  try {
    if (8 & this.f) return;
    if (void 0 === this.x) return;
    var t2 = this.x();
    if ("function" == typeof t2) this.j = t2;
  } finally {
    i2();
  }
};
m.prototype.S = function() {
  if (1 & this.f) throw new Error("Cycle detected");
  this.f |= 1;
  this.f &= -9;
  p(this);
  b(this);
  v++;
  var i2 = o;
  o = this;
  return S.bind(this, i2);
};
m.prototype.N = function() {
  if (!(2 & this.f)) {
    this.f |= 2;
    this.u = h;
    h = this;
  }
};
m.prototype.d = function() {
  this.f |= 8;
  if (!(1 & this.f)) g(this);
};
m.prototype.dispose = function() {
  this.d();
};
function x(i2, t2) {
  var r2 = new m(i2, t2);
  try {
    r2.c();
  } catch (i3) {
    r2.d();
    throw i3;
  }
  var n2 = r2.d.bind(r2);
  n2[Symbol.dispose] = n2;
  return n2;
}
function E(i2) {
  return function() {
    var t2 = arguments, n2 = this;
    return r(function() {
      return f(function() {
        return i2.apply(n2, [].slice.call(t2));
      });
    });
  };
}

// srcts/graph/graph.ts
function graphOf(board) {
  const blocks = Object.keys(board.blocks);
  const into = /* @__PURE__ */ new Map();
  const sources = /* @__PURE__ */ new Map();
  const targets = /* @__PURE__ */ new Map();
  for (const id2 of blocks) {
    into.set(id2, []);
    sources.set(id2, []);
    targets.set(id2, []);
  }
  for (const [id2, link] of Object.entries(board.links)) {
    const linksIn = into.get(link.to);
    const from = sources.get(link.to);
    const to = targets.get(link.from);
    if (linksIn === void 0 || from === void 0 || to === void 0) continue;
    linksIn.push(id2);
    from.push(link.from);
    to.push(link.to);
  }
  const upstream = once(sources);
  const downstream = once(targets);
  const order = topological(blocks, sources, targets);
  let cycle;
  return {
    order,
    linksInto: (id2) => into.get(id2) ?? [],
    upstream: (id2) => upstream.get(id2) ?? [],
    downstream: (id2) => downstream.get(id2) ?? [],
    withUpstream: (ids2) => closure(ids2, upstream),
    withDownstream: (ids2) => closure(ids2, downstream),
    cycle() {
      cycle ??= { found: findCycle(board, targets) };
      return cycle.found;
    }
  };
}
function once(ends) {
  return new Map([...ends].map(([id2, blocks]) => [id2, [...new Set(blocks)]]));
}
function closure(ids2, steps) {
  const reached = /* @__PURE__ */ new Set();
  const todo = [...ids2].filter((id2) => steps.has(id2));
  for (const id2 of todo) reached.add(id2);
  for (const id2 of todo) {
    for (const next of steps.get(id2) ?? []) {
      if (reached.has(next)) continue;
      reached.add(next);
      todo.push(next);
    }
  }
  return reached;
}
function topological(blocks, sources, targets) {
  const left2 = new Map(blocks.map((id2) => [id2, sources.get(id2)?.length ?? 0]));
  const order = blocks.filter((id2) => left2.get(id2) === 0);
  for (const id2 of order) {
    for (const next of targets.get(id2) ?? []) {
      const count = (left2.get(next) ?? 0) - 1;
      left2.set(next, count);
      if (count === 0) order.push(next);
    }
  }
  const placed = new Set(order);
  return [...order, ...blocks.filter((id2) => !placed.has(id2))];
}
function findCycle(board, targets) {
  const done = /* @__PURE__ */ new Set();
  const path = [];
  const onPath = /* @__PURE__ */ new Set();
  function visit(block) {
    if (onPath.has(block)) return path.slice(path.indexOf(block));
    if (done.has(block)) return void 0;
    path.push(block);
    onPath.add(block);
    for (const next of targets.get(block) ?? []) {
      const cycle = visit(next);
      if (cycle !== void 0) return cycle;
    }
    path.pop();
    onPath.delete(block);
    done.add(block);
    return void 0;
  }
  const starts = new Set(Object.values(board.links).map((link) => link.from));
  for (const block of starts) {
    if (!targets.has(block)) continue;
    const cycle = visit(block);
    if (cycle !== void 0) return cycle;
  }
  return void 0;
}

// srcts/util/util.ts
function own(record, key) {
  return Object.hasOwn(record, key) ? record[key] : void 0;
}
function messageOf(error) {
  return error instanceof Error ? error.message : String(error);
}
function asError(error) {
  return error instanceof Error ? error : new Error(String(error));
}
function report(error) {
  queueMicrotask(() => {
    throw error;
  });
}
function isObject(x2) {
  return typeof x2 === "object" && x2 !== null && !Array.isArray(x2);
}
function jsonEqual(a2, b2) {
  if (a2 === b2) return true;
  if (Array.isArray(a2)) {
    return Array.isArray(b2) && a2.length === b2.length && a2.every((x2, i2) => jsonEqual(x2, b2[i2]));
  }
  if (isObject(a2) && isObject(b2)) {
    const keys = Object.keys(a2);
    return keys.length === Object.keys(b2).length && keys.every((key) => Object.hasOwn(b2, key) && jsonEqual(a2[key], b2[key]));
  }
  return false;
}
function without(record, keys) {
  if (!keys.some((key) => Object.hasOwn(record, key))) return record;
  return Object.fromEntries(
    Object.entries(record).filter(([key]) => !keys.includes(key))
  );
}
function frozenCopy(x2) {
  return freeze(structuredClone(x2));
}
function freeze(x2) {
  if (typeof x2 === "object" && x2 !== null) {
    for (const value2 of Object.values(x2)) freeze(value2);
    Object.freeze(x2);
  }
  return x2;
}
function listing(items) {
  if (items.length <= 1) return items.join("");
  return `${items.slice(0, -1).join(", ")} and ${items.at(-1) ?? ""}`;
}
function quoted(items) {
  return listing(items.map((item) => `"${item}"`));
}
function packageOf(id2) {
  const at = id2.indexOf("-");
  if (at < 0) return null;
  const pkg = id2.slice(0, at);
  return isPackageName(pkg) ? pkg : null;
}
function isPackageName(name3) {
  return typeof name3 === "string" && /^[A-Za-z][A-Za-z0-9.]*[A-Za-z0-9]$/.test(name3);
}

// node_modules/@cfworker/json-schema/dist/esm/deep-compare-strict.js
function deepCompareStrict(a2, b2) {
  const typeofa = typeof a2;
  if (typeofa !== typeof b2) {
    return false;
  }
  const aIsArray = Array.isArray(a2);
  const bIsArray = Array.isArray(b2);
  if (aIsArray !== bIsArray) {
    return false;
  }
  if (aIsArray) {
    const length = a2.length;
    if (length !== b2.length) {
      return false;
    }
    for (let i2 = 0; i2 < length; i2++) {
      if (!deepCompareStrict(a2[i2], b2[i2])) {
        return false;
      }
    }
    return true;
  }
  if (typeofa === "object") {
    if (!a2 || !b2) {
      return a2 === b2;
    }
    const aKeys = Object.keys(a2);
    const bKeys = Object.keys(b2);
    const length = aKeys.length;
    if (length !== bKeys.length) {
      return false;
    }
    for (const k of aKeys) {
      if (!Object.prototype.hasOwnProperty.call(b2, k)) {
        return false;
      }
      if (!deepCompareStrict(a2[k], b2[k])) {
        return false;
      }
    }
    return true;
  }
  return a2 === b2;
}

// node_modules/@cfworker/json-schema/dist/esm/pointer.js
function encodePointer(p2) {
  return encodeURI(escapePointer(p2));
}
function escapePointer(p2) {
  return p2.replace(/~/g, "~0").replace(/\//g, "~1");
}

// node_modules/@cfworker/json-schema/dist/esm/dereference.js
var schemaArrayKeyword = {
  prefixItems: true,
  items: true,
  allOf: true,
  anyOf: true,
  oneOf: true
};
var schemaMapKeyword = {
  $defs: true,
  definitions: true,
  properties: true,
  patternProperties: true,
  dependentSchemas: true
};
var ignoredKeyword = {
  id: true,
  $id: true,
  $ref: true,
  $schema: true,
  $anchor: true,
  $vocabulary: true,
  $comment: true,
  default: true,
  enum: true,
  const: true,
  required: true,
  dependentRequired: true,
  type: true,
  maximum: true,
  minimum: true,
  exclusiveMaximum: true,
  exclusiveMinimum: true,
  multipleOf: true,
  maxLength: true,
  minLength: true,
  pattern: true,
  format: true,
  maxItems: true,
  minItems: true,
  uniqueItems: true,
  maxProperties: true,
  minProperties: true
};
var initialBaseURI = typeof self !== "undefined" && self.location && self.location.origin !== "null" ? new URL(self.location.origin + self.location.pathname + location.search) : new URL("https://github.com/cfworker");
function dereference(schema, lookup = /* @__PURE__ */ Object.create(null), baseURI = initialBaseURI, basePointer = "") {
  if (schema && typeof schema === "object" && !Array.isArray(schema)) {
    const id2 = schema.$id || schema["id"];
    if (id2) {
      const url = new URL(id2, baseURI.href);
      if (url.hash.length > 1) {
        lookup[url.href] = schema;
      } else {
        url.hash = "";
        if (basePointer === "") {
          baseURI = url;
        } else {
          dereference(schema, lookup, baseURI);
        }
      }
    }
  } else if (schema !== true && schema !== false) {
    return lookup;
  }
  const schemaURI = baseURI.href + (basePointer ? "#" + basePointer : "");
  if (lookup[schemaURI] !== void 0 && lookup[schemaURI] !== schema) {
    throw new Error(`Duplicate schema URI "${schemaURI}".`);
  }
  lookup[schemaURI] = schema;
  if (schema === true || schema === false) {
    return lookup;
  }
  if (schema.__absolute_uri__ === void 0) {
    Object.defineProperty(schema, "__absolute_uri__", {
      enumerable: false,
      value: schemaURI
    });
  }
  if (schema.$ref && schema.__absolute_ref__ === void 0) {
    const url = new URL(schema.$ref, baseURI.href);
    url.hash = url.hash;
    Object.defineProperty(schema, "__absolute_ref__", {
      enumerable: false,
      value: url.href
    });
  }
  if (schema.$recursiveRef && schema.__absolute_recursive_ref__ === void 0) {
    const url = new URL(schema.$recursiveRef, baseURI.href);
    url.hash = url.hash;
    Object.defineProperty(schema, "__absolute_recursive_ref__", {
      enumerable: false,
      value: url.href
    });
  }
  if (schema.$anchor) {
    const url = new URL("#" + schema.$anchor, baseURI.href);
    lookup[url.href] = schema;
  }
  for (let key in schema) {
    if (ignoredKeyword[key]) {
      continue;
    }
    const keyBase = `${basePointer}/${encodePointer(key)}`;
    const subSchema = schema[key];
    if (Array.isArray(subSchema)) {
      if (schemaArrayKeyword[key]) {
        const length = subSchema.length;
        for (let i2 = 0; i2 < length; i2++) {
          dereference(subSchema[i2], lookup, baseURI, `${keyBase}/${i2}`);
        }
      }
    } else if (schemaMapKeyword[key]) {
      for (let subKey in subSchema) {
        dereference(subSchema[subKey], lookup, baseURI, `${keyBase}/${encodePointer(subKey)}`);
      }
    } else {
      dereference(subSchema, lookup, baseURI, keyBase);
    }
  }
  return lookup;
}

// node_modules/@cfworker/json-schema/dist/esm/format.js
var DATE = /^(\d\d\d\d)-(\d\d)-(\d\d)$/;
var DAYS = [0, 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31];
var TIME = /^(\d\d):(\d\d):(\d\d)(\.\d+)?(z|[+-]\d\d(?::?\d\d)?)?$/i;
var HOSTNAME = /^(?=.{1,253}\.?$)[a-z0-9](?:[a-z0-9-]{0,61}[a-z0-9])?(?:\.[a-z0-9](?:[-0-9a-z]{0,61}[0-9a-z])?)*\.?$/i;
var URIREF = /^(?:[a-z][a-z0-9+\-.]*:)?(?:\/?\/(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|(?:[a-z0-9\-._~!$&'"()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:\/(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})*)*|\/(?:(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'"()*+,;=:@]|%[0-9a-f]{2})*)*)?(?:\?(?:[a-z0-9\-._~!$&'"()*+,;=:@/?]|%[0-9a-f]{2})*)?(?:#(?:[a-z0-9\-._~!$&'"()*+,;=:@/?]|%[0-9a-f]{2})*)?$/i;
var URITEMPLATE = /^(?:(?:[^\x00-\x20"'<>%\\^`{|}]|%[0-9a-f]{2})|\{[+#./;?&=,!@|]?(?:[a-z0-9_]|%[0-9a-f]{2})+(?::[1-9][0-9]{0,3}|\*)?(?:,(?:[a-z0-9_]|%[0-9a-f]{2})+(?::[1-9][0-9]{0,3}|\*)?)*\})*$/i;
var URL_ = /^(?:(?:https?|ftp):\/\/)(?:\S+(?::\S*)?@)?(?:(?!10(?:\.\d{1,3}){3})(?!127(?:\.\d{1,3}){3})(?!169\.254(?:\.\d{1,3}){2})(?!192\.168(?:\.\d{1,3}){2})(?!172\.(?:1[6-9]|2\d|3[0-1])(?:\.\d{1,3}){2})(?:[1-9]\d?|1\d\d|2[01]\d|22[0-3])(?:\.(?:1?\d{1,2}|2[0-4]\d|25[0-5])){2}(?:\.(?:[1-9]\d?|1\d\d|2[0-4]\d|25[0-4]))|(?:(?:[a-z\u{00a1}-\u{ffff}0-9]+-?)*[a-z\u{00a1}-\u{ffff}0-9]+)(?:\.(?:[a-z\u{00a1}-\u{ffff}0-9]+-?)*[a-z\u{00a1}-\u{ffff}0-9]+)*(?:\.(?:[a-z\u{00a1}-\u{ffff}]{2,})))(?::\d{2,5})?(?:\/[^\s]*)?$/iu;
var UUID = /^(?:urn:uuid:)?[0-9a-f]{8}-(?:[0-9a-f]{4}-){3}[0-9a-f]{12}$/i;
var JSON_POINTER = /^(?:\/(?:[^~/]|~0|~1)*)*$/;
var JSON_POINTER_URI_FRAGMENT = /^#(?:\/(?:[a-z0-9_\-.!$&'()*+,;:=@]|%[0-9a-f]{2}|~0|~1)*)*$/i;
var RELATIVE_JSON_POINTER = /^(?:0|[1-9][0-9]*)(?:#|(?:\/(?:[^~/]|~0|~1)*)*)$/;
var EMAIL = (input2) => {
  if (input2[0] === '"')
    return false;
  const [name3, host, ...rest] = input2.split("@");
  if (!name3 || !host || rest.length !== 0 || name3.length > 64 || host.length > 253)
    return false;
  if (name3[0] === "." || name3.endsWith(".") || name3.includes(".."))
    return false;
  if (!/^[a-z0-9.-]+$/i.test(host) || !/^[a-z0-9.!#$%&'*+/=?^_`{|}~-]+$/i.test(name3))
    return false;
  return host.split(".").every((part) => /^[a-z0-9]([a-z0-9-]{0,61}[a-z0-9])?$/i.test(part));
};
var IPV4 = /^(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)$/;
var IPV6 = /^((([0-9a-f]{1,4}:){7}([0-9a-f]{1,4}|:))|(([0-9a-f]{1,4}:){6}(:[0-9a-f]{1,4}|((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){5}(((:[0-9a-f]{1,4}){1,2})|:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3})|:))|(([0-9a-f]{1,4}:){4}(((:[0-9a-f]{1,4}){1,3})|((:[0-9a-f]{1,4})?:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){3}(((:[0-9a-f]{1,4}){1,4})|((:[0-9a-f]{1,4}){0,2}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){2}(((:[0-9a-f]{1,4}){1,5})|((:[0-9a-f]{1,4}){0,3}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(([0-9a-f]{1,4}:){1}(((:[0-9a-f]{1,4}){1,6})|((:[0-9a-f]{1,4}){0,4}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:))|(:(((:[0-9a-f]{1,4}){1,7})|((:[0-9a-f]{1,4}){0,5}:((25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)(\.(25[0-5]|2[0-4]\d|1\d\d|[1-9]?\d)){3}))|:)))$/i;
var DURATION = (input2) => input2.length > 1 && input2.length < 80 && (/^P\d+([.,]\d+)?W$/.test(input2) || /^P[\dYMDTHS]*(\d[.,]\d+)?[YMDHS]$/.test(input2) && /^P([.,\d]+Y)?([.,\d]+M)?([.,\d]+D)?(T([.,\d]+H)?([.,\d]+M)?([.,\d]+S)?)?$/.test(input2));
function bind(r2) {
  return r2.test.bind(r2);
}
var format = {
  date,
  time: time.bind(void 0, false),
  "date-time": date_time,
  duration: DURATION,
  uri,
  "uri-reference": bind(URIREF),
  "uri-template": bind(URITEMPLATE),
  url: bind(URL_),
  email: EMAIL,
  hostname: bind(HOSTNAME),
  ipv4: bind(IPV4),
  ipv6: bind(IPV6),
  regex,
  uuid: bind(UUID),
  "json-pointer": bind(JSON_POINTER),
  "json-pointer-uri-fragment": bind(JSON_POINTER_URI_FRAGMENT),
  "relative-json-pointer": bind(RELATIVE_JSON_POINTER)
};
function isLeapYear(year) {
  return year % 4 === 0 && (year % 100 !== 0 || year % 400 === 0);
}
function date(str) {
  const matches = str.match(DATE);
  if (!matches)
    return false;
  const yearPart = matches[1];
  const monthPart = matches[2];
  const dayPart = matches[3];
  if (!yearPart || !monthPart || !dayPart)
    return false;
  const year = +yearPart;
  const month = +monthPart;
  const day = +dayPart;
  const maxDay = month == 2 && isLeapYear(year) ? 29 : DAYS[month];
  return month >= 1 && month <= 12 && day >= 1 && maxDay !== void 0 && day <= maxDay;
}
function time(full, str) {
  const matches = str.match(TIME);
  if (!matches)
    return false;
  const hourPart = matches[1];
  const minutePart = matches[2];
  const secondPart = matches[3];
  if (!hourPart || !minutePart || !secondPart)
    return false;
  const hour = +hourPart;
  const minute = +minutePart;
  const second = +secondPart;
  const timeZone = !!matches[5];
  return (hour <= 23 && minute <= 59 && second <= 59 || hour == 23 && minute == 59 && second == 60) && (!full || timeZone);
}
var DATE_TIME_SEPARATOR = /t|\s/i;
function date_time(str) {
  const [datePart, timePart, extra] = str.split(DATE_TIME_SEPARATOR);
  return datePart !== void 0 && timePart !== void 0 && extra === void 0 && date(datePart) && time(true, timePart);
}
var NOT_URI_FRAGMENT = /\/|:/;
var URI_PATTERN = /^(?:[a-z][a-z0-9+\-.]*:)(?:\/?\/(?:(?:[a-z0-9\-._~!$&'()*+,;=:]|%[0-9a-f]{2})*@)?(?:\[(?:(?:(?:(?:[0-9a-f]{1,4}:){6}|::(?:[0-9a-f]{1,4}:){5}|(?:[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){4}|(?:(?:[0-9a-f]{1,4}:){0,1}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){3}|(?:(?:[0-9a-f]{1,4}:){0,2}[0-9a-f]{1,4})?::(?:[0-9a-f]{1,4}:){2}|(?:(?:[0-9a-f]{1,4}:){0,3}[0-9a-f]{1,4})?::[0-9a-f]{1,4}:|(?:(?:[0-9a-f]{1,4}:){0,4}[0-9a-f]{1,4})?::)(?:[0-9a-f]{1,4}:[0-9a-f]{1,4}|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?))|(?:(?:[0-9a-f]{1,4}:){0,5}[0-9a-f]{1,4})?::[0-9a-f]{1,4}|(?:(?:[0-9a-f]{1,4}:){0,6}[0-9a-f]{1,4})?::)|[Vv][0-9a-f]+\.[a-z0-9\-._~!$&'()*+,;=:]+)\]|(?:(?:25[0-5]|2[0-4]\d|[01]?\d\d?)\.){3}(?:25[0-5]|2[0-4]\d|[01]?\d\d?)|(?:[a-z0-9\-._~!$&'()*+,;=]|%[0-9a-f]{2})*)(?::\d*)?(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*|\/(?:(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)?|(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})+(?:\/(?:[a-z0-9\-._~!$&'()*+,;=:@]|%[0-9a-f]{2})*)*)(?:\?(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?(?:#(?:[a-z0-9\-._~!$&'()*+,;=:@/?]|%[0-9a-f]{2})*)?$/i;
function uri(str) {
  return NOT_URI_FRAGMENT.test(str) && URI_PATTERN.test(str);
}
var Z_ANCHOR = /[^\\]\\Z/;
function regex(str) {
  if (Z_ANCHOR.test(str))
    return false;
  try {
    new RegExp(str, "u");
    return true;
  } catch (e2) {
    return false;
  }
}

// node_modules/@cfworker/json-schema/dist/esm/ucs2-length.js
function ucs2length(s2) {
  let result = 0;
  let length = s2.length;
  let index = 0;
  let charCode;
  while (index < length) {
    result++;
    charCode = s2.charCodeAt(index++);
    if (charCode >= 55296 && charCode <= 56319 && index < length) {
      charCode = s2.charCodeAt(index);
      if ((charCode & 64512) == 56320) {
        index++;
      }
    }
  }
  return result;
}

// node_modules/@cfworker/json-schema/dist/esm/validate.js
function hasOwnProperty(instance, key) {
  return Object.prototype.hasOwnProperty.call(instance, key);
}
function decimalInteger(value2) {
  const coefficientAndExponent = Math.abs(value2).toString().split("e");
  const coefficient = coefficientAndExponent[0];
  const exponent = coefficientAndExponent[1] ?? "0";
  const point = coefficient.indexOf(".");
  const fractionLength = point === -1 ? 0 : coefficient.length - point - 1;
  return [
    BigInt(coefficient.replace(".", "")),
    Number(exponent) - fractionLength
  ];
}
function isMultipleOf(instance, divisor) {
  if (!Number.isFinite(instance) || !Number.isFinite(divisor) || divisor <= 0) {
    return false;
  }
  const [instanceCoefficient, instanceExponent] = decimalInteger(instance);
  const [divisorCoefficient, divisorExponent] = decimalInteger(divisor);
  const exponentDifference = instanceExponent - divisorExponent;
  if (exponentDifference >= 0) {
    return instanceCoefficient * 10n ** BigInt(exponentDifference) % divisorCoefficient === 0n;
  }
  return instanceCoefficient % (divisorCoefficient * 10n ** BigInt(-exponentDifference)) === 0n;
}
function validate(instance, schema, draft = "2019-09", lookup = dereference(schema), shortCircuit = true, recursiveAnchor = null, instanceLocation = "#", schemaLocation = "#", evaluated = /* @__PURE__ */ Object.create(null)) {
  if (schema === true) {
    return { valid: true, errors: [] };
  }
  if (schema === false) {
    return {
      valid: false,
      errors: [
        {
          instanceLocation,
          keyword: "false",
          keywordLocation: instanceLocation,
          error: "False boolean schema."
        }
      ]
    };
  }
  const rawInstanceType = typeof instance;
  let instanceType;
  switch (rawInstanceType) {
    case "boolean":
    case "number":
    case "string":
      instanceType = rawInstanceType;
      break;
    case "object":
      if (instance === null) {
        instanceType = "null";
      } else if (Array.isArray(instance)) {
        instanceType = "array";
      } else {
        instanceType = "object";
      }
      break;
    default:
      throw new Error(`Instances of "${rawInstanceType}" type are not supported.`);
  }
  const { $ref, $recursiveRef, $recursiveAnchor, type: $type, const: $const, enum: $enum, required: $required, not: $not, anyOf: $anyOf, allOf: $allOf, oneOf: $oneOf, if: $if, then: $then, else: $else, format: $format, properties: $properties, patternProperties: $patternProperties, additionalProperties: $additionalProperties, unevaluatedProperties: $unevaluatedProperties, minProperties: $minProperties, maxProperties: $maxProperties, propertyNames: $propertyNames, dependentRequired: $dependentRequired, dependentSchemas: $dependentSchemas, dependencies: $dependencies, prefixItems: $prefixItems, items: $items, additionalItems: $additionalItems, unevaluatedItems: $unevaluatedItems, contains: $contains, minContains: $minContains, maxContains: $maxContains, minItems: $minItems, maxItems: $maxItems, uniqueItems: $uniqueItems, minimum: $minimum, maximum: $maximum, exclusiveMinimum: $exclusiveMinimum, exclusiveMaximum: $exclusiveMaximum, multipleOf: $multipleOf, minLength: $minLength, maxLength: $maxLength, pattern: $pattern, __absolute_ref__, __absolute_recursive_ref__ } = schema;
  const errors = [];
  if ($recursiveAnchor === true && recursiveAnchor === null) {
    recursiveAnchor = schema;
  }
  if ($recursiveRef === "#") {
    const refSchema = recursiveAnchor === null ? lookup[__absolute_recursive_ref__] : recursiveAnchor;
    const keywordLocation = `${schemaLocation}/$recursiveRef`;
    const result = validate(instance, recursiveAnchor === null ? schema : recursiveAnchor, draft, lookup, shortCircuit, refSchema, instanceLocation, keywordLocation, evaluated);
    if (!result.valid) {
      errors.push({
        instanceLocation,
        keyword: "$recursiveRef",
        keywordLocation,
        error: "A subschema had errors."
      }, ...result.errors);
    }
  }
  if ($ref !== void 0) {
    const uri2 = __absolute_ref__ || $ref;
    const refSchema = lookup[uri2];
    if (refSchema === void 0) {
      let message = `Unresolved $ref "${$ref}".`;
      if (__absolute_ref__ && __absolute_ref__ !== $ref) {
        message += `  Absolute URI "${__absolute_ref__}".`;
      }
      message += `
Known schemas:
- ${Object.keys(lookup).join("\n- ")}`;
      throw new Error(message);
    }
    const keywordLocation = `${schemaLocation}/$ref`;
    const result = validate(instance, refSchema, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, keywordLocation, evaluated);
    if (!result.valid) {
      errors.push({
        instanceLocation,
        keyword: "$ref",
        keywordLocation,
        error: "A subschema had errors."
      }, ...result.errors);
    }
    if (draft === "4" || draft === "7") {
      return { valid: errors.length === 0, errors };
    }
  }
  if (Array.isArray($type)) {
    let length = $type.length;
    let valid = false;
    for (let i2 = 0; i2 < length; i2++) {
      if (instanceType === $type[i2] || $type[i2] === "integer" && instanceType === "number" && instance % 1 === 0 && instance === instance) {
        valid = true;
        break;
      }
    }
    if (!valid) {
      errors.push({
        instanceLocation,
        keyword: "type",
        keywordLocation: `${schemaLocation}/type`,
        error: `Instance type "${instanceType}" is invalid. Expected "${$type.join('", "')}".`
      });
    }
  } else if ($type === "integer") {
    if (instanceType !== "number" || instance % 1 || instance !== instance) {
      errors.push({
        instanceLocation,
        keyword: "type",
        keywordLocation: `${schemaLocation}/type`,
        error: `Instance type "${instanceType}" is invalid. Expected "${$type}".`
      });
    }
  } else if ($type !== void 0 && instanceType !== $type) {
    errors.push({
      instanceLocation,
      keyword: "type",
      keywordLocation: `${schemaLocation}/type`,
      error: `Instance type "${instanceType}" is invalid. Expected "${$type}".`
    });
  }
  if ($const !== void 0) {
    if (instanceType === "object" || instanceType === "array") {
      if (!deepCompareStrict(instance, $const)) {
        errors.push({
          instanceLocation,
          keyword: "const",
          keywordLocation: `${schemaLocation}/const`,
          error: `Instance does not match ${JSON.stringify($const)}.`
        });
      }
    } else if (instance !== $const) {
      errors.push({
        instanceLocation,
        keyword: "const",
        keywordLocation: `${schemaLocation}/const`,
        error: `Instance does not match ${JSON.stringify($const)}.`
      });
    }
  }
  if ($enum !== void 0) {
    if (instanceType === "object" || instanceType === "array") {
      if (!$enum.some((value2) => deepCompareStrict(instance, value2))) {
        errors.push({
          instanceLocation,
          keyword: "enum",
          keywordLocation: `${schemaLocation}/enum`,
          error: `Instance does not match any of ${JSON.stringify($enum)}.`
        });
      }
    } else if (!$enum.some((value2) => instance === value2)) {
      errors.push({
        instanceLocation,
        keyword: "enum",
        keywordLocation: `${schemaLocation}/enum`,
        error: `Instance does not match any of ${JSON.stringify($enum)}.`
      });
    }
  }
  if ($not !== void 0) {
    const keywordLocation = `${schemaLocation}/not`;
    const result = validate(instance, $not, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, keywordLocation);
    if (result.valid) {
      errors.push({
        instanceLocation,
        keyword: "not",
        keywordLocation,
        error: 'Instance matched "not" schema.'
      });
    }
  }
  let subEvaluateds = [];
  if ($anyOf !== void 0) {
    const keywordLocation = `${schemaLocation}/anyOf`;
    const errorsLength = errors.length;
    let anyValid = false;
    for (const [i2, subSchema] of $anyOf.entries()) {
      const subEvaluated = Object.create(evaluated);
      const result = validate(instance, subSchema, draft, lookup, shortCircuit, $recursiveAnchor === true ? recursiveAnchor : null, instanceLocation, `${keywordLocation}/${i2}`, subEvaluated);
      errors.push(...result.errors);
      anyValid = anyValid || result.valid;
      if (result.valid) {
        subEvaluateds.push(subEvaluated);
      }
    }
    if (anyValid) {
      errors.length = errorsLength;
    } else {
      errors.splice(errorsLength, 0, {
        instanceLocation,
        keyword: "anyOf",
        keywordLocation,
        error: "Instance does not match any subschemas."
      });
    }
  }
  if ($allOf !== void 0) {
    const keywordLocation = `${schemaLocation}/allOf`;
    const errorsLength = errors.length;
    let allValid = true;
    for (const [i2, subSchema] of $allOf.entries()) {
      const subEvaluated = Object.create(evaluated);
      const result = validate(instance, subSchema, draft, lookup, shortCircuit, $recursiveAnchor === true ? recursiveAnchor : null, instanceLocation, `${keywordLocation}/${i2}`, subEvaluated);
      errors.push(...result.errors);
      allValid = allValid && result.valid;
      if (result.valid) {
        subEvaluateds.push(subEvaluated);
      }
    }
    if (allValid) {
      errors.length = errorsLength;
    } else {
      errors.splice(errorsLength, 0, {
        instanceLocation,
        keyword: "allOf",
        keywordLocation,
        error: `Instance does not match every subschema.`
      });
    }
  }
  if ($oneOf !== void 0) {
    const keywordLocation = `${schemaLocation}/oneOf`;
    const errorsLength = errors.length;
    const matches = $oneOf.filter((subSchema, i2) => {
      const subEvaluated = Object.create(evaluated);
      const result = validate(instance, subSchema, draft, lookup, shortCircuit, $recursiveAnchor === true ? recursiveAnchor : null, instanceLocation, `${keywordLocation}/${i2}`, subEvaluated);
      errors.push(...result.errors);
      if (result.valid) {
        subEvaluateds.push(subEvaluated);
      }
      return result.valid;
    }).length;
    if (matches === 1) {
      errors.length = errorsLength;
    } else {
      errors.splice(errorsLength, 0, {
        instanceLocation,
        keyword: "oneOf",
        keywordLocation,
        error: `Instance does not match exactly one subschema (${matches} matches).`
      });
    }
  }
  if (instanceType === "object" || instanceType === "array") {
    Object.assign(evaluated, ...subEvaluateds);
  }
  if ($if !== void 0) {
    const keywordLocation = `${schemaLocation}/if`;
    const conditionResult = validate(instance, $if, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, keywordLocation, evaluated).valid;
    if (conditionResult) {
      if ($then !== void 0) {
        const thenResult = validate(instance, $then, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${schemaLocation}/then`, evaluated);
        if (!thenResult.valid) {
          errors.push({
            instanceLocation,
            keyword: "if",
            keywordLocation,
            error: `Instance does not match "then" schema.`
          }, ...thenResult.errors);
        }
      }
    } else if ($else !== void 0) {
      const elseResult = validate(instance, $else, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${schemaLocation}/else`, evaluated);
      if (!elseResult.valid) {
        errors.push({
          instanceLocation,
          keyword: "if",
          keywordLocation,
          error: `Instance does not match "else" schema.`
        }, ...elseResult.errors);
      }
    }
  }
  if (instanceType === "object") {
    if ($required !== void 0) {
      for (const key of $required) {
        if (!hasOwnProperty(instance, key)) {
          errors.push({
            instanceLocation,
            keyword: "required",
            keywordLocation: `${schemaLocation}/required`,
            error: `Instance does not have required property "${key}".`
          });
        }
      }
    }
    const keys = Object.keys(instance);
    if ($minProperties !== void 0 && keys.length < $minProperties) {
      errors.push({
        instanceLocation,
        keyword: "minProperties",
        keywordLocation: `${schemaLocation}/minProperties`,
        error: `Instance does not have at least ${$minProperties} properties.`
      });
    }
    if ($maxProperties !== void 0 && keys.length > $maxProperties) {
      errors.push({
        instanceLocation,
        keyword: "maxProperties",
        keywordLocation: `${schemaLocation}/maxProperties`,
        error: `Instance does not have at least ${$maxProperties} properties.`
      });
    }
    if ($propertyNames !== void 0) {
      const keywordLocation = `${schemaLocation}/propertyNames`;
      for (const key in instance) {
        const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
        const result = validate(key, $propertyNames, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, keywordLocation);
        if (!result.valid) {
          errors.push({
            instanceLocation,
            keyword: "propertyNames",
            keywordLocation,
            error: `Property name "${key}" does not match schema.`
          }, ...result.errors);
        }
      }
    }
    if ($dependentRequired !== void 0) {
      const keywordLocation = `${schemaLocation}/dependantRequired`;
      for (const key in $dependentRequired) {
        if (hasOwnProperty(instance, key)) {
          const required2 = $dependentRequired[key];
          for (const dependantKey of required2) {
            if (!hasOwnProperty(instance, dependantKey)) {
              errors.push({
                instanceLocation,
                keyword: "dependentRequired",
                keywordLocation,
                error: `Instance has "${key}" but does not have "${dependantKey}".`
              });
            }
          }
        }
      }
    }
    if ($dependentSchemas !== void 0) {
      for (const [key, dependentSchema] of Object.entries($dependentSchemas)) {
        const keywordLocation = `${schemaLocation}/dependentSchemas`;
        if (hasOwnProperty(instance, key)) {
          const result = validate(instance, dependentSchema, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${keywordLocation}/${encodePointer(key)}`, evaluated);
          if (!result.valid) {
            errors.push({
              instanceLocation,
              keyword: "dependentSchemas",
              keywordLocation,
              error: `Instance has "${key}" but does not match dependant schema.`
            }, ...result.errors);
          }
        }
      }
    }
    if ($dependencies !== void 0) {
      const keywordLocation = `${schemaLocation}/dependencies`;
      for (const key in $dependencies) {
        if (hasOwnProperty(instance, key)) {
          const propsOrSchema = $dependencies[key];
          if (Array.isArray(propsOrSchema)) {
            for (const dependantKey of propsOrSchema) {
              if (!hasOwnProperty(instance, dependantKey)) {
                errors.push({
                  instanceLocation,
                  keyword: "dependencies",
                  keywordLocation,
                  error: `Instance has "${key}" but does not have "${dependantKey}".`
                });
              }
            }
          } else {
            const result = validate(instance, propsOrSchema, draft, lookup, shortCircuit, recursiveAnchor, instanceLocation, `${keywordLocation}/${encodePointer(key)}`);
            if (!result.valid) {
              errors.push({
                instanceLocation,
                keyword: "dependencies",
                keywordLocation,
                error: `Instance has "${key}" but does not match dependant schema.`
              }, ...result.errors);
            }
          }
        }
      }
    }
    const thisEvaluated = /* @__PURE__ */ Object.create(null);
    let stop = false;
    if ($properties !== void 0) {
      const keywordLocation = `${schemaLocation}/properties`;
      for (const [key, propertySchema] of Object.entries($properties)) {
        if (!hasOwnProperty(instance, key)) {
          continue;
        }
        const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
        const result = validate(instance[key], propertySchema, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, `${keywordLocation}/${encodePointer(key)}`);
        thisEvaluated[key] = true;
        if (result.valid) {
          evaluated[key] = true;
        } else {
          stop = shortCircuit;
          errors.push({
            instanceLocation,
            keyword: "properties",
            keywordLocation,
            error: `Property "${key}" does not match schema.`
          }, ...result.errors);
          if (stop)
            break;
        }
      }
    }
    if (!stop && $patternProperties !== void 0) {
      const keywordLocation = `${schemaLocation}/patternProperties`;
      for (const [pattern, subSchema] of Object.entries($patternProperties)) {
        const regex2 = new RegExp(pattern, "u");
        for (const key in instance) {
          if (!regex2.test(key)) {
            continue;
          }
          const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
          const result = validate(instance[key], subSchema, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, `${keywordLocation}/${encodePointer(pattern)}`);
          thisEvaluated[key] = true;
          if (result.valid) {
            evaluated[key] = true;
          } else {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "patternProperties",
              keywordLocation,
              error: `Property "${key}" matches pattern "${pattern}" but does not match associated schema.`
            }, ...result.errors);
          }
        }
      }
    }
    if (!stop && $additionalProperties !== void 0) {
      const keywordLocation = `${schemaLocation}/additionalProperties`;
      for (const key in instance) {
        if (thisEvaluated[key]) {
          continue;
        }
        const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
        const result = validate(instance[key], $additionalProperties, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, keywordLocation);
        if (result.valid) {
          evaluated[key] = true;
        } else {
          stop = shortCircuit;
          errors.push({
            instanceLocation,
            keyword: "additionalProperties",
            keywordLocation,
            error: `Property "${key}" does not match additional properties schema.`
          }, ...result.errors);
        }
      }
    } else if (!stop && $unevaluatedProperties !== void 0) {
      const keywordLocation = `${schemaLocation}/unevaluatedProperties`;
      for (const key in instance) {
        if (!evaluated[key]) {
          const subInstancePointer = `${instanceLocation}/${encodePointer(key)}`;
          const result = validate(instance[key], $unevaluatedProperties, draft, lookup, shortCircuit, recursiveAnchor, subInstancePointer, keywordLocation);
          if (result.valid) {
            evaluated[key] = true;
          } else {
            errors.push({
              instanceLocation,
              keyword: "unevaluatedProperties",
              keywordLocation,
              error: `Property "${key}" does not match unevaluated properties schema.`
            }, ...result.errors);
          }
        }
      }
    }
  } else if (instanceType === "array") {
    if ($maxItems !== void 0 && instance.length > $maxItems) {
      errors.push({
        instanceLocation,
        keyword: "maxItems",
        keywordLocation: `${schemaLocation}/maxItems`,
        error: `Array has too many items (${instance.length} > ${$maxItems}).`
      });
    }
    if ($minItems !== void 0 && instance.length < $minItems) {
      errors.push({
        instanceLocation,
        keyword: "minItems",
        keywordLocation: `${schemaLocation}/minItems`,
        error: `Array has too few items (${instance.length} < ${$minItems}).`
      });
    }
    const length = instance.length;
    let i2 = 0;
    let stop = false;
    if ($prefixItems !== void 0) {
      const keywordLocation = `${schemaLocation}/prefixItems`;
      const length2 = Math.min($prefixItems.length, length);
      for (; i2 < length2; i2++) {
        const result = validate(instance[i2], $prefixItems[i2], draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, `${keywordLocation}/${i2}`);
        evaluated[i2] = true;
        if (!result.valid) {
          stop = shortCircuit;
          errors.push({
            instanceLocation,
            keyword: "prefixItems",
            keywordLocation,
            error: `Items did not match schema.`
          }, ...result.errors);
          if (stop)
            break;
        }
      }
    }
    if ($items !== void 0) {
      const keywordLocation = `${schemaLocation}/items`;
      if (Array.isArray($items)) {
        const length2 = Math.min($items.length, length);
        for (; i2 < length2; i2++) {
          const result = validate(instance[i2], $items[i2], draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, `${keywordLocation}/${i2}`);
          evaluated[i2] = true;
          if (!result.valid) {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "items",
              keywordLocation,
              error: `Items did not match schema.`
            }, ...result.errors);
            if (stop)
              break;
          }
        }
      } else {
        for (; i2 < length; i2++) {
          const result = validate(instance[i2], $items, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, keywordLocation);
          evaluated[i2] = true;
          if (!result.valid) {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "items",
              keywordLocation,
              error: `Items did not match schema.`
            }, ...result.errors);
            if (stop)
              break;
          }
        }
      }
      if (!stop && $additionalItems !== void 0) {
        const keywordLocation2 = `${schemaLocation}/additionalItems`;
        for (; i2 < length; i2++) {
          const result = validate(instance[i2], $additionalItems, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, keywordLocation2);
          evaluated[i2] = true;
          if (!result.valid) {
            stop = shortCircuit;
            errors.push({
              instanceLocation,
              keyword: "additionalItems",
              keywordLocation: keywordLocation2,
              error: `Items did not match additional items schema.`
            }, ...result.errors);
          }
        }
      }
    }
    if ($contains !== void 0) {
      if (length === 0 && $minContains === void 0) {
        errors.push({
          instanceLocation,
          keyword: "contains",
          keywordLocation: `${schemaLocation}/contains`,
          error: `Array is empty. It must contain at least one item matching the schema.`
        });
      } else if ($minContains !== void 0 && length < $minContains) {
        errors.push({
          instanceLocation,
          keyword: "minContains",
          keywordLocation: `${schemaLocation}/minContains`,
          error: `Array has less items (${length}) than minContains (${$minContains}).`
        });
      } else {
        const keywordLocation = `${schemaLocation}/contains`;
        const errorsLength = errors.length;
        let contained = 0;
        for (let j2 = 0; j2 < length; j2++) {
          const result = validate(instance[j2], $contains, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${j2}`, keywordLocation);
          if (result.valid) {
            evaluated[j2] = true;
            contained++;
          } else {
            errors.push(...result.errors);
          }
        }
        if (contained >= ($minContains || 0)) {
          errors.length = errorsLength;
        }
        if ($minContains === void 0 && contained === 0) {
          errors.splice(errorsLength, 0, {
            instanceLocation,
            keyword: "contains",
            keywordLocation,
            error: `Array does not contain item matching schema.`
          });
        } else if ($minContains !== void 0 && contained < $minContains) {
          errors.push({
            instanceLocation,
            keyword: "minContains",
            keywordLocation: `${schemaLocation}/minContains`,
            error: `Array must contain at least ${$minContains} items matching schema. Only ${contained} items were found.`
          });
        } else if ($maxContains !== void 0 && contained > $maxContains) {
          errors.push({
            instanceLocation,
            keyword: "maxContains",
            keywordLocation: `${schemaLocation}/maxContains`,
            error: `Array may contain at most ${$maxContains} items matching schema. ${contained} items were found.`
          });
        }
      }
    }
    if (!stop && $unevaluatedItems !== void 0) {
      const keywordLocation = `${schemaLocation}/unevaluatedItems`;
      for (i2; i2 < length; i2++) {
        if (evaluated[i2]) {
          continue;
        }
        const result = validate(instance[i2], $unevaluatedItems, draft, lookup, shortCircuit, recursiveAnchor, `${instanceLocation}/${i2}`, keywordLocation);
        evaluated[i2] = true;
        if (!result.valid) {
          errors.push({
            instanceLocation,
            keyword: "unevaluatedItems",
            keywordLocation,
            error: `Items did not match unevaluated items schema.`
          }, ...result.errors);
        }
      }
    }
    if ($uniqueItems) {
      for (let j2 = 0; j2 < length; j2++) {
        const a2 = instance[j2];
        const ao = typeof a2 === "object" && a2 !== null;
        for (let k = 0; k < length; k++) {
          if (j2 === k) {
            continue;
          }
          const b2 = instance[k];
          const bo = typeof b2 === "object" && b2 !== null;
          if (a2 === b2 || ao && bo && deepCompareStrict(a2, b2)) {
            errors.push({
              instanceLocation,
              keyword: "uniqueItems",
              keywordLocation: `${schemaLocation}/uniqueItems`,
              error: `Duplicate items at indexes ${j2} and ${k}.`
            });
            j2 = Number.MAX_SAFE_INTEGER;
            k = Number.MAX_SAFE_INTEGER;
          }
        }
      }
    }
  } else if (instanceType === "number") {
    if (draft === "4") {
      if ($minimum !== void 0 && ($exclusiveMinimum === true && instance <= $minimum || instance < $minimum)) {
        errors.push({
          instanceLocation,
          keyword: "minimum",
          keywordLocation: `${schemaLocation}/minimum`,
          error: `${instance} is less than ${$exclusiveMinimum ? "or equal to " : ""} ${$minimum}.`
        });
      }
      if ($maximum !== void 0 && ($exclusiveMaximum === true && instance >= $maximum || instance > $maximum)) {
        errors.push({
          instanceLocation,
          keyword: "maximum",
          keywordLocation: `${schemaLocation}/maximum`,
          error: `${instance} is greater than ${$exclusiveMaximum ? "or equal to " : ""} ${$maximum}.`
        });
      }
    } else {
      if ($minimum !== void 0 && instance < $minimum) {
        errors.push({
          instanceLocation,
          keyword: "minimum",
          keywordLocation: `${schemaLocation}/minimum`,
          error: `${instance} is less than ${$minimum}.`
        });
      }
      if ($maximum !== void 0 && instance > $maximum) {
        errors.push({
          instanceLocation,
          keyword: "maximum",
          keywordLocation: `${schemaLocation}/maximum`,
          error: `${instance} is greater than ${$maximum}.`
        });
      }
      if ($exclusiveMinimum !== void 0 && instance <= $exclusiveMinimum) {
        errors.push({
          instanceLocation,
          keyword: "exclusiveMinimum",
          keywordLocation: `${schemaLocation}/exclusiveMinimum`,
          error: `${instance} is less than ${$exclusiveMinimum}.`
        });
      }
      if ($exclusiveMaximum !== void 0 && instance >= $exclusiveMaximum) {
        errors.push({
          instanceLocation,
          keyword: "exclusiveMaximum",
          keywordLocation: `${schemaLocation}/exclusiveMaximum`,
          error: `${instance} is greater than or equal to ${$exclusiveMaximum}.`
        });
      }
    }
    if ($multipleOf !== void 0) {
      if (!isMultipleOf(instance, $multipleOf)) {
        errors.push({
          instanceLocation,
          keyword: "multipleOf",
          keywordLocation: `${schemaLocation}/multipleOf`,
          error: `${instance} is not a multiple of ${$multipleOf}.`
        });
      }
    }
  } else if (instanceType === "string") {
    const length = $minLength === void 0 && $maxLength === void 0 ? 0 : ucs2length(instance);
    if ($minLength !== void 0 && length < $minLength) {
      errors.push({
        instanceLocation,
        keyword: "minLength",
        keywordLocation: `${schemaLocation}/minLength`,
        error: `String is too short (${length} < ${$minLength}).`
      });
    }
    if ($maxLength !== void 0 && length > $maxLength) {
      errors.push({
        instanceLocation,
        keyword: "maxLength",
        keywordLocation: `${schemaLocation}/maxLength`,
        error: `String is too long (${length} > ${$maxLength}).`
      });
    }
    if ($pattern !== void 0 && !new RegExp($pattern, "u").test(instance)) {
      errors.push({
        instanceLocation,
        keyword: "pattern",
        keywordLocation: `${schemaLocation}/pattern`,
        error: `String does not match pattern.`
      });
    }
    if ($format !== void 0 && format[$format] && !format[$format](instance)) {
      errors.push({
        instanceLocation,
        keyword: "format",
        keywordLocation: `${schemaLocation}/format`,
        error: `String does not match format "${$format}".`
      });
    }
  }
  return { valid: errors.length === 0, errors };
}

// node_modules/@cfworker/json-schema/dist/esm/validator.js
var Validator = class {
  schema;
  draft;
  shortCircuit;
  lookup;
  constructor(schema, draft = "2019-09", shortCircuit = true) {
    this.schema = schema;
    this.draft = draft;
    this.shortCircuit = shortCircuit;
    this.lookup = dereference(schema);
  }
  validate(instance) {
    return validate(instance, this.schema, this.draft, this.lookup, this.shortCircuit);
  }
  addSchema(schema, id2) {
    if (id2) {
      schema = { ...schema, $id: id2 };
    }
    dereference(schema, this.lookup);
  }
};

// srcts/util/validate.ts
function compile(what, schema, fails = "fails its schema") {
  let validator;
  try {
    validator = new Validator(schema, draftOf(schema), false);
  } catch (error) {
    throw new Error(`The schema of ${what} cannot be read: ${messageOf(error)}`, {
      cause: error
    });
  }
  return (value2) => {
    const result = validator.validate(value2);
    return result.valid ? [] : describe(result.errors, fails);
  };
}
function draftOf(schema) {
  const declared = typeof schema.$schema === "string" ? schema.$schema : "";
  if (declared.includes("draft-04")) return "4";
  if (declared.includes("draft-07")) return "7";
  if (declared.includes("2019-09")) return "2019-09";
  return "2020-12";
}
var wrappers = /* @__PURE__ */ new Set([
  "properties",
  "items",
  "prefixItems",
  "additionalItems",
  "$ref",
  "$dynamicRef",
  "$recursiveRef",
  "allOf",
  "false"
]);
function describe(errors, fails) {
  return errors.filter((error) => !wrappers.has(error.keyword)).map((error) => {
    const at = error.instanceLocation.replace(/^#/, "");
    return `${fails}${at === "" ? "" : ` at ${at}`}: ${error.error}`;
  });
}

// srcts/protocol/client.ts
var SessionEndedError = class extends Error {
  constructor(id2) {
    super(`The session ended before R replied to request ${id2}.`);
    this.name = "SessionEndedError";
  }
};
function createClient(transport, options) {
  let next = 0;
  let state2 = "none";
  let shared = /* @__PURE__ */ new Set();
  const waiting2 = /* @__PURE__ */ new Map();
  let held2 = [];
  const sending = /* @__PURE__ */ new Set();
  let ends = 0;
  function mint() {
    next += 1;
    return String(next);
  }
  function dispatch(outbound) {
    const plain = outbound.filter((entry) => entry.call?.body === void 0);
    if (plain.length > 0) {
      transmit(plain, /* @__PURE__ */ new Map());
    }
    for (const entry of outbound) {
      if (entry.call?.body !== void 0) {
        transmit([entry], /* @__PURE__ */ new Map([[entry.call.id, entry.call.body]]));
      }
    }
  }
  function transmit(outbound, bodies) {
    const calls = outbound.flatMap((entry) => entry.call ?? []);
    for (const call of calls) {
      waiting2.set(call.id, call);
    }
    const sent2 = transport.send(
      outbound.map((entry) => entry.message),
      bodies
    ).then(
      () => {
        for (const call of calls) {
          call.sent = true;
          if (call.aborted) cancel(call);
        }
      },
      (error) => {
        for (const call of calls) {
          if (waiting2.get(call.id) === call) {
            waiting2.delete(call.id);
            call.settle({ error: asError(error) });
          }
        }
      }
    );
    if (bodies.size > 0) {
      sending.add(sent2);
      void sent2.finally(() => sending.delete(sent2));
    }
  }
  function cancel(call) {
    if (waiting2.get(call.id) !== call || !shared.has("cancel")) return;
    quietly(transport.send([{ kind: "cancel", request: call.id }], /* @__PURE__ */ new Map()));
  }
  function abort(entry, call) {
    const index = held2.indexOf(entry);
    if (index >= 0) {
      held2.splice(index, 1);
      call.settle(cancelled(call.id));
    } else if (call.sent) {
      cancel(call);
    } else {
      call.aborted = true;
    }
  }
  function queue(entry) {
    if (state2 === "ready") {
      dispatch([entry]);
    } else {
      held2.push(entry);
    }
  }
  function end() {
    state2 = "none";
    shared = /* @__PURE__ */ new Set();
    ends += 1;
    const lost = [...waiting2.values()];
    waiting2.clear();
    for (const call of lost) {
      call.settle({ error: new SessionEndedError(call.id) });
    }
  }
  function sendHello() {
    const named2 = options.hello();
    const id2 = mint();
    const call = {
      id: id2,
      body: void 0,
      progress: void 0,
      sent: false,
      aborted: false,
      settle(result) {
        if ("error" in result) {
          if (!(result.error instanceof SessionEndedError)) report(result.error);
          return;
        }
        const { reply } = result;
        if (reply.status === "ok" && "capabilities" in reply) {
          const theirs = new Set(reply.capabilities);
          shared = new Set(named2.capabilities.filter((c2) => theirs.has(c2)));
          state2 = "ready";
          const outbound = held2;
          held2 = [];
          dispatch(outbound);
        }
        guard(() => {
          options.session(reply);
        });
      }
    };
    transmit([{ message: { ...named2, id: id2, kind: "hello" }, call }], /* @__PURE__ */ new Map());
  }
  transport.listen({
    started() {
      end();
      state2 = "started";
      sendHello();
    },
    ended() {
      end();
    },
    received(item, body) {
      const call = waiting2.get(item.id);
      if (call === void 0) return;
      if (!("status" in item)) {
        const note = item.progress;
        guard(() => {
          call.progress?.(note);
        });
        return;
      }
      waiting2.delete(item.id);
      if (body === null) {
        call.settle({ reply: item, body: null });
      } else {
        body.then(
          (blob2) => {
            call.settle({ reply: item, body: blob2 });
          },
          (error) => {
            call.settle({ error: asError(error) });
          }
        );
      }
    }
  });
  return {
    request(request, { body, progress, signal } = {}) {
      const id2 = mint();
      const message = {
        ...request,
        id: id2,
        ...body === void 0 ? {} : { body: announce(body) }
      };
      return new Promise((resolve, reject) => {
        const onAbort2 = () => {
          abort(entry, call);
        };
        const call = {
          id: id2,
          body,
          progress,
          sent: false,
          aborted: false,
          settle(result) {
            signal?.removeEventListener("abort", onAbort2);
            if ("error" in result) {
              reject(result.error);
            } else {
              resolve(result);
            }
          }
        };
        const entry = { message, call };
        if (signal?.aborted === true) {
          call.settle(cancelled(id2));
          return;
        }
        signal?.addEventListener("abort", onAbort2, { once: true });
        queue(entry);
      });
    },
    forget(blocks) {
      queue({ message: { kind: "forget", blocks } });
    },
    capabilities: () => shared,
    hello() {
      const runs = state2 !== "none";
      const lost = held2;
      held2 = [];
      end();
      for (const { call } of lost) {
        call?.settle({ error: new SessionEndedError(call.id) });
      }
      if (!runs) return;
      state2 = "started";
      if (sending.size === 0) {
        sendHello();
        return;
      }
      const at = ends;
      void Promise.allSettled([...sending]).then(() => {
        if (ends === at) sendHello();
      });
    }
  };
}
function announce(body) {
  const type = body.type === "" ? "application/octet-stream" : body.type;
  return body instanceof File ? { type, name: body.name } : { type };
}
function cancelled(id2) {
  return { reply: { id: id2, status: "cancelled" }, body: null };
}
function guard(fn) {
  try {
    fn();
  } catch (error) {
    report(error);
  }
}
function quietly(sent2) {
  sent2.catch(() => void 0);
}

// srcts/protocol/messages.ts
var PROTOCOL_VERSION = "1.0";

// srcts/scheduler/status.ts
var NONE = Object.freeze({ warning: 0, message: 0 });
var NO_CONDITIONS = Object.freeze([]);
function outcomeOf(reply, revision) {
  if (reply.status === "cancelled" || reply.status === "missing") return void 0;
  const evaluated = reply;
  return {
    status: reply.status,
    revision,
    summary: evaluated.summary ?? null,
    conditions: evaluated.conditions ?? NO_CONDITIONS,
    omitted: evaluated.omitted ?? NONE,
    stdout: evaluated.stdout ?? "",
    stderr: evaluated.stderr ?? ""
  };
}
function unsent(revision, message) {
  return {
    status: "error",
    revision,
    summary: null,
    conditions: [
      {
        severity: "error",
        class: ["blockr_error", "error", "condition"],
        message: `The evaluation could not be sent to R: ${message}`
      }
    ],
    omitted: NONE,
    stdout: "",
    stderr: ""
  };
}
function unevaluated(revision, running2) {
  return {
    status: "unevaluated",
    revision,
    evaluated: null,
    running: running2,
    conditions: NO_CONDITIONS,
    omitted: NONE,
    stdout: "",
    stderr: "",
    summary: null
  };
}
function replied(outcome, revision, running2) {
  return {
    status: outcome.revision !== revision ? "stale" : outcome.status === "ok" ? "ready" : outcome.status === "error" ? "failed" : "invalid",
    revision,
    evaluated: outcome.revision,
    running: running2,
    conditions: outcome.conditions,
    omitted: outcome.omitted,
    stdout: outcome.stdout,
    stderr: outcome.stderr,
    summary: outcome.summary
  };
}
function waiting(why, revision, evaluated, running2) {
  return {
    status: "waiting",
    revision,
    evaluated,
    running: running2,
    conditions: [why],
    omitted: NONE,
    stdout: "",
    stderr: "",
    summary: null
  };
}
function waitingFor(what) {
  let message;
  if ("upstream" in what) {
    const blocks = what.upstream.length === 1 ? "block" : "blocks";
    message = `This block is waiting for ${blocks} ${quoted(what.upstream)}.`;
  } else if (what.unlinked.includes("...")) {
    message = "This block is waiting for a block to be linked to it.";
  } else {
    const inputs = what.unlinked.length === 1 ? "input" : "inputs";
    message = `This block is waiting for its ${inputs} ${quoted(what.unlinked)} to be linked.`;
  }
  return {
    severity: "message",
    class: ["blockr_message", "message", "condition"],
    phase: "status",
    message
  };
}
function sameStatus(a2, b2) {
  return a2.status === b2.status && a2.revision === b2.revision && a2.evaluated === b2.evaluated && a2.running === b2.running && a2.omitted === b2.omitted && a2.stdout === b2.stdout && a2.stderr === b2.stderr && a2.summary === b2.summary && a2.conditions.length === b2.conditions.length && a2.conditions.every(
    (condition, i2) => condition === b2.conditions[i2] || condition.phase === "status" && condition.message === b2.conditions[i2]?.message
  );
}
function statusesOf(input2) {
  const statuses = /* @__PURE__ */ new Map();
  const changed = [];
  for (const id2 of input2.graph.order) {
    const revision = input2.revisions.get(id2);
    if (revision === void 0) continue;
    const outcome = input2.outcomes.get(id2);
    const running2 = input2.running.has(id2);
    const reason = why(id2);
    const now = reason !== void 0 ? waiting(reason, revision, outcome?.revision ?? null, running2) : outcome === void 0 ? unevaluated(revision, running2) : replied(outcome, revision, running2);
    const before = input2.before.get(id2);
    if (before !== void 0 && sameStatus(before, now)) {
      statuses.set(id2, before);
    } else {
      statuses.set(id2, Object.freeze(now));
      changed.push(id2);
    }
  }
  for (const id2 of input2.before.keys()) {
    if (!statuses.has(id2)) changed.push(id2);
  }
  return { statuses, changed };
  function why(id2) {
    const inputs = input2.inputs.get(id2);
    const unlinked = inputs?.unlinked ?? [];
    if (unlinked.length > 0) return waitingFor({ unlinked });
    const reads = (inputs?.recipe ?? []).flatMap((read) => read.block ?? []);
    const blocked = [...new Set(reads)].filter((up) => {
      const status = statuses.get(up)?.status;
      return status === "failed" || status === "invalid" || status === "waiting";
    });
    return blocked.length > 0 ? waitingFor({ upstream: blocked }) : void 0;
  }
}
function waitingError(id2, status) {
  const why = status.conditions[0]?.message ?? "This block is waiting.";
  return new Error(
    `${why.replace(/^This block/, `Block "${id2}"`)} Its evaluation with parameters is not sent.`
  );
}

// srcts/scheduler/evaluations.ts
function oneTimeOf(blocks, params, priority, statuses) {
  if (params === void 0) {
    const onBoard = new Set(blocks.filter((id2) => statuses.has(id2)));
    if (onBoard.size === 0) {
      return new Error(`The board holds none of the blocks ${quoted(blocks)} to evaluate.`);
    }
    return { blocks: onBoard, priority };
  }
  if (blocks.length === 0) return new Error("An evaluation with parameters names no block.");
  const absent = blocks.filter((id2) => !statuses.has(id2));
  if (absent.length > 0) {
    const named2 = absent.length === 1 ? "block" : "blocks";
    return new Error(`The board holds no ${named2} ${quoted(absent)} to evaluate.`);
  }
  const parts = /* @__PURE__ */ new Map();
  for (const id2 of blocks) {
    const status = statuses.get(id2);
    if (status?.status === "waiting") return waitingError(id2, status);
    parts.set(id2, { recipe: void 0, sent: void 0, result: void 0 });
  }
  return { priority, params, parts };
}
function settle(oneTimes, statuses) {
  const settled = [];
  for (const [id2, oneTime] of oneTimes) {
    if ("params" in oneTime) continue;
    const blocks = [...oneTime.blocks];
    if (blocks.some((block) => !current(statuses.get(block)))) continue;
    const result = {};
    for (const block of blocks) {
      const status = statuses.get(block);
      if (status !== void 0) result[block] = status;
    }
    settled.push([id2, { statuses: result }]);
  }
  return { oneTimes: without2(oneTimes, settled.map(([id2]) => id2)), settled };
}
function left(oneTimes, gone) {
  const kept = /* @__PURE__ */ new Map();
  const settled = [];
  for (const [id2, oneTime] of oneTimes) {
    if (!("params" in oneTime)) {
      const blocks = new Set([...oneTime.blocks].filter((block) => !gone.has(block)));
      if (blocks.size === 0) {
        settled.push([id2, { error: new Error("Every block of the one-time evaluation left the board.") }]);
      } else {
        kept.set(id2, blocks.size === oneTime.blocks.size ? oneTime : { ...oneTime, blocks });
      }
      continue;
    }
    const early = [...oneTime.parts].find(
      ([block, part]) => gone.has(block) && part.sent === void 0 && part.result === void 0
    );
    if (early === void 0) {
      kept.set(id2, oneTime);
    } else {
      const error = new Error(
        `Block "${early[0]}" left the board before its evaluation with parameters was sent.`
      );
      settled.push([id2, { error }]);
    }
  }
  return { oneTimes: kept, settled };
}
function sent(oneTimes, id2, block, evaluation) {
  return withPart(oneTimes, id2, block, { recipe: evaluation.recipe, sent: evaluation.id });
}
function answered(oneTimes, id2, block, reply, outcome, revision) {
  const oneTime = oneTimes.get(id2);
  if (oneTime === void 0 || !("params" in oneTime)) return { oneTimes, settled: [] };
  const part = oneTime.parts.get(block);
  if (part === void 0) return { oneTimes, settled: [] };
  if (outcome !== void 0) {
    const result = Object.freeze(replied(outcome, revision, false));
    const parts = new Map(oneTime.parts).set(block, { ...part, result });
    const results = [...parts].flatMap(
      ([at, { result: status }]) => status === void 0 ? [] : [[at, status]]
    );
    if (results.length < parts.size) {
      return { oneTimes: new Map(oneTimes).set(id2, { ...oneTime, parts }), settled: [] };
    }
    return {
      oneTimes: without2(oneTimes, [id2]),
      settled: [[id2, { statuses: Object.fromEntries(results) }]]
    };
  }
  if (reply.status === "missing") {
    return { oneTimes: withPart(oneTimes, id2, block, { recipe: void 0, sent: void 0 }), settled: [] };
  }
  const error = new Error(
    `R dropped the evaluation of block "${block}" with parameters, as it does for a block that leaves the board.`
  );
  return { oneTimes: without2(oneTimes, [id2]), settled: [[id2, { error }]] };
}
function neededBy(oneTimes, graph) {
  const needs = [];
  for (const oneTime of oneTimes.values()) {
    if (!("params" in oneTime)) {
      needs.push({ blocks: oneTime.blocks, priority: oneTime.priority });
      continue;
    }
    for (const [block, part] of oneTime.parts) {
      if (part.result !== void 0) continue;
      const reads = part.recipe === void 0 ? graph.upstream(block) : part.recipe.inputs.flatMap((input2) => input2.block ?? []);
      needs.push({ blocks: reads, priority: oneTime.priority });
    }
  }
  return needs;
}
function pendingOf(oneTimes) {
  const pending = [];
  for (const [id2, oneTime] of oneTimes) {
    if (!("params" in oneTime)) continue;
    for (const [block, part] of oneTime.parts) {
      if (part.sent === void 0 && part.result === void 0) {
        pending.push({ oneTime: id2, block, priority: oneTime.priority });
      }
    }
  }
  return pending;
}
function current(status) {
  return status?.status !== "stale" && status?.status !== "unevaluated";
}
function withPart(oneTimes, id2, block, change) {
  const oneTime = oneTimes.get(id2);
  if (oneTime === void 0 || !("params" in oneTime)) return oneTimes;
  const part = oneTime.parts.get(block);
  if (part === void 0) return oneTimes;
  const parts = new Map(oneTime.parts).set(block, { ...part, ...change });
  return new Map(oneTimes).set(id2, { ...oneTime, parts });
}
function without2(oneTimes, ids2) {
  if (ids2.length === 0) return oneTimes;
  const kept = new Map(oneTimes);
  for (const id2 of ids2) kept.delete(id2);
  return kept;
}

// srcts/scheduler/needs.ts
function needsOf(graph, needs) {
  const onBoard = new Set(graph.order);
  const found = /* @__PURE__ */ new Map();
  const raise = (id2, priority) => {
    if (!onBoard.has(id2)) return;
    const was = found.get(id2);
    if (was === void 0 || priority > was) found.set(id2, priority);
  };
  for (const need of needs) {
    for (const id2 of need.blocks) raise(id2, need.priority);
  }
  for (const id2 of [...graph.order].reverse()) {
    const priority = found.get(id2);
    if (priority === void 0) continue;
    for (const up of graph.upstream(id2)) raise(up, priority);
  }
  return found;
}

// srcts/scheduler/plan.ts
var GIVE_WAY = 5e3;
function planOf(input2) {
  const { graph, revisions, statuses, needs, now } = input2;
  const sent2 = [...input2.sent];
  const asked = [...input2.asked];
  const pending = [...input2.pending];
  const index = new Map(graph.order.map((id2, i2) => [id2, i2]));
  const deadlines = [];
  const cancel = [];
  const cancelled2 = /* @__PURE__ */ new Set();
  const planned = /* @__PURE__ */ new Set();
  const priorityOf = (evaluation) => evaluation.oneTime !== void 0 ? evaluation.oneTime.priority : needs.get(evaluation.block) ?? (evaluation.background ? -1 : void 0);
  const inFlight = (id2, at) => planned.has(id2) || sent2.some(
    (evaluation) => evaluation.block === id2 && evaluation.revision === at && !evaluation.cancelled && !cancelled2.has(evaluation.id)
  );
  const needsEvaluation = (id2) => {
    const status = statuses.get(id2)?.status;
    return status === "stale" || status === "unevaluated";
  };
  const available = (id2) => {
    const at = revisions.get(id2);
    return at !== void 0 && (statuses.get(id2)?.status === "ready" || inFlight(id2, at));
  };
  const waitedSince = (evaluation, priority) => {
    let since;
    for (const other of asked) {
      if (other.id > evaluation.id) since = Math.min(since ?? other.at, other.at);
    }
    for (const other of sent2) {
      if (other.id <= evaluation.id) continue;
      if ((priorityOf(other) ?? -Infinity) > priority) {
        since = Math.min(since ?? other.at, other.at);
      }
    }
    return since;
  };
  const paramsFor = (id2, at) => pending.some((item) => item.block === id2) || sent2.some(
    (evaluation) => evaluation.oneTime !== void 0 && evaluation.block === id2 && evaluation.revision === at
  );
  let oldest = Infinity;
  for (const item of [...sent2, ...asked]) oldest = Math.min(oldest, item.id);
  const drop = (evaluation) => {
    cancel.push(evaluation.id);
    cancelled2.add(evaluation.id);
  };
  for (const evaluation of sent2) {
    if (evaluation.cancelled || evaluation.oneTime !== void 0) continue;
    const at = revisions.get(evaluation.block);
    if (at === void 0) continue;
    if (at !== evaluation.revision) {
      drop(evaluation);
      continue;
    }
    const priority = priorityOf(evaluation);
    if (priority === void 0) {
      if (evaluation.id !== oldest) drop(evaluation);
      continue;
    }
    if (!input2.interrupt || priority >= 0) continue;
    const since = waitedSince(evaluation, priority);
    if (since === void 0) continue;
    if (now - since >= GIVE_WAY) drop(evaluation);
    else deadlines.push(since + GIVE_WAY);
  }
  const queued = [];
  for (const [id2, priority] of needs) {
    const at = revisions.get(id2);
    if (at === void 0 || !needsEvaluation(id2)) continue;
    if (inFlight(id2, at) || paramsFor(id2, at)) continue;
    queued.push({ block: id2, priority });
  }
  for (const item of pending) {
    queued.push({ block: item.block, priority: item.priority, oneTime: item.oneTime });
  }
  queued.sort(
    (a2, b2) => (a2.priority === b2.priority ? 0 : a2.priority > b2.priority ? -1 : 1) || (index.get(a2.block) ?? 0) - (index.get(b2.block) ?? 0)
  );
  let slots = input2.queue + 1 - sent2.length;
  const send = [];
  const reject = [];
  const rejected = /* @__PURE__ */ new Set();
  if (!input2.holding) {
    for (const item of queued) {
      if (item.oneTime !== void 0) {
        if (rejected.has(item.oneTime)) continue;
        const status = statuses.get(item.block);
        if (status?.status === "waiting") {
          reject.push({ oneTime: item.oneTime, error: waitingError(item.block, status) });
          rejected.add(item.oneTime);
          continue;
        }
      }
      if (slots <= 0 || !graph.upstream(item.block).every(available)) continue;
      send.push(
        item.oneTime === void 0 ? { block: item.block } : { block: item.block, oneTime: item.oneTime }
      );
      planned.add(item.block);
      slots -= 1;
    }
  }
  const busy = asked.length > 0 || sent2.some((evaluation) => (priorityOf(evaluation) ?? 0) >= 0) || queued.some((item) => item.priority >= 0);
  let idleSince = input2.idleSince;
  if (busy || input2.holding || !Number.isFinite(input2.delay)) {
    idleSince = void 0;
  } else {
    idleSince ??= now;
    const due = idleSince + input2.delay * 1e3;
    if (now < due) {
      deadlines.push(due);
    } else if (slots > 0 && !sent2.some((evaluation) => evaluation.background)) {
      const block = inBackground();
      if (block !== void 0) send.push({ block, background: true });
    }
  }
  return {
    cancel,
    send,
    reject,
    idleSince,
    wakeAt: deadlines.length > 0 ? Math.min(...deadlines) : void 0
  };
  function inBackground() {
    for (const id2 of graph.order) {
      if (needs.has(id2) || !needsEvaluation(id2)) continue;
      const at = revisions.get(id2);
      if (at === void 0 || inFlight(id2, at)) continue;
      if (graph.upstream(id2).every((up) => statuses.get(up)?.status === "ready")) return id2;
    }
    return void 0;
  }
}

// srcts/scheduler/recipes.ts
function inputsOf(board, graph, registry, bound) {
  const inputs = /* @__PURE__ */ new Map();
  for (const [id2, block] of Object.entries(board.blocks)) {
    const declared = own(registry, block.registry_id)?.inputs ?? [];
    const links = graph.linksInto(id2).flatMap((link) => own(board.links, link) ?? []);
    inputs.set(
      id2,
      declared.includes("...") ? variadic(declared, links, bound) : fixed(declared, links, bound)
    );
  }
  return inputs;
}
function fixed(declared, links, bound) {
  const recipe = [];
  const unlinked = [];
  for (const name3 of declared) {
    const link = links.find((candidate) => candidate.input === name3);
    if (link !== void 0) recipe.push({ input: name3, block: link.from });
    else if (bound.has(name3)) recipe.push({ input: name3 });
    else unlinked.push(name3);
  }
  return { recipe, unlinked };
}
function variadic(declared, links, bound) {
  const recipe = links.map((link) => ({ input: link.input, block: link.from }));
  for (const name3 of bound) {
    if (!links.some((link) => link.input === name3)) recipe.push({ input: name3 });
  }
  const unlinked = recipe.length === 0 ? ["..."] : [];
  for (const name3 of declared) {
    if (name3 === "...") continue;
    if (!links.some((link) => link.input === name3) && !bound.has(name3)) unlinked.push(name3);
  }
  return { recipe, unlinked };
}
function sameInputs(a2, b2) {
  return a2.length === b2.length && a2.every((input2, i2) => input2.input === b2[i2]?.input && input2.block === b2[i2].block);
}

// srcts/scheduler/revisions.ts
function counter(last = 0) {
  return () => {
    if (last >= Number.MAX_SAFE_INTEGER) {
      throw new Error(
        "The page has used up its revisions, 2^53 - 1 of them, so the scheduler has stopped. Reload the page to start again."
      );
    }
    last += 1;
    return last;
  };
}
function opening(graph, next) {
  return new Map(graph.order.map((id2) => [id2, next()]));
}
function revise(revisions, before, after, removed, next) {
  const gone = [...revisions.keys()].filter(
    (id2) => !Object.hasOwn(after.board.blocks, id2) || removed.has(id2)
  );
  const left2 = new Set(gone);
  const moved = /* @__PURE__ */ new Set();
  for (const id2 of after.graph.order) {
    const was = left2.has(id2) ? void 0 : own(before.board.blocks, id2);
    const now = own(after.board.blocks, id2);
    if (was === void 0 || now === void 0) {
      moved.add(id2);
      continue;
    }
    const same2 = was.registry_id === now.registry_id && (was.state === now.state || jsonEqual(was.state, now.state)) && sameInputs(before.inputs.get(id2)?.recipe ?? [], after.inputs.get(id2)?.recipe ?? []);
    if (!same2) moved.add(id2);
  }
  const kept = new Map([...revisions].filter(([id2]) => !left2.has(id2)));
  return { revisions: bump(after.graph, kept, moved, next), gone };
}
function bump(graph, revisions, roots, next) {
  const hit = graph.withDownstream(roots);
  const result = /* @__PURE__ */ new Map();
  for (const id2 of graph.order) {
    const at = revisions.get(id2);
    result.set(id2, at === void 0 || hit.has(id2) ? next() : at);
  }
  return result;
}

// srcts/scheduler/scheduler.ts
var NO_BOARD = { blocks: {}, links: {} };
function createScheduler(options) {
  const { store, client } = options;
  const queue = setting(options.evaluationQueue, 2, "evaluation_queue");
  const delay = setting(options.backgroundEvaluationDelay, Infinity, "background_evaluation_delay");
  const board = y(void 0);
  const registry = y({});
  const bound = y(new Set(options.bound ?? []));
  const revisions = y(/* @__PURE__ */ new Map());
  const outcomes = y(/* @__PURE__ */ new Map());
  const sent2 = y(/* @__PURE__ */ new Map());
  const asked = y(/* @__PURE__ */ new Map());
  const demands = y(/* @__PURE__ */ new Map());
  const oneTimes = y(/* @__PURE__ */ new Map());
  const holding = y(true);
  const graph = O(() => graphOf(board.value ?? NO_BOARD));
  const inputs = O(
    () => inputsOf(board.value ?? NO_BOARD, graph.value, registry.value, bound.value)
  );
  let last = /* @__PURE__ */ new Map();
  const statuses = O(() => {
    last = statusesOf({
      graph: graph.value,
      revisions: revisions.value,
      outcomes: outcomes.value,
      inputs: inputs.value,
      running: running(sent2.value),
      before: last
    }).statuses;
    return last;
  });
  const needs = O(
    () => needsOf(graph.value, [
      ...[...demands.value.values()].map((demand) => ({
        blocks: demand.blocks.value,
        priority: demand.priority
      })),
      ...neededBy(oneTimes.value, graph.value)
    ])
  );
  const state2 = O(() => [
    board.value,
    registry.value,
    bound.value,
    revisions.value,
    outcomes.value,
    sent2.value,
    asked.value,
    demands.value,
    oneTimes.value,
    holding.value
  ]);
  const controllers = /* @__PURE__ */ new Map();
  const settles = /* @__PURE__ */ new Map();
  const subscribers = /* @__PURE__ */ new Set();
  let ids2 = 0;
  const count = options.nextRevision ?? counter();
  let stopped;
  const ended = () => options.signal?.aborted === true;
  const endedError = () => new Error("Another board opened.");
  let idleSince;
  let planned;
  let passTimer;
  let wakeTimer;
  function revision() {
    try {
      return count();
    } catch (error) {
      stopped = error;
      throw error;
    }
  }
  const opened = E((next) => {
    registry.value = store.blockRegistry();
    board.value = next;
    revisions.value = opening(graph.value, revision);
  });
  const applied = E((delta, next) => {
    const before = { board: board.value ?? next, inputs: inputs.value };
    board.value = next;
    const revised = revise(
      revisions.value,
      before,
      { board: next, inputs: inputs.value, graph: graph.value },
      new Set(delta.blocks?.rm ?? []),
      revision
    );
    revisions.value = revised.revisions;
    if (revised.gone.length === 0) return;
    const gone = new Set(revised.gone);
    outcomes.value = new Map([...outcomes.value].filter(([id2]) => !gone.has(id2)));
    update(left(oneTimes.value, gone));
    client.forget(revised.gone);
  });
  const lacking = E((missing) => {
    const lost = /* @__PURE__ */ new Set();
    for (const { block, revision: at } of missing) {
      if (revisions.value.get(block) === at && statuses.value.get(block)?.status === "ready") {
        lost.add(block);
      }
    }
    if (lost.size > 0) revisions.value = bump(graph.value, revisions.value, lost, revision);
  });
  function update(next) {
    oneTimes.value = next.oneTimes;
    for (const [id2, result] of next.settled) {
      const settle2 = settles.get(id2);
      settles.delete(id2);
      settle2?.(result);
    }
  }
  const end = E((id2, result) => {
    if (!oneTimes.value.has(id2)) return;
    const rest = new Map(oneTimes.value);
    rest.delete(id2);
    update({ oneTimes: rest, settled: [[id2, result]] });
  });
  function recipeOf(id2) {
    const block = board.value === void 0 ? void 0 : own(board.value.blocks, id2);
    const recipe = inputs.value.get(id2)?.recipe;
    if (block === void 0 || recipe === void 0) return void 0;
    return {
      registry_id: block.registry_id,
      state: { ...block.state },
      inputs: recipe.map(
        (input2) => input2.block === void 0 ? { input: input2.input } : { input: input2.input, block: input2.block, revision: revisions.value.get(input2.block) ?? 0 }
      )
    };
  }
  function send(item) {
    const recipe = recipeOf(item.block);
    const at = revisions.value.get(item.block);
    if (recipe === void 0 || at === void 0) return;
    let oneTime;
    if (item.oneTime !== void 0) {
      const found = oneTimes.value.get(item.oneTime);
      if (found === void 0 || !("params" in found)) return;
      oneTime = { id: item.oneTime, priority: found.priority, params: found.params };
    }
    const id2 = ++ids2;
    sent2.value = new Map(sent2.value).set(id2, {
      id: id2,
      at: Date.now(),
      block: item.block,
      revision: at,
      oneTime: oneTime === void 0 ? void 0 : { id: oneTime.id, priority: oneTime.priority },
      background: item.background === true,
      cancelled: false
    });
    if (oneTime !== void 0) {
      oneTimes.value = sent(oneTimes.value, oneTime.id, item.block, { id: id2, recipe });
    }
    const controller = new AbortController();
    controllers.set(id2, controller);
    client.request(
      {
        kind: "evaluate",
        block: item.block,
        revision: at,
        recipe,
        ...oneTime === void 0 ? {} : { params: oneTime.params }
      },
      { signal: controller.signal }
    ).then(
      ({ reply }) => {
        answered2(id2, reply);
      },
      (error) => {
        unanswered(id2, error);
      }
    );
  }
  function cancel(id2) {
    const evaluation = sent2.value.get(id2);
    if (evaluation === void 0) return;
    sent2.value = new Map(sent2.value).set(id2, { ...evaluation, cancelled: true });
    controllers.get(id2)?.abort();
  }
  function done(id2) {
    const evaluation = sent2.value.get(id2);
    if (evaluation === void 0) return void 0;
    const rest = new Map(sent2.value);
    rest.delete(id2);
    sent2.value = rest;
    controllers.delete(id2);
    return evaluation;
  }
  const answered2 = E((id2, reply) => {
    const evaluation = done(id2);
    if (evaluation === void 0 || ended()) return;
    if (reply.status === "missing") lacking(reply.missing);
    const outcome = outcomeOf(reply, evaluation.revision);
    if (outcome !== void 0 && evaluation.revision === revisions.value.get(evaluation.block) && statuses.value.get(evaluation.block)?.status !== "waiting") {
      outcomes.value = new Map(outcomes.value).set(evaluation.block, outcome);
    }
    if (evaluation.oneTime !== void 0) {
      update(
        answered(
          oneTimes.value,
          evaluation.oneTime.id,
          evaluation.block,
          reply,
          outcome,
          evaluation.revision
        )
      );
    }
  });
  const unanswered = E((id2, error) => {
    const evaluation = done(id2);
    if (evaluation === void 0 || ended()) return;
    if (evaluation.oneTime !== void 0) {
      end(evaluation.oneTime.id, { error });
    } else if (!(error instanceof SessionEndedError) && revisions.value.get(evaluation.block) === evaluation.revision) {
      const outcome = unsent(evaluation.revision, messageOf(error));
      outcomes.value = new Map(outcomes.value).set(evaluation.block, outcome);
    }
  });
  function schedule() {
    passTimer ??= setTimeout(pass, 0);
  }
  function wake(at) {
    if (wakeTimer !== void 0) clearTimeout(wakeTimer);
    wakeTimer = void 0;
    if (at === void 0) return;
    wakeTimer = setTimeout(
      () => {
        wakeTimer = void 0;
        pass();
      },
      Math.max(0, at - Date.now())
    );
  }
  function pass() {
    passTimer = void 0;
    r(() => {
      if (stopped === void 0 && board.value !== void 0) {
        const plan = planOf({
          graph: graph.value,
          revisions: revisions.value,
          statuses: statuses.value,
          needs: needs.value,
          sent: sent2.value.values(),
          asked: asked.value.values(),
          pending: pendingOf(oneTimes.value),
          queue,
          interrupt: client.capabilities().has("interrupt"),
          holding: holding.value,
          now: Date.now(),
          idleSince,
          delay
        });
        idleSince = plan.idleSince;
        for (const id2 of plan.cancel) cancel(id2);
        for (const { oneTime, error } of plan.reject) end(oneTime, { error });
        for (const item of plan.send) send(item);
        wake(plan.wakeAt);
      }
      planned = state2.value;
    });
  }
  function resendable(request) {
    if (request.kind === "output" || request.output !== void 0) return true;
    if (request.registry_id === void 0) return false;
    const entry = own(store.blockRegistry(), request.registry_id);
    return entry === void 0 || own(entry.requests, request.name)?.action !== true;
  }
  function demanding(id2, owner, blocks, priority) {
    let demanded;
    if (typeof blocks === "function") {
      demanded = O(() => {
        const of = board.value;
        if (of === void 0) return /* @__PURE__ */ new Set();
        return f(() => {
          try {
            return new Set(blocks(of));
          } catch (error) {
            report(
              new Error(
                `The demand of ${owner} threw, so it demands nothing until it returns: ${messageOf(error)}`,
                { cause: error }
              )
            );
            return /* @__PURE__ */ new Set();
          }
        });
      });
    } else {
      demanded = y(new Set(blocks));
    }
    demands.value = new Map(demands.value).set(id2, { owner, priority, blocks: demanded });
  }
  if (options.gateVisibility === false) {
    demanding(++ids2, "blockr.core", (all) => Object.keys(all.blocks), 0);
  }
  if (store.hasBoard()) opened(store.board());
  let heard = /* @__PURE__ */ new Map();
  const dispose = x(() => {
    const now = statuses.value;
    const settled = settle(oneTimes.value, now);
    if (settled.settled.length > 0) {
      f(() => {
        update(settled);
      });
    }
    const changed = changedSince(heard, now);
    heard = now;
    if (changed.length > 0) {
      f(() => {
        for (const fn of [...subscribers]) {
          try {
            fn(changed);
          } catch (error) {
            report(error);
          }
        }
      });
    }
    if (state2.value !== planned) schedule();
  });
  const offStore = store.subscribe((change, next) => {
    if ("opened" in change || board.peek() === void 0) opened(next);
    else applied(change.delta, next);
  });
  function stop() {
    stopped = endedError();
    if (passTimer !== void 0) clearTimeout(passTimer);
    passTimer = void 0;
    wake(void 0);
    dispose();
    offStore();
    subscribers.clear();
    const error = endedError();
    update({ oneTimes: /* @__PURE__ */ new Map(), settled: [...oneTimes.peek().keys()].map((id2) => [id2, { error }]) });
  }
  if (ended()) stop();
  else options.signal?.addEventListener("abort", stop, { once: true });
  return {
    demand: E((owner, blocks, { priority } = {}) => {
      const id2 = ++ids2;
      demanding(id2, owner, blocks, checked(priority));
      return {
        set: E((next, { priority: given } = {}) => {
          const demand = demands.value.get(id2);
          if (demand === void 0) {
            throw new Error(`The demand of ${owner} was released, so it can no longer be set.`);
          }
          demanding(id2, owner, next, given === void 0 ? demand.priority : checked(given));
        }),
        release: E(() => {
          if (!demands.value.has(id2)) return;
          const rest = new Map(demands.value);
          rest.delete(id2);
          demands.value = rest;
        })
      };
    }),
    evaluate: E((blocks, params, { priority, signal: abort } = {}) => {
      if (ended()) return Promise.reject(endedError());
      const at = priority ?? 0;
      const invalid = priorityError(at);
      if (invalid !== void 0) return Promise.reject(invalid);
      if (abort?.aborted === true) return Promise.reject(asError(abort.reason));
      return new Promise((resolve, reject) => {
        const id2 = ++ids2;
        const onAbort2 = () => {
          end(id2, { error: abort?.reason });
        };
        abort?.addEventListener("abort", onAbort2, { once: true });
        settles.set(id2, (result) => {
          abort?.removeEventListener("abort", onAbort2);
          if ("error" in result) reject(asError(result.error));
          else resolve(result.statuses);
        });
        const oneTime = oneTimeOf([...new Set(blocks)], params, at, statuses.value);
        if (oneTime instanceof Error) {
          update({ oneTimes: oneTimes.value, settled: [[id2, { error: oneTime }]] });
        } else {
          oneTimes.value = new Map(oneTimes.value).set(id2, oneTime);
        }
      });
    }),
    invalidate: E((id2) => {
      if (!revisions.value.has(id2)) return;
      revisions.value = bump(graph.value, revisions.value, /* @__PURE__ */ new Set([id2]), revision);
    }),
    status: (id2) => statuses.peek().get(id2),
    subscribe(fn) {
      const wrapped = (changed) => {
        fn(changed);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    async request(request, requestOptions = {}) {
      for (; ; ) {
        if (ended()) throw endedError();
        const id2 = ++ids2;
        asked.value = new Map(asked.peek()).set(id2, { id: id2, at: Date.now() });
        try {
          const result = await client.request(request, requestOptions);
          if (result.reply.status === "missing") lacking(result.reply.missing);
          return result;
        } catch (error) {
          if (error instanceof SessionEndedError && resendable(request) && !ended()) continue;
          throw error;
        } finally {
          const rest = new Map(asked.peek());
          rest.delete(id2);
          asked.value = rest;
        }
      }
    },
    inputs: (id2) => inputs.peek().get(id2),
    start: E(() => {
      holding.value = false;
    }),
    hold: E(() => {
      holding.value = true;
    }),
    session: E(() => {
      revisions.value = bump(graph.value, revisions.value, new Set(revisions.value.keys()), revision);
    })
  };
}
function running(sent2) {
  return new Set([...sent2.values()].map((evaluation) => evaluation.block));
}
function changedSince(before, now) {
  const changed = [];
  for (const [id2, status] of now) {
    if (before.get(id2) !== status) changed.push(id2);
  }
  for (const id2 of before.keys()) {
    if (!now.has(id2)) changed.push(id2);
  }
  return changed;
}
function setting(value2, fallback, name3) {
  if (value2 === void 0) return fallback;
  if (typeof value2 === "number" && value2 >= 0) return value2;
  report(
    new Error(
      `The blockr option ${name3} is ${String(value2)}, where it takes a number of 0 or more, so the scheduler takes ${String(fallback)}.`
    )
  );
  return fallback;
}
function checked(priority) {
  const at = priority ?? 0;
  const invalid = priorityError(at);
  if (invalid !== void 0) throw invalid;
  return at;
}
function priorityError(priority) {
  return typeof priority !== "number" || Number.isNaN(priority) ? new TypeError(`An evaluation priority is a number, not ${String(priority)}.`) : void 0;
}

// srcts/mount/answers.ts
function createAnswers(scheduler, signal) {
  const kept = /* @__PURE__ */ new Map();
  const flights = /* @__PURE__ */ new Map();
  signal?.addEventListener(
    "abort",
    () => {
      kept.clear();
      const given = [...flights.values()];
      flights.clear();
      for (const flight of given) flight.controller.abort();
    },
    { once: true }
  );
  scheduler.subscribe(() => {
    sweep();
  });
  function current2(request) {
    return readsOf(request).every(
      (read) => read.block === void 0 || scheduler.status(read.block)?.revision === read.revision
    );
  }
  function sweep() {
    for (const [key, { request }] of kept) {
      if (!current2(request)) kept.delete(key);
    }
  }
  function start2(ask, key) {
    const controller = new AbortController();
    const progress = /* @__PURE__ */ new Set();
    const done = scheduler.request(ask.request, {
      ...ask.body === void 0 ? {} : { body: ask.body },
      signal: controller.signal,
      progress(note) {
        for (const caller of [...progress]) {
          try {
            caller.fn(note);
          } catch (error) {
            report(error);
          }
        }
      }
    }).then((answer) => {
      if (key !== void 0 && ask.share === "keep" && answer.reply.status === "ok" && !("volatile" in answer.reply && answer.reply.volatile === true) && current2(ask.request)) {
        kept.set(key, { answer, request: ask.request });
      }
      return answer;
    }).finally(() => {
      if (key !== void 0 && flights.get(key) === flight) flights.delete(key);
    });
    done.catch(() => void 0);
    const flight = { done, controller, progress, callers: 0 };
    if (key !== void 0) flights.set(key, flight);
    return flight;
  }
  function join(flight, key, signal2, progress) {
    flight.callers += 1;
    const listener = progress === void 0 ? void 0 : { fn: progress };
    if (listener !== void 0) flight.progress.add(listener);
    return new Promise((resolve, reject) => {
      let left2 = false;
      const leave2 = () => {
        left2 = true;
        flight.callers -= 1;
        if (listener !== void 0) flight.progress.delete(listener);
        signal2?.removeEventListener("abort", abort);
      };
      const abort = () => {
        leave2();
        reject(asError(signal2?.reason));
        if (flight.callers === 0) {
          if (key !== void 0 && flights.get(key) === flight) flights.delete(key);
          flight.controller.abort();
        }
      };
      signal2?.addEventListener("abort", abort, { once: true });
      flight.done.then(
        (answer) => {
          if (left2) return;
          leave2();
          resolve(answer);
        },
        (error) => {
          if (left2) return;
          leave2();
          reject(asError(error));
        }
      );
    });
  }
  return {
    ask(ask, { signal: signal2, progress } = {}) {
      if (signal2?.aborted === true) return Promise.reject(asError(signal2.reason));
      const key = ask.share === "once" ? void 0 : keyOf(ask.request);
      const answer = key === void 0 ? void 0 : kept.get(key)?.answer;
      if (answer !== void 0) return Promise.resolve(answer);
      const flight = (key === void 0 ? void 0 : flights.get(key)) ?? start2(ask, key);
      return join(flight, key, signal2, progress);
    },
    size: () => kept.size
  };
}
function keyOf(request) {
  if (request.type === "block_class") {
    return JSON.stringify(
      sorted([
        request.type,
        request.registry_id,
        request.name,
        request.args,
        request.options ?? {},
        request.reads
      ])
    );
  }
  return JSON.stringify(
    sorted([
      request.type,
      request.type === "renderer" ? request.renderer : null,
      request.output,
      request.name,
      request.args,
      request.options,
      request.params
    ])
  );
}
function readsOf(request) {
  return request.type === "block_class" ? request.reads : [request.output];
}
function sorted(x2) {
  if (Array.isArray(x2)) return x2.map(sorted);
  if (isObject(x2)) {
    const object2 = x2;
    return Object.fromEntries(
      Object.keys(object2).sort().map((name3) => [name3, sorted(object2[name3] ?? null)])
    );
  }
  return x2;
}

// srcts/mount/load.ts
var API = 1;
var ModuleError = class extends Error {
  module;
  package;
  constructor(module, pkg, what, options) {
    const name3 = module.replace(/[?#].*$/, "");
    super(`The module ${name3} ${pkg === null ? "from no package" : `of ${pkg}`} ${what}`, options);
    this.name = "ModuleError";
    this.module = name3;
    this.package = pkg;
  }
};
function createLoader(options = {}) {
  const importer = options.importer ?? importModule;
  const stylesheet = options.stylesheet ?? linkStylesheet;
  const modules = options.modules ?? (() => ({}));
  const imported = /* @__PURE__ */ new Map();
  const sheets = /* @__PURE__ */ new Map();
  function once2(cache, url, load2) {
    let loading = cache.get(url);
    if (loading === void 0) {
      loading = load2(url);
      cache.set(url, loading);
    }
    return loading;
  }
  async function load(source, pkg) {
    const sheet = source.css === void 0 ? Promise.resolve() : once2(sheets, source.css, stylesheet).catch((error) => {
      report(new ModuleError(source.module, pkg, `has a stylesheet that failed to load: ${messageOf(error)}`, { cause: error }));
    });
    const exports = once2(imported, source.module, importer);
    await sheet;
    try {
      return await exports;
    } catch (error) {
      throw new ModuleError(source.module, pkg, `failed to load: ${messageOf(error)}`, { cause: error });
    }
  }
  return {
    load,
    module(id2) {
      const source = own(modules(), id2);
      if (source === void 0) {
        return Promise.reject(new Error(`No package offers a module "${id2}".`));
      }
      return load(source, packageOf(id2));
    }
  };
}
function importModule(url) {
  return import(
    /* @vite-ignore */
    new URL(url, document.baseURI).href
  );
}
function linkStylesheet(url) {
  return new Promise((resolve, reject) => {
    const link = document.createElement("link");
    link.rel = "stylesheet";
    link.href = url;
    link.addEventListener("load", () => {
      resolve();
    }, { once: true });
    link.addEventListener("error", () => {
      reject(new Error(`${url} could not be loaded.`));
    }, { once: true });
    document.head.append(link);
  });
}

// srcts/mount/measure.ts
var FALLBACK = { bg: "#ffffff", fg: "#000000", accent: "#155dfc" };
var ACCENT = "--bc-color-bg-accent";
function domMeasure() {
  return {
    info(el) {
      const rect = el.getBoundingClientRect();
      const width = Math.round(rect.width);
      const height = Math.round(rect.height);
      if (width <= 0 || height <= 0) return null;
      const style = getComputedStyle(el);
      return {
        width,
        height,
        pixel_ratio: window.devicePixelRatio > 0 ? window.devicePixelRatio : 1,
        bg: backgroundOf(el) ?? FALLBACK.bg,
        fg: hexOf(style.color) ?? FALLBACK.fg,
        accent: hexOf(style.getPropertyValue(ACCENT).trim()) ?? FALLBACK.accent,
        font: style.fontFamily
      };
    },
    watch(el, fn) {
      const stops = [];
      if (typeof ResizeObserver === "function") {
        const resized = new ResizeObserver(() => {
          fn();
        });
        resized.observe(el);
        stops.push(() => {
          resized.disconnect();
        });
      }
      const themed = new MutationObserver(() => {
        fn();
      });
      themed.observe(document.documentElement, {
        attributes: true,
        attributeFilter: ["data-bs-theme", "class", "style"]
      });
      stops.push(() => {
        themed.disconnect();
      });
      stops.push(watchRatio(fn));
      return () => {
        for (const stop of stops) stop();
      };
    }
  };
}
function watchRatio(fn) {
  if (typeof matchMedia !== "function") return () => void 0;
  let query;
  const changed = () => {
    listen();
    fn();
  };
  function listen() {
    query?.removeEventListener("change", changed);
    query = matchMedia(`(resolution: ${String(window.devicePixelRatio)}dppx)`);
    query.addEventListener("change", changed);
  }
  listen();
  return () => {
    query?.removeEventListener("change", changed);
  };
}
function backgroundOf(el) {
  for (let at = el; at !== null; at = at.parentElement) {
    const colour = getComputedStyle(at).backgroundColor;
    if (!transparent(colour)) return hexOf(colour);
  }
  return null;
}
function transparent(colour) {
  return colour === "" || colour === "transparent" || /^rgba\(.*,\s*0\)$/.test(colour);
}
function hexOf(colour) {
  const text = colour.trim();
  if (/^#[0-9a-f]{6}$/i.test(text)) return text.toLowerCase();
  if (/^#[0-9a-f]{3}$/i.test(text)) {
    const [r2, g2, b2] = [text.charAt(1), text.charAt(2), text.charAt(3)];
    return `#${r2}${r2}${g2}${g2}${b2}${b2}`.toLowerCase();
  }
  const rgb = /^rgba?\(\s*(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)[\s,]+(\d+(?:\.\d+)?)/.exec(text);
  if (rgb !== null) return hex(rgb.slice(1, 4).map(Number));
  return text === "" ? null : drawn(text);
}
function hex(channels) {
  return `#${channels.map((c2) => Math.round(Math.min(255, Math.max(0, c2))).toString(16).padStart(2, "0")).join("")}`;
}
function drawn(colour) {
  try {
    const context = document.createElement("canvas").getContext("2d", { willReadFrequently: true });
    if (context === null) return null;
    context.fillStyle = "#000000";
    context.fillStyle = colour;
    context.fillRect(0, 0, 1, 1);
    const [r2 = 0, g2 = 0, b2 = 0] = context.getImageData(0, 0, 1, 1).data;
    return hex([r2, g2, b2]);
  } catch {
    return null;
  }
}

// srcts/mount/requests.ts
var ATTEMPTS = 3;
var RequestError = class extends Error {
  status;
  conditions;
  inputs;
  blocks;
  constructor(message, fields = {}) {
    const conditions = fields.conditions ?? [];
    super([message, ...conditions.map((condition) => `- ${condition.message}`)].join("\n"));
    this.name = "RequestError";
    this.status = fields.status ?? null;
    this.conditions = conditions;
    this.inputs = fields.inputs ?? [];
    this.blocks = fields.blocks ?? [];
  }
};
async function blockRequest(deps, block, entry, name3, given, opts) {
  const spec = own(entry.requests, name3);
  if (spec === void 0) {
    throw new Error(`The block class "${entry.id}" has no request "${name3}".`);
  }
  if (!isObject(given)) {
    throw new TypeError(`The args of the request "${name3}" are an object of state args and parameters.`);
  }
  const share = spec.action === true || opts.file !== void 0 ? "once" : spec.volatile === true ? "merge" : "keep";
  const what = `The request "${name3}" of block "${block}"`;
  for (let attempt3 = 1; ; attempt3++) {
    const reads = await readsOf2(deps, block, entry, name3, spec.inputs, opts.signal);
    if (!deps.bound()) throw deps.unboundError();
    const board = deps.store.board();
    const state2 = own(board.blocks, block)?.state;
    if (state2 === void 0) throw new Error(`Block "${block}" left the board, so its request "${name3}" did not go to R.`);
    const args = {};
    for (const arg of spec.state_args) {
      const value2 = own(state2, arg);
      if (value2 !== void 0 && !Object.hasOwn(given, arg)) args[arg] = value2;
    }
    for (const [arg, value2] of Object.entries(given)) {
      if (value2 !== void 0) args[arg] = value2;
    }
    const options = {};
    for (const id2 of spec.options) {
      const value2 = own(board.options, id2);
      if (value2 !== void 0) options[id2] = value2;
    }
    const request = {
      kind: "request",
      type: "block_class",
      registry_id: entry.id,
      block,
      name: name3,
      args,
      reads,
      ...spec.options.length === 0 ? {} : { options }
    };
    const { reply, body } = await deps.answers.ask(
      { request, share, ...opts.file === void 0 ? {} : { body: opts.file } },
      {
        ...opts.signal === void 0 ? {} : { signal: opts.signal },
        ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
      }
    );
    if (!deps.bound()) throw deps.unboundError();
    switch (reply.status) {
      case "ok":
        if (body !== null) return body;
        return "value" in reply && reply.value !== void 0 ? reply.value : null;
      case "missing":
        if (attempt3 < ATTEMPTS) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, {
          status: "missing"
        });
      case "cancelled":
        throw new RequestError(`R cancelled the request "${name3}" of block "${block}".`, { status: "cancelled" });
      default:
        throw new RequestError(`R replied ${reply.status} to the request "${name3}" of block "${block}".`, {
          status: reply.status,
          conditions: "conditions" in reply ? reply.conditions : []
        });
    }
  }
}
async function readsOf2(deps, block, entry, name3, names, signal) {
  const what = `The request "${name3}" of block "${block}"`;
  if (names.length === 0) return [];
  const inputs = deps.scheduler.inputs(block);
  if (inputs === void 0) throw new Error(`Block "${block}" left the board, so its request "${name3}" did not go to R.`);
  const unlinked = inputs.unlinked.filter((input2) => names.includes(input2));
  if (unlinked.length > 0) {
    throw new RequestError(`${what} did not go to R, since no link feeds its ${unlinked.length === 1 ? "input" : "inputs"} ${quoted(unlinked)}.`);
  }
  const read = inputs.recipe.filter(
    (input2) => names.includes(input2.input) || names.includes("...") && !entry.inputs.includes(input2.input)
  );
  const upstream = [...new Set(read.flatMap((input2) => input2.block ?? []))];
  if (upstream.length === 0) return read.map((input2) => ({ input: input2.input }));
  const statuses = await deps.scheduler.evaluate(upstream, void 0, signal === void 0 ? {} : { signal });
  const unready = read.flatMap((input2) => {
    if (input2.block === void 0) return [];
    const status = own(statuses, input2.block);
    return status === void 0 || status.status === "ready" ? [] : [{ input: input2.input, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ input: input2, status }) => `"${input2}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since an input it reads is not ready: ${listed.join(", ")}.`, {
      inputs: unready
    });
  }
  return read.map(
    (input2) => input2.block === void 0 ? { input: input2.input } : { input: input2.input, block: input2.block, revision: own(statuses, input2.block)?.revision ?? 0 }
  );
}

// srcts/mount/shared.ts
function moduleOf(exports, module, pkg) {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new ModuleError(module, pkg, "declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new ModuleError(
      module,
      pkg,
      `was built against major ${String(exports.api)} of core's JS API, where core's is ${String(API)}.`
    );
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.render !== "function") {
    throw new ModuleError(module, pkg, "has no default export with a render() function.");
  }
  const named2 = mod.options;
  if (named2 !== void 0 && !(Array.isArray(named2) && named2.every((id2) => typeof id2 === "string"))) {
    throw new ModuleError(module, pkg, "names its board options in something other than an array of their ids.");
  }
  return mod;
}
function showError(el, message) {
  const box = document.createElement("div");
  box.setAttribute("role", "alert");
  box.className = "bc:rounded bc:border bc:border-border-danger bc:p-2 bc:font-sans bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap";
  box.textContent = message;
  el.replaceChildren(box);
}
function attempt(fn) {
  try {
    fn();
  } catch (error) {
    report(error);
  }
}
function onAbort(signal, fn) {
  signal.addEventListener("abort", fn, { once: true });
  return () => {
    signal.removeEventListener("abort", fn);
  };
}
var REPLACED = "Another board opened, so core mounts nothing more of the board before.";
function announceTo(listeners) {
  for (const fn of [...listeners]) attempt(fn);
}

// srcts/mount/option.ts
function mountOption(deps, el, option) {
  const { store, loader } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const def = own(store.options.registered(), option);
  if (def === void 0) {
    throw new Error(`No package registers the board option "${option}" whose UI to mount.`);
  }
  const chosen = Object.entries(deps.renderers()).find(([, renderer]) => renderer.option === option);
  const controller = new AbortController();
  const ended = () => controller.signal.aborted;
  const listeners = /* @__PURE__ */ new Set();
  let value2;
  let fallback;
  let seen = store.board().options;
  let cleanup;
  function bound() {
    return !ended() && !deps.replaced.aborted;
  }
  const unboundError = () => new Error(`Another board opened, so the host of the board option "${option}" no longer changes it.`);
  function current2() {
    const held2 = own(store.board().options, option);
    return held2 === void 0 ? fallback : held2;
  }
  const host = {
    option,
    value: () => value2,
    on(event, fn) {
      const heard = event;
      if (heard !== "value") {
        throw new Error(`The host of a board option announces "value", not ${JSON.stringify(event)}.`);
      }
      const wrapped = () => {
        fn();
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    },
    submit(next) {
      if (!bound()) return Promise.reject(unboundError());
      return deps.commands.run("blockr.core-set_option", { id: option, value: next });
    },
    request(name3, args = {}, opts = {}) {
      if (typeof name3 !== "string" || name3 === "") {
        return Promise.reject(new TypeError("A request of a board option is named by a non-empty string."));
      }
      if (!isObject(args)) return Promise.reject(new TypeError(`The args of the request "${name3}" are an object.`));
      if (!bound()) return Promise.reject(unboundError());
      return optionRequest(deps, option, name3, args, opts);
    }
  };
  const offStore = store.subscribe((_change, board) => {
    if (board.options === seen) return;
    seen = board.options;
    const next = current2();
    if (next === value2) return;
    value2 = next;
    announceTo(listeners);
  });
  const offEnd = onAbort(deps.replaced, end);
  function end() {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    listeners.clear();
    if (cleanup !== void 0) attempt(cleanup);
    el.replaceChildren();
  }
  void (async () => {
    let mod;
    const [id2, renderer] = chosen ?? [];
    try {
      if (renderer === void 0 || id2 === void 0) {
        mod = def.ui;
      } else {
        const pkg = packageOf(id2);
        const source = renderer.css === void 0 ? { module: renderer.module } : { module: renderer.module, css: renderer.css };
        mod = moduleOf(await loader.load(source, pkg), renderer.module, pkg);
      }
      if (ended()) return;
      await deps.addOptions(mod.options ?? [], `the UI of the board option "${option}"`);
      if (ended()) return;
      fallback = Object.hasOwn(store.board().options, option) ? void 0 : await deps.defaultOf(option, def);
    } catch (error) {
      fail(error);
      return;
    }
    if (ended()) return;
    value2 = current2();
    try {
      const rendered = await mod.render({ el, host, signal: controller.signal });
      if (typeof rendered !== "function") return;
      if (ended()) attempt(rendered);
      else cleanup = rendered;
    } catch (error) {
      const module = renderer?.module ?? `the UI of the board option "${option}"`;
      fail(new ModuleError(module, id2 === void 0 ? null : packageOf(id2), `failed in render(): ${messageOf(error)}`, { cause: error }));
    }
  })();
  function fail(error) {
    if (ended()) return;
    showError(el, messageOf(error));
    report(error);
  }
  return end;
}
async function optionRequest(deps, option, name3, args, opts) {
  const { reply, body } = await deps.scheduler.request(
    { kind: "request", type: "option", option, name: name3, args: { ...args } },
    {
      ...opts.file === void 0 ? {} : { body: opts.file },
      ...opts.signal === void 0 ? {} : { signal: opts.signal },
      ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
    }
  );
  switch (reply.status) {
    case "ok":
      if (body !== null) return body;
      return "value" in reply && reply.value !== void 0 ? reply.value : null;
    case "cancelled":
      throw new RequestError(`R cancelled the request "${name3}" of the board option "${option}".`, { status: "cancelled" });
    default:
      throw new RequestError(`R replied ${reply.status} to the request "${name3}" of the board option "${option}".`, {
        status: reply.status,
        conditions: "conditions" in reply ? reply.conditions : []
      });
  }
}

// srcts/mount/output.ts
var SETTLE_MS = 150;
var ATTEMPTS2 = 3;
var EMPTY = Object.freeze({});
function createShelves() {
  const shelves = /* @__PURE__ */ new Map();
  const giveUp = (shelf) => {
    for (const flight of shelf.flights.values()) flight.controller.abort();
    shelf.flights.clear();
  };
  return {
    of(block) {
      let shelf = shelves.get(block);
      if (shelf === void 0) {
        shelf = { params: {}, last: void 0, mounts: /* @__PURE__ */ new Set(), flights: /* @__PURE__ */ new Map() };
        shelves.set(block, shelf);
      }
      return shelf;
    },
    drop(block) {
      const shelf = shelves.get(block);
      if (shelf === void 0) return;
      giveUp(shelf);
      shelves.delete(block);
    }
  };
}
function mountOutput(deps, el, block) {
  const { store, scheduler, loader, measure } = deps;
  if (deps.replaced.aborted) throw new Error(REPLACED);
  const found = own(store.board().blocks, block);
  if (found === void 0) {
    throw new Error(`The board holds no block "${block}" whose output to mount.`);
  }
  const entry = own(store.blockRegistry(), found.registry_id);
  if (entry === void 0) {
    throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
  }
  const shelf = deps.shelves.of(block);
  const controller = new AbortController();
  const ended = () => controller.signal.aborted;
  let renderer = entry.renderer;
  let ready;
  const preparing = /* @__PURE__ */ new Map();
  let info = measure.info(el);
  let held2;
  let asking;
  let error = null;
  let drawing;
  let options = store.board().options;
  const waiting2 = /* @__PURE__ */ new Set();
  const broken = /* @__PURE__ */ new WeakSet();
  function bound() {
    return !ended() && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }
  const unboundError = () => new Error(
    deps.replaced.aborted ? `Another board opened, so the host of the output of block "${block}" no longer asks R.` : `Block "${block}" left the board, so the host of its output no longer asks R.`
  );
  function prepare(given) {
    let loading = preparing.get(given.id);
    if (loading === void 0) {
      loading = (async () => {
        const pkg = packageOf(given.id);
        const source = given.css === void 0 ? { module: given.module } : { module: given.module, css: given.css };
        const mod = moduleOf(await loader.load(source, pkg), given.module, pkg);
        const named3 = mod.options ?? [];
        await deps.addOptions(named3, `the renderer "${given.id}"`);
        return { renderer: given, mod, named: named3 };
      })();
      preparing.set(given.id, loading);
    }
    return loading;
  }
  function expect(given) {
    renderer = given;
    if (ready?.renderer.id === given.id) return;
    prepare(given).then(
      (prepared) => {
        if (ended() || renderer?.id !== prepared.renderer.id) return;
        ready = prepared;
        sync();
      },
      (failure) => {
        fail(failure);
      }
    );
  }
  function named2() {
    if (renderer === void 0) return null;
    return ready?.renderer.id === renderer.id ? ready.named : void 0;
  }
  function values2(ids2) {
    const options2 = store.board().options;
    if (ids2 === null) return options2;
    return Object.fromEntries(
      ids2.flatMap((id2) => {
        const value2 = own(options2, id2);
        return value2 === void 0 ? [] : [[id2, value2]];
      })
    );
  }
  function wanted() {
    if (info === null) return void 0;
    const ids2 = named2();
    if (ids2 === void 0) return void 0;
    const status = scheduler.status(block);
    if (status?.status !== "ready") return void 0;
    return { asked: { revision: status.revision, options: values2(ids2), info, params: shelf.params }, ids: ids2 };
  }
  function fits2(asked, want, ids2) {
    if (asked.revision !== want.revision || !jsonEqual(asked.info, want.info)) return false;
    if (!jsonEqual(asked.params, want.params)) return false;
    if (ids2 === null) return jsonEqual(asked.options, want.options);
    return ids2.every((id2) => jsonEqual(own(asked.options, id2), own(want.options, id2)));
  }
  function sync() {
    if (ended()) return;
    const want = wanted();
    if (want !== void 0) {
      if (held2 !== void 0 && fits2(held2.asked, want.asked, want.ids)) {
        stopAsking();
      } else if (shelf.last !== void 0 && shelf.last !== held2 && fits2(shelf.last.asked, want.asked, want.ids)) {
        stopAsking();
        take(shelf.last);
      } else {
        const key = keyOf2(want.asked);
        if (asking?.key !== key) {
          stopAsking();
          asking = join(key, want.asked);
          announce2();
        }
      }
    }
    wake();
  }
  function stopAsking() {
    if (asking === void 0) return;
    asking.leave();
    asking = void 0;
    announce2();
  }
  function join(key, asked) {
    let flight = shelf.flights.get(key);
    if (flight === void 0) {
      const aborted = new AbortController();
      const done = scheduler.request(
        { kind: "output", block, revision: asked.revision, options: asked.options, info: asked.info, params: asked.params },
        { signal: aborted.signal }
      ).then((answer) => {
        if (answer.reply.status === "ok" && "output" in answer.reply && !deps.replaced.aborted) {
          shelf.last = { asked, output: answer.reply.output, body: answer.body };
        }
        return answer;
      });
      const started = { done, controller: aborted, callers: 0 };
      void done.catch(() => void 0).finally(() => {
        if (shelf.flights.get(key) === started) shelf.flights.delete(key);
      });
      shelf.flights.set(key, started);
      flight = started;
    }
    const mine = flight;
    mine.callers += 1;
    let left2 = false;
    const ask = {
      key,
      leave() {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (mine.callers === 0) {
          if (shelf.flights.get(key) === mine) shelf.flights.delete(key);
          mine.controller.abort();
        }
      }
    };
    mine.done.then(
      (answer) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        received2(asked, answer);
      },
      (failure) => {
        if (left2) return;
        left2 = true;
        mine.callers -= 1;
        if (asking === ask) asking = void 0;
        fail(failure);
      }
    );
    return ask;
  }
  function received2(asked, { reply, body }) {
    if (ended()) return;
    switch (reply.status) {
      case "ok":
        take({ asked, output: reply.output, body });
        return;
      case "error": {
        const conditions = "conditions" in reply ? reply.conditions : [];
        failed2(
          conditions[0] ?? {
            severity: "error",
            class: ["blockr_error", "error", "condition"],
            phase: "output",
            message: `R failed to make the output of block "${block}".`
          },
          conditions
        );
        return;
      }
      default:
        announce2();
    }
  }
  function failed2(condition, conditions) {
    error = condition;
    if (held2 === void 0) {
      showError(el, conditions.length > 0 ? conditions.map((c2) => c2.message).join("\n") : condition.message);
    }
    announce2();
  }
  function fail(failure) {
    if (ended()) return;
    if (failure instanceof ModuleError) {
      showError(el, failure.message);
      if (!broken.has(failure)) report(failure);
      broken.add(failure);
      return;
    }
    failed2(
      { severity: "error", class: ["blockr_error", "error", "condition"], phase: "output", message: messageOf(failure) },
      []
    );
  }
  function take(next) {
    held2 = next;
    error = null;
    const named3 = next.output.renderer;
    if (renderer?.id !== named3.id) expect(named3);
    if (drawing !== void 0 && drawing.prepared.renderer.id !== named3.id) unmountRenderer();
    if (drawing === void 0) {
      prepare(named3).then(
        (prepared) => {
          if (ended() || drawing !== void 0 || held2?.output.renderer.id !== prepared.renderer.id) return;
          mountRenderer(prepared);
        },
        (failure) => {
          fail(failure);
        }
      );
    }
    announce2();
    wake();
  }
  function mountRenderer(prepared) {
    const signal = new AbortController();
    const current3 = { prepared, host: outputHost(prepared), controller: signal, cleanup: void 0 };
    drawing = current3;
    el.replaceChildren();
    void (async () => {
      try {
        const rendered = await prepared.mod.render({ el, host: current3.host, signal: signal.signal });
        if (typeof rendered !== "function") return;
        if (signal.signal.aborted) attempt(rendered);
        else current3.cleanup = rendered;
      } catch (failure) {
        if (signal.signal.aborted) return;
        const pkg = packageOf(prepared.renderer.id);
        const why = new ModuleError(prepared.renderer.module, pkg, `failed in render(): ${messageOf(failure)}`, {
          cause: failure
        });
        showError(el, why.message);
        report(why);
      }
    })();
  }
  function unmountRenderer() {
    const current3 = drawing;
    if (current3 === void 0) return;
    drawing = void 0;
    current3.host.drop();
    current3.controller.abort();
    if (current3.cleanup !== void 0) attempt(current3.cleanup);
    el.replaceChildren();
  }
  function announce2() {
    drawing?.host.announce();
  }
  function wake() {
    announceTo(waiting2);
  }
  function optionsFor(prepared, asked) {
    const now = store.board().options;
    return Object.fromEntries(
      prepared.named.flatMap((id2) => {
        const value2 = asked !== void 0 && Object.hasOwn(asked.options, id2) ? asked.options[id2] : own(now, id2);
        return value2 === void 0 ? [] : [[id2, value2]];
      })
    );
  }
  function replaceParams(value2) {
    if (!bound()) return;
    const next = frozenCopy(value2);
    if (jsonEqual(next, shelf.params)) return;
    shelf.params = next;
    announceTo(shelf.mounts);
  }
  function current2(name3, signal) {
    return new Promise((resolve, reject) => {
      const settle2 = () => {
        if (!bound()) {
          reject(unboundError());
          return true;
        }
        if (signal?.aborted === true) {
          reject(asError(signal.reason));
          return true;
        }
        const status = scheduler.status(block);
        if (status === void 0) {
          reject(unboundError());
          return true;
        }
        if (status.status === "failed" || status.status === "invalid" || status.status === "waiting") {
          reject(
            new RequestError(
              `The request "${name3}" of the output of block "${block}" did not go to R, since the block reads ${status.status}.`,
              { blocks: [{ block, status }] }
            )
          );
          return true;
        }
        if (status.status === "ready" && held2?.asked.revision === status.revision) {
          resolve(held2);
          return true;
        }
        return false;
      };
      if (settle2()) return;
      const check2 = () => {
        if (settle2()) stop();
      };
      const stop = () => {
        waiting2.delete(check2);
        signal?.removeEventListener("abort", check2);
      };
      waiting2.add(check2);
      signal?.addEventListener("abort", check2, { once: true });
    });
  }
  async function request(prepared, name3, args, opts) {
    const what = `The request "${name3}" of the output of block "${block}"`;
    for (let attempt3 = 1; ; attempt3++) {
      const target2 = await current2(name3, opts.signal);
      const output = { block, revision: target2.asked.revision };
      const fields = { output, name: name3, args: { ...args }, options: target2.asked.options, params: target2.asked.params };
      const asked = opts.scope === "renderer" ? { kind: "request", type: "renderer", renderer: prepared.renderer.id, ...fields } : { kind: "request", type: "output_class", ...fields };
      const { reply, body } = await deps.answers.ask(
        { request: asked, share: opts.file === void 0 ? "keep" : "once", ...opts.file === void 0 ? {} : { body: opts.file } },
        {
          ...opts.signal === void 0 ? {} : { signal: opts.signal },
          ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
        }
      );
      if (!bound()) throw unboundError();
      switch (reply.status) {
        case "ok":
          if (body !== null) return body;
          return "value" in reply && reply.value !== void 0 ? reply.value : null;
        case "missing":
          if (attempt3 < ATTEMPTS2) continue;
          throw new RequestError(`${what} gave up, since R kept lacking the result it reads.`, { status: "missing" });
        case "cancelled":
          throw new RequestError(`R cancelled ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, { status: "cancelled" });
        default:
          throw new RequestError(`R replied ${reply.status} to ${what.charAt(0).toLowerCase()}${what.slice(1)}.`, {
            status: reply.status,
            conditions: "conditions" in reply ? reply.conditions : []
          });
      }
    }
  }
  function checkRequest(name3, args, opts) {
    if (typeof name3 !== "string" || name3 === "") return new TypeError("A request of an output is named by a non-empty string.");
    if (!isObject(args)) return new TypeError(`The args of the request "${name3}" are an object.`);
    if (!isObject(opts)) return new TypeError(`The options of the request "${name3}" are an object.`);
    const scope = opts.scope;
    if (scope !== void 0 && scope !== "class" && scope !== "renderer") {
      return new TypeError(`A request of an output has the scope "class" or "renderer", not ${JSON.stringify(scope)}.`);
    }
    return void 0;
  }
  function outputHost(prepared) {
    const listeners = /* @__PURE__ */ new Set();
    let named3;
    let following = true;
    return {
      block,
      output: () => held2?.output ?? null,
      body: () => held2?.body ?? null,
      params: () => held2?.asked.params ?? EMPTY,
      options() {
        let memo = named3;
        if (memo === void 0 || memo.for !== held2) {
          memo = { for: held2, values: optionsFor(prepared, held2?.asked) };
          named3 = memo;
        }
        return memo.values;
      },
      loading: () => asking !== void 0,
      error: () => error,
      on(event, fn) {
        const heard = event;
        if (heard !== "output") {
          throw new Error(`The host of an output announces "output", not ${JSON.stringify(event)}.`);
        }
        const wrapped = () => {
          fn();
        };
        listeners.add(wrapped);
        return () => {
          listeners.delete(wrapped);
        };
      },
      setParams(params) {
        if (!isObject(params)) throw new TypeError("An output's parameters are an object.");
        if (following) replaceParams(params);
      },
      request(name3, args = {}, opts = {}) {
        const invalid = checkRequest(name3, args, opts);
        if (invalid !== void 0) return Promise.reject(invalid);
        if (!following || !bound()) return Promise.reject(unboundError());
        return request(prepared, name3, args, opts);
      },
      emit(name3, payload) {
        if (following && bound()) deps.emit({ block, name: name3, payload });
      },
      announce() {
        if (following) announceTo(listeners);
      },
      drop() {
        following = false;
        listeners.clear();
      }
    };
  }
  const offStore = store.subscribe((_change, next) => {
    if (!Object.hasOwn(next.blocks, block)) {
      end();
      return;
    }
    if (next.options === options) return;
    options = next.options;
    sync();
  });
  const offScheduler = scheduler.subscribe((changed) => {
    if (changed.includes(block)) sync();
  });
  shelf.mounts.add(sync);
  let settling;
  const stopWatching = measure.watch(el, () => {
    if (settling !== void 0) clearTimeout(settling);
    settling = setTimeout(
      () => {
        settling = void 0;
        const next = measure.info(el);
        if (jsonEqual(next, info)) return;
        info = next;
        sync();
      },
      info === null ? 0 : SETTLE_MS
    );
  });
  const offEnd = onAbort(deps.replaced, end);
  function end() {
    if (ended()) return;
    controller.abort();
    offEnd();
    offStore();
    offScheduler();
    shelf.mounts.delete(sync);
    stopWatching();
    if (settling !== void 0) clearTimeout(settling);
    asking?.leave();
    asking = void 0;
    unmountRenderer();
    el.replaceChildren();
    wake();
  }
  if (renderer !== void 0) expect(renderer);
  if (shelf.last !== void 0) take(shelf.last);
  sync();
  return end;
}
function keyOf2(asked) {
  return JSON.stringify([asked.revision, sorted2(asked.options), sorted2(asked.info), sorted2(asked.params)]);
}
function sorted2(x2) {
  if (Array.isArray(x2)) return x2.map(sorted2);
  if (isObject(x2)) {
    const object2 = x2;
    return Object.fromEntries(
      Object.keys(object2).sort().map((name3) => [name3, sorted2(object2[name3] ?? null)])
    );
  }
  return x2;
}

// srcts/mount/block.ts
var CORE = "blockr.core";
var NO_STATUS = Object.freeze(unevaluated(0, false));
function createMounts(options) {
  const { store, commands, scheduler, loader } = options;
  const replaced = options.signal ?? new AbortController().signal;
  const answers = createAnswers(scheduler, replaced);
  const shelves = createShelves();
  const measure = options.measure ?? domMeasure();
  const emit = options.emit ?? (() => void 0);
  const renderers = options.renderers ?? (() => ({}));
  const defaults = /* @__PURE__ */ new Map();
  store.subscribe((change) => {
    if ("opened" in change) return;
    for (const id2 of change.delta.blocks?.rm ?? []) shelves.drop(id2);
  });
  function defaultOf(id2, def) {
    let value2 = defaults.get(id2);
    if (value2 === void 0) {
      const given = def.default;
      value2 = typeof given === "function" ? Promise.resolve().then(given) : Promise.resolve(given);
      defaults.set(id2, value2);
    }
    return value2;
  }
  async function addOptions(ids2, by) {
    await Promise.all(
      ids2.map(async (id2) => {
        if (Object.hasOwn(store.board().options, id2)) return;
        const def = own(store.options.registered(), id2);
        if (def === void 0) {
          report(new Error(`The board option "${id2}", which ${by} requires, is defined by no package loaded.`));
          return;
        }
        let value2;
        try {
          value2 = await defaultOf(id2, def);
        } catch (error) {
          report(new Error(`The default of the board option "${id2}" failed: ${messageOf(error)}`, { cause: error }));
          return;
        }
        if (replaced.aborted || Object.hasOwn(store.board().options, id2)) return;
        try {
          await commands.run("blockr.core-set_option", { id: id2, value: value2 }, CORE);
        } catch (error) {
          report(error);
        }
      })
    );
  }
  function mountBlock(el, block) {
    if (replaced.aborted) throw new Error(REPLACED);
    const found = own(store.board().blocks, block);
    if (found === void 0) {
      throw new Error(`The board holds no block "${block}" to mount.`);
    }
    const entry = own(store.blockRegistry(), found.registry_id);
    if (entry === void 0) {
      throw new Error(`The block registry has no class "${found.registry_id}" for block "${block}".`);
    }
    const pkg = packageOf(entry.id);
    const controller = new AbortController();
    const { signal } = controller;
    const ended = () => signal.aborted;
    let cleanup;
    const end = () => {
      if (ended()) return;
      offEnd();
      host.drop();
      controller.abort();
      if (cleanup !== void 0) attempt(cleanup);
      el.replaceChildren();
    };
    const host = blockHost({
      store,
      commands,
      scheduler,
      loader,
      answers,
      block,
      entry,
      replaced,
      unbound: end
    });
    const offEnd = onAbort(replaced, end);
    void (async () => {
      let exports;
      try {
        exports = await loader.load({ module: entry.module, ...entry.css === void 0 ? {} : { css: entry.css } }, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      if (ended()) return;
      let mod;
      try {
        mod = moduleOf(exports, entry.module, pkg);
      } catch (error) {
        fail(error);
        return;
      }
      const named2 = mod.options ?? [];
      await addOptions(named2, `the UI module of block class "${entry.id}"`);
      if (ended()) return;
      host.name(named2);
      try {
        const rendered = await mod.render({ el, host, signal });
        if (typeof rendered !== "function") return;
        if (ended()) attempt(rendered);
        else cleanup = rendered;
      } catch (error) {
        fail(new ModuleError(entry.module, pkg, `failed in render(): ${messageOf(error)}`, { cause: error }));
      }
    })();
    function fail(error) {
      if (ended()) return;
      showError(el, messageOf(error));
      report(error);
    }
    return end;
  }
  return {
    mountBlock,
    mountOutput: (el, block) => mountOutput(
      { store, scheduler, loader, answers, shelves, measure, replaced, addOptions, emit },
      el,
      block
    ),
    mountOption: (el, option) => mountOption(
      { store, commands, scheduler, loader, renderers, replaced, addOptions, defaultOf },
      el,
      option
    ),
    addOptions
  };
}
function blockHost(deps) {
  const { store, commands, scheduler, block, entry } = deps;
  let seen = store.board();
  let named2 = [];
  let state2 = blockOf(seen)?.state ?? {};
  let context = contextOf(blockOf(seen));
  let following = true;
  let gone = false;
  const listeners = { state: /* @__PURE__ */ new Set(), context: /* @__PURE__ */ new Set() };
  function blockOf(board) {
    return own(board.blocks, block);
  }
  function contextOf(now) {
    const recipe = scheduler.inputs(block)?.recipe ?? [];
    return Object.freeze({
      name: now?.name ?? "",
      status: scheduler.status(block) ?? NO_STATUS,
      inputs: Object.freeze(
        recipe.flatMap(
          (input2) => input2.block === void 0 ? [] : [Object.freeze({ input: input2.input, revision: scheduler.status(input2.block)?.revision ?? 0 })]
        )
      ),
      options: Object.freeze(
        Object.fromEntries(
          named2.flatMap((id2) => {
            const value2 = own(seen.options, id2);
            return value2 === void 0 ? [] : [[id2, value2]];
          })
        )
      )
    });
  }
  function bound() {
    return !gone && !deps.replaced.aborted && Object.hasOwn(store.board().blocks, block);
  }
  function announce2(event) {
    for (const fn of [...listeners[event]]) {
      try {
        fn();
      } catch (error) {
        report(error);
      }
    }
  }
  function refresh() {
    const now = blockOf(seen);
    if (now === void 0) {
      unbind();
      return;
    }
    const stateChanged = now.state !== state2;
    if (stateChanged) state2 = now.state;
    const next = contextOf(now);
    const contextChanged = !sameContext(context, next);
    if (contextChanged) context = next;
    if (stateChanged) announce2("state");
    if (contextChanged) announce2("context");
  }
  function unbind() {
    if (gone) return;
    gone = true;
    stop();
    deps.unbound();
  }
  const offStore = store.subscribe((_change, board) => {
    seen = board;
    refresh();
  });
  const offScheduler = scheduler.subscribe(() => {
    if (following && store.board() === seen) refresh();
  });
  function stop() {
    following = false;
    offStore();
    offScheduler();
  }
  const unboundError = () => new Error(
    deps.replaced.aborted ? `Another board opened, so the host of block "${block}" no longer changes it.` : `Block "${block}" left the board, so its host no longer changes it.`
  );
  return {
    block,
    class: entry.id,
    state: () => state2,
    context: () => context,
    on(event, fn) {
      const heard = own(listeners, event);
      if (heard === void 0) {
        throw new Error(`A block's host announces "state" and "context", not ${JSON.stringify(event)}.`);
      }
      const wrapped = () => {
        fn();
      };
      heard.add(wrapped);
      return () => {
        heard.delete(wrapped);
      };
    },
    submit(change) {
      if (!bound()) return Promise.reject(unboundError());
      if (!isObject(change)) {
        return Promise.reject(new TypeError("A submit takes the state args it changes, as an object."));
      }
      const set = Object.fromEntries(Object.entries(change).filter(([, value2]) => value2 !== void 0));
      return commands.run("blockr.core-set_block", { id: block, state: set });
    },
    request(name3, args = {}, opts = {}) {
      if (!bound()) return Promise.reject(unboundError());
      return blockRequest({ ...deps, bound, unboundError }, block, entry, name3, args, opts);
    },
    async evaluate(params) {
      if (!bound()) throw unboundError();
      if (!isObject(params)) {
        throw new TypeError("A host's evaluate() takes the parameters that the block's block_eval() method reads, as an object.");
      }
      const statuses = await scheduler.evaluate([block], params);
      const status = own(statuses, block);
      if (status === void 0) throw unboundError();
      return status;
    },
    module(id2) {
      return deps.loader.module(id2);
    },
    name(options) {
      named2 = options;
      context = contextOf(blockOf(seen));
    },
    drop: stop
  };
}
function sameContext(a2, b2) {
  return a2.name === b2.name && a2.status === b2.status && a2.inputs.length === b2.inputs.length && a2.inputs.every((input2, i2) => input2.input === b2.inputs[i2]?.input && input2.revision === b2.inputs[i2].revision) && jsonEqual(a2.options, b2.options);
}

// srcts/options/schemas.ts
var optionSchemas = {
  n_rows: {
    type: "integer",
    // A page of a table holds a row at least, as the coercion of table
    // outputs takes it.
    minimum: 1,
    description: "How many rows of a table a block's output shows"
  },
  show_conditions: {
    type: "array",
    items: { enum: ["message", "warning", "error"] },
    uniqueItems: true,
    description: "The severities of a block's conditions that the board shows"
  },
  dark_mode: {
    enum: ["light", "dark"],
    description: "Whether the board shows in its light or its dark theme"
  }
};

// srcts/inputs/inputs.tsx
var import_react = __toESM(require_react(), 1);

// srcts/binding/binding.ts
var PAUSE = 500;
function bind2(host, key, show, options = {}) {
  const source = sourceOf(host);
  source.check(key);
  let shown;
  let typed;
  let timer;
  let released = false;
  const display = (again = false) => {
    if (released || timer !== void 0) return;
    const value2 = source.get(key);
    if (!again && shown !== void 0 && jsonEqual(shown.value, value2)) return;
    shown = { value: value2 };
    show(value2);
  };
  const submit = async (value2) => {
    if (jsonEqual(source.get(key), value2)) return true;
    try {
      await source.submit(key, value2);
    } catch (error) {
      options.settled?.(error);
      return false;
    }
    options.settled?.(null);
    return true;
  };
  const send = () => {
    const value2 = typed;
    typed = void 0;
    if (value2 !== void 0) void submit(value2);
  };
  const flush = () => {
    if (timer === void 0) return;
    clearTimeout(timer);
    timer = void 0;
    send();
  };
  const off = source.onValues(() => {
    display();
  });
  display(true);
  return {
    get: () => source.get(key),
    async commit(value2) {
      const took = await submit(value2);
      if (!took) display(true);
      return took;
    },
    type(value2) {
      if (released) return;
      typed = value2;
      clearTimeout(timer);
      timer = setTimeout(() => {
        timer = void 0;
        send();
      }, PAUSE);
    },
    flush,
    release() {
      if (released) return;
      released = true;
      off();
      flush();
    }
  };
}
function sourceOf(host) {
  if ("option" in host) {
    const check2 = (key) => {
      if (key !== host.option) {
        throw new Error(`The host of the board option "${host.option}" holds no value "${key}".`);
      }
    };
    return {
      check: check2,
      get(key) {
        check2(key);
        return host.value();
      },
      submit(key, value2) {
        check2(key);
        return host.submit(value2);
      },
      onValues: (fn) => host.on("value", fn),
      onContext: () => () => void 0,
      inputs: () => null,
      request: (name3, args, opts) => host.request(name3, args, opts)
    };
  }
  return {
    check: () => void 0,
    get: (key) => own(host.state(), key),
    submit: (key, value2) => host.submit({ [key]: value2 }),
    onValues: (fn) => host.on("state", fn),
    onContext: (fn) => host.on("context", fn),
    inputs: () => host.context().inputs.map((input2) => ({ input: input2.input, revision: input2.revision })),
    request: (name3, args, opts) => host.request(name3, args, opts)
  };
}
function strings(value2) {
  return Array.isArray(value2) ? value2.filter((x2) => typeof x2 === "string") : [];
}

// srcts/styles/styles.ts
var styles = {
  stack: "bc:grid bc:gap-3 bc:font-sans bc:text-text-default",
  field: "bc:grid bc:gap-1 bc:font-sans bc:text-text-default",
  label: "bc:text-sm bc:font-medium",
  box: "bc:min-w-0 bc:justify-self-stretch bc:rounded-md bc:border bc:border-border-default bc:bg-transparent bc:px-2 bc:py-1 bc:font-sans bc:text-sm bc:text-text-default bc:focus:outline-2 bc:focus:outline-offset-1 bc:focus:outline-border-accent bc:aria-invalid:border-border-danger",
  check: "bc:flex bc:items-center bc:gap-2 bc:font-sans bc:text-sm bc:text-text-default",
  checkbox: "bc:accent-bg-accent",
  fieldset: "bc:m-0 bc:grid bc:min-w-0 bc:gap-1 bc:border-0 bc:p-0 bc:font-sans bc:text-text-default",
  legend: "bc:text-sm bc:font-medium bc:mb-1 bc:p-0",
  list: "bc:grid bc:max-h-48 bc:gap-1 bc:overflow-y-auto",
  hint: "bc:m-0 bc:text-sm bc:text-text-muted",
  alert: "bc:m-0 bc:text-sm bc:text-text-danger bc:whitespace-pre-wrap"
};

// srcts/inputs/inputs.tsx
var import_jsx_runtime = __toESM(require_jsx_runtime(), 1);
function Field({ label, children }) {
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("label", { className: styles.field, children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("span", { className: styles.label, children: label }),
    children
  ] });
}
function NumberInput({
  bound,
  label,
  min,
  max
}) {
  const [text, typed] = useTyped(bound);
  const shown = text ?? (typeof bound.value === "number" ? String(bound.value) : "");
  const read = (input2) => {
    const value2 = input2.trim() === "" ? Number.NaN : Number(input2);
    if (!Number.isInteger(value2)) return void 0;
    if (min !== void 0 && value2 < min) return void 0;
    if (max !== void 0 && value2 > max) return void 0;
    return value2;
  };
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(import_jsx_runtime.Fragment, { children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Field, { label, children: /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
      "input",
      {
        type: "number",
        step: 1,
        min,
        max,
        required: true,
        className: styles.box,
        value: shown,
        "aria-invalid": read(shown) === void 0,
        onChange: (event) => {
          typed(event.target.value);
          bound.type(read(event.target.value));
        },
        onKeyDown: (event) => {
          if (event.key === "Enter" && !event.nativeEvent.isComposing) bound.flush();
        },
        onBlur: () => {
          bound.flush();
        }
      }
    ) }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Refusal, { error: bound.error })
  ] });
}
function Select({
  bound,
  label,
  placeholder,
  choices
}) {
  const current2 = typeof bound.value === "string" ? bound.value : void 0;
  const shown = current2 === void 0 || choices.some((choice) => choice.value === current2) ? choices : [...choices, { value: current2, label: current2 }];
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(import_jsx_runtime.Fragment, { children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Field, { label, children: /* @__PURE__ */ (0, import_jsx_runtime.jsxs)(
      "select",
      {
        className: styles.box,
        value: current2 ?? "",
        onChange: (event) => {
          void bound.commit(event.target.value);
        },
        children: [
          current2 === void 0 && /* @__PURE__ */ (0, import_jsx_runtime.jsx)("option", { value: "", disabled: true, children: placeholder }),
          shown.map((choice) => /* @__PURE__ */ (0, import_jsx_runtime.jsx)("option", { value: choice.value, children: choice.label }, choice.value))
        ]
      }
    ) }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Refusal, { error: bound.error })
  ] });
}
function Checklist({
  bound,
  legend,
  choices,
  empty: empty2,
  ordered = false
}) {
  const held2 = strings(bound.value);
  const known = choices ?? [];
  const shown = [...known, ...held2.filter((value2) => !known.includes(value2))];
  const toggle = (value2, checked3) => {
    const now = strings(bound.get());
    const next = checked3 ? now.includes(value2) ? now : [...now, value2] : now.filter((other) => other !== value2);
    void bound.commit(
      ordered ? [...known.filter((choice) => next.includes(choice)), ...next.filter((other) => !known.includes(other))] : next
    );
  };
  return /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("fieldset", { className: styles.fieldset, children: [
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)("legend", { className: styles.legend, children: legend }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("div", { className: styles.list, children: [
      shown.map((value2) => /* @__PURE__ */ (0, import_jsx_runtime.jsxs)("label", { className: styles.check, children: [
        /* @__PURE__ */ (0, import_jsx_runtime.jsx)(
          "input",
          {
            type: "checkbox",
            className: styles.checkbox,
            value: value2,
            checked: held2.includes(value2),
            onChange: (event) => {
              toggle(value2, event.target.checked);
            }
          }
        ),
        value2
      ] }, value2)),
      choices !== void 0 && empty2 !== void 0 && shown.length === 0 && /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { className: styles.hint, children: empty2 })
    ] }),
    /* @__PURE__ */ (0, import_jsx_runtime.jsx)(Refusal, { error: bound.error })
  ] });
}
function Refusal({ error }) {
  return error === null ? null : /* @__PURE__ */ (0, import_jsx_runtime.jsx)("p", { role: "alert", className: styles.alert, children: error });
}
function useTyped(bound) {
  const [text, setText] = (0, import_react.useState)(null);
  const [seen, setSeen] = (0, import_react.useState)(bound.value);
  if (!Object.is(seen, bound.value)) {
    setSeen(bound.value);
    setText(null);
  }
  return [text, setText];
}

// srcts/react/hooks.ts
var import_react2 = __toESM(require_react(), 1);
function useValue(host, key) {
  const name3 = key ?? ("option" in host ? host.option : "");
  const [value2, setValue] = (0, import_react2.useState)(() => held(host, name3));
  const [error, setError] = (0, import_react2.useState)(null);
  const binding = (0, import_react2.useRef)(null);
  (0, import_react2.useLayoutEffect)(() => {
    const bound = bind2(host, name3, setValue, {
      settled: (refused2) => {
        setError(refused2 === null ? null : messageOf(refused2));
      }
    });
    binding.current = bound;
    return () => {
      binding.current = null;
      bound.release();
    };
  }, [host, name3]);
  return (0, import_react2.useMemo)(
    () => ({
      value: value2,
      error,
      get: () => binding.current?.get() ?? held(host, name3),
      commit: (next) => binding.current?.commit(next) ?? Promise.resolve(false),
      type: (next) => {
        binding.current?.type(next);
      },
      flush: () => {
        binding.current?.flush();
      }
    }),
    [value2, error, host, name3]
  );
}
function held(host, key) {
  return "option" in host ? host.value() : own(host.state(), key);
}

// srcts/react/module.tsx
var import_react3 = __toESM(require_react(), 1);
var import_react_dom = __toESM(require_react_dom(), 1);
var import_client2 = __toESM(require_client(), 1);
var import_jsx_runtime2 = __toESM(require_jsx_runtime(), 1);
function reactModule(Draw, options = {}) {
  return {
    ...options.options === void 0 ? {} : { options: options.options },
    render({ el, host, signal }) {
      const root = (0, import_client2.createRoot)(el);
      (0, import_react_dom.flushSync)(() => {
        root.render(/* @__PURE__ */ (0, import_jsx_runtime2.jsx)(Draw, { host, signal }));
      });
      return () => {
        root.unmount();
      };
    }
  };
}

// srcts/options/ui.tsx
var import_jsx_runtime3 = __toESM(require_jsx_runtime(), 1);
function numberUi(label, min) {
  function NumberUi({ host }) {
    return /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(NumberInput, { bound: useValue(host), label, min });
  }
  return reactModule(NumberUi);
}
function checklistUi(legend, choices) {
  function ChecklistUi({ host }) {
    return /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(Checklist, { bound: useValue(host), legend, choices, ordered: true });
  }
  return reactModule(ChecklistUi);
}
function selectUi(label, placeholder, choices) {
  function SelectUi({ host }) {
    return /* @__PURE__ */ (0, import_jsx_runtime3.jsx)(Select, { bound: useValue(host), label, placeholder, choices });
  }
  return reactModule(SelectUi);
}

// srcts/options/options.ts
function coreOptions(core) {
  return [nRows(core), showConditions(core), darkMode(core)];
}
function nRows(core) {
  const schema = optionSchemas.n_rows;
  const fallback = 10;
  return {
    id: "n_rows",
    label: "Preview rows",
    category: "Table options",
    schema,
    default: async () => checked2("n_rows", schema, await core.blockrOption("n_rows", fallback), fallback),
    ui: numberUi("Preview rows", schema.minimum)
  };
}
var SEVERITIES = ["message", "warning", "error"];
function showConditions(core) {
  const schema = optionSchemas.show_conditions;
  const fallback = ["warning", "error"];
  return {
    id: "show_conditions",
    label: "Message types to display",
    category: "Board options",
    schema,
    // R writes a blockr option of one severity as a string, as an
    // environment variable gives it.
    default: async () => {
      const value2 = await core.blockrOption("show_conditions", fallback);
      return checked2("show_conditions", schema, typeof value2 === "string" ? [value2] : value2, fallback);
    },
    ui: checklistUi("Message types to display", SEVERITIES)
  };
}
function darkMode(core) {
  const schema = optionSchemas.dark_mode;
  return {
    id: "dark_mode",
    label: "Light/dark mode",
    category: "Theme options",
    schema,
    default: async () => {
      const value2 = await core.blockrOption("dark_mode", null);
      const preferred = preferredTheme();
      if (value2 === null) return preferred;
      if (typeof value2 === "boolean") return value2 ? "dark" : "light";
      return checked2("dark_mode", schema, value2, preferred);
    },
    ui: selectUi("Light/dark mode", "Choose a mode", [
      { value: "light", label: "Light" },
      { value: "dark", label: "Dark" }
    ]),
    onChange(value2) {
      setTheme(value2);
    }
  };
}
function setTheme(theme) {
  document.documentElement.setAttribute("data-bs-theme", theme === "dark" ? "dark" : "light");
}
function preferredTheme() {
  return matchMedia("(prefers-color-scheme: dark)").matches ? "dark" : "light";
}
function checked2(id2, schema, value2, fallback) {
  const failures = compile(`the board option "${id2}"`, schema)(value2);
  if (failures.length === 0) return value2;
  report(
    new Error(
      `The blockr option ${id2} is ${JSON.stringify(value2)}, which the board option ${id2} does not take, so its default is ${JSON.stringify(fallback)}.`
    )
  );
  return fallback;
}

// srcts/transport/shiny.ts
var name = "blockr";
function shinyTransport(options = {}) {
  const page = window;
  const shiny = options.shiny ?? page.Shiny;
  if (shiny === void 0) {
    throw new Error("The Shiny transport needs Shiny's JS API, window.Shiny.");
  }
  const fetcher = options.fetch ?? window.fetch.bind(window);
  const onDisconnected = options.onDisconnected ?? disconnected(page.jQuery);
  let start2 = null;
  let session = 0;
  return {
    async send(message, bodies) {
      const at = session;
      for (const [id2, body] of bodies) {
        await post(fetcher, start2, id2, body);
      }
      if (at !== session) {
        throw new Error("The session ended before its message was sent.");
      }
      shiny.setInputValue(name, JSON.stringify(message), {
        priority: "event"
      });
    },
    listen(listener) {
      shiny.addCustomMessageHandler(name, (message) => {
        if (message instanceof ArrayBuffer) {
          for (const [item, body] of framed(message)) {
            listener.received(item, body);
          }
        } else if (Array.isArray(message)) {
          for (const item of message) {
            if (typeof item !== "object" || item === null) continue;
            const [carried, body] = received(item, fetcher);
            listener.received(carried, body);
          }
        } else if (isSessionStart(message)) {
          session += 1;
          start2 = message;
          listener.started();
        }
      });
      onDisconnected(() => {
        session += 1;
        start2 = null;
        listener.ended();
      });
    }
  };
}
function disconnected(jquery) {
  if (jquery === void 0) {
    throw new Error("The Shiny transport needs the jQuery that Shiny puts on the page.");
  }
  return (fn) => {
    jquery(document).on("shiny:disconnected", fn);
  };
}
function isSessionStart(message) {
  return typeof message === "object" && message !== null && typeof message.body_url === "string";
}
function framed(frame) {
  const length = new DataView(frame).getUint32(0, true);
  const json = new TextDecoder().decode(new Uint8Array(frame, 4, length));
  const bytes = frame.slice(4 + length);
  return JSON.parse(json).flatMap((item) => {
    if (typeof item !== "object" || item === null) return [];
    const announced = "status" in item && "body" in item ? item.body : null;
    if (typeof announced !== "object" || announced === null) {
      return [[item, null]];
    }
    const body = Promise.resolve(blob([bytes], announced));
    return [[item, body]];
  });
}
function received(item, fetcher) {
  const announced = "status" in item && "body" in item ? item.body : null;
  if (typeof announced !== "object" || announced === null) return [item, null];
  const { data, url, ...body } = announced;
  const reply = { ...item, body };
  let bytes;
  if (data !== void 0) {
    bytes = new Promise((resolve) => {
      resolve(blob([fromBase64(data)], body));
    });
  } else if (url !== void 0) {
    bytes = fetchBody(fetcher, url, body);
  } else {
    bytes = Promise.reject(
      new Error(`R sent no bytes of the body of its reply to ${reply.id}.`)
    );
  }
  bytes.catch(() => void 0);
  return [reply, bytes];
}
async function fetchBody(fetcher, url, body) {
  const response = await fetcher(url);
  if (!response.ok) {
    throw new Error(
      `R served no body at ${url}: ${String(response.status)} ${await response.text()}`
    );
  }
  const bytes = await response.blob();
  fetcher(url, { method: "DELETE" }).then((response2) => response2.text()).catch(() => void 0);
  return blob([bytes], body);
}
async function post(fetcher, start2, id2, body) {
  if (start2 === null) {
    throw new Error("No session runs that a body could be posted to.");
  }
  const max = start2.max_request_size;
  if (max !== void 0 && body.size > max) {
    throw new Error(
      `The body of request ${id2}, of ${String(body.size)} bytes, is larger than the ${String(max)} bytes that R takes.`
    );
  }
  const url = `${start2.body_url}${start2.body_url.includes("?") ? "&" : "?"}id=${encodeURIComponent(id2)}`;
  const response = await fetcher(url, {
    method: "POST",
    body,
    headers: {
      "Content-Type": body.type === "" ? "application/octet-stream" : body.type
    }
  });
  const text = await response.text();
  if (!response.ok) {
    throw new Error(`R refused the body of request ${id2}: ${text}`);
  }
}
function blob(parts, body) {
  return body.name === void 0 ? new Blob(parts, { type: body.type }) : new File(parts, body.name, { type: body.type });
}
function fromBase64(data) {
  const text = atob(data);
  const bytes = new Uint8Array(text.length);
  for (let i2 = 0; i2 < text.length; i2 += 1) {
    bytes[i2] = text.charCodeAt(i2);
  }
  return bytes;
}

// srcts/core/blockr-options.ts
function createBlockrOptions(client) {
  const found = /* @__PURE__ */ new Map();
  let batch;
  function ask(name3) {
    let asked = found.get(name3);
    if (asked === void 0) {
      asked = new Promise((resolve) => {
        if (batch === void 0) {
          const sending = /* @__PURE__ */ new Map();
          batch = sending;
          void Promise.resolve().then(() => {
            if (batch === sending) batch = void 0;
            void send(sending);
          });
        }
        batch.set(name3, resolve);
      });
      found.set(name3, asked);
    }
    return asked;
  }
  async function send(names) {
    const asked = [...names.keys()];
    let values2;
    try {
      values2 = await valuesOf(client, asked);
    } catch (error) {
      report(new Error(`R sent no values of the blockr options ${quoted(asked)}, so core takes their fallbacks: ${messageOf(error)}`, { cause: error }));
      for (const [name3, settle2] of names) {
        found.delete(name3);
        settle2(null);
      }
      return;
    }
    for (const [name3, settle2] of names) {
      settle2(Object.hasOwn(values2, name3) ? { value: values2[name3] ?? null } : null);
    }
  }
  return {
    async read(name3, fallback) {
      if (typeof name3 !== "string" || name3 === "") {
        throw new TypeError("A blockr option is read by its name, a non-empty string.");
      }
      return converted(name3, await ask(name3), fallback);
    }
  };
}
async function valuesOf(client, names) {
  for (; ; ) {
    let reply;
    try {
      ({ reply } = await client.request({ kind: "blockr_options", names }));
    } catch (error) {
      if (error instanceof SessionEndedError) continue;
      throw error;
    }
    if (reply.status !== "ok" || !("values" in reply)) {
      throw new Error(`R replied ${reply.status}.`);
    }
    return reply.values;
  }
}
function converted(name3, found, fallback) {
  if (found === null) return fallback;
  const { value: value2 } = found;
  if (typeof fallback === "number") {
    if (value2 === null) return Infinity;
    const number = typeof value2 === "string" ? numberOf(value2) : value2;
    if (typeof number === "number" && !Number.isNaN(number)) return number;
  } else if (typeof fallback === "boolean") {
    const logical = typeof value2 === "string" ? logicalOf(value2) : value2;
    if (typeof logical === "boolean") return logical;
  } else if (typeof fallback === "string") {
    if (typeof value2 === "string") return value2;
  } else {
    return value2;
  }
  report(
    new Error(
      `The blockr option ${name3} is ${JSON.stringify(value2)}, which is no ${typeof fallback}, so core takes its fallback ${JSON.stringify(fallback)}.`
    )
  );
  return fallback;
}
function numberOf(text) {
  const trimmed = text.trim();
  if (/^[+-]?inf(inity)?$/i.test(trimmed)) return trimmed.startsWith("-") ? -Infinity : Infinity;
  return trimmed === "" ? NaN : Number(trimmed);
}
function logicalOf(text) {
  const trimmed = text.trim();
  if (["TRUE", "true", "True", "T"].includes(trimmed)) return true;
  if (["FALSE", "false", "False", "F"].includes(trimmed)) return false;
  return void 0;
}

// srcts/core/requests.ts
var CORE_TYPES = ["block_class", "output_class", "renderer", "option"];
var CORE_FIELDS = ["id", "kind", "type", "reads", "body", "registry_id", "option", "output", "renderer"];
var ATTEMPTS3 = 3;
async function addedRequest(deps, type, fields, opts) {
  if (typeof type !== "string" || type === "") {
    throw new TypeError("A request names its type, a non-empty string.");
  }
  if (CORE_TYPES.includes(type)) {
    throw new Error(`A request of type "${type}" is core's own, which a host sends, rather than one that a package adds.`);
  }
  if (!isObject(fields)) {
    throw new TypeError(`The fields of a request of type "${type}" are an object.`);
  }
  const taken = Object.keys(fields).filter((name3) => CORE_FIELDS.includes(name3));
  if (taken.length > 0) {
    throw new Error(`A request of type "${type}" cannot hold the fields ${quoted(taken)}, which are core's.`);
  }
  const reads = opts.reads ?? [];
  if (!Array.isArray(reads) || !reads.every((block) => typeof block === "string")) {
    throw new TypeError(`The blocks that a request of type "${type}" reads are an array of their ids.`);
  }
  const what = `The request of type "${type}"`;
  for (let attempt3 = 1; ; attempt3++) {
    const revisions = await readsOf3(deps, what, reads, opts.signal);
    const { reply, body } = await deps.scheduler.request(
      { kind: "request", type, ...fields, ...reads.length === 0 ? {} : { reads: revisions } },
      {
        ...opts.signal === void 0 ? {} : { signal: opts.signal },
        ...opts.progress === void 0 ? {} : { progress: opts.progress.bind(opts) }
      }
    );
    switch (reply.status) {
      case "ok":
        if (body !== null) return body;
        return "value" in reply && reply.value !== void 0 ? reply.value : null;
      case "missing":
        if (attempt3 < ATTEMPTS3) continue;
        throw new RequestError(`${what} gave up, since R kept lacking the results it reads.`, { status: "missing" });
      case "cancelled":
        throw new RequestError(`R cancelled the request of type "${type}".`, { status: "cancelled" });
      default:
        throw new RequestError(`R replied ${reply.status} to the request of type "${type}".`, {
          status: reply.status,
          conditions: "conditions" in reply ? reply.conditions : []
        });
    }
  }
}
async function readsOf3(deps, what, blocks, signal) {
  if (blocks.length === 0) return [];
  const absent = blocks.filter((block) => deps.scheduler.status(block) === void 0);
  if (absent.length > 0) {
    throw new Error(`${what} reads ${absent.length === 1 ? "the block" : "the blocks"} ${quoted(absent)}, which the board lacks.`);
  }
  const unique = [...new Set(blocks)];
  const statuses = await deps.scheduler.evaluate(unique, void 0, signal === void 0 ? {} : { signal });
  const unready = unique.flatMap((block) => {
    const status = own(statuses, block);
    return status === void 0 || status.status === "ready" ? [] : [{ block, status }];
  });
  if (unready.length > 0) {
    const listed = unready.map(({ block, status }) => `"${block}" reads ${status.status}`);
    throw new RequestError(`${what} did not go to R, since a block it reads is not ready: ${listed.join(", ")}.`, {
      blocks: unready
    });
  }
  const left2 = unique.filter((block) => own(statuses, block) === void 0);
  if (left2.length > 0) {
    throw new Error(`${what} did not go to R, since ${left2.length === 1 ? "the block" : "the blocks"} ${quoted(left2)} left the board.`);
  }
  return unique.map((block) => ({ block, revision: own(statuses, block)?.revision ?? 0 }));
}

// srcts/core/core.ts
function createCore(parts) {
  const { store, commands, scheduler, loader, blockrOptions } = parts;
  const listeners = /* @__PURE__ */ new Set();
  parts.signal?.addEventListener(
    "abort",
    () => {
      listeners.clear();
    },
    { once: true }
  );
  const outputEvents = {
    subscribe(fn) {
      const wrapped = (event) => {
        fn(event);
      };
      listeners.add(wrapped);
      return () => {
        listeners.delete(wrapped);
      };
    }
  };
  const mounts = createMounts({
    store,
    commands,
    scheduler,
    loader,
    renderers: parts.renderers,
    ...parts.measure === void 0 ? {} : { measure: parts.measure },
    ...parts.signal === void 0 ? {} : { signal: parts.signal },
    emit(event) {
      for (const fn of [...listeners]) {
        try {
          fn(event);
        } catch (error) {
          report(error);
        }
      }
    }
  });
  const api = {
    store,
    commands,
    scheduler: {
      demand: (owner, blocks, options) => scheduler.demand(owner, blocks, options),
      evaluate: (blocks, params, options) => scheduler.evaluate(blocks, params, options),
      invalidate: (id2) => {
        scheduler.invalidate(id2);
      },
      status: (id2) => scheduler.status(id2),
      subscribe: (fn) => scheduler.subscribe(fn)
    },
    blockRegistry: () => store.blockRegistry(),
    options: store.options,
    plugins: {
      register: (def) => store.plugins.register(def)
    },
    lock: parts.lock,
    outputEvents,
    request: (type, fields = {}, opts = {}) => addedRequest({ scheduler }, type, fields, opts),
    module: (id2) => loader.module(id2),
    blockrOption: (name3, fallback) => blockrOptions.read(name3, fallback),
    mountBlock: (el, block) => mounts.mountBlock(el, block),
    mountOutput: (el, block) => mounts.mountOutput(el, block),
    mountOption: (el, option) => mounts.mountOption(el, option)
  };
  let heard = {};
  store.subscribe((change, board) => {
    const before = "opened" in change ? {} : heard;
    if (board.options === heard && !("opened" in change)) return;
    heard = board.options;
    for (const [id2, value2] of Object.entries(board.options)) {
      if (!jsonEqual(own(before, id2), value2)) changed(id2, value2);
    }
  });
  function changed(id2, value2) {
    const def = own(store.options.registered(), id2);
    if (def?.onChange === void 0) return;
    try {
      def.onChange(value2, api);
    } catch (error) {
      report(error);
    }
  }
  return api;
}

// srcts/core/frontends.ts
function loadingNote() {
  const box = document.createElement("div");
  box.setAttribute("role", "status");
  box.className = "bc:p-4 bc:font-sans bc:text-sm bc:text-text-muted";
  box.textContent = "Loading the board\u2026";
  return box;
}
function pageFrontEnds(el) {
  let drawn2;
  let next;
  function end() {
    const current2 = drawn2;
    if (current2 === void 0) return;
    drawn2 = void 0;
    current2.off();
    current2.controller.abort();
    if (current2.cleanup !== void 0) attempt(current2.cleanup);
    el.replaceChildren(loadingNote());
  }
  return {
    async prepare(front, loader) {
      let prepared;
      const { id: id2, source } = front;
      const pkg = packageOf(id2);
      if (source === void 0) {
        prepared = { id: id2, module: id2, error: new Error(`R serves no module ${id2}, the front-end that draws the board.`) };
      } else {
        try {
          const mod = moduleOf(await loader.load(source, pkg), source.module, pkg);
          prepared = { id: id2, module: source.module, render: mod.render.bind(mod) };
        } catch (error) {
          prepared = { id: id2, module: source.module, error };
        }
      }
      end();
      next = prepared;
    },
    async render(core, signal) {
      const prepared = next;
      next = void 0;
      if (prepared === void 0 || signal.aborted) return;
      el.replaceChildren();
      if (prepared.render === void 0) {
        showError(el, `The front-end ${prepared.id} cannot draw the board: ${messageOf(prepared.error)}`);
        report(prepared.error);
        return;
      }
      const controller = new AbortController();
      const ending = () => {
        if (drawn2 === current2) end();
      };
      signal.addEventListener("abort", ending, { once: true });
      const current2 = {
        controller,
        off: () => {
          signal.removeEventListener("abort", ending);
        },
        cleanup: void 0
      };
      drawn2 = current2;
      try {
        const rendered = await prepared.render({ el, core, signal: controller.signal });
        if (typeof rendered !== "function") return;
        if (controller.signal.aborted) attempt(rendered);
        else current2.cleanup = rendered;
      } catch (error) {
        if (controller.signal.aborted) return;
        const why = new ModuleError(prepared.module, packageOf(prepared.id), `failed in render(): ${messageOf(error)}`, {
          cause: error
        });
        showError(el, why.message);
        report(why);
      }
    }
  };
}

// srcts/store/words.ts
var adjectives = [
  "ablaze",
  "able",
  "acid",
  "acidic",
  "active",
  "actual",
  "added",
  "adept",
  "adored",
  "adroit",
  "aerial",
  "agile",
  "agreed",
  "airy",
  "alert",
  "alive",
  "allied",
  "almond",
  "amber",
  "ample",
  "amused",
  "apt",
  "arcane",
  "arctic",
  "ardent",
  "arid",
  "artful",
  "arty",
  "ash",
  "astral",
  "astute",
  "atomic",
  "auburn",
  "avian",
  "awake",
  "aware",
  "azure",
  "balmy",
  "basic",
  "beachy",
  "beige",
  "benign",
  "beryl",
  "best",
  "big",
  "binary",
  "bionic",
  "black",
  "blithe",
  "blocky",
  "blond",
  "blowy",
  "blue",
  "bold",
  "bonny",
  "boreal",
  "bouncy",
  "brainy",
  "brass",
  "brave",
  "breezy",
  "brick",
  "brief",
  "bright",
  "briny",
  "brisk",
  "broad",
  "bronze",
  "brown",
  "bubbly",
  "bumpy",
  "bushy",
  "busy",
  "calm",
  "candid",
  "carbon",
  "caring",
  "catchy",
  "cerise",
  "chalky",
  "chatty",
  "cheeky",
  "cheery",
  "chewy",
  "chic",
  "chief",
  "chilly",
  "choral",
  "chosen",
  "chummy",
  "chunky",
  "citric",
  "civic",
  "civil",
  "classy",
  "clay",
  "clean",
  "clever",
  "cloudy",
  "cocoa",
  "cold",
  "comfy",
  "comic",
  "common",
  "cool",
  "copper",
  "coral",
  "cosmic",
  "cotton",
  "covert",
  "coy",
  "cozy",
  "crafty",
  "craggy",
  "cream",
  "creamy",
  "crispy",
  "cubic",
  "cuddly",
  "curly",
  "curved",
  "cushy",
  "cute",
  "cyan",
  "dainty",
  "dapper",
  "daring",
  "dark",
  "dear",
  "decent",
  "deep",
  "deft",
  "demure",
  "denim",
  "dense",
  "dewy",
  "divine",
  "diving",
  "docile",
  "dogged",
  "dotted",
  "dovish",
  "downy",
  "dreamy",
  "dressy",
  "droll",
  "dry",
  "dusky",
  "dusty",
  "eager",
  "earthy",
  "easy",
  "ebony",
  "edgy",
  "edible",
  "elated",
  "elder",
  "elfin",
  "elite",
  "elvish",
  "equal",
  "even",
  "exact",
  "exotic",
  "expert",
  "extra",
  "fab",
  "fabled",
  "fair",
  "famed",
  "famous",
  "fancy",
  "far",
  "fast",
  "feisty",
  "ferny",
  "fierce",
  "fiery",
  "final",
  "fine",
  "firm",
  "first",
  "fit",
  "fixed",
  "fizzy",
  "flashy",
  "flat",
  "flax",
  "fleecy",
  "flinty",
  "floaty",
  "floppy",
  "floral",
  "fluent",
  "fluffy",
  "fluid",
  "flying",
  "foamy",
  "foggy",
  "fond",
  "forest",
  "formal",
  "frank",
  "free",
  "fresh",
  "frilly",
  "frisky",
  "frizzy",
  "frosty",
  "frothy",
  "frozen",
  "frugal",
  "fruity",
  "full",
  "fun",
  "funny",
  "furry",
  "future",
  "fuzzy",
  "garnet",
  "gauzy",
  "gem",
  "genial",
  "genius",
  "gentle",
  "giant",
  "giddy",
  "gifted",
  "giggly",
  "glad",
  "glass",
  "glassy",
  "glazed",
  "global",
  "glossy",
  "gnarly",
  "gold",
  "golden",
  "good",
  "goodly",
  "goofy",
  "gothic",
  "grainy",
  "grand",
  "grassy",
  "great",
  "green",
  "grey",
  "groovy",
  "grown",
  "gutsy",
  "handy",
  "happy",
  "hasty",
  "hazy",
  "heavy",
  "herbal",
  "herby",
  "heroic",
  "hidden",
  "high",
  "hilly",
  "hip",
  "hollow",
  "homey",
  "honest",
  "huge",
  "humane",
  "humble",
  "humid",
  "hungry",
  "iced",
  "iconic",
  "icy",
  "ideal",
  "idle",
  "immune",
  "impish",
  "indigo",
  "indoor",
  "inky",
  "inland",
  "inner",
  "intact",
  "ionic",
  "ironic",
  "ivory",
  "jade",
  "jagged",
  "jaunty",
  "jazzy",
  "jet",
  "jewel",
  "jolly",
  "jovial",
  "joyful",
  "joyous",
  "juicy",
  "jumpy",
  "junior",
  "just",
  "keen",
  "key",
  "khaki",
  "kind",
  "kindly",
  "kingly",
  "knotty",
  "known",
  "lace",
  "lacy",
  "lapis",
  "large",
  "lavish",
  "lawful",
  "leafy",
  "lean",
  "legal",
  "lemon",
  "lemony",
  "level",
  "light",
  "liked",
  "lilac",
  "lime",
  "limpid",
  "linear",
  "linen",
  "little",
  "live",
  "lively",
  "living",
  "local",
  "long",
  "loved",
  "lovely",
  "loving",
  "low",
  "lowkey",
  "loyal",
  "lucid",
  "lucky",
  "lunar",
  "lupine",
  "lush",
  "magic",
  "main",
  "maize",
  "major",
  "marble",
  "marine",
  "maroon",
  "marshy",
  "mauve",
  "medium",
  "meek",
  "mega",
  "mellow",
  "merry",
  "metal",
  "mica",
  "mid",
  "middle",
  "mighty",
  "mild",
  "milky",
  "mini",
  "minor",
  "minute",
  "misty",
  "mobile",
  "modern",
  "modest",
  "modish",
  "mossy",
  "moving",
  "mutual",
  "myrtle",
  "narrow",
  "naval",
  "navy",
  "near",
  "nearby",
  "neat",
  "neon",
  "new",
  "newsy",
  "next",
  "nice",
  "nickel",
  "nifty",
  "nimble",
  "noble",
  "noir",
  "normal",
  "novel",
  "nylon",
  "oblong",
  "ochre",
  "olive",
  "onyx",
  "opal",
  "open",
  "optic",
  "orange",
  "ornate",
  "outer",
  "oval",
  "paper",
  "papery",
  "peach",
  "peachy",
  "pearl",
  "pearly",
  "pebbly",
  "peppy",
  "perky",
  "pine",
  "pink",
  "placid",
  "plaid",
  "plain",
  "plucky",
  "plush",
  "poetic",
  "polar",
  "polite",
  "postal",
  "potent",
  "pretty",
  "prim",
  "prime",
  "proper",
  "proud",
  "public",
  "pure",
  "purple",
  "quaint",
  "quartz",
  "quick",
  "rainy",
  "random",
  "rapid",
  "rapt",
  "rare",
  "ready",
  "real",
  "recent",
  "red",
  "reedy",
  "regal",
  "rich",
  "right",
  "ripe",
  "ritzy",
  "robust",
  "rocky",
  "roomy",
  "rosy",
  "round",
  "royal",
  "rubber",
  "ruby",
  "runic",
  "rural",
  "russet",
  "rust",
  "rustic",
  "safe",
  "saline",
  "salmon",
  "salted",
  "salty",
  "sand",
  "sandy",
  "sane",
  "sassy",
  "satin",
  "satiny",
  "savvy",
  "secret",
  "secure",
  "sedate",
  "select",
  "sepia",
  "serene",
  "shaded",
  "shaggy",
  "shared",
  "sharp",
  "sheer",
  "shiny",
  "short",
  "shrewd",
  "shy",
  "sienna",
  "silk",
  "silken",
  "silky",
  "silly",
  "silver",
  "simple",
  "single",
  "slate",
  "sleepy",
  "slick",
  "slight",
  "slow",
  "sly",
  "small",
  "smart",
  "smoky",
  "smooth",
  "snappy",
  "snazzy",
  "snowy",
  "snug",
  "soapy",
  "social",
  "soft",
  "solar",
  "sole",
  "solemn",
  "solid",
  "sonic",
  "sound",
  "spare",
  "sparse",
  "speedy",
  "spicy",
  "spiky",
  "spiny",
  "sporty",
  "square",
  "stable",
  "starry",
  "static",
  "steamy",
  "steel",
  "steep",
  "still",
  "stoic",
  "stone",
  "stony",
  "stormy",
  "stout",
  "strict",
  "stripy",
  "strong",
  "suave",
  "subtle",
  "sudden",
  "sudsy",
  "suede",
  "sugar",
  "sugary",
  "sunlit",
  "sunny",
  "super",
  "superb",
  "sure",
  "sweet",
  "swift",
  "swirly",
  "tall",
  "tame",
  "tamed",
  "tan",
  "tangy",
  "tasty",
  "taupe",
  "taut",
  "teal",
  "techy",
  "tender",
  "thick",
  "thin",
  "thorny",
  "tidal",
  "tidy",
  "timely",
  "timid",
  "tin",
  "tinted",
  "tiny",
  "topaz",
  "tough",
  "toy",
  "trendy",
  "tricky",
  "tweed",
  "tweedy",
  "twill",
  "undyed",
  "unique",
  "united",
  "unlit",
  "upbeat",
  "upper",
  "urban",
  "urbane",
  "usable",
  "useful",
  "usual",
  "valid",
  "valued",
  "varied",
  "vast",
  "vellum",
  "velour",
  "velvet",
  "vernal",
  "vinyl",
  "violet",
  "vital",
  "vivid",
  "wacky",
  "warm",
  "wavy",
  "wax",
  "waxy",
  "wee",
  "well",
  "wheat",
  "white",
  "wide",
  "wild",
  "windy",
  "winged",
  "wintry",
  "wise",
  "wispy",
  "witty",
  "wood",
  "wooded",
  "wooden",
  "woodsy",
  "woody",
  "wool",
  "woolly",
  "worthy",
  "yellow",
  "young",
  "yummy",
  "zany",
  "zippy",
  "zircon"
];
var animals = [
  "addax",
  "adder",
  "agouti",
  "alpaca",
  "anole",
  "ant",
  "ape",
  "auk",
  "avocet",
  "baboon",
  "badger",
  "bass",
  "bat",
  "beagle",
  "bear",
  "beaver",
  "bee",
  "beetle",
  "bilby",
  "bird",
  "bison",
  "boa",
  "boar",
  "bobcat",
  "bongo",
  "bonobo",
  "borzoi",
  "bream",
  "bronco",
  "buck",
  "budgie",
  "bug",
  "bull",
  "bunny",
  "burro",
  "caiman",
  "calf",
  "camel",
  "canary",
  "carp",
  "cat",
  "cicada",
  "civet",
  "clam",
  "coati",
  "cobra",
  "cod",
  "collie",
  "colt",
  "conch",
  "condor",
  "coot",
  "cow",
  "coyote",
  "coypu",
  "crab",
  "crane",
  "crow",
  "cub",
  "cuckoo",
  "curlew",
  "cygnet",
  "deer",
  "degu",
  "dikdik",
  "dingo",
  "dipper",
  "dodo",
  "doe",
  "dog",
  "donkey",
  "dove",
  "drake",
  "drongo",
  "duck",
  "dugong",
  "dunlin",
  "eagle",
  "eel",
  "egret",
  "eider",
  "eland",
  "elk",
  "emu",
  "ermine",
  "ewe",
  "falcon",
  "fawn",
  "ferret",
  "finch",
  "fish",
  "fly",
  "foal",
  "fossa",
  "fowl",
  "fox",
  "frog",
  "fulmar",
  "galago",
  "galah",
  "gander",
  "gannet",
  "gaur",
  "gecko",
  "gelada",
  "genet",
  "gerbil",
  "gibbon",
  "gnu",
  "goat",
  "godwit",
  "goose",
  "gopher",
  "grebe",
  "grouse",
  "gull",
  "guppy",
  "hake",
  "hare",
  "hawk",
  "hen",
  "heron",
  "hog",
  "hoopoe",
  "hornet",
  "horse",
  "hound",
  "husky",
  "hyena",
  "ibex",
  "ibis",
  "iguana",
  "impala",
  "jackal",
  "jaguar",
  "jay",
  "jerboa",
  "joey",
  "junco",
  "kakapo",
  "kite",
  "kitten",
  "kiwi",
  "koala",
  "koi",
  "krill",
  "kudu",
  "lamb",
  "langur",
  "lark",
  "lemur",
  "liger",
  "limpet",
  "lion",
  "lizard",
  "llama",
  "loon",
  "loris",
  "lynx",
  "macaw",
  "magpie",
  "mamba",
  "manta",
  "mantis",
  "mare",
  "marlin",
  "marmot",
  "marten",
  "martin",
  "mayfly",
  "merlin",
  "mink",
  "minnow",
  "mole",
  "monkey",
  "moose",
  "moray",
  "morpho",
  "moth",
  "mouse",
  "mule",
  "mullet",
  "muskox",
  "mussel",
  "myna",
  "nene",
  "newt",
  "numbat",
  "nutria",
  "nyala",
  "ocelot",
  "okapi",
  "orca",
  "oriole",
  "oryx",
  "osprey",
  "otter",
  "owl",
  "ox",
  "oyster",
  "paca",
  "panda",
  "parrot",
  "perch",
  "phoebe",
  "pig",
  "pigeon",
  "piglet",
  "pika",
  "pike",
  "pipit",
  "plover",
  "pony",
  "poodle",
  "possum",
  "prawn",
  "puffer",
  "puffin",
  "pug",
  "puma",
  "puppy",
  "python",
  "quagga",
  "quail",
  "quokka",
  "quoll",
  "rabbit",
  "ram",
  "raven",
  "ray",
  "rhea",
  "rhino",
  "robin",
  "rook",
  "sable",
  "saiga",
  "salmon",
  "scarab",
  "seal",
  "serval",
  "shark",
  "sheep",
  "shrew",
  "shrike",
  "shrimp",
  "siskin",
  "skink",
  "skua",
  "skunk",
  "sloth",
  "snail",
  "snake",
  "snipe",
  "spider",
  "squid",
  "stag",
  "steed",
  "steer",
  "stilt",
  "stoat",
  "stork",
  "swan",
  "swift",
  "takin",
  "tapir",
  "teal",
  "tenrec",
  "tern",
  "thrush",
  "tiger",
  "toad",
  "topi",
  "toucan",
  "towhee",
  "trout",
  "tuna",
  "turaco",
  "turkey",
  "turtle",
  "urchin",
  "vervet",
  "vicuna",
  "viper",
  "vole",
  "walrus",
  "wasp",
  "weasel",
  "whale",
  "wisent",
  "wolf",
  "wombat",
  "wrasse",
  "wren",
  "yak",
  "zebra",
  "zebu"
];

// srcts/store/ids.ts
function isId(id2) {
  return typeof id2 === "string" && id2 !== "" && !/^\d+$/.test(id2);
}
function mintId(taken, randId = "adjective_animal") {
  const draw = draws[randId];
  for (let i2 = 0; i2 < 100; i2++) {
    const id3 = draw();
    if (isId(id3) && !taken(id3)) return id3;
  }
  const id2 = draw();
  let n2 = 2;
  while (taken(`${id2}_${String(n2)}`)) n2++;
  return `${id2}_${String(n2)}`;
}
function sentenceCase(id2) {
  const words = id2.replace(/([A-Z])/g, " $1").replace(/[_\-.]/g, " ").trim().split(" ");
  const [first = "", ...rest] = words;
  return [
    first.charAt(0).toUpperCase() + first.slice(1).toLowerCase(),
    ...rest.map((word) => word.toLowerCase())
  ].join(" ");
}
var draws = {
  adjective_animal: () => `${pick(adjectives)}_${pick(animals)}`,
  adjective_adjective_animal: () => {
    const [first, second] = pickTwo(adjectives);
    return `${first}_${second}_${pick(animals)}`;
  },
  letters_6: () => randomLetters(6),
  letters_8: () => randomLetters(8),
  letters_12: () => randomLetters(12),
  bytes_4: () => hex2(randomBytes(4)),
  bytes_8: () => hex2(randomBytes(8)),
  uuid: randomUuid
};
function randomInt(n2) {
  return Math.floor(Math.random() * n2);
}
function pick(words) {
  return words[randomInt(words.length)] ?? "";
}
function pickTwo(words) {
  const i2 = randomInt(words.length);
  let j2 = randomInt(words.length - 1);
  if (j2 >= i2) j2++;
  return [words[i2] ?? "", words[j2] ?? ""];
}
function randomLetters(n2) {
  return Array.from({ length: n2 }, () => String.fromCharCode(97 + randomInt(26))).join("");
}
function randomBytes(n2) {
  return Array.from({ length: n2 }, () => randomInt(256));
}
function hex2(bytes) {
  return bytes.map((byte) => byte.toString(16).padStart(2, "0")).join("");
}
function randomUuid() {
  const bytes = randomBytes(16);
  bytes[6] = (bytes[6] ?? 0) & 15 | 64;
  bytes[8] = (bytes[8] ?? 0) & 63 | 128;
  const text = hex2(bytes);
  return [text.slice(0, 8), text.slice(8, 12), text.slice(12, 16), text.slice(16, 20), text.slice(20)].join("-");
}

// srcts/store/plugins.ts
var ROLES = ["frontend", "storage"];
function inRole(plugins, classOf2, role) {
  return Object.entries(plugins).filter(([, plugin]) => classOf2(plugin.class)?.[role] !== void 0).map(([id2]) => id2);
}
function defaultState(def) {
  return frozenCopy(def.state === void 0 ? {} : def.state.default);
}
function keptPlugins(board, opening2, classOf2) {
  return Object.fromEntries(
    Object.entries(board).filter(
      (entry) => isObject(entry[1]) && typeof entry[1].class === "string" && own(opening2, entry[0])?.class === entry[1].class && classOf2(entry[1].class) !== void 0
    )
  );
}
function appPlugins(board, opening2, classOf2) {
  const kept = keptPlugins(board, opening2, classOf2);
  const plugins = {};
  for (const [id2, plugin] of Object.entries(opening2)) {
    const def = classOf2(plugin.class);
    if (def === void 0) continue;
    plugins[id2] = own(kept, id2) ?? { class: plugin.class, name: plugin.name, state: defaultState(def) };
  }
  return plugins;
}

// srcts/store/check.ts
var HEX_COLOR = "^#[0-9a-fA-F]{6}$";
var hexColor = new RegExp(HEX_COLOR);
function check(board, registry, registrations, changed) {
  const failures = [];
  if (!isId(board.id)) {
    failures.push(`The board's id "${board.id}" is empty or made of digits alone.`);
  }
  if (typeof board.name !== "string") {
    failures.push("The board's name is no string.");
  }
  checkBlocks(board, registry, registrations, changed, failures);
  checkLinks(board, registry, changed, failures);
  checkStacks(board, changed, failures);
  checkValues(board, registrations, changed, failures);
  checkPlugins(board, registrations, changed, failures);
  return failures;
}
function ids(what, record, failures) {
  for (const id2 of Object.keys(record)) {
    if (!isId(id2)) failures.push(`The ${what} id "${id2}" is empty or made of digits alone.`);
  }
}
function checkBlocks(board, registry, registrations, changed, failures) {
  ids("block", board.blocks, failures);
  for (const [id2, block] of Object.entries(board.blocks)) {
    if (changed !== void 0 && !changed.blocks.has(id2)) continue;
    const fields = shape(block, { registry_id: "string", state: "object", name: "string" });
    if (fields.length > 0) {
      failures.push(`The block "${id2}" ${fields.join(", ")}.`);
      continue;
    }
    if (own(registry, block.registry_id) === void 0) {
      failures.push(
        `The block "${id2}" is of the class "${block.registry_id}", which the block registry lacks.`
      );
    }
  }
}
function checkLinks(board, registry, changed, failures) {
  ids("link", board.links, failures);
  const fed = /* @__PURE__ */ new Map();
  for (const [id2, link] of Object.entries(board.links)) {
    if (changed === void 0 || changed.links.has(id2)) {
      const fields = shape(link, { from: "string", to: "string", input: "string" });
      if (fields.length > 0) {
        failures.push(`The link "${id2}" ${fields.join(", ")}.`);
        continue;
      }
    }
    const from = own(board.blocks, link.from);
    const to = own(board.blocks, link.to);
    if (from === void 0) {
      failures.push(`The link "${id2}" comes from block "${link.from}", which the board lacks.`);
    }
    if (to === void 0) {
      failures.push(`The link "${id2}" goes to block "${link.to}", which the board lacks.`);
      continue;
    }
    const inputs = own(registry, to.registry_id)?.inputs;
    if (inputs === void 0 || inputs.includes("...")) continue;
    if (!inputs.includes(link.input)) {
      failures.push(
        `The link "${id2}" feeds the input "${link.input}" of block "${link.to}", whose class has no such input.`
      );
      continue;
    }
    const at = JSON.stringify([link.to, link.input]);
    fed.set(at, [...fed.get(at) ?? [], id2]);
  }
  for (const [at, links] of fed) {
    if (links.length < 2) continue;
    const [to, input2] = JSON.parse(at);
    failures.push(
      `The input "${input2}" of block "${to}" takes the links ${quoted(links)}, where it takes one.`
    );
  }
  const cycle = graphOf(board).cycle();
  if (cycle !== void 0) {
    failures.push(`The links form a cycle through the blocks ${quoted(cycle)}.`);
  }
}
function checkStacks(board, changed, failures) {
  ids("stack", board.stacks, failures);
  const holder = /* @__PURE__ */ new Map();
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (changed === void 0 || changed.stacks.has(id2)) {
      const fields = shape(stack, { name: "string", blocks: "array", color: "string or null" });
      if (fields.length > 0) {
        failures.push(`The stack "${id2}" ${fields.join(", ")}.`);
        continue;
      }
      if (stack.color !== null && !hexColor.test(stack.color)) {
        failures.push(
          `The stack "${id2}" has the colour "${stack.color}", which is no hex colour of six digits, such as "#66c2a5".`
        );
      }
    }
    for (const block of stack.blocks) {
      if (own(board.blocks, block) === void 0) {
        failures.push(`The stack "${id2}" holds block "${block}", which the board lacks.`);
        continue;
      }
      const other = holder.get(block);
      if (other === id2) {
        failures.push(`The stack "${id2}" holds block "${block}" twice.`);
      } else if (other !== void 0) {
        failures.push(`The block "${block}" sits in the stacks "${other}" and "${id2}", where it sits in one.`);
      }
      holder.set(block, id2);
    }
  }
}
function checkValues(board, registrations, changed, failures) {
  for (const [id2, value2] of Object.entries(board.options)) {
    if (changed !== void 0 && !changed.options.has(id2)) continue;
    const option = registrations.option(id2);
    if (option === void 0) {
      failures.push(`The board holds a value of the board option "${id2}", which no package registered.`);
      continue;
    }
    for (const failure of option.validate(value2)) {
      failures.push(`The value of the board option "${id2}" ${failure}`);
    }
  }
}
function checkPlugins(board, registrations, changed, failures) {
  ids("plugin", board.plugins, failures);
  for (const [id2, plugin] of Object.entries(board.plugins)) {
    if (changed !== void 0 && !changed.plugins.has(id2)) continue;
    const fields = shape(plugin, { class: "string", name: "string", state: "value" });
    if (fields.length > 0) {
      failures.push(`The plugin "${id2}" ${fields.join(", ")}.`);
      continue;
    }
    const cls = registrations.plugin(plugin.class);
    if (cls === void 0) {
      failures.push(`The plugin "${id2}" is of the class "${plugin.class}", which no package registered.`);
      continue;
    }
    for (const failure of cls.validate(plugin.state)) {
      failures.push(`The state of the plugin "${id2}" ${failure}`);
    }
  }
  const classOf2 = (id2) => registrations.plugin(id2)?.def;
  for (const role of ROLES) {
    const held2 = inRole(board.plugins, classOf2, role);
    if (held2.length > 1) {
      failures.push(`The board holds the ${ROLE_NAMES[role]} ${quoted(held2)}, where it holds one at most.`);
    }
  }
}
var ROLE_NAMES = {
  frontend: "frontends",
  storage: "storage plugins"
};
function shape(value2, fields) {
  if (!isObject(value2)) return ["is no object"];
  const problems = [];
  for (const [field, kind] of Object.entries(fields)) {
    if (!fits(value2[field], kind)) problems.push(`has no ${kind} as its ${field}`);
  }
  for (const field of Object.keys(value2)) {
    if (!Object.hasOwn(fields, field)) problems.push(`holds "${field}", which core does not keep`);
  }
  return problems;
}
function fits(x2, kind) {
  switch (kind) {
    case "value":
      return x2 !== void 0;
    case "array":
      return Array.isArray(x2);
    case "object":
      return isObject(x2);
    case "string or null":
      return x2 === null || typeof x2 === "string";
    case "string":
      return typeof x2 === "string";
  }
}

// srcts/store/patch.ts
function pointer(path) {
  if (!path.startsWith("/")) return void 0;
  const parts = path.slice(1).split("/");
  if (parts.some((part) => /~(?![01])/.test(part))) return void 0;
  return parts.map((part) => part.replaceAll("~1", "/").replaceAll("~0", "~"));
}
function pluginOf(path) {
  const [top, plugin] = pointer(path) ?? [];
  return top === "plugins" ? plugin : void 0;
}
function pathOf(parts) {
  return parts.map((part) => `/${part.replaceAll("~", "~0").replaceAll("/", "~1")}`).join("");
}
function overlap(a2, b2) {
  const n2 = Math.min(a2.length, b2.length);
  return a2.slice(0, n2).every((part, i2) => part === b2[i2]);
}
function malformed(patch2) {
  if (!isObject(patch2)) return ["is no object"];
  const problems = [];
  const { op, path, edit } = patch2;
  if (op !== "add" && op !== "remove" && op !== "replace") {
    problems.push(`has the op ${JSON.stringify(op)}, where it takes "add", "remove" or "replace"`);
  }
  if (typeof path !== "string" || pointer(path) === void 0) {
    problems.push(`has the path ${JSON.stringify(path)}, which is no JSON Pointer below the board's top`);
  }
  if ((op === "add" || op === "replace") && patch2.value === void 0) {
    problems.push("has no value");
  }
  if (edit !== void 0 && (typeof edit !== "number" || !Number.isInteger(edit))) {
    problems.push(`has the edit id ${JSON.stringify(edit)}, which is no whole number`);
  }
  for (const field of Object.keys(patch2)) {
    if (!["op", "path", "value", "edit"].includes(field)) {
      problems.push(`holds "${field}", which a patch does not keep`);
    }
  }
  return problems;
}
function applyPatch(board, patch2, parts, place) {
  const result = step(board, parts, patch2, [], place);
  if (typeof result === "string") return { failure: result };
  return { ...result, next: result.next };
}
function step(at, parts, patch2, done, place) {
  const [part = "", ...rest] = parts;
  if (Array.isArray(at)) {
    const items = at;
    const last = rest.length === 0;
    const index = last && part === "-" && patch2.op === "add" ? items.length : arrayIndex(part);
    const bound = last && patch2.op === "add" ? items.length : items.length - 1;
    if (index === void 0 || index > bound) {
      return `finds no element "${part}" in the array at "${pathOf(done)}"`;
    }
    const here2 = pathOf([...done, String(index)]);
    if (!last) {
      const inner = step(items[index], rest, patch2, [...done, String(index)], place);
      if (typeof inner === "string") return inner;
      if (inner.next === items[index]) return { ...inner, next: at };
      return { ...inner, next: spliced(items, index, 1, inner.next) };
    }
    const old2 = items[index];
    switch (patch2.op) {
      case "add":
        return {
          next: spliced(items, index, 0, patch2.value),
          inverse: { op: "remove", path: here2 }
        };
      case "remove":
        return {
          next: spliced(items, index, 1),
          inverse: { op: "add", path: here2, value: old2 }
        };
      case "replace":
        if (jsonEqual(old2, patch2.value)) return { next: at, inverse: void 0 };
        return {
          next: spliced(items, index, 1, patch2.value),
          inverse: { op: "replace", path: here2, value: old2 }
        };
    }
  }
  if (!isObject(at)) {
    return `finds neither an object nor an array at "${pathOf(done)}"`;
  }
  const has = Object.hasOwn(at, part);
  const old = has ? at[part] : void 0;
  const here = pathOf([...done, part]);
  if (rest.length > 0) {
    if (!has) return `finds no "${part}" in "${pathOf(done)}"`;
    const inner = step(old, rest, patch2, [...done, part], place);
    if (typeof inner === "string") return inner;
    if (inner.next === old) return { ...inner, next: at };
    return { ...inner, next: { ...at, [part]: inner.next } };
  }
  switch (patch2.op) {
    case "add":
    case "replace":
      if (!has && patch2.op === "replace") return `finds no "${part}" in "${pathOf(done)}" to replace`;
      if (!has) {
        return { next: inserted(at, part, patch2.value, place), inverse: { op: "remove", path: here } };
      }
      if (jsonEqual(old, patch2.value)) return { next: at, inverse: void 0 };
      return {
        next: { ...at, [part]: patch2.value },
        inverse: { op: "replace", path: here, value: old }
      };
    case "remove": {
      if (!has) return `finds no "${part}" in "${pathOf(done)}" to remove`;
      const members = Object.keys(at);
      return {
        next: without(at, [part]),
        inverse: { op: "add", path: here, value: old },
        place: members[members.indexOf(part) + 1] ?? null
      };
    }
  }
}
function inserted(record, key, value2, before) {
  if (typeof before !== "string" || !Object.hasOwn(record, before)) {
    return { ...record, [key]: value2 };
  }
  const members = Object.entries(record);
  members.splice(members.findIndex(([name3]) => name3 === before), 0, [key, value2]);
  return Object.fromEntries(members);
}
function spliced(items, index, count, ...added) {
  const copy = [...items];
  copy.splice(index, count, ...added);
  return copy;
}
function arrayIndex(part) {
  return /^(0|[1-9]\d*)$/.test(part) ? Number(part) : void 0;
}

// srcts/store/commit.ts
function target(of) {
  return "block" in of ? JSON.stringify(["block", of.block]) : JSON.stringify(["plugin", of.plugin]);
}
function commit(board, delta, places = {}) {
  const failures = [];
  const targets = /* @__PURE__ */ new Set();
  const blocks = collection(
    (id2) => `block "${id2}"`,
    board.blocks,
    delta.blocks,
    (old, mod, id2) => modifyBlock(old, mod, id2, failures),
    places.blocks,
    void 0,
    failures
  );
  for (const id2 of blocks.touched) targets.add(target({ block: id2 }));
  const links = collection(
    (id2) => `link "${id2}"`,
    board.links,
    delta.links,
    modifyFields,
    places.links,
    delta.links?.before,
    failures
  );
  const stacks = collection(
    (id2) => `stack "${id2}"`,
    board.stacks,
    delta.stacks,
    modifyFields,
    places.stacks,
    void 0,
    failures
  );
  const options = values(
    board.options,
    delta.options,
    (id2) => `the board option "${id2}"`,
    failures
  );
  let next = {
    ...board,
    name: delta.name ?? board.name,
    blocks: blocks.next,
    links: links.next,
    stacks: stacks.next,
    options: options.next
  };
  const patched = patch(next, delta.patches ?? [], places.patches ?? {}, failures);
  for (const plugin of patched.plugins) targets.add(target({ plugin }));
  next = patched.board;
  if (failures.length > 0) return { failures };
  const inverse = {
    ...empty(blocks.inverse) ? {} : { blocks: blocks.inverse },
    ...empty(links.inverse) ? {} : { links: links.inverse },
    ...empty(stacks.inverse) ? {} : { stacks: stacks.inverse },
    ...options.inverse === void 0 ? {} : { options: options.inverse },
    ...delta.name === void 0 || delta.name === board.name ? {} : { name: board.name },
    ...patched.inverse.length === 0 ? {} : { patches: patched.inverse }
  };
  return {
    board: next,
    inverse: {
      delta: inverse,
      places: {
        blocks: blocks.places,
        links: links.places,
        stacks: stacks.places,
        patches: patched.places
      }
    },
    changed: {
      blocks: new Set(blocks.changed),
      links: new Set(links.changed),
      stacks: new Set(stacks.changed),
      options: options.set,
      plugins: patched.plugins,
      targets
    }
  };
}
function patch(board, patches2, placed, failures) {
  let next = board;
  const inverse = [];
  const places = {};
  const plugins = /* @__PURE__ */ new Set();
  for (const [i2, given] of patches2.entries()) {
    const name3 = `The patch ${String(i2 + 1)}`;
    const problems = malformed(given);
    if (problems.length > 0) {
      failures.push(`${name3} ${problems.join(", ")}.`);
      continue;
    }
    const parts = pointer(given.path) ?? [];
    const misplaced2 = outside(parts, given.op);
    if (misplaced2 !== void 0) {
      failures.push(`${name3}, ${given.op} at "${given.path}", ${misplaced2}.`);
      continue;
    }
    const place = given.op === "add" ? own(placed, given.path) : void 0;
    const applied = applyPatch(next, given, parts, place);
    if ("failure" in applied) {
      failures.push(`${name3}, ${given.op} at "${given.path}", ${applied.failure}.`);
      continue;
    }
    next = applied.next;
    if (applied.inverse === void 0) continue;
    inverse.unshift(applied.inverse);
    if (applied.place !== void 0) places[applied.inverse.path] = applied.place;
    plugins.add(parts[1] ?? "");
  }
  return { board: next, inverse, places, plugins };
}
function outside(parts, op) {
  const [top, plugin, part] = parts;
  if (top !== "plugins") return `changes "${top ?? ""}", where a patch changes the board's plugins alone`;
  if (plugin === void 0) return "changes the board's plugins as a whole, where a patch changes one plugin";
  if (part === void 0) {
    return `changes the plugin "${plugin}" as a whole, where a patch changes a plugin's name or its state, since the app's opening board sets the plugins that a board holds`;
  }
  if (part !== "name" && part !== "state") {
    return `changes "${part}" of the plugin "${plugin}", where a patch below a plugin changes its name or its state`;
  }
  if (parts.length === 3 && op === "remove") {
    return `removes the ${part} of the plugin "${plugin}", which a plugin always holds`;
  }
  return void 0;
}
function collection(name3, current2, part, modify, places, before, failures) {
  if (part === void 0) {
    return { next: current2, inverse: {}, places: {}, changed: [], touched: [] };
  }
  const order = Object.keys(current2);
  const removed = /* @__PURE__ */ new Set();
  for (const id2 of part.rm ?? []) {
    if (Object.hasOwn(current2, id2)) removed.add(id2);
    else failures.push(`The board holds no ${name3(id2)} to remove.`);
  }
  const readd = {};
  const readdPlaces = {};
  for (const id2 of removed) {
    readd[id2] = current2[id2];
    const after = order.slice(order.indexOf(id2) + 1);
    readdPlaces[id2] = after.find((other) => !removed.has(other)) ?? null;
  }
  const kept = order.filter((id2) => !removed.has(id2)).map((id2) => [id2, current2[id2]]);
  const added = [];
  for (const [id2, value2] of Object.entries(part.add ?? {})) {
    if (kept.some(([other]) => other === id2)) {
      failures.push(`The board holds ${name3(id2)} already.`);
      continue;
    }
    const named2 = own(before ?? {}, id2);
    const successor = named2 === void 0 ? own(places ?? {}, id2) ?? null : removed.has(named2) ? own(readdPlaces, named2) ?? null : named2;
    const at = successor === null ? -1 : kept.findIndex(([other]) => other === successor);
    if (at < 0 && successor !== null && named2 !== void 0) {
      failures.push(
        `The board holds no ${name3(successor)} for ${name3(id2)} to go before.`
      );
      continue;
    }
    if (at < 0) kept.push([id2, value2]);
    else kept.splice(at, 0, [id2, value2]);
    added.push(id2);
  }
  const remod = {};
  const modded = [];
  for (const [id2, mod] of Object.entries(part.mod ?? {})) {
    const at = kept.findIndex(([other]) => other === id2);
    const entry = kept[at];
    if (entry === void 0) {
      failures.push(`The board holds no ${name3(id2)} to change.`);
      continue;
    }
    const { value: value2, inverse } = modify(entry[1], mod, id2);
    if (Object.keys(inverse).length === 0) continue;
    kept[at] = [id2, value2];
    if (!added.includes(id2)) remod[id2] = inverse;
    modded.push(id2);
  }
  return {
    next: Object.fromEntries(kept),
    inverse: {
      ...added.length === 0 ? {} : { rm: added },
      ...removed.size === 0 ? {} : { add: readd },
      ...Object.keys(remod).length === 0 ? {} : { mod: remod }
    },
    places: readdPlaces,
    changed: [.../* @__PURE__ */ new Set([...added, ...modded])],
    touched: [.../* @__PURE__ */ new Set([...removed, ...added, ...modded])]
  };
}
function empty(part) {
  return Object.keys(part).length === 0;
}
function changes(current2, set, unset, both, failures) {
  const setting2 = Object.entries(set).filter(
    ([name3, value2]) => !jsonEqual(own(current2, name3), value2)
  );
  const restore = {};
  const remove2 = [];
  const removing = [];
  for (const name3 of unset) {
    if (Object.hasOwn(set, name3)) failures.push(both(name3));
    const was = own(current2, name3);
    if (was === void 0) continue;
    removing.push(name3);
    restore[name3] = was;
  }
  for (const [name3] of setting2) {
    const was = own(current2, name3);
    if (was === void 0) remove2.push(name3);
    else restore[name3] = was;
  }
  return {
    next: setting2.length === 0 && removing.length === 0 ? current2 : { ...without(current2, removing), ...Object.fromEntries(setting2) },
    set: setting2.map(([name3]) => name3),
    restore,
    remove: remove2
  };
}
function values(current2, change, what, failures) {
  if (change === void 0) {
    return { next: current2, inverse: void 0, set: /* @__PURE__ */ new Set() };
  }
  const result = changes(
    current2,
    change.set ?? {},
    change.unset ?? [],
    (name3) => `The change both sets and unsets ${what(name3)}.`,
    failures
  );
  const changed = Object.keys(result.restore).length > 0 || result.remove.length > 0;
  return {
    next: result.next,
    inverse: changed ? {
      ...Object.keys(result.restore).length === 0 ? {} : { set: result.restore },
      ...result.remove.length === 0 ? {} : { unset: result.remove }
    } : void 0,
    set: new Set(result.set)
  };
}
function modifyBlock(old, mod, id2, failures) {
  const state2 = changes(
    old.state,
    mod.state ?? {},
    mod.unset ?? [],
    (arg) => `The change to block "${id2}" both sets and unsets "${arg}".`,
    failures
  );
  const name3 = mod.name === old.name ? void 0 : mod.name;
  return {
    value: { ...old, state: state2.next, name: name3 ?? old.name },
    inverse: {
      ...Object.keys(state2.restore).length === 0 ? {} : { state: state2.restore },
      ...state2.remove.length === 0 ? {} : { unset: state2.remove },
      ...name3 === void 0 ? {} : { name: old.name }
    }
  };
}
function modifyFields(old, mod) {
  const inverse = {};
  const changed = {};
  for (const field of Object.keys(mod)) {
    if (jsonEqual(old[field], mod[field])) continue;
    inverse[field] = old[field];
    changed[field] = mod[field];
  }
  return { value: { ...old, ...changed }, inverse };
}

// srcts/store/delta.ts
function isDeltas(delta) {
  return Array.isArray(delta);
}

// srcts/store/error.ts
var StoreError = class extends Error {
  failures;
  constructor(message, failures) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"));
    this.name = "StoreError";
    this.failures = failures;
  }
};

// srcts/store/expand.ts
var ROUNDS = 10;
function expand(delta, board, registrations) {
  let expanded = delta;
  for (let round = 0; round < ROUNDS; round++) {
    const adding = [];
    const failures = [];
    const core = expandCore(expanded, board);
    const grew = core !== expanded;
    expanded = core;
    for (const [id2, plugin] of Object.entries(board.plugins)) {
      const def = registrations.plugin(plugin.class)?.def;
      if (def?.update === void 0) continue;
      let patches2;
      try {
        patches2 = def.update(expanded, board, id2);
      } catch (error) {
        failures.push(`The plugin class "${def.id}" failed to update the delta for the plugin "${id2}": ${messageOf(error)}`);
        continue;
      }
      const wrong = misplaced(def.id, id2, patches2);
      if (wrong.length > 0) {
        failures.push(...wrong);
        continue;
      }
      const merged = merge(expanded, delta, patches2);
      if (merged !== expanded) adding.push(id2);
      expanded = merged;
    }
    if (failures.length > 0) return { failures };
    if (!grew && adding.length === 0) return expanded;
    if (round === ROUNDS - 1) {
      return {
        failures: [
          `The delta kept expanding after ${String(ROUNDS)} rounds, through the update of the plugin${adding.length > 1 ? "s" : ""} ${quoted(adding)}.`
        ]
      };
    }
  }
  return expanded;
}
function expandCore(delta, board) {
  const gone = new Set(delta.blocks?.rm ?? []);
  if (gone.size === 0) return delta;
  const linksRm = delta.links?.rm ?? [];
  const incident = Object.entries(board.links).filter(([id2, link]) => (gone.has(link.from) || gone.has(link.to)) && !linksRm.includes(id2)).map(([id2]) => id2);
  const stacksRm = delta.stacks?.rm ?? [];
  const stacksMod = delta.stacks?.mod ?? {};
  const placed = {};
  for (const [id2, stack] of Object.entries(board.stacks)) {
    if (stacksRm.includes(id2)) continue;
    const mod = own(stacksMod, id2);
    const blocks = mod?.blocks ?? stack.blocks;
    if (blocks.some((block) => gone.has(block))) {
      placed[id2] = { ...mod, blocks: blocks.filter((block) => !gone.has(block)) };
    }
  }
  if (incident.length === 0 && Object.keys(placed).length === 0) return delta;
  return {
    ...delta,
    ...incident.length === 0 ? {} : { links: { ...delta.links, rm: [...linksRm, ...incident] } },
    ...Object.keys(placed).length === 0 ? {} : { stacks: { ...delta.stacks, mod: { ...stacksMod, ...placed } } }
  };
}
function misplaced(cls, id2, patches2) {
  const of = `The plugin class "${cls}" updated the delta for the plugin "${id2}"`;
  if (!Array.isArray(patches2)) return [`${of} with no array of patches.`];
  const failures = [];
  for (const patch2 of patches2) {
    const problems = malformed(patch2);
    if (problems.length > 0) {
      failures.push(`${of} with a patch that ${problems.join(", ")}.`);
      continue;
    }
    const { path } = patch2;
    const [top, plugin, part] = pointer(path) ?? [];
    if (top !== "plugins" || plugin !== id2 || part !== "state") {
      failures.push(`${of} with a patch at "${path}", outside that plugin's state.`);
    }
  }
  return failures;
}
function merge(delta, given, patches2) {
  const held2 = delta.patches ?? [];
  const fixed2 = (given.patches ?? []).map(({ path }) => pointer(path) ?? []);
  const added = [];
  for (const patch2 of patches2) {
    const parts = pointer(patch2.path) ?? [];
    if ([...held2, ...added].some((other) => same(other, patch2))) continue;
    if (fixed2.some((path) => overlap(path, parts))) continue;
    added.push(patch2);
  }
  if (added.length === 0) return delta;
  return { ...delta, patches: [...held2, ...added] };
}
function same(a2, b2) {
  return a2.op === b2.op && a2.path === b2.path && jsonEqual(a2.value, b2.value);
}

// srcts/store/undo.ts
function fit(delta, board) {
  const blocks = collection2(board.blocks, delta.blocks);
  const kept = /* @__PURE__ */ new Set([
    ...Object.keys(board.blocks).filter((id2) => !(blocks.rm ?? []).includes(id2)),
    ...Object.keys(blocks.add ?? {})
  ]);
  const holds = (id2) => kept.has(id2);
  const links = collection2(
    board.links,
    delta.links,
    (link) => (link.from === void 0 || holds(link.from)) && (link.to === void 0 || holds(link.to))
  );
  const before = Object.fromEntries(
    Object.entries(delta.links?.before ?? {}).filter(([id2]) => Object.hasOwn(links.add ?? {}, id2))
  );
  const stacks = collection2(board.stacks, delta.stacks);
  const placed = (stack) => stack.blocks === void 0 ? stack : { ...stack, blocks: stack.blocks.filter(holds) };
  return prune({
    blocks,
    links: { ...links, ...Object.keys(before).length === 0 ? {} : { before } },
    stacks: {
      ...stacks.rm === void 0 ? {} : { rm: stacks.rm },
      ...stacks.add === void 0 ? {} : { add: Object.fromEntries(Object.entries(stacks.add).map(([id2, s2]) => [id2, placed(s2)])) },
      ...stacks.mod === void 0 ? {} : { mod: Object.fromEntries(Object.entries(stacks.mod).map(([id2, s2]) => [id2, placed(s2)])) }
    },
    ...delta.options === void 0 ? {} : { options: delta.options },
    ...delta.name === void 0 ? {} : { name: delta.name },
    patches: patches(delta.patches ?? [], board)
  });
}
function patches(given, board) {
  let at = board;
  const fitted = [];
  for (const patch2 of given) {
    const parts = pointer(patch2.path) ?? [];
    if (parts[0] !== "plugins") continue;
    const applied = applyPatch(at, patch2, parts);
    if ("failure" in applied) continue;
    at = applied.next;
    fitted.push(patch2);
  }
  return fitted;
}
function collection2(current2, part, keep = () => true) {
  if (part === void 0) return {};
  const rm = (part.rm ?? []).filter((id2) => Object.hasOwn(current2, id2));
  const there = (id2) => Object.hasOwn(current2, id2) && !rm.includes(id2);
  const add = Object.entries(part.add ?? {}).filter(([id2, entry]) => !there(id2) && keep(entry));
  const mod = Object.entries(part.mod ?? {}).filter(([id2, change]) => there(id2) && keep(change));
  return {
    ...rm.length === 0 ? {} : { rm },
    ...add.length === 0 ? {} : { add: Object.fromEntries(add) },
    ...mod.length === 0 ? {} : { mod: Object.fromEntries(mod) }
  };
}
function prune(delta) {
  const parts = Object.entries(delta);
  return Object.fromEntries(
    parts.filter(
      ([, part]) => Array.isArray(part) ? part.length > 0 : !isObject(part) || Object.keys(part).length > 0
    )
  );
}
function isEmpty(delta) {
  return Object.keys(prune(delta)).length === 0;
}

// srcts/store/registrations.ts
var BASIC_OPTIONS = ["show_conditions", "dark_mode"];
function createRegistrations() {
  const classes = y(/* @__PURE__ */ new Map());
  const options = y([]);
  const migrations = y(/* @__PURE__ */ new Map());
  const optionIndex = O(
    () => new Map(options.value.map((option) => [option.def.id, option]))
  );
  const classDefs = O(
    () => Object.fromEntries([...classes.value].map(([id2, cls]) => [id2, cls.def]))
  );
  const optionDefs = O(
    () => Object.fromEntries(options.value.map((option) => [option.def.id, option.def]))
  );
  function checkInitial(what, validate2, initial, field) {
    if (initial === void 0) return;
    const failures = validate2(initial);
    if (failures.length > 0) {
      throw new Error(`The ${field} of ${what} ${failures.join(" ")}`);
    }
  }
  function registerPlugin(def) {
    const { id: id2 } = def;
    if (typeof id2 !== "string" || !isPluginClassId(id2)) {
      throw new Error(
        `The plugin class id ${JSON.stringify(id2)} is not named as R names a plugin class, "{pkg}-{name}_plugin".`
      );
    }
    if (classes.value.has(id2)) {
      throw new Error(`The plugin class "${id2}" is registered already.`);
    }
    const given = def;
    for (const [member, article] of [["update", "an"], ["migrate", "a"], ["start", "a"]]) {
      if (given[member] !== void 0 && typeof given[member] !== "function") {
        throw new Error(`The plugin class "${id2}" has ${article} ${member} that is no function.`);
      }
    }
    for (const member of ["ui", "frontend"]) {
      if (given[member] !== void 0 && (typeof given[member] !== "string" || given[member] === "")) {
        throw new Error(`The plugin class "${id2}" has a ${member} that is no module's id.`);
      }
    }
    const storage = given.storage;
    if (storage !== void 0 && (!isObject(storage) || typeof storage.save !== "function" || typeof storage.open !== "function")) {
      throw new Error(`The plugin class "${id2}" has a storage without save() and open() functions.`);
    }
    if (given.options !== void 0 && (!Array.isArray(given.options) || !given.options.every((option) => typeof option === "string"))) {
      throw new Error(`The plugin class "${id2}" requires board options that are no array of their ids.`);
    }
    const what = `the state of the plugin class "${id2}"`;
    let validate2 = () => [];
    const state2 = given.state;
    if (state2 !== void 0) {
      if (!isObject(state2) || !isObject(state2.schema) || !Object.hasOwn(state2, "default")) {
        throw new Error(`The plugin class "${id2}" gives a state without a schema and a default.`);
      }
      validate2 = compile(what, state2.schema);
      checkInitial(what, validate2, state2.default, "default");
    }
    const item = { def, validate: validate2 };
    classes.value = new Map([...classes.value, [id2, item]]);
    return () => {
      if (classes.value.get(id2) !== item) return;
      const rest = new Map(classes.value);
      rest.delete(id2);
      classes.value = rest;
    };
  }
  return {
    plugins: {
      register: registerPlugin,
      registered: () => classDefs.value
    },
    options: {
      register(def) {
        const what = `the board option "${def.id}"`;
        if (optionIndex.value.has(def.id)) {
          throw new Error(`The board option "${def.id}" is registered already.`);
        }
        const ui = def.ui;
        if (!isObject(ui) || typeof ui.render !== "function") {
          throw new Error(`The board option "${def.id}" has no UI module with a render() function.`);
        }
        const { onChange } = def;
        if (onChange !== void 0 && typeof onChange !== "function") {
          throw new Error(`The onChange of the board option "${def.id}" is no function.`);
        }
        if (def.migrate !== void 0 && typeof def.migrate !== "function") {
          throw new Error(`The board option "${def.id}" has a migrate that is no function.`);
        }
        const validate2 = compile(what, def.schema);
        if (typeof def.default !== "function") {
          checkInitial(what, validate2, def.default, "default");
        }
        const item = { def, validate: validate2 };
        options.value = [...options.value, item];
        return () => {
          options.value = options.value.filter((x2) => x2 !== item);
        };
      },
      registered: () => optionDefs.value
    },
    registerMigration(registryId2, fn) {
      if (migrations.value.has(registryId2)) {
        throw new Error(`The block class "${registryId2}" has a migration registered already.`);
      }
      migrations.value = new Map([...migrations.value, [registryId2, fn]]);
      return () => {
        if (migrations.value.get(registryId2) !== fn) return;
        const rest = new Map(migrations.value);
        rest.delete(registryId2);
        migrations.value = rest;
      };
    },
    plugin: (id2) => classes.value.get(id2),
    option: (id2) => optionIndex.value.get(id2),
    migration: (registryId2) => migrations.value.get(registryId2)
  };
}
function isPluginClassId(id2) {
  const pkg = packageOf(id2);
  return pkg !== null && /^.+_plugin$/.test(id2.slice(pkg.length + 1));
}

// srcts/store/open.ts
var FORMAT = 1;
var COLLECTIONS = ["blocks", "links", "stacks", "options", "plugins"];
var PROPERTIES = ["id", "name", ...COLLECTIONS];
var refused = "The store refused to open the board";
function parse(text) {
  try {
    return JSON.parse(text);
  } catch (error) {
    throw new StoreError(refused, [`The text holds no JSON: ${messageOf(error)}`]);
  }
}
function checkJsonBoard(json) {
  if (!isObject(json)) throw new StoreError(refused, ["The JSON is no JSON board."]);
  if (!Object.hasOwn(json, "format")) {
    throw new StoreError(refused, [
      "The JSON board has no format, as a file of today's stack has none, which a conversion outside core turns into a JSON board first."
    ]);
  }
  if (typeof json.format !== "number" || !Number.isInteger(json.format)) {
    throw new StoreError(refused, [`The JSON board's format ${JSON.stringify(json.format)} is no whole number.`]);
  }
  if (json.format > FORMAT) {
    throw new StoreError(refused, [
      `The JSON board's format is ${String(json.format)}, while this client reads format ${String(FORMAT)} at most.`
    ]);
  }
  if (!isObject(json.packages) || !Object.values(json.packages).every((v2) => typeof v2 === "string")) {
    throw new StoreError(refused, ["The JSON board names no packages with their versions."]);
  }
  if (!isObject(json.board)) throw new StoreError(refused, ["The JSON board holds no board."]);
  const board = json.board;
  const lacking = COLLECTIONS.filter((name3) => !isObject(board[name3]));
  if (lacking.length > 0) {
    throw new StoreError(refused, [`The board holds its ${listing(lacking)} in no object by id.`]);
  }
  checkVersions(board);
  return json;
}
function isVersion(value2) {
  return typeof value2 === "number" && Number.isInteger(value2) && value2 >= 1;
}
function checkVersions(board) {
  const failures = [];
  if (!isObject(board.versions) || !Object.values(board.versions).every(isVersion)) {
    failures.push("The board records the versions of its plugin classes as no whole numbers from 1 by class.");
  }
  for (const [id2, block] of Object.entries(board.blocks)) {
    if (isObject(block) && !isVersion(block.version)) {
      failures.push(`The block "${id2}" records no version of its class's state.`);
    }
  }
  for (const [id2, option] of Object.entries(board.options)) {
    if (!isObject(option) || !Object.hasOwn(option, "value") || !isVersion(option.version)) {
      failures.push(`The board option "${id2}" is recorded as no value with the version of its shape.`);
    }
  }
  if (failures.length > 0) throw new StoreError(refused, failures);
}
function unversioned(board) {
  const record = board;
  return {
    ...Object.fromEntries(Object.entries(record).filter(([name3]) => PROPERTIES.includes(name3))),
    blocks: Object.fromEntries(
      Object.entries(board.blocks).map(([id2, block]) => [
        id2,
        without(block, ["version"])
      ])
    ),
    options: Object.fromEntries(Object.entries(board.options).map(([id2, option]) => [id2, option.value]))
  };
}
function migrate(board, json, reply, registrations) {
  const failures = /* @__PURE__ */ new Set();
  const changed = /* @__PURE__ */ new Set();
  const plugins = Object.fromEntries(
    Object.entries(board.plugins).map(([id2, plugin]) => {
      const cls = classOf(plugin);
      const def = cls === void 0 ? void 0 : registrations.plugin(cls)?.def;
      if (cls === void 0 || def === void 0) return [id2, plugin];
      const from = own(json.versions, cls);
      const to = own(reply.plugin_versions, cls) ?? 1;
      if (from === void 0 || from === to) return [id2, plugin];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the plugin class "${cls}", while R gives version ${String(to)}.`
        );
        return [id2, plugin];
      }
      if (def.migrate === void 0) return [id2, plugin];
      changed.add("plugins");
      const state2 = migratedValue(
        `The migration of the plugin class "${cls}"`,
        () => def.migrate?.(frozenCopy(plugin.state), from)
      );
      return [id2, { ...plugin, state: state2 }];
    })
  );
  const blocks = Object.fromEntries(
    Object.entries(board.blocks).map(([id2, block]) => {
      const from = own(json.blocks, id2)?.version;
      const to = own(reply.block_registry, block.registry_id)?.version;
      if (from === void 0 || to === void 0 || from === to) return [id2, block];
      if (from > to) {
        failures.add(
          `The block "${id2}" records version ${String(from)} of the state of "${block.registry_id}", while its class is at version ${String(to)}.`
        );
        return [id2, block];
      }
      const fn = registrations.migration(block.registry_id);
      if (fn === void 0) return [id2, block];
      changed.add("blocks");
      const state2 = migrated(
        `The migration of the block class "${block.registry_id}"`,
        () => fn(frozenCopy(block.state), from)
      );
      return [id2, { ...block, state: state2 }];
    })
  );
  const options = Object.fromEntries(
    Object.entries(board.options).map(([id2, value2]) => {
      const from = own(json.options, id2)?.version;
      const to = own(reply.option_versions, id2) ?? 1;
      if (from === void 0 || from === to) return [id2, value2];
      if (from > to) {
        failures.add(
          `The board records version ${String(from)} of the value of the board option "${id2}", while R gives version ${String(to)}.`
        );
        return [id2, value2];
      }
      const def = registrations.option(id2)?.def;
      if (def?.migrate === void 0) return [id2, value2];
      changed.add("options");
      return [
        id2,
        migratedValue(`The migration of the board option "${id2}"`, () => def.migrate?.(frozenCopy(value2), from))
      ];
    })
  );
  if (failures.size > 0) throw new StoreError(refused, [...failures]);
  if (changed.size === 0) return board;
  return { ...board, plugins, blocks, options };
}
function migrated(what, fn) {
  const out = migratedValue(what, fn);
  if (!isObject(out)) throw new StoreError(refused, [`${what} returned no object.`]);
  return out;
}
function migratedValue(what, fn) {
  let out;
  try {
    out = fn();
  } catch (error) {
    throw new StoreError(refused, [`${what} failed: ${messageOf(error)}`]);
  }
  if (out === void 0) throw new StoreError(refused, [`${what} returned nothing.`]);
  return frozenCopy(out);
}
function openJsonBoard(json, reply, registrations) {
  const opening2 = openingPlugins(reply.board);
  const pluginClass = (id2) => registrations.plugin(id2)?.def;
  const read = unversioned(json.board);
  const held2 = keptPlugins(read.plugins, opening2, pluginClass);
  const plugins = Object.entries(read.plugins).flatMap(([id2, plugin]) => {
    const cls = classOf(plugin);
    return cls === void 0 || Object.hasOwn(held2, id2) ? [] : [{ plugin: id2, class: cls, package: packageOf(cls) }];
  });
  const migrated2 = migrate({ ...read, plugins: held2 }, json.board, reply, registrations);
  const gone = Object.entries(migrated2.blocks).filter(([, block]) => own(reply.block_registry, block.registry_id) === void 0).map(([id2]) => id2);
  const blocks = gone.map((id2) => {
    const registryId2 = migrated2.blocks[id2]?.registry_id ?? "";
    return { block: id2, registry_id: registryId2, package: packageOf(registryId2) };
  });
  const kept = remove(prune2(migrated2, registrations), gone, registrations);
  const board = { ...kept, plugins: appPlugins(kept.plugins, opening2, pluginClass) };
  const failures = check(board, reply.block_registry, registrations);
  if (failures.length > 0) throw new StoreError(refused, failures);
  const packages = Object.entries(json.packages).filter(([pkg]) => own(reply.packages, pkg) === void 0).map(([pkg, version]) => ({ package: pkg, version }));
  return { board, dropped: [...packages, ...blocks, ...plugins] };
}
function classOf(plugin) {
  return isObject(plugin) && typeof plugin.class === "string" ? plugin.class : void 0;
}
function openingPlugins(opening2) {
  const plugins = isObject(opening2) && isObject(opening2.board) ? opening2.board.plugins : void 0;
  if (!isObject(plugins)) return {};
  return Object.fromEntries(
    Object.entries(plugins).filter(
      (entry) => isObject(entry[1]) && typeof entry[1].class === "string"
    )
  );
}
function prune2(board, registrations) {
  const unfit = Object.entries(board.options).filter(([id2, value2]) => {
    const def = registrations.option(id2);
    return def === void 0 || def.validate(value2).length > 0;
  }).map(([id2]) => id2);
  return { ...board, options: without(board.options, unfit) };
}
function remove(board, gone, registrations) {
  if (gone.length === 0) return board;
  const delta = expand({ blocks: { rm: gone } }, board, registrations);
  if ("failures" in delta) throw new StoreError(refused, delta.failures);
  const committed = commit(board, delta);
  if ("failures" in committed) throw new StoreError(refused, committed.failures);
  return committed.board;
}
async function required(board, registrations) {
  const lacking = BASIC_OPTIONS.filter((id2) => !Object.hasOwn(board.options, id2)).map((id2) => ({
    id: id2,
    option: registrations.option(id2)
  }));
  const missing = lacking.filter(({ option }) => option === void 0);
  if (missing.length > 0) {
    throw new StoreError(
      refused,
      missing.map(
        ({ id: id2 }) => `Core's basic frontend requires the board option "${id2}", which no loaded package defines.`
      )
    );
  }
  const defined = lacking.flatMap(({ id: id2, option }) => option === void 0 ? [] : [{ id: id2, option }]);
  const values2 = await Promise.all(
    defined.map(async ({ id: id2, option }) => {
      const what = `The default of the board option "${id2}", which core's basic frontend requires,`;
      let value2;
      try {
        const given = option.def.default;
        value2 = typeof given === "function" ? await given() : given;
      } catch (error) {
        return `${what} failed: ${messageOf(error)}`;
      }
      const failures2 = option.validate(value2);
      return failures2.length > 0 ? `${what} ${failures2.join(" ")}` : [id2, value2];
    })
  );
  const failures = values2.filter((entry) => typeof entry === "string");
  if (failures.length > 0) throw new StoreError(refused, failures);
  return Object.fromEntries(values2.filter((entry) => typeof entry !== "string"));
}

// srcts/store/store.ts
var ENDED = "Another board opened in a new store, so this store changes its board no more.";
function createStore(options = {}) {
  const registrations = createRegistrations();
  const board = y(void 0);
  let hello = options.hello;
  const registry = y(options.hello?.block_registry ?? {});
  const packages = y(options.hello?.packages ?? {});
  const history = y(/* @__PURE__ */ new Map());
  const subscribers = /* @__PURE__ */ new Set();
  let opening2 = false;
  const ended = () => options.signal?.aborted === true;
  options.signal?.addEventListener(
    "abort",
    () => {
      subscribers.clear();
    },
    { once: true }
  );
  const edits = /* @__PURE__ */ new Map();
  let lastStep = 0;
  let busy = false;
  function guard2() {
    if (busy) {
      throw new Error(
        "The store is applying or announcing a change, which a subscriber, a plugin class's update or a migration may not change, since what follows from a delta is a plugin class's update."
      );
    }
  }
  function current2(refused2) {
    if (ended()) throw new StoreError(refused2, [ENDED]);
    if (board.value !== void 0) return board.value;
    throw new StoreError(refused2, ["The store holds no board until it opens one."]);
  }
  function announce2(change, next) {
    for (const fn of [...subscribers]) {
      try {
        fn(change, next);
      } catch (error) {
        report(error);
      }
    }
  }
  function steps(origin) {
    return history.value.get(origin) ?? { undo: [], redo: [] };
  }
  function setSteps(origin, steps2) {
    history.value = new Map([...history.value, [origin, steps2]]);
  }
  function run(start2, deltas, how, refused2) {
    const applied = [];
    const inverses = [];
    let current3 = start2;
    for (const [i2, given] of deltas.entries()) {
      const prefix = deltas.length > 1 ? `Delta ${String(i2 + 1)}: ` : "";
      const fail = (failures2) => new StoreError(refused2, failures2.map((failure) => prefix + failure));
      const delta = how.fit === true ? fit(given, current3) : given;
      if (how.fit === true && isEmpty(delta)) continue;
      const expanded = expand(delta, current3, registrations);
      if ("failures" in expanded) throw fail(expanded.failures);
      const committed = commit(current3, expanded, how.places?.[i2]);
      if ("failures" in committed) throw fail(committed.failures);
      if (isEmpty(committed.inverse.delta)) continue;
      const failures = check(
        committed.board,
        registry.value,
        registrations,
        committed.changed
      );
      if (failures.length > 0) throw fail(failures);
      applied.push({ delta: expanded, board: committed.board, changed: committed.changed });
      inverses.unshift(committed.inverse);
      current3 = committed.board;
    }
    return { applied, inverses };
  }
  function exists(next, key) {
    const [kind, id2] = JSON.parse(key);
    return Object.hasOwn(kind === "block" ? next.blocks : next.plugins, id2);
  }
  function describe2(key) {
    const [kind, id2] = JSON.parse(key);
    return kind === "block" ? `block "${id2}"` : `the plugin "${id2}"`;
  }
  function named2(deltas) {
    const failures = [];
    const compare = (key, edit) => {
      if (edit === void 0) return;
      const id2 = edits.get(key);
      if (id2 !== edit) {
        failures.push(
          id2 === void 0 ? `The delta was built on edit ${String(edit)} of ${describe2(key)}, which the board lacks.` : `The delta was built on edit ${String(edit)} of ${describe2(key)}, which another edit has replaced with edit ${String(id2)}.`
        );
      }
    };
    for (const delta of deltas) {
      for (const [id2, mod] of Object.entries(delta.blocks?.mod ?? {})) {
        compare(target({ block: id2 }), mod.edit);
      }
      for (const patch2 of delta.patches ?? []) {
        const plugin = pluginOf(patch2.path);
        if (plugin !== void 0) compare(target({ plugin }), patch2.edit);
      }
    }
    return [...new Set(failures)];
  }
  function number(applied, last) {
    const step2 = ++lastStep;
    for (const key of new Set(applied.flatMap(({ changed }) => [...changed.targets]))) {
      if (exists(last, key)) edits.set(key, step2);
      else edits.delete(key);
    }
  }
  function apply(delta, origin = "user") {
    guard2();
    const deltas = isDeltas(delta) ? delta : [delta];
    if (deltas.length === 0) return;
    busy = true;
    try {
      const refused2 = "The store rejected the delta";
      const start2 = current2(refused2);
      const failures = named2(deltas);
      if (failures.length > 0) throw new StoreError(refused2, failures);
      const { applied, inverses } = run(start2, deltas, {}, refused2);
      if (applied.length === 0) return;
      number(applied, applied.at(-1)?.board ?? start2);
      setSteps(origin, { undo: [...steps(origin).undo, inverses], redo: [] });
      for (const { delta: delta2, board: next } of applied) {
        board.value = next;
        announce2({ delta: delta2, origin }, next);
      }
    } finally {
      busy = false;
    }
  }
  function revert(origin, back) {
    guard2();
    const { undo, redo } = steps(origin);
    const step2 = back ? undo.at(-1) : redo.at(-1);
    if (step2 === void 0) return;
    busy = true;
    try {
      const refused2 = back ? "The store refused to undo the step" : "The store refused to redo the step";
      const start2 = current2(refused2);
      const { applied, inverses } = run(
        start2,
        step2.map(({ delta }) => delta),
        { fit: true, places: step2.map(({ places }) => places) },
        refused2
      );
      number(applied, applied.at(-1)?.board ?? start2);
      setSteps(
        origin,
        back ? { undo: undo.slice(0, -1), redo: [...redo, inverses] } : { undo: [...undo, inverses], redo: redo.slice(0, -1) }
      );
      for (const { delta, board: next } of applied) {
        board.value = next;
        announce2({ delta, origin }, next);
      }
    } finally {
      busy = false;
    }
  }
  const refusedOpen = "The store refused to open the board";
  function jsonOf(given) {
    if (typeof given !== "string") {
      hello = given;
      registry.value = given.block_registry;
      packages.value = given.packages;
      return { json: checkJsonBoard(given.board), reply: given };
    }
    if (hello === void 0) {
      throw new StoreError(refusedOpen, ["The store opens a JSON board only once R has answered `hello`."]);
    }
    if (hello.lock !== null) {
      throw new StoreError(refusedOpen, ["The board is locked, so it opens no other board."]);
    }
    return { json: checkJsonBoard(parse(given)), reply: hello };
  }
  async function build(json, reply) {
    guard2();
    let opened;
    busy = true;
    try {
      opened = openJsonBoard(json, reply, registrations);
    } finally {
      busy = false;
    }
    const values2 = await required(opened.board, registrations);
    const next = Object.keys(values2).length === 0 ? opened.board : { ...opened.board, options: { ...opened.board.options, ...values2 } };
    return { board: next, dropped: opened.dropped };
  }
  function stillOpen() {
    if (ended()) throw new StoreError(refusedOpen, [ENDED]);
  }
  async function open(given) {
    stillOpen();
    if (opening2) {
      throw new StoreError(refusedOpen, [
        "The store holds a board or opens one already: a store opens one board, and core opens the next in a new store as it starts again."
      ]);
    }
    opening2 = true;
    try {
      const { json, reply } = jsonOf(given);
      await options.loaded?.(reply);
      stillOpen();
      const opened = await build(json, reply);
      stillOpen();
      guard2();
      busy = true;
      try {
        board.value = opened.board;
        const step2 = ++lastStep;
        for (const key of everyTarget(opened.board)) edits.set(key, step2);
        announce2({ opened: opened.board, dropped: opened.dropped }, opened.board);
        return opened.dropped;
      } finally {
        busy = false;
      }
    } catch (error) {
      opening2 = board.value !== void 0;
      throw error;
    }
  }
  async function checkText(text) {
    stillOpen();
    const { json, reply } = jsonOf(text);
    const opened = await build(json, reply);
    stillOpen();
    return opened.dropped;
  }
  function jsonBoard() {
    const next = board.value;
    if (next === void 0) {
      throw new Error("The store holds no board until it opens one.");
    }
    const classes = [...new Set(Object.values(next.plugins).map((plugin) => plugin.class))];
    const owned = /* @__PURE__ */ new Set([
      ...classes.flatMap((cls) => packageOf(cls) ?? []),
      ...Object.values(next.blocks).flatMap(({ registry_id }) => packageOf(registry_id) ?? [])
    ]);
    const recorded = Object.fromEntries(
      [...owned].sort().flatMap((pkg) => {
        const version = own(packages.value, pkg);
        return version === void 0 ? [] : [[pkg, version]];
      })
    );
    const versions = Object.fromEntries(
      classes.map((cls) => [cls, own(hello?.plugin_versions ?? {}, cls) ?? 1])
    );
    const blocks = Object.fromEntries(
      Object.entries(next.blocks).map(([id3, block]) => [
        id3,
        {
          registry_id: block.registry_id,
          version: own(registry.value, block.registry_id)?.version ?? 1,
          state: block.state,
          name: block.name
        }
      ])
    );
    const options2 = Object.fromEntries(
      Object.entries(next.options).map(([id3, value2]) => [
        id3,
        { value: value2, version: own(hello?.option_versions ?? {}, id3) ?? 1 }
      ])
    );
    const { id: id2, name: name3, links, stacks, plugins } = next;
    return {
      format: FORMAT,
      packages: recorded,
      board: { id: id2, versions, name: name3, blocks, links, stacks, options: options2, plugins }
    };
  }
  function save() {
    return `${JSON.stringify(jsonBoard(), null, 2)}
`;
  }
  function everyTarget(next) {
    const targets = [
      ...Object.keys(next.blocks).map((id2) => ({ block: id2 })),
      ...Object.keys(next.plugins).map((id2) => ({ plugin: id2 }))
    ];
    return targets.map(target);
  }
  return {
    board() {
      if (board.value === void 0) {
        throw new Error("The store holds no board until it opens one.");
      }
      return board.value;
    },
    hasBoard: () => board.value !== void 0,
    subscribe(fn) {
      if (ended()) return () => void 0;
      const wrapped = (change, next) => {
        fn(change, next);
      };
      subscribers.add(wrapped);
      return () => {
        subscribers.delete(wrapped);
      };
    },
    apply,
    open,
    check: checkText,
    undo: (origin = "user") => {
      revert(origin, true);
    },
    redo: (origin = "user") => {
      revert(origin, false);
    },
    canUndo: (origin = "user") => steps(origin).undo.length > 0,
    canRedo: (origin = "user") => steps(origin).redo.length > 0,
    plugins: registrations.plugins,
    registerMigration: (registryId2, fn) => registrations.registerMigration(registryId2, fn),
    options: registrations.options,
    jsonBoard,
    save,
    blockRegistry: () => registry.value,
    packages: () => packages.value,
    editId: (of) => edits.get(target(of))
  };
}

// srcts/commands/schemas.ts
var id = { type: "string", pattern: "\\D" };
var blockId = {
  ...id,
  description: "The id of a block, unique among the board's blocks."
};
var linkId = {
  ...id,
  description: "The id of a link, unique among the board's links."
};
var stackId = {
  ...id,
  description: "The id of a stack, unique among the board's stacks."
};
var registryId = {
  type: "string",
  minLength: 1,
  description: `A block class's id in the block registry, such as "blockr.core-head_block".`
};
var name2 = {
  type: "string",
  description: "A name that the user sees and may change, which nothing refers to."
};
var input = {
  type: "string",
  description: 'An input of the block a link goes to, such as "data", or empty for a variadic block, which takes such links in their order.'
};
var state = {
  type: "object",
  description: "Values of a block's state args, which the schema of its class types."
};
var stateArgs = {
  type: "array",
  items: { type: "string" },
  uniqueItems: true,
  description: "Names of a block's state args."
};
var blockIds = {
  type: "array",
  items: blockId,
  uniqueItems: true,
  description: "Ids of blocks, in order."
};
var color = {
  type: ["string", "null"],
  pattern: HEX_COLOR,
  description: 'A hex colour, such as "#66c2a5", which front-ends draw a stack in, or null for none. A front-end draws a stack without a colour in a default of its own.'
};
var optionId = {
  type: "string",
  minLength: 1,
  description: 'The id of a board option, such as "n_rows".'
};
var value = {
  description: "A JSON value, which the schema of what it is set on types."
};
var pluginId = {
  ...id,
  description: "The id of a plugin, unique among the board's plugins."
};
var statePatches = {
  type: "array",
  items: {
    type: "object",
    properties: {
      op: { enum: ["add", "remove", "replace"] },
      path: {
        type: "string",
        pattern: "^(/.*)?$",
        description: `A JSON Pointer within the plugin's state, such as "/positions/a", or "" for the whole state.`
      },
      value: { description: "What an add or a replace puts at the path." }
    },
    required: ["op", "path"],
    additionalProperties: false
  },
  description: "Changes of a plugin's state, as JSON Patch operations, which apply in their order and which the schema of the plugin's class types."
};
var described = /* @__PURE__ */ new WeakMap();
function stateSchema(entry) {
  const known = described.get(entry);
  if (known !== void 0) return known;
  const { properties } = entry.schema;
  let schema = entry.schema;
  if (isRecord(properties)) {
    schema = {
      ...schema,
      properties: Object.fromEntries(
        Object.entries(properties).map(([arg, property]) => {
          const spec = own(entry.arguments, arg);
          if (spec === void 0 || !isRecord(property)) return [arg, property];
          return [
            arg,
            {
              ...spec.description === void 0 ? {} : { description: spec.description },
              ...spec.example === void 0 ? {} : { examples: [spec.example] },
              ...property
            }
          ];
        })
      )
    };
  }
  described.set(entry, schema);
  return schema;
}
function isRecord(x2) {
  return isObject(x2);
}

// srcts/commands/core.ts
var newBlock = {
  registry_id: { ...registryId, description: "The class of the new block." },
  id: {
    ...blockId,
    description: "The new block's id, which is minted where it is left out."
  },
  name: {
    ...name2,
    description: "The new block's name, which its class gives where it is left out."
  },
  state: {
    ...state,
    description: "Values of the new block's state args, which the schema of its class types, in place of those its class starts a block with."
  }
};
function coreCommands(store, options = {}) {
  const validators = /* @__PURE__ */ new WeakMap();
  function stateFailures(entry, state2) {
    let validate2 = validators.get(entry);
    if (validate2 === void 0) {
      validate2 = compile(`the state of the block class "${entry.id}"`, stateSchema(entry));
      validators.set(entry, validate2);
    }
    return validate2(state2);
  }
  function classOf2(command2, registryId2) {
    const entry = own(store.blockRegistry(), registryId2);
    if (entry === void 0) {
      throw refusal(command2, [`The block registry has no block class "${registryId2}".`]);
    }
    return entry;
  }
  function added(command2, board2, args) {
    const entry = classOf2(command2, args.registry_id);
    const state2 = { ...entry.state, ...args.state };
    const failures = stateFailures(entry, state2);
    if (failures.length > 0) {
      throw refusal(command2, failures.map((failure) => `The state of the new block ${failure}`));
    }
    const id2 = args.id ?? mintId((id3) => Object.hasOwn(board2.blocks, id3));
    return [
      id2,
      { registry_id: entry.id, state: state2, name: args.name ?? blockName(entry.id) },
      entry
    ];
  }
  function linkable(board2, to, given) {
    if (!Object.hasOwn(board2.blocks, to)) return false;
    return named(given) || (freeInputs(board2, store.blockRegistry(), to)?.length ?? 0) > 0;
  }
  function linkInput(command2, board2, to, given, except) {
    if (named(given)) return given;
    const free = freeInputs(board2, store.blockRegistry(), to, except) ?? [];
    return one(
      command2,
      free,
      `The block "${to}" has no input that no link feeds.`,
      `The block "${to}" has the free inputs ${quoted(free)}, so the args have to name the one to use.`
    );
  }
  function newInput(command2, entry, given) {
    if (named(given)) return given;
    if (entry.inputs.includes("...")) return "";
    return one(
      command2,
      entry.inputs,
      `The block class "${entry.id}" has no input to take a link.`,
      `The block class "${entry.id}" has the inputs ${quoted(entry.inputs)}, so the args have to name the one to use.`
    );
  }
  const blocks = [
    command({
      id: "blockr.core-add_block",
      label: "Add block",
      description: "Adds a block of a class from the block registry, with the state its class starts a block with, changed by the state args given.",
      args: object(newBlock, ["registry_id"]),
      run(board2, args) {
        const [id2, block] = added(this.id, board2, args);
        return { blocks: { add: { [id2]: block } } };
      }
    }),
    command({
      id: "blockr.core-remove_block",
      label: "Remove block",
      description: "Removes a block, with its links and its place in its stack.",
      args: object({ id: blockId }, ["id"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2 }) => ({ blocks: { rm: [id2] } })
    }),
    command({
      id: "blockr.core-rename_block",
      label: "Rename block",
      description: "Gives a block a new name.",
      args: object({ id: blockId, name: name2 }, ["id", "name"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run: (_board, { id: id2, name: name3 }) => ({ blocks: { mod: { [id2]: { name: name3 } } } })
    }),
    command({
      id: "blockr.core-set_block",
      label: "Set block state",
      description: "Sets values of a block's state args, which the schema of its class types, and unsets the state args named, as a block's UI submits an edit.",
      args: object(
        {
          id: blockId,
          state: { ...state, description: "Values of the block's state args to set." },
          unset: { ...stateArgs, description: "Names of the block's state args to unset." }
        },
        ["id"]
      ),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.blocks, id2),
      run(board2, { id: id2, state: state2, unset }) {
        const block = own(board2.blocks, id2);
        if (block === void 0) return [];
        const entry = classOf2(this.id, block.registry_id);
        const before = new Set(stateFailures(entry, block.state));
        const failures = stateFailures(entry, {
          ...without(block.state, unset ?? []),
          ...state2
        }).filter((failure) => !before.has(failure));
        if (failures.length > 0) {
          throw refusal(this.id, failures.map((failure) => `The state of block "${id2}" ${failure}`));
        }
        return {
          blocks: {
            mod: {
              [id2]: { ...state2 === void 0 ? {} : { state: state2 }, ...unset === void 0 ? {} : { unset } }
            }
          }
        };
      }
    }),
    command({
      id: "blockr.core-append_block",
      label: "Append block",
      description: "Adds a block that takes the result of another, through a new link from that block into an input of the new block.",
      args: object(
        {
          from: { ...blockId, description: "The block whose result the new block takes." },
          ...newBlock,
          input: {
            ...input,
            description: "The input of the new block that takes the result, which may be left out where the new block has one input or is variadic."
          }
        },
        ["from", "registry_id"]
      ),
      enabled: (board2, { from }) => Object.hasOwn(board2.blocks, from),
      run(board2, args) {
        const [id2, block, entry] = added(this.id, board2, args);
        const input2 = newInput(this.id, entry, args.input);
        const link = mintId((id3) => Object.hasOwn(board2.links, id3));
        return {
          blocks: { add: { [id2]: block } },
          links: { add: { [link]: { from: args.from, to: id2, input: input2 } } }
        };
      }
    }),
    command({
      id: "blockr.core-prepend_block",
      label: "Prepend block",
      description: "Adds a block whose result another takes, through a new link from the new block into an input of that block.",
      args: object(
        {
          to: { ...blockId, description: "The block that takes the new block's result." },
          ...newBlock,
          input: {
            ...input,
            description: "The input of that block that takes the result, which may be left out where only one of its inputs is free or it is variadic."
          }
        },
        ["to", "registry_id"]
      ),
      enabled: (board2, { to, input: input2 }) => linkable(board2, to, input2),
      run(board2, args) {
        const input2 = linkInput(this.id, board2, args.to, args.input);
        const [id2, block] = added(this.id, board2, args);
        const link = mintId((id3) => Object.hasOwn(board2.links, id3));
        return {
          blocks: { add: { [id2]: block } },
          links: { add: { [link]: { from: id2, to: args.to, input: input2 } } }
        };
      }
    }),
    command({
      id: "blockr.core-insert_block",
      label: "Insert block",
      description: "Adds a block on a link, which it replaces with a link from the link's source into an input of the new block and one from the new block into the input that the link fed.",
      args: object(
        {
          link: { ...linkId, description: "The link that the new block goes on." },
          ...newBlock,
          input: {
            ...input,
            description: "The input of the new block that takes the result of the link's source, which may be left out where the new block has one input or is variadic."
          }
        },
        ["link", "registry_id"]
      ),
      enabled: (board2, { link }) => Object.hasOwn(board2.links, link),
      run(board2, args) {
        const split = own(board2.links, args.link);
        if (split === void 0) return [];
        const [id2, block, entry] = added(this.id, board2, args);
        const input2 = newInput(this.id, entry, args.input);
        const near = mintId((id3) => Object.hasOwn(board2.links, id3));
        const far = mintId((id3) => Object.hasOwn(board2.links, id3) || id3 === near);
        return {
          blocks: { add: { [id2]: block } },
          links: {
            rm: [args.link],
            add: {
              [near]: { from: split.from, to: id2, input: input2 },
              [far]: { from: id2, to: split.to, input: split.input }
            },
            before: { [far]: args.link }
          }
        };
      }
    })
  ];
  const links = [
    command({
      id: "blockr.core-add_link",
      label: "Add link",
      description: "Adds a link, through which a block takes the result of another as one of its inputs.",
      args: object(
        {
          from: { ...blockId, description: "The block whose result the link carries." },
          to: { ...blockId, description: "The block that takes the result." },
          input: {
            ...input,
            description: "The input of the block that takes the result, which may be left out where only one of its inputs is free or it is variadic."
          },
          id: {
            ...linkId,
            description: "The new link's id, which is minted where it is left out."
          }
        },
        ["from", "to"]
      ),
      enabled: (board2, { from, to, input: input2 }) => Object.hasOwn(board2.blocks, from) && linkable(board2, to, input2),
      run(board2, { from, to, input: input2, id: id2 }) {
        const link = id2 ?? mintId((id3) => Object.hasOwn(board2.links, id3));
        const resolved = linkInput(this.id, board2, to, input2);
        return { links: { add: { [link]: { from, to, input: resolved } } } };
      }
    }),
    command({
      id: "blockr.core-change_link",
      label: "Change link",
      description: "Changes the block a link comes from, the block it goes to or the input it feeds, keeping its id.",
      args: object(
        {
          id: linkId,
          from: { ...blockId, description: "The block whose result the link carries." },
          to: { ...blockId, description: "The block that takes the result." },
          input: {
            ...input,
            description: "The input of the block that takes the result, which the link keeps where it is left out, unless the link goes to another block, where it may be left out only if one input of that block is free or the block is variadic."
          }
        },
        ["id"]
      ),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.links, id2),
      run(board2, args) {
        const link = own(board2.links, args.id);
        if (link === void 0) return [];
        const to = args.to ?? link.to;
        const input2 = args.input === void 0 && to === link.to ? link.input : linkInput(this.id, board2, to, args.input, args.id);
        return { links: { mod: { [args.id]: { from: args.from ?? link.from, to, input: input2 } } } };
      }
    }),
    command({
      id: "blockr.core-remove_link",
      label: "Remove link",
      description: "Removes a link.",
      args: object({ id: linkId }, ["id"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.links, id2),
      run: (_board, { id: id2 }) => ({ links: { rm: [id2] } })
    })
  ];
  const stacks = [
    command({
      id: "blockr.core-add_stack",
      label: "Add stack",
      description: "Adds a stack of blocks, taking each out of the stack it is in, since a block sits in one stack at most.",
      args: object({
        id: {
          ...stackId,
          description: "The new stack's id, which is minted where it is left out."
        },
        name: { ...name2, description: `The new stack's name, "Stack" where it is left out.` },
        blocks: { ...blockIds, description: "The blocks of the new stack, in order." },
        color: {
          ...color,
          description: "The new stack's colour, which front-ends draw it in, or null for none. The stack gets null where this is left out, and a front-end draws a stack without a colour in a default of its own."
        }
      }),
      run(board2, { id: id2, name: name3 = "Stack", blocks: blocks2 = [], color: color2 = null }) {
        const stack = id2 ?? mintId((id3) => Object.hasOwn(board2.stacks, id3));
        const left2 = leave(board2, stack, blocks2);
        return {
          stacks: {
            add: { [stack]: { name: name3, blocks: blocks2, color: color2 } },
            ...Object.keys(left2).length === 0 ? {} : { mod: left2 }
          }
        };
      }
    }),
    command({
      id: "blockr.core-change_stack",
      label: "Change stack",
      description: "Changes the blocks of a stack or its colour, taking each block out of the stack it is in, since a block sits in one stack at most.",
      args: object(
        {
          id: stackId,
          blocks: {
            ...blockIds,
            description: "The blocks of the stack, in order, which it keeps where they are left out."
          },
          color: {
            ...color,
            description: "The stack's colour, which front-ends draw it in, or null to clear it. A front-end draws a stack without a colour in a default of its own. The stack keeps its colour where this is left out."
          }
        },
        ["id"]
      ),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (board2, { id: id2, blocks: blocks2, color: color2 }) => ({
        stacks: {
          mod: {
            ...blocks2 === void 0 ? {} : leave(board2, id2, blocks2),
            [id2]: {
              ...blocks2 === void 0 ? {} : { blocks: blocks2 },
              ...color2 === void 0 ? {} : { color: color2 }
            }
          }
        }
      })
    }),
    command({
      id: "blockr.core-rename_stack",
      label: "Rename stack",
      description: "Gives a stack a new name.",
      args: object({ id: stackId, name: name2 }, ["id", "name"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (_board, { id: id2, name: name3 }) => ({ stacks: { mod: { [id2]: { name: name3 } } } })
    }),
    command({
      id: "blockr.core-remove_stack",
      label: "Remove stack",
      description: "Removes a stack, leaving its blocks on the board.",
      args: object({ id: stackId }, ["id"]),
      enabled: (board2, { id: id2 }) => Object.hasOwn(board2.stacks, id2),
      run: (_board, { id: id2 }) => ({ stacks: { rm: [id2] } })
    })
  ];
  const board = [
    command({
      id: "blockr.core-rename_board",
      label: "Rename board",
      description: "Gives the board a new name.",
      args: object({ name: name2 }, ["name"]),
      run: (_board, { name: name3 }) => ({ name: name3 })
    }),
    command({
      id: "blockr.core-set_option",
      label: "Set board option",
      description: "Sets the value of a board option, which the board option's schema types.",
      args: object({ id: optionId, value }, ["id", "value"]),
      enabled: (_board, { id: id2 }) => own(store.options.registered(), id2) !== void 0,
      run: (_board, { id: id2, value: value2 }) => ({ options: { set: { [id2]: value2 } } })
    })
  ];
  const plugins = [
    command({
      id: "blockr.core-set_plugin_state",
      label: "Set plugin state",
      description: "Changes a plugin's state by patches at paths within that state, as JSON Patch has them, which the schema of the plugin's class types. It is not enabled on a locked board, where only the commands of a plugin's class know what the plugin locks.",
      args: object(
        {
          plugin: { ...pluginId, description: "The plugin whose state the patches change." },
          patches: statePatches
        },
        ["plugin", "patches"]
      ),
      enabled: (board2, { plugin }) => options.locked !== true && Object.hasOwn(board2.plugins, plugin),
      run: (_board, { plugin, patches: patches2 }) => ({
        patches: patches2.map((patch2) => ({ ...patch2, path: `${pathOf(["plugins", plugin, "state"])}${patch2.path}` }))
      })
    })
  ];
  return [...blocks, ...links, ...stacks, ...board, ...plugins];
}
function freeInputs(board, registry, to, except) {
  const block = own(board.blocks, to);
  if (block === void 0) return void 0;
  const inputs = own(registry, block.registry_id)?.inputs;
  if (inputs === void 0) return void 0;
  if (inputs.includes("...")) return [""];
  const fed = new Set(
    Object.entries(board.links).filter(([id2, link]) => id2 !== except && link.to === to).map(([, link]) => link.input)
  );
  return inputs.filter((input2) => !fed.has(input2));
}
function blockName(registryId2) {
  const pkg = packageOf(registryId2);
  const name3 = pkg === null ? registryId2 : registryId2.slice(pkg.length + 1);
  return sentenceCase(name3.replace(/_block$/, ""));
}
function leave(board, id2, blocks) {
  const moving = new Set(blocks);
  return Object.fromEntries(
    Object.entries(board.stacks).filter(([other, stack]) => other !== id2 && stack.blocks.some((block) => moving.has(block))).map(([other, stack]) => [
      other,
      { blocks: stack.blocks.filter((block) => !moving.has(block)) }
    ])
  );
}
function object(properties, required2 = []) {
  return { type: "object", properties, required: required2, additionalProperties: false };
}
function named(input2) {
  return input2 !== void 0 && input2 !== "";
}
function one(command2, inputs, none, several) {
  const [input2, ...rest] = inputs;
  if (input2 === void 0) throw refusal(command2, [none]);
  if (rest.length > 0) throw refusal(command2, [several]);
  return input2;
}
function command(def) {
  return {
    ...def,
    run: def.run.bind(def),
    ...def.enabled === void 0 ? {} : { enabled: def.enabled.bind(def) }
  };
}

// srcts/commands/commands.ts
var CommandError = class extends Error {
  failures;
  constructor(message, failures, options) {
    super([message, ...failures.map((failure) => `- ${failure}`)].join("\n"), options);
    this.name = "CommandError";
    this.failures = failures;
  }
};
function refusal(id2, failures) {
  return new CommandError(`The command "${id2}" refused to run`, failures);
}
function createCommands(store, options = {}) {
  const commands = y(/* @__PURE__ */ new Map());
  const registered = O(
    () => Object.fromEntries([...commands.value].map(([id2, { command: command2 }]) => [id2, command2]))
  );
  const ended = () => options.signal?.aborted === true;
  function register(command2) {
    const { id: id2 } = command2;
    if (typeof id2 !== "string" || !isCommandId(id2)) {
      throw new Error(
        `The command id ${JSON.stringify(id2)} is not named as a registry id is, "{pkg}-{name}".`
      );
    }
    if (typeof command2.label !== "string" || command2.label === "") {
      throw new Error(`The command "${id2}" has no label.`);
    }
    if (typeof command2.run !== "function") {
      throw new Error(`The command "${id2}" has no run().`);
    }
    if (commands.value.has(id2)) {
      throw new Error(`The command "${id2}" is registered already.`);
    }
    const validate2 = command2.args === void 0 ? void 0 : compile(`the args of the command "${id2}"`, command2.args, "fail their schema");
    const item = { command: command2, validate: validate2 };
    commands.value = new Map([...commands.value, [id2, item]]);
    return () => {
      if (commands.value.get(id2) !== item) return;
      const rest = new Map(commands.value);
      rest.delete(id2);
      commands.value = rest;
    };
  }
  function lookup(id2, given) {
    const item = commands.value.get(id2);
    if (item === void 0) {
      throw refusal(id2, [`No command "${id2}" is registered.`]);
    }
    const args = given ?? {};
    const failures = item.validate?.(args) ?? [];
    if (failures.length > 0) {
      throw refusal(id2, failures.map((failure) => `The args ${failure}`));
    }
    return { command: item.command, args };
  }
  function current2(id2) {
    if (ended()) {
      throw refusal(id2, ["Another board opened in a new store, so this command registry runs no command."]);
    }
    if (!store.hasBoard()) {
      throw refusal(id2, ["The store holds no board until it opens one."]);
    }
    return store.board();
  }
  function allow(command2, board, args) {
    const enabled = attempt2(command2, () => command2.enabled?.(board, args) ?? true);
    if (!enabled) {
      throw refusal(command2.id, [
        `It is not enabled on the board as it is, with the args ${JSON.stringify(args)}.`
      ]);
    }
  }
  function build(command2, board, args) {
    const returned = attempt2(command2, () => command2.run(board, args));
    const deltas2 = isDeltas(returned) ? returned : [returned];
    return deltas2.map(withEdits);
  }
  function deltas(id2, given) {
    const { command: command2, args } = lookup(id2, given);
    const board = current2(id2);
    allow(command2, board, args);
    if (command2.prepare === void 0) return build(command2, board, args);
    return prepared(command2, board, args);
  }
  async function prepared(command2, board, args) {
    let input2;
    try {
      input2 = await command2.prepare?.(board, args);
    } catch (error) {
      throw failed(command2, error);
    }
    if (ended()) {
      throw refusal(command2.id, ["A board opened while it prepared, which drops it."]);
    }
    const now = current2(command2.id);
    allow(command2, now, args);
    return build(command2, now, input2);
  }
  function withEdits(delta) {
    const blocks = delta.blocks?.mod;
    const patches2 = delta.patches;
    if (blocks === void 0 && patches2 === void 0) return delta;
    const edit = (change, id2) => change.edit !== void 0 || id2 === void 0 ? change : { ...change, edit: id2 };
    return {
      ...delta,
      ...blocks === void 0 ? {} : {
        blocks: {
          ...delta.blocks,
          mod: Object.fromEntries(
            Object.entries(blocks).map(([id2, mod]) => [
              id2,
              edit(mod, store.editId({ block: id2 }))
            ])
          )
        }
      },
      ...patches2 === void 0 ? {} : {
        patches: patches2.map((patch2) => {
          const plugin = pluginOf(patch2.path);
          return plugin === void 0 ? patch2 : edit(patch2, store.editId({ plugin }));
        })
      }
    };
  }
  const registry = {
    register,
    registered: () => registered.value,
    enabled(id2, given) {
      try {
        const { command: command2, args } = lookup(id2, given);
        allow(command2, current2(id2), args);
        return true;
      } catch {
        return false;
      }
    },
    async run(id2, given, origin = "user") {
      const step2 = deltas(id2, given);
      store.apply(Array.isArray(step2) ? step2 : await step2, origin);
    },
    async plan(id2, given) {
      return deltas(id2, given);
    }
  };
  for (const command2 of coreCommands(store, { locked: (options.lock ?? null) !== null })) {
    registry.register(command2);
  }
  return registry;
}
function isCommandId(id2) {
  const pkg = packageOf(id2);
  return pkg !== null && id2.length > pkg.length + 1;
}
function failed(command2, error) {
  if (error instanceof CommandError) return error;
  return new CommandError(`The command "${command2.id}" failed`, [messageOf(error)], {
    cause: error
  });
}
function attempt2(command2, fn) {
  try {
    return fn();
  } catch (error) {
    throw failed(command2, error);
  }
}

// srcts/core/start.ts
var CAPABILITIES = ["cancel", "interrupt", "progress"];
var BASIC = "blockr.core-basic";
function start(options) {
  const page = new AbortController();
  const fail = options.fail ?? ((error) => {
    report(error);
  });
  let first;
  let latest;
  let running2;
  const holdsBoard = () => running2?.store.hasBoard() === true;
  let restarting;
  const nextRevision = counter();
  let settle2;
  const ready = new Promise((resolve, reject) => {
    settle2 = { resolve, reject };
  });
  ready.catch(() => void 0);
  const client = createClient(options.transport, {
    hello: () => ({ protocol: PROTOCOL_VERSION, capabilities: CAPABILITIES }),
    session(reply) {
      heard(reply);
    }
  });
  const blockrOptions = createBlockrOptions(client);
  const loader = createLoader({ ...options.loader, modules: () => latest?.modules ?? {} });
  function heard(reply) {
    if (page.signal.aborted) return;
    const error = helloError(reply);
    if (error !== void 0) {
      if (first === void 0) {
        settle2?.reject(error);
        fail(error);
      } else if (restarting !== void 0) {
        const waiting2 = restarting;
        restarting = void 0;
        waiting2.reject(error);
      } else {
        report(error);
      }
      return;
    }
    const hello = reply;
    latest = hello;
    if (first === void 0) {
      first = hello;
      boot(hello, void 0).then(
        ({ core }) => settle2?.resolve(core),
        (failure) => {
          settle2?.reject(failure);
          fail(asError(failure));
        }
      );
      return;
    }
    const changed = changedPackages(first, hello);
    if (changed.length > 0) (options.reload ?? askReload)(changed);
    if (restarting !== void 0) {
      const waiting2 = restarting;
      restarting = void 0;
      waiting2.resolve(hello);
      return;
    }
    running2?.scheduler.session();
  }
  async function boot(reply, later) {
    const controller = new AbortController();
    const { signal } = controller;
    const ending = () => {
      controller.abort();
    };
    if (page.signal.aborted) ending();
    page.signal.addEventListener("abort", ending, { once: true });
    signal.addEventListener(
      "abort",
      () => {
        page.signal.removeEventListener("abort", ending);
      },
      { once: true }
    );
    const [evaluationQueue, backgroundEvaluationDelay, gateVisibility] = await Promise.all([
      blockrOptions.read("evaluation_queue", 2),
      blockrOptions.read("background_evaluation_delay", Infinity),
      blockrOptions.read("gate_visibility", true)
    ]);
    const store = createStore({
      hello: reply,
      loaded: () => prepare(),
      signal
    });
    const commands = createCommands(store, { signal, lock: reply.lock });
    for (const option of options.options({ blockrOption: (name3, fallback) => blockrOptions.read(name3, fallback) })) {
      store.options.register(option);
    }
    const bound = later === void 0 ? reply.bound : void 0;
    const scheduler = createScheduler({
      store,
      client,
      evaluationQueue,
      backgroundEvaluationDelay,
      gateVisibility,
      nextRevision,
      signal,
      ...bound === void 0 ? {} : { bound }
    });
    const core = createCore({
      store: {
        ...store,
        open: (given) => reopen(given, controller)
      },
      commands,
      scheduler,
      loader,
      blockrOptions,
      lock: (first ?? reply).lock,
      renderers: () => latest?.renderers ?? {},
      signal,
      ...options.measure === void 0 ? {} : { measure: options.measure }
    });
    running2 = { store, scheduler, core, controller };
    const named2 = await initialize(reply, core, signal, later === void 0);
    let dropped = [];
    let failure;
    if (later === void 0) {
      const instead = chosen(reply, named2);
      if (instead === void 0) {
        dropped = await store.open(reply);
      } else {
        try {
          dropped = await store.open(instead);
        } catch (error) {
          report(error);
          dropped = await store.open(reply);
        }
      }
    } else {
      try {
        dropped = await store.open(later.text);
      } catch (error) {
        failure = asError(error);
        try {
          await store.open(later.before);
        } catch (again) {
          report(again);
          throw failure;
        }
      }
    }
    try {
      await options.frontEnds?.render(core, signal);
    } finally {
      scheduler.start();
    }
    if (failure !== void 0) throw failure;
    return { core, dropped };
  }
  async function initialize(reply, core, signal, naming) {
    const named2 = [];
    for (const entry of reply.startup_modules) {
      if (signal.aborted) break;
      try {
        const source = entry.css === void 0 ? { module: entry.module } : { module: entry.module, css: entry.css };
        const mod = startupOf(await loader.load(source, entry.package), entry.module, entry.package);
        let result;
        try {
          result = await mod.initialize({ core, signal });
        } catch (error) {
          throw new ModuleError(entry.module, entry.package, `failed in initialize(): ${messageOf(error)}`, { cause: error });
        }
        if (typeof result === "string") {
          if (naming) {
            named2.push({ package: entry.package, text: result });
          } else {
            report(
              new Error(
                `The startup module of ${entry.package} named a board to open as another board opened, which core ignores, since only a startup module that runs as the page starts names one.`
              )
            );
          }
        } else if (result !== void 0) {
          throw new ModuleError(entry.module, entry.package, "resolved initialize() with something other than the text of a JSON board.");
        }
      } catch (error) {
        report(error);
      }
    }
    return named2;
  }
  function chosen(reply, named2) {
    const [only, ...more] = named2;
    if (only === void 0) return void 0;
    const packages = quoted(named2.map((entry) => entry.package));
    if (more.length > 0) {
      report(new Error(`The startup modules of ${packages} each named a board to open, so core opens the board that R sent.`));
      return void 0;
    }
    if (reply.lock !== null) {
      report(new Error(`The board is locked, so core opens the board that R sent rather than the one that ${only.package} named.`));
      return void 0;
    }
    try {
      JSON.parse(only.text);
    } catch (error) {
      report(new Error(`The startup module of ${only.package} named a board that holds no JSON, so core opens the board that R sent: ${messageOf(error)}`));
      return void 0;
    }
    return only.text;
  }
  async function prepare() {
    await options.frontEnds?.prepare({ id: BASIC, source: own(latest?.modules ?? {}, BASIC) }, loader);
  }
  let queue = Promise.resolve();
  function reopen(given, of) {
    const run = queue.then(() => reopening(given, of));
    queue = run.catch(() => void 0);
    return run;
  }
  async function reopening(given, of) {
    const refused2 = "The store refused to open the board";
    const current2 = running2;
    if (current2?.controller !== of || of.signal.aborted) {
      throw new StoreError(refused2, ["Another board opened since, so this store opens no board."]);
    }
    if (!current2.store.hasBoard()) {
      throw new StoreError(refused2, ["The store holds no board yet, so it opens none in place of it."]);
    }
    const text = typeof given === "string" ? given : JSON.stringify(given.board);
    await current2.store.check(text);
    const before = current2.store.save();
    of.abort();
    running2 = void 0;
    try {
      const reply = await afresh();
      const { dropped } = await boot(reply, { text, before });
      return dropped;
    } catch (error) {
      if (!page.signal.aborted && !holdsBoard()) fail(asError(error));
      throw error;
    }
  }
  function afresh() {
    return new Promise((resolve, reject) => {
      if (page.signal.aborted) {
        reject(new Error("Core has stopped."));
        return;
      }
      restarting = { resolve, reject };
      client.hello();
    });
  }
  return {
    ready,
    core: () => running2?.core,
    stop() {
      page.abort();
      const waiting2 = restarting;
      restarting = void 0;
      waiting2?.reject(new Error("Core has stopped."));
    }
  };
}
function helloError(reply) {
  if (reply.status !== "ok" || !("startup_modules" in reply)) {
    const said = "conditions" in reply ? reply.conditions.map((c2) => c2.message).join(" ") : "";
    return new Error(`R replied ${reply.status} to the client's hello. ${said}`.trim());
  }
  const theirs = reply.protocol.split(".")[0];
  const ours = PROTOCOL_VERSION.split(".")[0];
  if (theirs !== ours) {
    return new Error(
      `R speaks version ${reply.protocol} of the wire protocol, where this page speaks ${PROTOCOL_VERSION}. Reload the page, which then loads the client that R serves.`
    );
  }
  return void 0;
}
function changedPackages(first, reply) {
  return Object.entries(reply.packages).flatMap(([pkg, now]) => {
    const loaded = own(first.packages, pkg);
    return loaded === void 0 || loaded === now ? [] : [{ package: pkg, loaded, now }];
  });
}
function askReload(changed) {
  report(
    new Error(
      `R now runs ${changed.map((c2) => `${c2.package} ${c2.now}`).join(", ")}, where the page loaded ${changed.map((c2) => `${c2.package} ${c2.loaded}`).join(", ")}. Reload the page.`
    )
  );
}
function startupOf(exports, module, pkg) {
  if (!isObject(exports) || typeof exports.api !== "number") {
    throw new ModuleError(module, pkg, "declares no major of core's JS API, as `export const api = 1`.");
  }
  if (exports.api !== API) {
    throw new ModuleError(
      module,
      pkg,
      `was built against major ${String(exports.api)} of core's JS API, where core's is ${String(API)}.`
    );
  }
  const mod = exports.default;
  if (!isObject(mod) || typeof mod.initialize !== "function") {
    throw new ModuleError(module, pkg, "has no default export with an initialize() function.");
  }
  return mod;
}

// srcts/page/page.ts
function startPage(options) {
  const { el } = options;
  el.replaceChildren(loadingNote());
  return start({
    transport: options.transport ?? shinyTransport(),
    ...options.loader === void 0 ? {} : { loader: options.loader },
    ...options.measure === void 0 ? {} : { measure: options.measure },
    options: coreOptions,
    frontEnds: pageFrontEnds(el),
    reload(changed) {
      reloadBanner(changed);
    },
    fail(error) {
      showError(el, error.message);
      report(error);
    }
  });
}
function reloadBanner(changed) {
  const id2 = "blockr-core-reload";
  document.getElementById(id2)?.remove();
  const banner = document.createElement("div");
  banner.id = id2;
  banner.setAttribute("role", "alert");
  banner.className = "bc:fixed bc:inset-x-0 bc:top-0 bc:z-50 bc:flex bc:items-center bc:gap-3 bc:border-b bc:border-border-default bc:bg-white bc:p-3 bc:font-sans bc:text-sm bc:text-black";
  const text = document.createElement("span");
  const versions = changed.map((c2) => `${c2.package} ${c2.loaded} to ${c2.now}`).join(", ");
  text.textContent = `R now runs other versions of packages than this page loaded (${versions}). Save the board, then reload the page.`;
  const button = document.createElement("button");
  button.type = "button";
  button.className = "bc:rounded-md bc:border bc:border-border-default bc:px-2 bc:py-1";
  button.textContent = "Reload";
  button.addEventListener("click", () => {
    location.reload();
  });
  banner.append(text, button);
  document.body.append(banner);
}
export {
  SessionEndedError,
  createClient,
  shinyTransport,
  startPage
};
